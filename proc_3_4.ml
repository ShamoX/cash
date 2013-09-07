(***********************************************************************)
(*                                Cash                                 *)
(*                                                                     *)
(*          Bruno Verlyck, projet Cristal, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License.          *)
(*                                                                     *)
(*  Cash is based on Scsh, by Olin Shivers.                            *)
(***********************************************************************)
include
  (Unix :
   sig
     type process_status =
       Unix.process_status ==
         [ WEXITED of int
         | WSIGNALED of int
         | WSTOPPED of int ]
     ;
     type wait_flag =
       Unix.wait_flag ==
         [ WNOHANG
         | WUNTRACED ]
     ;
   end);

include
  (Procobj :
   sig
     type proc =
       Procobj.proc == { p_id : int; p_status : mutable option process_status }
     ;
     type probe_pid =
       Procobj.probe_pid ==
         [ Probe
         | Create
         | Don't_probe ]
     ;
     value proc_of_pid : ?probe: probe_pid -> int -> option proc;
     value pid_of_proc_maybe : option Procobj.proc -> int;
   end);

(* ;; EXEC support
 ;; Assumes a low-level %exec procedure:
 ;; (%exec prog arglist env)
 ;;   ENV is either #t, meaning the current environment, or a string->string
 ;;       alist.
 ;;   %EXEC stringifies PROG and the elements of ARGLIST. *)

value low_exec prog ?env arglist =
  let args = Array.of_list arglist in
  match env with
  [ None -> Unix.execv prog args
  | Some env -> Unix.execve prog args (Env_3_11.env_array_of_alist env) ]
;

value exec_path_search prog pathlist =
  if Strings_5_1.is_file_name_absolute prog then
    if Io_3_3.is_file_executable_fn prog then prog else raise Not_found
  else
    let slash_prog = "/" ^ prog in
    List.find (fun dir -> Io_3_3.is_file_executable_fn (dir ^ slash_prog)) pathlist ^
      slash_prog
;

value exec_with_env prog ?env args =
  do { Io_3_2.flush_all_chans (); low_exec prog ?env [prog :: args] }
;

value exec prog args =
  exec_with_env prog args
;

(* 
;; This procedure is bummed by tying in directly to %%exec/errno and pulling
;; some of %exec's code out of the inner loop so that the inner loop will be
;; fast. Folks don't like waiting...
Note: This loop is also necessary to close the race condition described in the
documentation for exec_path_search.
Note: We don't use Unix.execvp[e] because we don't want to muck with $PATH
value in C's *environ; cf scsh documentation § 3.11.2
 *)
value exec_path_with_env prog ?env args =
  do {
    if Env_3_11.internal_index '/' prog 0 >= 0 then exec_with_env prog ?env args
    else
      let slash_prog = "/" ^ prog
      and args = Array.of_list [prog :: args] in
      let execer =
        match env with
        [ None -> fun dir -> Unix.execv (dir ^ slash_prog) args
        | Some env ->
            let env = Env_3_11.env_array_of_alist env in
            fun dir -> Unix.execve (dir ^ slash_prog) args env ]
      in
      List.iter
        (fun dir ->
           try execer dir with
           [ Unix.Unix_error Unix.ENOENT "execv" _ |
             Unix.Unix_error Unix.ENOENT "execve" _ ->
               () ])
        (Env_3_11.exec_path_list ());
    failwith (String.concat " " ["exec_path: No executable found:"; prog :: args])
  }
;

value exec_path prog args = exec_path_with_env prog args;

(*
 ;; Call THUNK, then die.
 ;; A clever definition in a clever implementation allows the caller's stack
 ;; and dynamic env to be gc'd away, since this procedure never returns.
 O'Caml is not clever enough here. *)
value call_terminally =
  fun
  [ None -> None
  | Some thunk -> do { thunk (); exit 0 } ]
;

(* XXX.
value suspend () = signal_process 0 Sys.sigstop
 *)
value suspend () = Unix.kill 0 Sys.sigstop;

(* 3.4.3.  Analysing process status codes. *)

(* Useless. *)

(* 3.4.2.  Process waiting. *)

type wait_pid =
  [ Pid_not_ready | Status of process_status | Retry_pid | Recheck_pid of exn ]
;

(* ;; (%wait-pid pid flags) (%wait-any flags) (%wait-process-group pgrp flags)
;; Direct interfaces to waitpid(2) call.
;; [#f #f] means no processes ready on a non-blocking wait.     -> None_ready
;; [#f #t] means no waitable process on wait-any.               -> No_children
*)

value low_wait_pid_once pid wflags =
  try
    let (pid, status) = Unix.waitpid wflags pid in
    if pid = 0 then Pid_not_ready else Status status
  with
  [ Unix.Unix_error Unix.EINTR _ _ -> Retry_pid ]
;

value low_wait_pid pid wflags =
  loop () where rec loop () =
    match low_wait_pid_once pid wflags with
    [ Retry_pid -> loop ()
    | r -> r ]
;

type low_wait_any =
  [ Low_none_ready
  | Low_exited of (int * process_status)
  | Low_no_children
  | Low_retry
  | Low_recheck of exn ]
;

value low_wait_any_once wflags =
  try
    let (pid, status) = Unix.waitpid wflags (-1) in
    if pid = 0 then Low_none_ready else Low_exited (pid, status)
  with
  [ Unix.Unix_error Unix.ECHILD _ _ -> Low_no_children
  | Unix.Unix_error Unix.EINTR _ _ -> Low_retry ]
;

value low_wait_any wflags =
  loop () where rec loop () =
    match low_wait_any_once wflags with
    [ Low_retry -> loop ()
    | r -> r ]
;

value low_wait_process_group pgrp wflags =
  loop () where rec loop () =
    match
      try
        let (pid, status) = Unix.waitpid wflags (- pgrp) in
        if pid = 0 then Low_none_ready else Low_exited (pid, status)
      with
      [ Unix.Unix_error Unix.ECHILD _ _ -> Low_no_children
      | Unix.Unix_error Unix.EINTR _ _ -> Low_retry ]
    with
    [ Low_retry -> loop ()
    | r -> r ]
;
(* ;; This list contains procs that haven't exited yet. 
;; FORK adds new procs to the list. When a proc exits, it is removed from the
;; list.  Being on this list keeps live children's proc objects from being gc'd.
 *)
value unexited_procs = ref [];

value new_child_proc pid =
  let proc1 = {p_id = pid; p_status = None} in
  do {
    Procobj.add_to_proc_table proc1;
    unexited_procs.val := [proc1 :: unexited_procs.val];
    proc1
  }
;

value mark_proc_exited proc =
  (* Sanity check. *)
  if List.memq proc unexited_procs.val then
    unexited_procs.val := List.filter ( \<> proc) unexited_procs.val
  else
    failwith ("mark_proc_exited: proc not in unexited_procs: " ^ string_of_int proc.p_id)
;

(* Do nothing if proc isn't dead; else cache its status and make proc gc'able. *)
value cache_wait_status proc status =
  match status with
  [ WSTOPPED _ -> status
  | _ -> do { proc.p_status := Some status; mark_proc_exited proc; status } ]
;

exception Child_not_ready;

(* ;; (wait proc/pid [flags]) => status or #f
;; FLAGS (default 0) is the exclusive or of the following:
;;      wait/poll       
;;              Return #f immediately if there are no 
;;              unwaited children available. 
;;      wait/stopped-children
;;              Report on suspended children as well.
;;
;;      If the process hasn't terminated (or suspended, if wait/stopped is set)
;;      and wait/poll is set, return #f.
-- we raise Child_not_ready
;; WAIT waits for a specific process. Before performing a waitpid(2) systcall,
;; wait first consults the proc object to see if the process has been reaped
;; already. If so, its saved status is returned immediately.
 *)

(* win makes proc not eligible for a WAIT-ANY. *)
(* First match sees if we've already waited or reaped the process. *)
(* About Recheck: I don't see in what circumstances this can happen; scsh code
   just says:
  ;; We got an error -- before reporting it, check
  ;; the proc record one last time.. *)

value wait ?(wflags = []) proc =
  let win status = do { Procobj.mark_proc_waited proc; status } in
  let rec loop () =
    match proc.p_status with
    [ None ->
        match
          try low_wait_pid_once proc.p_id wflags with
          [ Unix.Unix_error _ _ _ as err -> Recheck_pid err ]
        with
        [ Pid_not_ready -> raise Child_not_ready
        | Status status -> win (cache_wait_status proc status)
        | Recheck_pid err ->
            match proc.p_status with
            [ None -> raise err
            | Some status -> win status ]
        | Retry_pid -> loop () ]
    | Some status -> win status ]
  in
  loop ()
;

value wait_pid ?wflags pid =
  wait ?wflags (Procobj.hard_proc_of_pid pid);

type wait_any =
  [ None_ready | No_children | Exited of (proc * process_status) ]
;

(* Wait-any: essentially try get_reaped_proc() before returning anything but Exited. *)
value wait_any ?(wflags = []) () =
  let make_exited fname proc =
    Exited
      (proc,
       match proc.p_status with
       [ None ->
           failwith
             ("wait_any: " ^ fname ^ " yields a proc with no status: " ^
                string_of_int proc.p_id)
       | Some status -> status ])
  in
  let try_reaped_proc no_proc_fun =
    match Procobj.get_reaped_proc () with
    [ Some proc -> make_exited "get_reaped_proc" proc
    | None -> no_proc_fun () ]
  in
  let rec loop () =
    try_reaped_proc
      (fun () ->
         match
           try low_wait_any_once wflags with
           [ Unix.Unix_error _ _ _ as err -> Low_recheck err ]
         with
         [ Low_recheck err -> try_reaped_proc (fun () -> raise err)
         | Low_retry -> try_reaped_proc loop
         | Low_none_ready as x -> try_reaped_proc (fun () -> None_ready)
         | Low_no_children -> try_reaped_proc (fun () -> No_children)
         | Low_exited (pid, status) ->
             let proc = Procobj.hard_proc_of_pid pid in
             do {
               ignore (cache_wait_status proc status);
               make_exited "low_wait_any_once" proc
             } ])
  in
  loop ()
;

(* ; If you are doing process-group waits, you do *not* want to use 
  ;; early autoreaping, since the reaper loses process-group information. *)
value wait_process_group_pgrp ?(wflags = []) proc_group =
  match low_wait_process_group proc_group wflags with
  [ Low_exited (pid, status) ->
      let proc = Procobj.hard_proc_of_pid pid in
      do { ignore (cache_wait_status proc status); Exited (proc, status) }
  | Low_none_ready -> None_ready
  | Low_no_children -> No_children
  | _ -> assert False ]
;

value wait_process_group ?wflags proc = wait_process_group_pgrp ?wflags proc.p_id;

(* 3.4.1.  Process objects ans process reaping.
   Cf procobj too. *)

value pid_of_proc p = p.p_id;

(* ;; Process reaping
;; "Reaping" a process means using wait(2) to move its exit status from the
;; kernel's process table into scsh, thus cleaning up the kernel's process
;; table and saving the value in a gc'd data structure, where it can be
;; referenced multiple times.
;;
;; - Stopped processes are never reaped, only dead ones.
;; 
;; - Stopped process status codes are never cached in proc objects, 
;;   only status codes for dead processes. So you can wait for a
;;   dead process multiple times, but only once per process-stop.
;; 
;; - Unfortunately, reaping a process loses the information specifying its
;;   process group, so if a process is reaped into scsh, it cannot be
;;   waited for by WAIT-PROCESS-GROUP. Notice that only dead processes are
;;   reaped, not suspended ones. Programs almost never use WAIT-PROCESS-GROUP
;;   to wait for dead processes, so this is not likely to be a problem. If
;;   it is, turn autoreaping off with (autoreap-policy #f).
;; 
;; - Reaping can be encouraged by calling (REAP-ZOMBIES).
 *)

value reap_zombies =
  loop where rec loop () =
    match low_wait_any [WNOHANG] with
    [ Low_exited (proc, status) -> do { Procobj.add_reaped_proc proc status; loop () }
    | Low_none_ready -> False
    | Low_no_children -> True
    | _ -> assert False ]
;

type autoreap_policy =
  [ No_autoreaping
  | Early
  | Late ]
;

value autoreap_policy =
  let _autoreap_policy_ = ref No_autoreaping in
  fun ?policy () ->
    match policy with
    [ None -> _autoreap_policy_.val
    | Some policy ->
        if policy = Late then failwith "Autoreap policy Late not implemented"
        else do {
          let old_policy = _autoreap_policy_.val in
          _autoreap_policy_.val := policy;
          ignore
            (Sys.signal Sys.sigchld
               (Sys.Signal_handle
                  (if policy = Early then fun _ -> ignore (reap_zombies ())
                   else ignore)));
          old_policy
        } ]
;

(* Initialize sigchld handling. *)
autoreap_policy ~policy:Early ();

(* XXX Qu'est-ce que ça fait ?? *)
value set_batch_mode batch = Sys.interactive.val := not batch;

value really_fork ?child clear_interactive =
  let f =
    Procobj.with_no_interrupts
      (fun () ->
         let pid = Unix.fork () in
         if pid = 0 then
           fun () ->
             do {
               Procobj.clear_reaped_procs ();
               if clear_interactive then set_batch_mode True else ();
               call_terminally child
             }
         else
           let proc = new_child_proc pid in
           fun () -> Some proc)
  in
  f ()
;

value unsome =
  fun
  [ None -> assert False
  | Some x -> x ]
;

value low_fork ?child () = really_fork ?child False;

value fork () = do { Io_3_2.flush_all_chans (); really_fork ?child:None True };
value fork_child child =
  do { Io_3_2.flush_all_chans (); unsome (really_fork ~child True) }
;

(* Pourquoi ne synchronise-t-on pas les ports/canaux stdout/stdin ici ?  C'est
   dans la documentation: Note that these procedures affect file descriptors,
   not channels *)
value really_fork_with_pipe ?child forker =
  let (r, w) = Io_3_2.pipe () in
  match forker () with
  [ None ->
      (* Child. *)
      do {
        Io_3_2.ignoring_close_in r;
        Io_3_2.ignoring_move_out_channel_to_fdes w 1;
        call_terminally child
      }
  | some_proc ->
      (* Parent. *)
      do {
        Io_3_2.ignoring_close_out w;
        ignore (Io_3_2.move_in_channel_to_fdes r 0);
        some_proc
      } ]
;

value low_fork_with_pipe ?child () = really_fork_with_pipe ?child (low_fork ?child:None);

value fork_with_pipe () = really_fork_with_pipe ?child:None fork;
value fork_child_with_pipe child = unsome (really_fork_with_pipe ~child fork);

(* XXX (low,}fork_with_pipe_plus. *)

value really_fork_with_pipe_plus ?child conns forker =
  let pipes = List.map (fun _ -> Io_3_2.pipe ()) conns
  and rev_conns = List.map List.rev conns in
  let (tos, froms) =
    List.split
      (List.map
         (fun
          [ [infd :: outfds] -> (infd, List.rev outfds)
          | _ ->
              raise (Failure "fork_with_pipe_plus: not enough elements in connection") ])
         rev_conns)
  in
  match forker () with
  [ None ->
      (* Child. *)
    do {
        List.iter2
          (fun from_fds (r, w) ->
             do {
               ignore (Io_3_2.close_in r);
               List.iter (fun from_fd -> ignore (Io_3_2.fdes_of_dup_out ~newfd:from_fd w))
                 from_fds;
               (* ; Unrevealed ports win. *)
               ignore (Io_3_2.close_out w)
             })
          froms pipes;
        call_terminally child
      }
  | some_proc ->
      (* Parent. *)
      do {
        List.iter2
          (fun to_fd (r, w) ->
             do {
               ignore (Io_3_2.close_out w);
               ignore (Io_3_2.move_in_channel_to_fdes r to_fd)
             })
          tos pipes;
        some_proc
      } ]
;

value low_fork_with_pipe_plus ?child conns =
  really_fork_with_pipe_plus ?child conns (low_fork ?child:None)
;

value fork_with_pipe_plus conns = really_fork_with_pipe_plus ?child:None conns fork;
value fork_child_with_pipe_plus child conns =
  unsome (really_fork_with_pipe_plus ~child conns fork)
;
