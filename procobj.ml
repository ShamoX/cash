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

(* XXX mettre ailleurs. *)
value all_interrupts =
  [Sys.sigabrt; Sys.sigalrm; Sys.sigfpe; Sys.sighup; Sys.sigill; Sys.sigint; Sys.sigkill;
   Sys.sigpipe; Sys.sigquit; Sys.sigsegv; Sys.sigterm; Sys.sigusr1; Sys.sigusr2;
   Sys.sigchld; Sys.sigcont; Sys.sigstop; Sys.sigtstp; Sys.sigttin; Sys.sigttou;
   Sys.sigvtalrm; Sys.sigprof]
;

value with_no_interrupts f =
  let old_mask = Unix.sigprocmask Unix.SIG_SETMASK all_interrupts in
  let r = f () in
  do { ignore (Unix.sigprocmask Unix.SIG_SETMASK old_mask); r }
;

(* Weak table for process_table and reaped_procs. *)

type weak_pt 'p = { pt_used : mutable int; pt_t : mutable Weak.t 'p };

value check_used pt =
  if pt.pt_used < 0 || pt.pt_used > Weak.length pt.pt_t then
    invalid_arg ("check_used: bad pt_used: " ^ string_of_int pt.pt_used)
  else ()
;

value decr_used pt = do { pt.pt_used := pred pt.pt_used; check_used pt };
value incr_used pt = do { pt.pt_used := succ pt.pt_used; check_used pt };

(* Assure que pt_used suit un élément plein. *)
value adjust_pt_used pt =
  while pt.pt_used > 0 && Weak.get pt.pt_t (pred pt.pt_used) = None do { decr_used pt }
;

(* Remplace l'élément i (vide) par le dernier élément plein (un peu avant pt_used). *)
(* On calcule p avant le 1er if pour faire un safe get à tout coup. *)
value decr_pt pt i =
  do {
    adjust_pt_used pt;
    let p = Weak.get pt.pt_t i in
    if pt.pt_used <= i then ()
    else if p <> None then
      invalid_arg ("decr_pt: non-empty subject element at " ^ string_of_int i)
    else do {
      decr_used pt;
      Weak.set pt.pt_t i (Weak.get pt.pt_t pt.pt_used);
      Weak.set pt.pt_t pt.pt_used None
    }
  }
;

(* Chooser sélectionne un elt parmi ceux valides de pt. *)
(* Ne tasse pas. *)
value pt_find pt chooser =
  loop (pred pt.pt_used) where rec loop i =
    if i < 0 then (i, None)
    else
      let elt = Weak.get pt.pt_t i in
      if match elt with
         [ Some proc -> chooser proc
         | _ -> False ] then
        (i, elt)
      else loop (pred i)
;

value pt_mem pt chooser = let (_, elt) = pt_find pt chooser in elt <> None;

(* 1.62 ~= nombre d'or.  Implique d'allouer 2 éléments au départ. *)
value grow_pt pt =
  let new_t = Weak.create (pt.pt_used * 162 / 100) in
  do { Weak.blit pt.pt_t 0 new_t 0 pt.pt_used; pt.pt_t := new_t }
;

value make_pt () = {pt_used = 0; pt_t = Weak.create 2};

(* Result is: compression has succeeded, no need to grow. *)
value compress_pt pt =
  loop 0 where rec loop i =
    if i >= pt.pt_used then pt.pt_used < Weak.length pt.pt_t
    else if Weak.get pt.pt_t i = None then do { decr_pt pt i; loop i }
    else loop (succ i)
;

value add_to_pt pt elt =
  do {
    if pt.pt_used < Weak.length pt.pt_t then ()
    else if compress_pt pt then ()
    else grow_pt pt;
    Weak.set pt.pt_t pt.pt_used elt;
    incr_used pt
  }
;

(* ;; This is a GC'd abstraction for Unix process id's.
   ;; The problem with Unix pids is (a) they clutter up the kernel process table
   ;; until you wait(2) them, and (b) you can only wait(2) them once. Scsh's
   ;; process objects are similar, but allow the storage to be allocated in the
   ;; scsh address space, and out of the kernel process table, and they can be
   ;; waited on multiple times.
   *)

(* ;; Process objects. *)
type proc = { p_id : int; p_status : mutable option Unix.process_status };

type process_table = weak_pt proc;

(* ;; Indexing this table by pid requires a linear scan. 
   ;; Probably not an important op, tho. *)
value proc_table = make_pt ();

value pid_is pid proc = proc.p_id = pid;

value add_to_proc_table p =
  if pt_mem proc_table (fun proc -> proc == p) then ()
  else if pt_mem proc_table (pid_is p.p_id) then
    failwith ("add_to_proc_table: pid already there: " ^ string_of_int p.p_id)
  else add_to_pt proc_table (Some p)
;

(* for optional proc arguments defaulting to pid 0. *)
value pid_of_proc_maybe =
  fun
  [ None -> 0
  | Some proc -> proc.p_id ]
;

type probe_pid =
  [ Probe
  | Create
  | Don't_probe ]
;

value proc_of_pid ?(probe = Don't_probe) pid =
  match pt_find proc_table (pid_is pid) with
  [ (_, None) ->
      match probe with
      [ Probe -> None
      | Create ->
          let p = {p_id = pid; p_status = None} in
          do { add_to_proc_table p; Some p }
      | Don't_probe ->
          failwith ("Pid has no corresponding process object: " ^ string_of_int pid) ]
  | (_, x) -> x ]
;

(* Internal usage: cf (low_)wait*. *)
value hard_proc_of_pid pid =
  match pt_find proc_table (pid_is pid) with
  [ (_, None) ->
      failwith ("Pid has no corresponding process object: " ^ string_of_int pid)
  | (_, Some proc) -> proc ]
;

(* ;; Reaped process table
;; We keep track of procs that have been reaped but not yet waited on by the
;; user's code. These processes are eligible for return by WAIT-ANY.  We keep
;; track of these so that WAIT-ANY will hand them out exactly once.  Whenever
;; WAIT, WAIT-ANY, WAIT-PROCESS-GROUP waits on a process to exit, it removes the
;; process from this table if it's in it.  This code is bogus -- we use weak
;; pointers. We need populations that support deletion or filtering.
 *)

value reaped_procs = make_pt ();

value clear_reaped_procs () =
  do {
    Weak.fill reaped_procs.pt_t 0 (Weak.length reaped_procs.pt_t) None;
    reaped_procs.pt_used := 0
  }
;

(* ;; Add a newly-reaped proc to the list. *)
(* the warning fun is applied w/interrupts enabled. *)
value add_reaped_proc pid status =
  let f =
    with_no_interrupts
      (fun () ->
         match pt_find proc_table (pid_is pid) with
         [ (_, None) ->
             fun () ->
               prerr_endline
                 ("Exiting child pid has no proc object: " ^ string_of_int pid)
         | (_, (Some proc as p)) ->
             do { proc.p_status := Some status; add_to_pt reaped_procs p; ignore } ])
  in
  f ()
;

(* ;; Pop one off the list. *)
value get_reaped_proc () =
  with_no_interrupts
    (fun () ->
       match pt_find reaped_procs (fun p -> True) with
       [ (_, None) -> None
       | (i, proc) -> do { Weak.set reaped_procs.pt_t i None; proc } ])
;

(* ;; PROC no longer eligible to be in the list. Delete it. *)
(* The _ case is OK when autoreap_policy = No_autoreaping. *)
value mark_proc_waited proc =
  with_no_interrupts
    (fun () ->
       match pt_find reaped_procs (fun p -> p == proc) with
       [ (i, Some _) -> Weak.set reaped_procs.pt_t i None
       | _ -> () ])
;
(* 
 ;; The mark-proc-waited! machinery above is a crock. It is inefficient --
 ;; we should have a flag in the proc saying if it's eligible for a WAIT-ANY.
 ;; Starts off #t, changes to #f after a wait. On a #t->#f transition, we
 ;; delete it from the WAIT-ANY population. Right now, every time the user
 ;; waits on the proc, we re-delete it from the population -- which is
 ;; a no-op after the first time. *)
