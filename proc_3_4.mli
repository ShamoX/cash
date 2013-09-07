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

value low_exec : string -> ?env: (list (string * string)) -> list string -> unit;

value exec : string -> list string -> unit;
value exec_with_env : string -> ?env: (list (string * string)) -> list string -> unit;

value exec_path_search : string -> list string -> string;
value exec_path : string -> list string -> 'a;
value exec_path_with_env : string -> ?env: (list (string * string)) -> list string -> 'a;

(* C'est toujours None, sinon ça ne revient pas.  Mais rendre option 'a
   simplifie le typage de really_fork *)
value call_terminally : option (unit -> unit) -> option 'a;

value suspend : unit -> unit;

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

type wait_pid =
  [ Pid_not_ready | Status of process_status | Retry_pid | Recheck_pid of exn ]
;

type proc = Procobj.proc == { p_id : int; p_status : mutable option process_status };

type probe_pid =
  Procobj.probe_pid ==
    [ Probe
    | Create
    | Don't_probe ]
;

exception Child_not_ready;

value wait : ?wflags: (list wait_flag) -> proc -> process_status;

value wait_pid : ?wflags: (list wait_flag) -> int -> process_status;

type wait_any =
  [ None_ready
  | No_children
  | Exited of (proc * process_status) ]
;

value wait_any : ?wflags: (list wait_flag) -> unit -> wait_any;

value wait_process_group : ?wflags: (list wait_flag) -> proc -> wait_any;

value wait_process_group_pgrp : ?wflags: (list wait_flag) -> int -> wait_any;

value proc_of_pid : ?probe: probe_pid -> int -> option proc;

value pid_of_proc : proc -> int;

value reap_zombies : unit -> bool;

type autoreap_policy = [ No_autoreaping | Early | Late ];

value autoreap_policy : ?policy: autoreap_policy -> unit -> autoreap_policy;

(* value really_fork : ?child: (unit -> unit) -> bool -> option proc; *)

value low_fork : ?child: (unit -> unit) -> unit -> option proc;

value fork : unit -> option proc;
value fork_child : (unit -> unit) -> proc;

value really_fork_with_pipe : ?child: (unit -> unit) -> (unit -> option 'b) -> option 'b;

value low_fork_with_pipe : ?child: (unit -> unit) -> unit -> option proc;

value fork_with_pipe : unit -> option proc;
value fork_child_with_pipe : (unit -> unit) -> proc;

value low_fork_with_pipe_plus :
  ?child: (unit -> 'a) -> list (list Io_3_2.fd) -> option proc;

value fork_with_pipe_plus : list (list Io_3_2.fd) -> option proc;
value fork_child_with_pipe_plus : (unit -> 'a) -> list (list Io_3_2.fd) -> proc;
