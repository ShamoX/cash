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

value set_umask perms = ignore (Unix.umask perms);

value umask () = let m = Unix.umask 0 in do { set_umask m; m };

value with_umask perms thunk =
  let old_mask = Unix.umask perms in
  Env_3_11.unwind_protect thunk set_umask old_mask
;

value chdir ?(dir = Env_3_11.home_directory.val) () = Unix.chdir dir;
value cwd = Unix.getcwd;

value with_cwd dir thunk =
  let old_working_dir = cwd () in
  do { Unix.chdir dir; Env_3_11.unwind_protect thunk Unix.chdir old_working_dir }
;

value pid = Unix.getpid;
value parent_pid = Unix.getppid;

(* XXX passer dans Unix. *)
external process_group : unit -> int = "unix_getpgrp";
external setpgid : int -> int -> unit = "unix_setpgid";

(* scsh uses (pid) as default value, but man says 0 does the same. *)
value set_process_group ?proc pgrp = setpgid (Procobj.pid_of_proc_maybe proc) pgrp;

(* No default; use set_process_group if you want one. *)
value set_process_group_pid pid pgrp = setpgid pid pgrp;

type prio = [ Prio_process | Prio_pgrp | Prio_user ];
external low_set_priority : prio -> int -> int -> unit = "unix_setpriority";
external low_priority : prio -> int -> int = "unix_priority";

value set_priority ?who which prio =
  low_set_priority which (Procobj.pid_of_proc_maybe who) prio
;

value set_priority_pid who which prio = low_set_priority which who prio;

value priority ?who which = low_priority which (Procobj.pid_of_proc_maybe who);

value priority_pid who which = low_priority which who;

value nice_pid pid delta =
  low_set_priority Prio_process pid (delta + low_priority Prio_process pid)
;

value nice ?proc = nice_pid (Procobj.pid_of_proc_maybe proc);

value user_uid = Unix.getuid;
value user_gid = Unix.getgid;

value user_login_name () =
  try Unix.getlogin () with
  [ Unix.Unix_error Unix.ENOENT _ _ -> (Unix.getpwuid (user_uid ())).Unix.pw_name ]
;

value user_effective_uid = Unix.geteuid;
value user_effective_gid = Unix.getegid;
value user_supplementary_gids = Unix.getgroups;

value set_uid = Unix.setuid;
value set_gid = Unix.setgid;

value process_times = Unix.times;

external cpu_ticks_per_sec : unit -> int = "cpu_clock_ticks_per_sec";
