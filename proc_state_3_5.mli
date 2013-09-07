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

value umask : unit -> int;
value set_umask : int -> unit;
value with_umask : int -> (unit -> 'a) -> 'a;

value chdir : ?dir:string -> unit -> unit;
value cwd : unit -> string;
value with_cwd : string -> (unit -> 'a) -> 'a;

value pid : unit -> int;
value parent_pid : unit -> int;
value process_group : unit -> int;
value set_process_group : ?proc: Procobj.proc -> int -> unit;
value set_process_group_pid : int -> int -> unit;

type prio = [ Prio_process | Prio_pgrp | Prio_user ];
value set_priority : ?who: Procobj.proc -> prio -> int -> unit;
value set_priority_pid : int -> prio -> int -> unit;
value priority : ?who: Procobj.proc -> prio -> int;
value priority_pid : int -> prio -> int;
value nice : ?proc: Procobj.proc -> int -> unit;
value nice_pid : int -> int -> unit;

value user_login_name : unit -> string;
value user_uid : unit -> int;
value user_effective_uid : unit -> int;

value user_gid : unit -> int;
value user_effective_gid : unit -> int;
value user_supplementary_gids : unit -> array int;

value set_uid : int -> unit;
value set_gid : int -> unit;

value process_times : unit -> Unix.process_times;
value cpu_ticks_per_sec : unit -> int;
