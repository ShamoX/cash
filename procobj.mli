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
(* value all_interrupts : list int; *)

value with_no_interrupts : (unit -> 'a) -> 'a;

type proc = { p_id : int; p_status : mutable option Unix.process_status };

value add_to_proc_table : proc -> unit;

type probe_pid = [ Probe | Create | Don't_probe ];

value pid_of_proc_maybe : option proc -> int;

value proc_of_pid : ?probe: probe_pid -> int -> option proc;

value hard_proc_of_pid : int -> proc;

value clear_reaped_procs : unit -> unit;

value add_reaped_proc : int -> Unix.process_status -> unit;

value get_reaped_proc : unit -> option proc;

value mark_proc_waited : proc -> unit;
