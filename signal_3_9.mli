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

value signal_process : Procobj.proc -> int -> unit;
value signal_process_pid : int -> int -> unit;
value signal_process_group : Procobj.proc -> int -> unit;
value signal_process_group_pgrp : int -> int -> unit;

value itimer :
  ?newstat: Unix.interval_timer_status -> Unix.interval_timer ->
    Unix.interval_timer_status;
value pause_until_interrupt : unit -> unit;

value sleep : int -> unit;
value sleep_until : float -> unit;
