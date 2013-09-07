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

value signal_process proc = Unix.kill proc.Procobj.p_id;
value signal_process_pid = Unix.kill;

value signal_process_group procgroup = Unix.kill (- procgroup.Procobj.p_id);
value signal_process_group_pgrp prgrp = Unix.kill (- prgrp);

value pause_until_interrupt = Unix.pause;

value itimer ?newstat timer =
  match newstat with
  [ None -> Unix.getitimer timer
  | Some stat -> Unix.setitimer timer stat ]
;

value sleep = Unix.sleep;

value rec sleep_until time =
  let now = Unix.time () in
  let delta = time -. now in
  if delta <= 0. then ()
  else
    let retry = try do { ignore (Unix.select [] [] [] delta); False } with _ -> True in
    if retry then sleep_until time else ()
;
