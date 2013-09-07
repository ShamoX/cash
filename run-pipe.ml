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
open Cash;

(* Ou mettre Early. *)
value test_run_with_port_plus_proc () =
  do {
    ignore (autoreap_policy ~policy:No_autoreaping ());
    let (chan, child_proc) =
      run_with_inchan_plus_proc
        (fun () -> do { print_string "turlututu chapeau pointu\n"; exit 1 })
    ;
    Unix.sleep 2;
    with_stdin chan (char_filter Char.uppercase);
    print_endline
      (match wait child_proc with
       [ WEXITED exit_val -> "Exit: " ^ string_of_int exit_val
       | WSIGNALED signum -> "Signal: " ^ string_of_int signum
       | WSTOPPED _ -> "Oh, my subprocess is stopped?" ])
  }
;

if Sys.interactive.val then ()
else Unix.handle_unix_error test_run_with_port_plus_proc ();
