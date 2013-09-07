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

(* Autre test: run_with_string a bien stdout sur le fd 1. *)
value test_run () =
  let s =
    run_with_string
      (fun () ->
         do {
           print_endline "stdout";
           output_string (out_channel_of_fd 1) (string_of_int (fd_of_out_channel stdout))
         })
  in
  print_endline s
;

value test_with_stdout () =
  do {
    print_endline "Go!";
    flush stdout;
    with_stdout (open_out "/tmp/test_with_stdout")
      (fun () ->
         do { output_string (out_channel_of_fd 1) "from inside\n"; print_endline "foo" });
    print_endline "Gone";
    set_stdout (open_out "/tmp/test_with_stdout1");
    print_int (fd_of_out_channel stdout);
    print_newline ();
    let out = out_channel_of_fd 1;
    prerr_int (fd_of_out_channel out);
    flush out;
    (* ^^ flushe jusqu'à Gone. *)
    prerr_newline ();
    (* ^^ flushe le n° de fd de out. *)
    set_stdout out;
    (* ^^ remet stdout en place. *)
    test_run ()
  }
;

if Sys.interactive.val then () else 
test_with_stdout();
