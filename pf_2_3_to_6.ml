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

value fold_in_channel (ichan : in_channel) reader op =
  fold where rec fold seed =
    match try Some (reader ichan) with [ End_of_file -> None ] with
    [ Some s -> fold (op s seed)
    | None -> seed ]
;

(* ;; Read items from PORT with READER until EOF. Collect items into a list. *)
value list_of_in_channel reader ichan =
  List.rev (fold_in_channel ichan reader (fun car cdr -> [car :: cdr]) [])
;

(* ;; Read characters from PORT until EOF, collect into a string. *)
value string_of_in_channel ichan =
  String.concat ""
    (list_of_in_channel (fun ichan -> Io_3_2.read_string_in ~src:ichan 4096) ichan)
;
(* This is possible and faster, but stresses memory/GC a lot more (and would
   need call_with_fdes_in and limits tests)
 value string_of_fd fd = Io_3_2.read_string ~src:fd Sys.max_string_length;. *)

value string_list_of_in_channel = list_of_in_channel input_line;

value sexp_list_of_in_channel = list_of_in_channel Sexp.read;

value run_with_inchan_plus_proc thunk =
  let (r, w) = Io_3_2.pipe () in
  let proc =
    Proc_3_4.fork_child
      (fun () ->
         do {
           Io_3_2.ignoring_close_in r;
           Io_3_2.ignoring_move_out_channel_to_fdes w 1;
           Io_3_2.with_stdout w thunk
         })
  in
  do { Io_3_2.ignoring_close_out w; (r, proc) }
;

value run_with_outchan_plus_proc thunk =
  let (r, w) = Io_3_2.pipe () in
  let proc =
    Proc_3_4.fork_child
      (fun () ->
         do {
           Io_3_2.ignoring_close_out w;
           Io_3_2.ignoring_move_in_channel_to_fdes r 0;
           Io_3_2.with_stdin r thunk
         })
  in
  do { Io_3_2.ignoring_close_in r; (w, proc) }
;

value run_with_in_channel thunk =
  let (ichan, _) = run_with_inchan_plus_proc thunk in
  ichan
;

value run_with_out_channel thunk =
  let (ochan, _) = run_with_outchan_plus_proc thunk in
  ochan
;

value run_with_string thunk =
  Io_3_2.close_in_after (run_with_in_channel thunk) string_of_in_channel
;

value run_with_sexp thunk =
  Io_3_2.close_in_after (run_with_in_channel thunk) Sexp.read
;

value run_with_strings thunk =
  Io_3_2.close_in_after (run_with_in_channel thunk) string_list_of_in_channel
;

value run_with_sexps thunk =
  Io_3_2.close_in_after (run_with_in_channel thunk) sexp_list_of_in_channel
;

(* XXX scsh implémente en syntaxe de processus; REVOIR. *)
value run_with_file thunk =
  let fname = Io_3_3.create_temp_file () in
  let proc =
    Proc_3_4.fork_child
      (fun () ->
         let ochan = open_out fname in
         do { Io_3_2.with_stdout ochan thunk; Io_3_2.ignoring_close_out ochan })
  in
  do { ignore (Proc_3_4.wait proc); fname }
;

(* XXX idem. *)
value run_with_collecting fds thunk =
  (* ; First, generate a pair of ports for each communications channel.
    ;; Each channel buffers through a temp file.. *)
  let (ichans, ochans) =
    let chans = List.map (fun _ -> Io_3_3.temp_file_channel ()) fds in
    (List.map fst chans, List.map snd chans)
  in
  let proc =
    Proc_3_4.fork_child
      (fun () ->
         do {
           (* ; In a subprocess, close the read ports, redirect input from
             ;; the write ports, and run THUNK.. *)
           List.iter Io_3_2.ignoring_close_in ichans;
           List.iter2 Io_3_2.ignoring_move_out_channel_to_fdes ochans fds;
           thunk ()
         })
  in
  (* ; In this process, close the write ports and return the exit status
    ;; and all the read ports.. *)
  do { List.iter Io_3_2.ignoring_close_out ochans; (Proc_3_4.wait proc, ichans) }
;

value string_filter ?(buflen = 1024) filter =
  let buf = String.create buflen in
  let rec loop () =
    let nread = try Io_3_2.read_string_bang buf with [ End_of_file -> 0 ] in
    if nread = 0 then ()
    else
      loop
        (print_string (filter (if nread = buflen then buf else String.sub buf 0 nread)))
  in
  loop
;

value char_filter filter =
  string_filter
    (fun s -> do { for i = 0 to String.length s - 1 do { s.[i] := filter s.[i] }; s })
;

(* Should be in Io_3_2. *)
(* 3.2.3.  String ports, *)
value make_string_in_channel s =
  let (ichan, ochan) = Io_3_3.temp_file_channel () in
  do { output_string ochan s; Io_3_2.ignoring_close_out ochan; ichan }
;

value string_out_channel_htbl = Hashtbl.create 20;

value make_string_out_channel () =
  let (ichan, ochan) = Io_3_3.temp_file_channel () in
  do { Hashtbl.add string_out_channel_htbl ochan ichan; ochan }
;

value string_out_channel_output ?(close = False) ochan =
  try
    let ichan = Hashtbl.find string_out_channel_htbl ochan in
    do {
      try flush ochan with [ Sys_error _ -> () ];
      Pervasives.seek_in ichan 0;
      let s = string_of_in_channel ichan in
      if close then do {
        try Io_3_2.ignoring_close_out ochan with [ Sys_error _ -> () ];
        Io_3_2.ignoring_close_in ichan;
        Hashtbl.remove string_out_channel_htbl ochan
      }
      else ();
      s
    }
  with
  [ Not_found -> invalid_arg "string_out_channel_output: not a string_out_channel" ]
;

value call_with_string_out_channel ?close proc =
  let ochan = make_string_out_channel () in
  do { proc ochan; string_out_channel_output ?close ochan }
;
