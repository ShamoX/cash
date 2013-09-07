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

type handle_delim =
  [ Trim
  | Peek
  | Concat ]
; (* | Split: would give incompatible results, so we define special <fun>_split *)

(* For Rec_field_8. *)
value no_peek name = invalid_arg (name ^ ": ~handle_delim:Peek is illegal");

(* XX fournir une interface avec délimiteur = regexp est défendable (awk sait
   faire, perl non), mais assez compliqué: on peut scanner le buffer du channel,
   si on trouve, c'est OK, sinon, il faut accumuler, et chercher là, car le
   délimiteur peut être à cheval sur 2 buffers du channel (donc un seul scan
   dans le buffer est utile, et il faut pouvoir reculer, on espère dans le
   dernier paquet lu, sauf Peek...  Ooouupps). *)

(* ;; (%read-delimited! delims buf gobble? [port start end])
;; This low-level routine uses a different interface. It returns two values:
;; - TERMINATOR: A value describing why the read was terminated:
;;   + character or eof-object => read terminated by this value; 
;;   + #f                      => filled buffer w/o terminating read.
;; - NUM-READ: Number of chars read into buf.
;; 
;; Note:
;; - Invariant: TERMINATOR = #f  =>  NUM-READ = END - START.
;; - Invariant: TERMINATOR = eof-object and NUM-READ = 0 => at EOF.
;; - When determining the TERMINATOR return value, ties are broken
;;   favoring character or the eof-object over #f. That is, if the buffer
;;   fills up, %READ-DELIMITED! will peek at one more character from the
;;   input stream to determine if it terminates the input. If so, that
;;   is returned, not #f.
;;
;; If GOBBLE? is true, then a terminator character is removed from
;; the input stream. Otherwise, it is left in place for a following input
;; operation.
 *)
(* NOTE: do we break ties like scsh ? *)
type termination_kind =
  [ Eof
  | Read of char
  | Full_buffer ]
;

external unsafe_input : in_channel -> string -> int -> int -> int = "caml_input";
external read_delimited_scan :
  in_channel -> Charset_14.t -> int = "cash_read_delimited_scan";

(* This way is from Pervasives. *)
(* XXX Mettre unsafe_input quand tout aura été couvert ? *)
value unsafe_low_read_delimited_bang chan start end_ delims buf handle_delim =
  let end_1 = if handle_delim = Concat then end_ else succ end_ in
  let rec loop start total =
    let n = read_delimited_scan chan delims in
    if n = 0 then (Eof, total)
    else if if n > 0 then n > end_1 - start else - n >= end_ - start then
      (* Not enough space: Full. *)
      let _ = input chan buf start (end_ - start) in
      (Full_buffer, total + end_ - start)
    else if n > 0 then do {
      let len =(* There's enough space. *)
        if handle_delim = Concat then n else pred n;
      ignore (input chan buf start len);
      let total = total + len;
      let delim = if handle_delim = Concat then buf.[pred len] else input_char chan;
      if handle_delim = Peek then seek_in chan (pred (pos_in chan)) else ();
      (Read delim, total)
    }
    else
      (* Found no delim, buf not full, must loop. *)
      let _ = input chan buf start (- n) in
      loop (start - n) (total - n)
  in
  loop start 0
;

value low_read_delimited_bang ?(chan = stdin) ?(start = 0) ?end_ delims buf handle_delim =
  let end_ = Strings_5_1.opt_end buf end_
  and delims = Charset_14.to_charset delims in
  do {
    Strings_5_1.check_substring_spec buf start end_ "low_read_delimited_bang";
    unsafe_low_read_delimited_bang chan start end_ delims buf handle_delim
  }
;

(* ;; (read-delimited! delims buf [port delim-action start end])
;; Returns:
;; - EOF if at end of file, and a non-zero read was requested.
;; - Integer j if that many chars read into BUF.
;; - #f if the buffer was filled w/o finding a delimiter.
;;
;; DELIM-ACTION determines what to do with the terminating delimiter;
;; it is as in READ-DELIMITED.
;;
;; In determining the return value, there is an ambiguous case: when the 
;; buffer is full, *and* the following char is a delimiter char or EOF.
;; Ties are broken favoring termination over #f -- after filling the buffer,
;; READ-DELIMITED! won't return #f until it has peeked one past the end
;; of the buffer to ensure the next char doesn't terminate input (or is EOF).
;; However, this rule is relaxed with delim-action = CONCAT -- if the buffer
;; is full, READ-DELIMITED! won't wait around trying to peek at the following
;; char to determine whether or not it is a delimiter char, since it doesn't
;; have space to store the character anyway. It simply immediately returns #f;
;; a following read can pick up the delimiter char.
*)
(* NOTE: do we break ties like scsh ? *)

value read_delimited_bang_split ?chan ?start ?end_ delims buf =
  match low_read_delimited_bang ?chan ?start ?end_ delims buf Trim with
  [ (Eof, 0) -> raise End_of_file
  | (Full_buffer, _) -> None
  | (term, n) -> Some (n, term) ]
;

value read_delimited_bang ?chan ?(handle_delim = Trim) ?(start = 0) ?end_ delims buf =
  let end_ =
    (* We'll try to add one char if Full_buffer. *)
    if handle_delim <> Concat then end_ else Some (pred (Strings_5_1.opt_end buf end_))
  in
  match low_read_delimited_bang ?chan ~start ?end_ delims buf handle_delim with
  [ (Eof, 0) -> raise End_of_file
  | (Eof, n) -> Some n
  | (Read c, nread) -> Some nread
  | (Full_buffer, nread) ->
      match handle_delim with
      [ Concat ->
          try
            let c = input_char (Arg_3_7.default_value chan stdin) in
            do {
              buf.[start + nread] := c;
              if Charset_14.contains (Charset_14.to_charset delims) c then
                Some (succ nread)
              else None
            }
          with
          [ End_of_file -> Some nread ]
      | _ -> None ] ]
;

value cons_revconc s =
  fun
  [ [] -> (* ; Gratuitous opt. *) s
  | [str] -> (* More gratuitous opt. *) str ^ s
  | strs -> String.concat "" (List.rev [s :: strs]) ]
;

(* ;; (read-delimited delims [port delim-action])
;; Returns a string or the EOF object. DELIM-ACTION determines what to do
;; with the terminating delimiter:
;; - PEEK
;;   Leave it in the input stream for later reading.
;; - TRIM (the default)
;;   Drop it on the floor.
;; - CONCAT
;;   Append it to the returned string.
;; - SPLIT
;;   Return it as a second return value.
;;
;; We repeatedly allocate a buffer and fill it with READ-DELIMITED!
;; until we hit a delimiter or EOF. Each time through the loop, we
;; double the total buffer space, so the loop terminates with a log
;; number of reads, but uses at most double the optimal buffer space.
 *)

(* This way is from Pervasives. *)

(* As read_delimited_scan tries to make the line fit in the i/o buffer, n < 0
   happens when the line length is over the buffer's one; this is very uncommon.
   But: we don't want quadratic behavior if there's no delimiter in the file. *)
value by_chunks chan handle_delim delims len =
  let make_chunk len =
    let chunk = String.create len in
    do { ignore (unsafe_input chan chunk 0 len); chunk }
  in
  let rec loop prev_chunk chunks =
    let n = read_delimited_scan chan delims in
    if n = 0 then cons_revconc prev_chunk chunks
    else if n > 0 then do {
      let len = if handle_delim = Concat then n else pred n in
      let last_chunk = String.create len in
      ignore (unsafe_input chan last_chunk 0 len);
      if handle_delim = Trim then ignore (input_char chan) else ();
      cons_revconc last_chunk [prev_chunk :: chunks]
    }
    else loop (make_chunk (- n)) [prev_chunk :: chunks]
  in
  loop (make_chunk len) []
;

value one_rec delims handle_delim chan =
  let n = read_delimited_scan chan delims in
  if n = 0 then raise End_of_file
  else if n > 0 then do {
    let len = if handle_delim = Concat then n else pred n in
    let line = String.create len in
    ignore (unsafe_input chan line 0 len);
    if handle_delim = Trim then ignore (input_char chan) else ();
    line
  }
  else by_chunks chan handle_delim delims (- n)
;

value one_rec_split delims chan =
  try
    let line = one_rec delims Peek chan in
    try (line, Read (input_char chan)) with [ End_of_file -> (line, Eof) ]
  with
  [ End_of_file -> ("", Eof) ]
;

value read_delimited ?(chan = stdin) ?(handle_delim = Trim) delims =
  one_rec (Charset_14.to_charset delims) handle_delim chan
;

value read_delimited_split ?(chan = stdin) delims =
  one_rec_split (Charset_14.to_charset delims) chan
;

value cs_newline_i = Charset_14.of_char '\n';
(* For Rec_field_8. *)
value cs_newline = Charset_14.Cset cs_newline_i;

(* Don't use unnecessary optionals, nor force conversion to_charset; so use
   cs_newline_i, and don't call read_delimited.  This is *way* cheaper. *)
value read_line_split = one_rec_split cs_newline_i;

(* For Rec_field_8. *)
value one_line = one_rec cs_newline_i;

value read_line ?(handle_newline = Trim) = one_line handle_newline;

(* XXX fournir une interface pour passer la définition de la ligne délimiteur
   (par défaut la proc is_white). *)
value (read_paragraph_split, read_paragraph) =
  (* Est-ce bien efficace ?  Si non, revoir read_line aussi *)
  let one_line_c = one_line Concat
  and skip = Charset_14.skip Charset_14.whitespace in
  let is_white line =
    let len = String.length line in
    skip line 0 len = len
  in
  let rec skip_blank_lines chan =
    let line = one_line_c chan in
    (* End_of_file OK here. *)
    if is_white line then skip_blank_lines chan else line
  in
  let low_read_paragraph make_res chan =
    collect (skip_blank_lines chan) [] where rec collect prev_line lines =
      let line = try one_line_c chan with [ End_of_file -> "" ] in
      if is_white line then make_res line prev_line lines
      else collect line [prev_line :: lines]
  in
  (fun chan ->
     low_read_paragraph (fun line prev_line lines -> (cons_revconc prev_line lines, line))
       chan,
   fun ?(handle_delim = Trim) chan ->
     low_read_paragraph
       (fun line prev_line lines ->
          match handle_delim with
          [ Trim -> cons_revconc prev_line lines
          | Concat ->
              if line = "" then cons_revconc prev_line lines
              else cons_revconc line [prev_line :: lines]
          | Peek -> no_peek "read_paragraph" ])
       chan)
;

value skip_char_set ?(chan = stdin) skip_chars =
  let cset = Charset_14.to_charset skip_chars
  and pos = pos_in chan
  and buf = String.create 200 in
  let rec loop total =
    let upto = input chan buf 0 200 in
    let delim_index = Charset_14.skip cset buf 0 upto in
    let total = total + delim_index in
    if upto = 0 || delim_index < upto then do { seek_in chan (pos + total); total }
    else loop total
  in
  loop 0
;
