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
(* ;; Reading records.
;; (record-reader [delims elide? handle-delim]) -> reader
;; (reader [port]) -> string or eof
 *)

value record_reader
  ?(delims = Delim_7.cs_newline) ?(elide_delims = False) ?(handle_delim = Delim_7.Trim)
    () =
  let raw_delims = Charset_14.to_charset delims in
  let delims = Charset_14.Cset raw_delims in
  match handle_delim with
  [ Delim_7.Trim ->
      fun chan ->
        let s = Delim_7.read_delimited ~chan delims in
        do { if elide_delims then ignore (Delim_7.skip_char_set ~chan delims) else (); s }
  | Delim_7.Concat ->
      let not_delims = Charset_14.Cset (Charset_14.complement raw_delims) in
      fun chan ->
        let s = Delim_7.read_delimited ~chan ~handle_delim:(Delim_7.Concat) delims in
        if not elide_delims then s
        else s ^ Delim_7.read_delimited ~chan ~handle_delim:(Delim_7.Peek) not_delims
  | Delim_7.Peek -> Delim_7.no_peek "record_reader" ]
;

value record_reader_split ?(delims = Delim_7.cs_newline) ?(elide_delims = False) () =
  let raw_delims = Charset_14.to_charset delims in
  let delims = Charset_14.Cset raw_delims
  and not_delims = Charset_14.Cset (Charset_14.complement raw_delims) in
  fun chan ->
    let (s, delim) = Delim_7.read_delimited_split ~chan delims in
    (s,
     match delim with
     [ Delim_7.Eof -> ""
     | Delim_7.Read c ->
         let delim = String.make 1 c in
         if elide_delims then
           (* ;; Elide: slurp in extra delims. *)
           delim ^ Delim_7.read_delimited ~chan ~handle_delim:(Delim_7.Peek) not_delims
         else delim
     | Delim_7.Full_buffer -> assert False ])
;

(* ;; Looping primitives:
;; It is nicer for loops that loop over a bunch of different things if you can
;; encapsulate the idea of iterating over a data structure with a
;;     (next-element state) -> elt next-state
;;     (more-elements? state) -? #t/#f
;; generator/termination-test pair. You can use the generator with REDUCE to
;; make a list; you can stick it into a loop macro to loop over the
;; elements. For example, if we had an extensible Yale-loop style loop macro, we
;; could have a loop clause like
;;     (loop (for field in-infix-delimited-string ":" path)
;;           (do (display field) (newline)))
;; and it would be simple to expand this into code using the generator.  With
;; procedural inlining, you can get pretty optimal loops over data structures
;; this way.

;; As of now, you are forced to parse fields into a buffer, and loop over
;; that. This is inefficient of time and space.  If I ever manage to do an
;; extensible loop macro for Scheme 48, I'll have to come back to this package
;; and rethink how to provide this functionality.
 *)
(* ;; Forward-progress guarantees and empty string matches.
;; A loop that pulls text off a string by matching a regexp against that string
;; can conceivably get stuck in an infinite loop if the regexp matches the empty
;; string. For example, the regexps ^, $, .*, foo|[^f]* can all match the empty
;; string.

;; The regexp-loop routines in this code are careful to handle this case.  If a
;; regexp matches the empty string, the next search starts, not from the end of
;; the match (which in the empty string case is also the beginning -- there's
;; the rub), but from the next character over.  This is the correct
;; behaviour. Regexps match the longest possible string at a given location, so
;; if the regexp matched the empty string at location i, then it is guaranteed
;; they could not have matched a longer pattern starting with character #i. So
;; we can safely begin our search for the next match at char i+1.

;; So every iteration through the loop makes some forward progress, and the loop
;; is guaranteed to terminate.

;; This has the effect you want with field parsing. For example, if you split a
;; string with the empty pattern, you will explode the string into its
;; individual characters:
;;     ((suffix-splitter (rx "")) "foo") -> #("" "f" "o" "o")
;; However, even though this boundary case is handled correctly, we don't
;; recommend using it. Say what you mean -- just use a field splitter:
;;     ((field-splitter (rx any)) "foo") -> #("f" "o" "o")
 *)

type handle_field_delim =
  [ Trim_f
  | Split_f
  | Concat_f ]
;

type delim_matcher =
  [ Match_proc of string -> int -> (int * int)
  | String of string
  | Charset of Charset_14.any_t
  | Regexp of Pcre.regexp
  | Pattern of string ]
;

(* ;; FIELD PARSERS
;; This section defines routines to split a string into fields.  You can parse
;; by specifying a pattern that *separates* fields, a pattern that *terminates*
;; fields, or a pattern that *matches* fields.
*)
value to_delim_matcher =
  fun
  [ Match_proc p -> p
  | Charset cs ->
      let cs = Charset_14.to_charset cs in
      fun s i ->
        let upto = String.length s in
        let j = Charset_14.skip_to cs s i upto in
        if j >= upto then raise Not_found else (j, succ j)
  | pat ->
      let rex =
        match pat with
        [ String s -> Pcre.regexp (Pcre.quote s)
        | Regexp re -> re
        | Pattern p -> Pcre.regexp p
        | _ -> assert False ]
      in
      fun s pos -> Pcre.get_substring_ofs (Pcre.exec ~rex ~pos s) 0 ]
;

value (too_few, too_many) =
  let fail why num_fields s =
    failwith
      (String.concat " "
         ["Too"; why; "fields"; "(" ^ string_of_int num_fields ^ ")"; "in record:"; s])
  in
  (fail "few", fail "many")
;

type nfields_option =
  [ Exact
  | At_least
  | Any ]
;

(* ;; These four procedures implement the guts of each parser
;; (field, infix, suffix, and sloppy-suffix).

;; The CONS-FIELD argument is a procedure that parameterises the HANDLE-DELIM
;; action for the field parser.

;; The MATCH-DELIM argument is used to match a delimiter.  (MATCH-DELIM S I)
;; returns two integers [start, end] marking the next delimiter after index I in
;; string S. If no delimiter is found, it returns [#f #f].

;; In the main loop of each parser, the loop variable LAST-NULL? tells if the
;; previous delimiter-match matched the empty string. If it did, we start our
;; next delimiter search one character to the right of the match, so we won't
;; loop forever. This means that an empty delimiter regexp "" simply splits the
;; string at each character, which is the correct thing to do.

;; These routines return the answer as a reversed list.
*)

value fieldspec_field_loop s start match_field num_fields nfields_option =
  let ends = String.length s in
  let rec loop i nfields fields last_is_null =
    let finish_up () =
      (* ; Check to see if we made our quota before returning answer. *)
      if nfields_option <> Any && nfields < num_fields then too_few num_fields s
      else fields
    in
    (* ; Where to start next delim search. *)
    let j = if last_is_null then succ i else i in
    if j > ends then finish_up ()
    else
      match nfields_option with
      [ Exact when nfields > num_fields ->
          (* ; Read too many fields. Bomb out. *)
          too_many num_fields s
      | At_least when nfields = num_fields ->
          (* ; Made our lower-bound quota. Quit early. *)
          if i = ends then (* ; Special case hackery. *)  
            fields
          else [Strings_5_1.substring s i ends :: fields]
      | _ ->
          try
            (* ; Match off another field & loop. *)
            let (m0, m1) = match_field s j in
            loop m1 (succ nfields) [Strings_5_1.substring s m0 m1 :: fields] (m0 = m1)
          with
          [ Not_found -> finish_up () ] ]
  in
  loop start 0 [] False
;

value infix_field_loop s start match_delim cons_field num_fields nfields_option =
  let ends = String.length s in
  if start = ends then []
  else (* ; Specially hack empty string. *)
    let rec loop i nfields fields last_is_null =
      let finish_up () =
        (* ;; s[i,end) is the last field. Terminate the loop. *)
        if nfields_option <> Any && succ nfields < num_fields then too_few num_fields s
        else if nfields_option = Exact && nfields >= num_fields then too_many num_fields s
        else [Strings_5_1.substring s i ends :: fields]
      in
      (* ; Where to start next search. *)
      let j = if last_is_null then succ i else i in
      (* ; If we've read NUM-FIELDS fields, quit early. *)
      if nfields = num_fields then
        if nfields_option = Exact then too_many num_fields s
        else [Strings_5_1.substring s i ends :: fields]
      else if j <= ends then
        try
          (* ; Match off another field. *)
          let (m0, m1) = match_delim s j in
          loop m1 (succ nfields) (cons_field s i m0 m1 fields) (m0 = m1)
        with
        [ Not_found ->  (* ; No more delimiters. *) 
            finish_up () ]
      else
        (* ; We've run off the end of the string. This is a weird
           ; boundary case occuring with empty-string delimiters. *)
        finish_up ()
    in
    loop start 0 [] False
;

value suffix_field_loop s start match_delim cons_field num_fields nfields_option =
  let ends = String.length s in
  let rec loop i nfields fields last_is_null =
    (* ; Where to start next delim search. *)
    let j = if last_is_null then succ i else i in
    if i = ends then
      (* ; We are done. *)
      if nfields_option <> Any && nfields < num_fields then
        (* ; Didn't make quota. *)
        too_few num_fields s
      else fields
    else
      match nfields_option with
      [ Exact when nfields = num_fields ->
          (* ; Read too many fields. Bomb out. *)
          too_many num_fields s
      | At_least when nfields = num_fields ->
          (* ; Made our lower-bound quota. Quit early. *)
          [Strings_5_1.substring s i ends :: fields]
      | _ ->
          (* ; Match off another field. *)
          let (m0, m1) =
            try match_delim s j with
            [ Not_found -> invalid_arg ("Missing field terminator: " ^ s) ]
          in
          loop m1 (succ nfields) (cons_field s i m0 m1 fields) (m0 = m1) ]
  in
  loop start 0 [] False
;

(* ;; Match off an optional initial delimiter,
   ;; then jump off to the suffix parser. *)
value sloppy_suffix_field_loop s start match_delim =
  (* ; If sloppy-suffix, skip an initial delimiter if it's there. *)
  let start =
    try
      let (i, j) = match_delim s start in
      if i = 0 then j else start
    with
    [ Not_found -> start ]
  in
  suffix_field_loop s start match_delim
;

value fix_nfields =
  fun
  [ None -> (max_int, Any)
  | Some n -> if n < 0 then (- n, At_least) else (n, Exact) ]
;

(*
;; (infix-splitter         [re num-fields handle-delim])        -> parser
;; (suffix-splitter        [re num-fields handle-delim])        -> parser
;; (sloppy-suffix-splitter [re num-fields handle-delim])        -> parser
;; (field-splitter         [re num-fields])                     -> parser
;;
;; (parser string [start]) -> string-list
*)

(* ;; Default field spec is runs of non-whitespace chars. *)
value default_field_matcher = Regexp (Pcre.regexp "\S+");

value field_splitter ?(field = default_field_matcher) ?num_fields () =
  let match_field = to_delim_matcher field in
  let (num_fields, nfields_option) = fix_nfields num_fields in
  (* ;; This is the parser procedure. *)
  fun ?(start = 0) s ->
    List.rev (fieldspec_field_loop s start match_field num_fields nfields_option)
;

(* ; This is the parser-generator. *)
value make_field_parser_generator
  default_delim_matcher loop_proc ?(delim = default_delim_matcher) ?num_fields
    ?(handle_delim = Trim_f) () =
  let match_delim = to_delim_matcher delim
  and cons_field =
    (* ; Field is s[i,j); Delimiter is s[j,k). *)
    match handle_delim with
    [ Trim_f -> fun s i j k fields -> [Strings_5_1.substring s i j :: fields]
    | Split_f ->
        fun s i j k fields ->
          [Strings_5_1.substring s j k; Strings_5_1.substring s i j :: fields]
    | Concat_f -> fun s i j k fields -> [Strings_5_1.substring s i k :: fields] ]
  and (num_fields, nfields_option) = fix_nfields num_fields in
  (* ;; This is the parser. *)
  fun ?(start = 0) s ->
    List.rev (loop_proc s start match_delim cons_field num_fields nfields_option)
;

(* ;; Now, build the exported procedures: {infix,suffix,sloppy-suffix}-splitter. *)

value default_infix_matcher =  Regexp (Pcre.regexp "\s+");

value infix_splitter = make_field_parser_generator default_infix_matcher infix_field_loop;

value default_suffix_matcher = Regexp (Pcre.regexp "\s+|\z");

value suffix_splitter =
  make_field_parser_generator default_suffix_matcher suffix_field_loop
;
value sloppy_suffix_splitter =
  make_field_parser_generator default_suffix_matcher sloppy_suffix_field_loop
;

(* ;; Reading and parsing records
;; (field-reader [field-parser rec-reader]) -> reader
;; (reader [port]) -> [raw-record parsed-record] or [eof #()]
 
;; This is the field reader, which is basically just a composition of
;; RECORD-READER and FIELD-PARSER.
*)

value default_field_parser = field_splitter ();

value gen_field_reader field_parser rec_reader chan =
  let record = rec_reader chan in
  (record, field_parser record)
;

value field_reader
  ?(field_parser = default_field_parser) ?(rec_reader = Delim_7.one_line Delim_7.Trim)
    () =
  gen_field_reader field_parser rec_reader
;
