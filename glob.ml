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
(*
;; Code for processing file names with a glob pattern.
;; See glob spec near `Usage:  (glob pattern-list)'.
 *)

(*
;; This section of code is responsible for processing the braces in glob
;; patterns. I.e., "{foo,bar}/*.c" -> ("foo/*.c" "bar/*.c")
 *)
value stringify pat =
  let i = List.length pat in
  let res = String.create i in
  let rec loop i =
    fun
    [ [] -> res
    | [c :: cs] -> do { res.[i] := c; loop (pred i) cs } ]
  in
  loop (pred i) pat
;

value append_suffix suffix = List.map (fun s -> s ^ suffix);

value rev_cross_append prefixes =
  List.fold_left (fun strs suffix -> List.rev_append (append_suffix suffix prefixes) strs)
    []
;

(* #prefixes + #suffixes < #prefixes * #suffixes, so we reverse the args rather
   than the result. *)
(* value cross_append prefixes suffixes =
  rev_cross_append (List.rev prefixes) (List.rev suffixes)
; At first sight, we don't care of the result's order *)

(*
;; Parse the internals of a {foo,bar,baz} brace list from a glob pattern.
;; START is the index of the char following the open brace.
;; Returns two values:
;; - an equivalent list of brace-free glob patterns
;; - the index of the char after the terminating brace.
 *)
value rec parse_comma_sequence pattern start =
  let pattern_len = String.length pattern in
  let rec loop i patterns =
    (* ; PATTERNS is the list of comma-separated patterns read. *)
    if i = pattern_len then
      invalid_arg ("Glob brace-expression pattern not terminated: " ^ pattern)
    else
      let (pats, i) = parse_glob_braces pattern i True in
      let patterns = patterns @ pats in
      if i = pattern_len then
        invalid_arg ("Unterminated brace in glob pattern: " ^ pattern)
      else
        match pattern.[i] with
        [ '}' -> (patterns, succ i)
        | ',' -> loop (succ i) patterns
        | _ -> assert False ]
  in
  loop start []


(*
;; Parse a glob pattern into an equivalent series of brace-free patterns.
;; The pattern starts at START and is terminated by (1) end of string,
;; (2) an unmatched close brace, or (3) a comma (if COMMA-TERMINATES? is set).
;; Returns two values:
;; - the list of patterns
;; - the string index after the pattern terminates. This points at
;;   the comma or brace if they terminated the scan, since they are
;;   not part of the pattern.
 *)

and parse_glob_braces =
  let finish prefixes pat i = (append_suffix (stringify pat) prefixes, i) in
  fun pattern start comma_terminates ->
    let pattern_len = String.length pattern in
    let rec loop i prefixes pat =
      if i = pattern_len then finish prefixes pat i
      else
        match pattern.[i] with
        [ '{' ->
            let prefixes = append_suffix (stringify pat) prefixes in
            let (pats, i) = parse_comma_sequence pattern (succ i) in
            loop i (rev_cross_append prefixes pats) []
        | '\\' ->
            let i = succ i in
            if i = pattern_len then invalid_arg "Dangling escape char in glob pattern"
            else loop (succ i) prefixes [pattern.[i] :: pat]
        | ',' ->
            if comma_terminates then finish prefixes pat i
            else loop (succ i) prefixes [',' :: pat]
        | '}' -> finish prefixes pat i
        | c -> loop (succ i) prefixes [c :: pat] ]
    in
    loop start [""] []
;

(*
;; Make an effort to get the files in the putative directory PATH.  If PATH isn't
;; a directory, or some filesys error happens (such as a broken symlink, or a
;; permissions problem), don't error out, just quietly return the empty list.
 *)
value maybe_directory_files path dot_files =
  try Io_3_3.directory_files ~dot_files path with _ -> []
;

(*
;; Make an effort to find out if the file is a directory. If there's any error,
;; return #f.
 *)
value maybe_isdir dir = try Io_3_3.is_file_directory_fn dir with _ -> False;

value ill_formed reason pattern =
  invalid_arg ("Ill-formed glob pattern -- " ^ reason ^ ": " ^ pattern)
;

value end_in_backslash pattern = ill_formed "ends in backslash" pattern;

value must_quote c = "must add \\ before `" ^ String.make 1 c ^ "'";

(* ;; Is the glob pattern free of *'s, ?'s and [...]'s? *)
value is_constant_glob pattern =
  let patlen = String.length pattern in
  let rec loop i =
    i = patlen ||
    (let nexti = succ i in
     match pattern.[i] with
     [ '\\' ->
         (* ; Escape char *)
         if nexti = patlen then end_in_backslash pattern
         else loop (succ nexti)
     | '*' | '?' | '[' -> (* Special chars *) False
     | _ -> loop nexti ])
  in
  loop 0
;

(*
;; A glob bracket expression is [...] or [^...].
;; The body is a sequence of <char> and <char>-<char> ranges.
;; A <char> is any character except right-bracket, caret, hypen or backslash, or
;; a backslash followed by any character at all.
Warning: I added ! to ^
 *)
value parse_glob_braket pat start buf =
  let patlen = String.length pat in
  let negate =
    start < patlen &&
    (match pat.[start] with
     [ '^' | '!' -> True
     | _ -> False ])
  in
  let i0 = if negate then do { Buffer.add_char buf '^'; succ start } else start in
  let rec loop i0 i1 =
    let _ = do { Printf.eprintf "i0: %d i1: %d" i0 i1; prerr_newline () } in
    (* i0 is start of body or after last range. *)
    if i1 >= patlen then ill_formed "no terminating close-bracket: " pat
    else
      let c = pat.[i1]
      and i = succ i1 in
      let _ = do { Printf.eprintf "c: %c i: %d" c i; prerr_newline () } in
      match c with
      [ ']' -> i
      | '\\' ->
          if i >= patlen then end_in_backslash pat
          else do { Buffer.add_char buf pat.[i]; loop i0 (succ i) }
      | '[' | '^' | '!' ->(* We are stricter than scsh here. *)
              ill_formed (must_quote c) pat
      | '-' ->
          if i >= patlen then ill_formed "unterminated range" pat
          else if i1 = i0 then ill_formed "range has no beginning" pat
          else if i = i0 then ill_formed (must_quote c ^ " when ending a range") pat
          else do { Buffer.add_char buf c; loop (succ i) i }
      | _ -> do { Buffer.add_char buf c; loop i0 i } ]
  in
  loop i0 i0
;

(* ;; Translate a brace-free glob pattern to a regular expression. *)
(* From scsh 5.0.1, because 5.0.2 uses SRE.  Hope it had no bug. *)
value regexp_of_glob =
  let c2s = String.make 1
  and esc_c2s c =
    (* Modifying constants is *BAD*. *)
    let r = "\\c" in
    do { r.[1] := c; r }
  in
  fun pat ->
    let patlen = String.length pat in
    let buf = Buffer.create (3 * patlen / 2) in
    let rec loop i to_add =
      do {
        Buffer.add_string buf to_add;
        if i = patlen then do { Buffer.add_char buf '$'; Buffer.contents buf }
        else
          let c = pat.[i]
          and i = succ i in
          match c with
          [ '\\' ->
              let to_add =
                if i < patlen then
                  match pat.[i] with
                  [ '$' | '^' | '.' | '+' | '?' | '*' | '|' | '(' | ')' |
                    '[' as escaped ->
                      esc_c2s escaped
                  | c -> c2s c ]
                else end_in_backslash pat
              in
              loop (succ i) to_add
          | '[' ->
              do {
                Buffer.add_char buf c; let i = parse_glob_braket pat i buf in loop i "]"
              }
          | _ ->
              let to_add =
                match c with
                [ '*' -> ".*"
                | '?' -> "."
                | '$' | '^' | '.' | '+' | '|' | '(' | ')' -> esc_c2s c
                | _ -> c2s c ]
              in
              loop i to_add ]
      }
    in
    loop 0 "^"
;

(*
;; Return the elts of directory FNAME that match pattern PAT.  If PAT contains
;; no wildcards, we cheat and do not match the constant pattern against every
;; file in FNAME/; we just immediately return FNAME/PAT.  In this case, we
;; indicate that we aren't actually sure the file exists by returning a false
;; SURE? value.  Not only does this vastly speed up the matcher, it also allows
;; us to match the constant patterns "." and "..".
 *)
value glob_subpat fname pat =
  (* ; PAT doesn't contain a slash.
   ;; The initial special-case [below] isn't really for the fast-path; it's an
   ;; obscure and unlikely case. But since we have to check pat[0] for an
   ;; initial dot, we have to do the check anyway... *)
  if pat = "" then ([], True)
  else if is_constant_glob pat then (* ; Don't check filesys. *) ([pat], False)
  else
    let match_dot_files = pat.[0] = '.'
    and re = Pcre.regexp (regexp_of_glob pat) in
    let candidates = maybe_directory_files fname match_dot_files in
    (List.filter (fun f -> Pcre.pmatch ~rex:re f) candidates, True)
;

value really_glob root_file patterns directories_only =
  (* ; This is the heart of the matcher. *)
  recur root_file patterns False where rec recur file pats sure =
    (* ; sure is True if we are sure this file exists. *)
    match pats with
    [ [pat :: pats] ->
        let dir = Strings_5_1.file_name_as_directory file in
        let (winners, sure) = glob_subpat file pat in
        (* We give no warranty on the order of the files in the result
           (but should be tail-rec). *)
        List.fold_left (fun patterns f -> List.rev_append (recur (dir ^ f) pats sure) patterns)
          [] winners
    | [] ->
        if directories_only then
          if maybe_isdir file then [Strings_5_1.file_name_as_directory file] else []
        else if sure || Io_3_3.is_file_existing_fn file then [file]
        else [] ]
;

value glob_one_pattern pattern =
  let plen = Env_3_11.last_index pattern in
  if plen < 0 then []
  else
    let directories_only = pattern.[plen] = '/' in
    match Strings_5_1.split_file_name pattern with
    [ [] -> (* ; Must be non-null. *) assert False
    | [car :: cdr] as patterns ->
        let (root, pats) = if car = "" then (car, cdr) else (".", patterns) in
        really_glob root pats directories_only ]
;

value glob_remove_braces pattern =
  let (pats, i) = parse_glob_braces pattern 0 False in
  if i = String.length pattern then pats
  else
    invalid_arg
      ("Unmatched close brace at " ^ string_of_int i ^ " in glob pattern: " ^ pattern)
;

(*
;; Usage:  (glob pattern-list)
;;            pattern-list := a list of glob-pattern strings

;; Return: list of file names (strings)
;;          The files "." and ".." are never returned by glob.  Dot files will
;;          only be returned if the first character of a glob pattern is a ".".

;; The empty pattern matches nothing.
;; A pattern beginning with / starts at root; otherwise, we start at cwd.
;; A pattern ending with / matches only directories, e.g., "/usr/man/man?/"
 *)

(* ;; Expand out braces, and apply GLOB-ONE-PATTERN to all the result patterns. *)
value glob pattern_list =
  List.fold_left (fun pats pattern -> List.rev_append (glob_one_pattern pattern) pats) []
    (List.fold_left
       (fun pats pattern -> List.rev_append (glob_remove_braces pattern) pats) []
       pattern_list)
;

(*
;; Convert a string into a glob pattern that matches that string exactly --
;; in other words, quote the \ * ? [] and {} chars with backslashes.
 *)
value glob_quote string =
  let len = Env_3_11.last_index string in
  let buf = Buffer.create (2 * len) in
  let () =
    for i = 0 to len do {
      let c = string.[i] in
      match c with
      [ '[' | ']' | '*' | '?' | '{' | '}' | '\\' -> Buffer.add_char buf '\\'
      | _ -> () ];
      Buffer.add_char buf c
    }
  in
  Buffer.contents buf
;

(*
;; Usage:
;;  (file-match root dots? . pattern-list)
;;     root      Search starts from here. Usefully "." (cwd)
;;     dots? => if true, dot files will be matched.
;;              if false, dot files will not be matched.
;;     pattern-list := a list of 
;;                       - strings (they are split at /'s and then treated as
;;                         Posix regexp strings)
;;                       - regexps (typically made with RX macro)
;;                       - predicates
;;  Each member of the list corresponds to one or more levels in a directory.
;;  (A string with embedded "/" characters corresponds to multiple levels.)
;;  Example: (file-match "." #f "foo" "bar" "\\.c$")
;;      means match files that end in ".c" if they reside in a directory with a
;;      name that contains "bar", which itself must reside in a directory with a
;;      name that contains "foo".
;;   Here are two more equivalent specs for the example above:
;;   (file-match "." #f "foo/bar/\\.c$")
;;   (file-match "." #f (rx "foo") (rx "bar")
;;                      (rx ".c" eos))
;;   If a member in the list is a predicate, the predicate must be a procedure
;;   of one argument.  This procedure is applied to the file name being
;;   processed. If it returns true, then the file is considered a match.

;; Return:      list of matching file names (strings)
;;   The matcher never considers "." or "..".

;; Subtle point:
;;   If a file-match predicate raises an error condition, it is caught by
;;   FILE-MATCH, and the file under consideration is not matched. This means
;;   that (file-match "." #f file-directory?) doesn't error out if you happen to
;;   run it in a directory containing a dangling symlink when FILE-DIRECTORY? is
;;   applied to the bogus symlink.
 *)

type file_match_pattern =
  [ String_pat of string | Regexp_pat of Pcre.regexp | Predicate_pat of string -> bool ]
;

(* XXXX Utiliser Pcre.split, et List.revmap pour appliquer frobnicator. *)
value rev_split_pat frobnicator pat =
  let patlen = String.length pat in
  let rec loop i res =
    let j = Env_3_11.internal_index '/' pat i in
    let sub = Env_3_11.substring pat i (if j < 0 then patlen else j) in
    let res = [frobnicator sub :: res] in
    if j < 0 then res else loop (succ j) res
  in
  loop 0 []
;

value split_strings_regexpify =
  let regexpify pattern pats =
    match pattern with
    [ String_pat str ->
        List.rev_append (rev_split_pat (fun s -> Regexp_pat (Pcre.regexp s)) str) pats
    | _ -> [pattern :: pats] ]
  in
  (* fold_right isn't tail-rec, but the patterns are not many. *)
  fun pats -> List.fold_right regexpify pats []
;

(* XXX Catching all exceptions raised by predicates surely make them undebugable. *)
value file_match ?(dot_files = False) root patterns =
  let patterns = split_strings_regexpify patterns in
  let rec recur root =
    fun
    [ [] -> [root]
    | [pattern :: patterns] ->
        let dir = Strings_5_1.file_name_as_directory root in
        let matcher =
          match pattern with
          [ Regexp_pat re -> fun fn -> Pcre.pmatch ~rex:re fn
          | Predicate_pat predicate -> fun f -> try predicate (dir ^ f) with _ -> False
          | _ -> assert False ]
        and candidates = maybe_directory_files root dot_files in
        (* If maybe_directory_files took a filter argument, we'd zap List.filter. *)
        let winners = List.filter matcher candidates in
        (* concat isn't tail-rec, map neither; this could overflow the stack. *)
        List.concat (List.map (fun fn -> recur (dir ^ fn) patterns) winners) ]
  in
  recur root patterns
;
