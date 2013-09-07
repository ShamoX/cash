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

(* May be compiled with -pp camlp4r -unsafe, the necessary checks are there (except for bugs :-) *)

type t = string;

type any_t =
  [ Cset of t
  | Str of string
  | Char of char
  | Predicate of char -> bool ]
;

#load "pa_extend.cmo";
#load "charset_macros.cmo";

value char_of_latin1 = Char.chr;
(* These funs are inlined by charset_macros:
value unsafe_char_of_latin1 = Char.unsafe_chr;
value latin1_of_char = Char.code; (* identity. *)

value si s i = latin1_of_char s.[i];
value is_zero s i = 0 = si s i;
value is_one s i = 0 <> si s i;

value c0 = char_of_latin1 0;
value c1 = char_of_latin1 1;
value set0 s i = s.[i] := c0; 
value set1 s i = s.[i] := c1;

value set0c s c = set0 s (latin1_of_char c);
value set1c s c = set1 s (latin1_of_char c);
 *)

value make_empty () = String.make 256 c0;
value make_full () = String.make 256 c1;
value copy = String.copy;

value default_base =
  fun
  [ None -> make_empty ()
  | Some cs -> copy cs ]
;

value invalid m = invalid_arg ("Charset." ^ m);

value check cs = assert (String.length cs = 256);

value low_iter (p : int -> int -> unit) cs =
  do { check cs; for i = 0 to 255 do { p i (si cs i) } }
;
value low_algebra cs1 proc css = do { check cs1; List.iter (low_iter proc) css; cs1 };
value algebra_wrapper def_thunk op_bang =
  fun
  [ [] -> def_thunk ()
  | [cs1 :: css] -> op_bang (copy cs1) css ]
;

value constants = ref ([] : list t);

(* simply copy the would-be-modified constant. *)
value don't_modify_consts cs = if List.memq cs constants.val then copy cs else cs;

(* --- The real meat ---. *)

value equal (cs1 : t) cs2 = cs1 = cs2;

value equals =
  fun
  [ [] | [_] -> True
  | [cs1 :: css] -> List.for_all (equal cs1) css ]
;

value not_greater (cs1 : t) cs2 = cs1 <= cs2;

value rec not_greaters =
  fun
  [ [] | [_] -> True
  | [cs1 :: ([cs2 :: _] as css)] -> cs1 <= cs2 && not_greaters css ]
;

value hash ?(bound = 0) cs =
  if bound < 0 then invalid "hash: negative bound"
  else
    let hash = Hashtbl.hash cs in
    if bound = 0 then hash else hash mod bound
;

type cursor = int;

(* is_one is safe here because cursor is abstract, made by (cursor cs) (just
   below), then decremented/tested here. *)
value low_cursor_next cs cursor =
  loop cursor where rec loop cur =
    let cur = pred cur in
    if cur < 0 || is_one cs cur then cur else loop cur
;

value cursor cs = low_cursor_next cs 256;

(* The cursor may be negative here => don't use unsafe_char_of_latin1. *)
value cref cs = char_of_latin1;

value is_end_of_charset cursor = cursor < 0;

value cursor_next cs cursor =
  let () = check cs in
  if is_end_of_charset cursor then invalid "cursor_next: cursor is at end"
  else low_cursor_next cs cursor
;

value fold kons knil cs =
  loop 255 knil where rec loop i ans =
    if i < 0 then ans
    else loop (pred i) (if is_zero cs i then ans else kons (unsafe_char_of_latin1 i) ans)
;

value (unfold, unfold_bang) =
  let unfold_bang_nc base_cs get stop next =
    loop where rec loop seed =
      if stop seed then base_cs else do { set1c base_cs (get seed); loop (next seed) }
  in
  (* NO closure; would need a bang too. *)
  (fun ?base_cs get stop next seed ->
     unfold_bang_nc (default_base base_cs) get stop next seed,
   fun base_cs -> unfold_bang_nc (don't_modify_consts base_cs))
;

value for_each proc cs =
  for i = 0 to 255 do { if is_one cs i then proc (unsafe_char_of_latin1 i) else () }
;
value iter = for_each;

value map proc cs =
  let ans = make_empty () in
  do {
    for i = 0 to 255 do {
      if is_one cs i then set1c ans (proc (unsafe_char_of_latin1 i)) else ()
    };
    ans
  }
;

value (of_list, of_list_bang, adjoin) =
  let of_list_bang_nc cs chars = do { List.iter (fun c -> set1c cs c) chars; cs } in
  (* NO closure; would need a bang too. *)
  (fun ?base_cs l -> of_list_bang_nc (default_base base_cs) l,
   fun base_cs -> of_list_bang_nc (don't_modify_consts base_cs),
   (* NO closure; would need a bang too. *)
   fun cs chars -> of_list_bang_nc (copy cs) chars)
;
value adjoin_bang = of_list_bang;

(* NO closure; would need a bang too. *)
value of_char ?base_cs c = of_list ?base_cs [c];
value of_char_bang base_cs c = of_list_bang base_cs [c];
(* NO closure; would need a bang too. *)
value adjoin1 cs c = adjoin cs [c];
value adjoin1_bang = of_char_bang;(* since adjoin_bang = of_list_bang. *)

value (of_string, of_string_bang) =
  let of_string_bang_nc base_cs s =
    do { for i = 0 to pred (String.length s) do { set1c base_cs (s.[i]) }; base_cs }
  in
  (* NO closure; would need a bang too. *)
  (fun ?base_cs s -> of_string_bang_nc (default_base base_cs) s,
   fun base_cs -> of_string_bang_nc (don't_modify_consts base_cs))
;

value (filter, filter_bang) =
  let filter_bang_nc base_cs predicate cs =
    do {
      for i = 0 to 255 do {
        if is_one cs i && predicate (unsafe_char_of_latin1 i) then set1 base_cs i else ()
      };
      base_cs
    }
  in
  (* NO closure; would need a bang too. *)
  (fun ?base_cs p cs -> filter_bang_nc (default_base base_cs) p cs,
   fun base_cs -> filter_bang_nc (don't_modify_consts base_cs))
;

value (of_ucs_range, of_ucs_range_bang) =
  let of_ucs_range_bang_nc error base_cs lower upper =
    if lower < 0 || upper < lower then invalid "of_ucs_range*: bad range"
    else if error && lower < upper && 256 < upper then
      invalid_arg "Requested UCS range contains unavailable characters -- this implementation only supports Latin-1"
    else do { for i = lower to min (pred upper) 255 do { set1 base_cs i }; base_cs }
  in
  (* NO closure; would need a bang too. *)
  (fun ?(error = False) ?base_cs l u ->
     of_ucs_range_bang_nc error (default_base base_cs) l u,
   fun error base_cs -> of_ucs_range_bang_nc error (don't_modify_consts base_cs))
;

value size cs =
  loop 255 0 where rec loop i size =
    if i < 0 then size else loop (pred i) (size + si cs i)
;

value count predicate cs =
  loop 255 0 where rec loop i count =
    if i < 0 then count
    else loop (pred i) (if predicate (unsafe_char_of_latin1 i) then succ count else count)
;

value to_list cs =
  loop 255 [] where rec loop i lst =
    if i < 0 then lst
    else loop (pred i) (if is_zero cs i then lst else [unsafe_char_of_latin1 i :: lst])
;

(* This IS imperative. *)
value to_string cs =
  let res = String.create 256
  and j = ref 0 in
  do {
    for i = 0 to 255 do {
      if is_zero cs i then () else do { res.[j.val] := unsafe_char_of_latin1 i; incr j }
    };
    if j.val = 256 then res else String.sub res 0 j.val
  }
;

value contains cs c = is_one cs (latin1_of_char c);

value every predicate cs =
  loop 255 where rec loop i =
    i < 0 || (is_zero cs i || predicate (unsafe_char_of_latin1 i)) && loop (pred i)
;
value for_all = every;

value any predicate cs =
  loop 255 where rec loop i =
    i >= 0 && (is_one cs i && predicate (unsafe_char_of_latin1 i) || loop (pred i))
;
value exists = any;

(* not in SRFI-14. *)
value find predicate cs =
  loop 0 where rec loop i =
    if i > 255 then raise Not_found
    else if is_one cs i && predicate (unsafe_char_of_latin1 i) then unsafe_char_of_latin1 i
    else loop (succ i)
;

value (delete, delete_bang) =
  let delete_bang_nc cs chars = do { List.iter (fun c -> set0c cs c) chars; cs } in
  (* NO closure; would need a bang too. *)
  (fun cs chars -> delete_bang_nc (copy cs) chars,
   fun cs -> delete_bang_nc (don't_modify_consts cs))
;

(* NO closure; would need a bang too. *)
value delete1 cs c = delete cs [c];
value delete1_bang cs c = delete_bang cs [c];

value (complement, complement_bang) =
  let complement_bang_nc cs =
    let () = low_iter (fun i v -> cs.[i] := unsafe_char_of_latin1 (1 - v)) cs in
    cs
  in
  (fun cs -> complement_bang_nc (copy cs),
   fun cs -> complement_bang_nc (don't_modify_consts cs))
;

value (union, union_bang) =
  let union_bang_nc cs1 =
    low_algebra cs1 (fun i v -> if v = 0 then () else set1 cs1 i)
  in
  (algebra_wrapper make_empty union_bang_nc,
   fun cs -> union_bang_nc (don't_modify_consts cs))
;

value (intersection, intersection_bang) =
  let intersection_bang_nc cs1 =
    low_algebra cs1 (fun i v -> if v = 0 then set0 cs1 i else ())
  in
  (algebra_wrapper make_full intersection_bang_nc,
   fun cs -> intersection_bang_nc (don't_modify_consts cs))
;

value (difference, difference_bang) =
  let difference_bang_nc cs1 =
    low_algebra cs1 (fun i v -> if v = 0 then () else set0 cs1 i )
  in
  (* NO closure; would need a bang too. *)
  (fun cs1 l -> difference_bang_nc (copy cs1) l,
   fun cs1 -> difference_bang_nc (don't_modify_consts cs1))
;

value (xor, xor_bang) =
  let xor_bang_nc cs1 =
    low_algebra cs1
      (fun i v -> if v = 0 then () else cs1.[i] := unsafe_char_of_latin1 (1 - si cs1 i))
  in
  (algebra_wrapper make_empty xor_bang_nc, fun cs -> xor_bang_nc (don't_modify_consts cs))
;

value (diff_intersection, diff_intersection_bang) =
  let myname = "diff_intersection*" in
  let low_diff_inter diff int css =
    do {
      List.iter
        (low_iter
           (fun i v ->
              if v = 0 then ()
              else if diff.[i] = c0 then ()
              else do { set0 diff i; set1 int i }))
        css;
      (diff, int)
    }
  in
  (fun cs1 -> low_diff_inter (copy cs1) (make_empty ()),
   fun cs1 cs2 ->
     let cs1 = don't_modify_consts cs1
     and cs2 = don't_modify_consts cs2 in
     do {
       check cs2;
       low_iter
         (fun i v ->
            if v = 0 then set0 cs2 i else if cs2.[i] = c0 then () else set0 cs1 i)
         cs1;
       low_diff_inter cs1 cs2
     })
;

(* Not in SRFI-14; a fast version is needed to boost read-delimited (so a C
   version might be justified). *)
(*
value caml_skip cs buf from upto =
  do {
    Strings_5_1.check_substring_spec buf from upto "Charset.skip";
    let rec loop i =
      if i >= upto || is_one cs (latin1_of_char (String.unsafe_get buf i)) then i
      else loop (succ i)
    in
    loop from
  }
; *)

external unsafe_skip :  t -> string -> int -> int -> int = "cash_charset_skip";
external unsafe_skip_to :  t -> string -> int -> int -> int = "cash_charset_skip_to";

value (skip, skip_to) =
  let skip1 name skipper cs buf from upto =
    do { Strings_5_1.check_substring_spec buf from upto name; skipper cs buf from upto }
  in
  (skip1 "Charset.skip" unsafe_skip, skip1 "Charset.skip_to" unsafe_skip_to)
;

(* protected constants. *)

value empty = make_empty ();
value full = make_full ();

value lower_case =
  let a_z = of_ucs_range 0x61 0x7B in
  let latin1 = of_ucs_range_bang True a_z 0xdf 0xf7 in
  let latin2 = of_ucs_range_bang True latin1 0xf8 0x100 in
  adjoin_bang latin2 [char_of_latin1 0xb5]
;

value upper_case =
  let big_a_z = of_ucs_range 0x41 0x5B in
  let latin1 = of_ucs_range_bang True big_a_z 0xc0 0xd7 in
  of_ucs_range_bang True latin1 0xd8 0xdf
;

value title_case = empty;

value letter =
  let u_l = union [upper_case; lower_case] in
  adjoin_bang u_l [char_of_latin1 0xaa;(* ; FEMININE ORDINAL INDICATOR. *)
    (* ; MASCULINE ORDINAL INDICATOR. *) char_of_latin1 0xba]
;

value digit = of_string "0123456789";
value hex_digit = of_string "0123456789abcdefABCDEF";
value letter_plus_digit = union [letter; digit];

(* Pretty-print oops. *)
value punctuation =
  let ascii = of_string "!\"#%&'()*,-./:;?@[\\]_{}" in
  let latin_1_chars = List.map char_of_latin1
    [0xA1;(* INVERTED EXCLAMATION MARK. *)
     0xAB;(* LEFT-POINTING DOUBLE ANGLE QUOTATION MARK. *)
     0xAD;(* SOFT HYPHEN. *)
     0xB7;(* MIDDLE DOT. *)
     0xBB;(* RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK. *)
     0xBF (* INVERTED QUESTION MARK. *)] in
  of_list_bang ascii latin_1_chars
;

value symbol =
  let ascii = of_string "$+<=>^`|~" in
  let latin_1_chars =
    List.map char_of_latin1
      [0xA2; (* CENT SIGN. *)
       0xA3; (* POUND SIGN. *)
       0xA4; (* CURRENCY SIGN. *)
       0xA5; (* YEN SIGN. *)
       0xA6; (* BROKEN BAR. *)
       0xA7; (* SECTION SIGN. *)
       0xA8; (* DIAERESIS. *)
       0xA9; (* COPYRIGHT SIGN. *)
       0xAC; (* NOT SIGN. *)
       0xAE; (* REGISTERED SIGN. *)
       0xAF; (* MACRON. *)
       0xB0; (* DEGREE SIGN. *)
       0xB1; (* PLUS-MINUS SIGN. *)
       0xB4; (* ACUTE ACCENT. *)
       0xB6; (* PILCROW SIGN. *)
       0xB8; (* CEDILLA. *)
       0xD7; (* MULTIPLICATION SIGN. *)
       0xF7  (* DIVISION SIGN. *)]
  in
  of_list_bang ascii latin_1_chars
;

value graphic = union [letter_plus_digit; punctuation; symbol];

value whitespace =
  of_list (List.map char_of_latin1
    [0x09; (* HORIZONTAL TABULATION. *)
     0x0A; (* LINE FEED. *)
     0x0B; (* VERTICAL TABULATION. *)
     0x0C; (* FORM FEED. *)
     0x0D; (* CARRIAGE RETURN. *)
     0x20; (* SPACE. *)
     0xA0])(* NO-BREAK SPACE. *)
;

value printing = union [whitespace; graphic];

value blank =
  of_list (List.map char_of_latin1
    [0x09; (* HORIZONTAL TABULATION. *)
     0x20; (* SPACE. *)
     0xA0])(* NO-BREAK SPACE. *)
;

value iso_control = of_ucs_range_bang True (of_ucs_range 0 32) 0x7F 0xA0;

value ascii = of_ucs_range 0 128;

constants.val :=
  [whitespace; lower_case; upper_case; letter; digit; hex_digit; letter_plus_digit; blank;
   empty; full; punctuation; symbol; graphic; printing; iso_control; ascii; title_case];

(* Conversion. *)
value to_charset =
  fun
  [ Cset cs -> cs
  | Str s -> of_string s
  | Char c -> of_char c
  | Predicate p -> filter p full ]
;
