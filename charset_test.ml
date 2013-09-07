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
open Charset_14;

value vowel c = List.mem c ['a'; 'e'; 'i'; 'o'; 'u'];

value test () =
  do {
    assert (equals []);
    assert (equals [of_list []]);
    assert (equal (of_list ['a'; 'e'; 'i'; 'o'; 'u']) (of_string "ioeauaiii"));
    assert (not (equal (of_list ['e'; 'i'; 'o'; 'u']) (of_string "ioeauaiii")));
    (* . *)
    assert (not_greaters []);
    assert (not_greaters [of_list []]);
    assert (not_greaters [of_list ['a'; 'e'; 'i'; 'o'; 'u']; of_string "ioeauaiii"]);
    assert (not_greaters [of_list ['e'; 'i'; 'o'; 'u']; of_string "ioeauaiii"]);
    ((* . *)
    let h = hash ~bound:100 graphic in
     assert (0 <= h && h <= 99));
    (* . *)
    assert (4 = fold (fun c i -> succ i) 0 (of_list ['e'; 'i'; 'o'; 'u'; 'e'; 'e']));
    (* cset-tests.scm ne copie pas digit, mais ça le modifie...  On le copie maintenant *)
    assert
      (equals
         [of_string "eiaou2468013579999";
          unfold_bang digit List.hd (fun x -> x = []) List.tl
            ['a'; 'e'; 'i'; 'o'; 'u'; 'u'; 'u']]);
    (* . *)
    assert
      (equal (of_string "eiaou2468013579999")
         (unfold_bang (of_string "0123456789") List.hd ( \= []) List.tl
            ['a'; 'e'; 'i'; 'o'; 'u']));
    (* . *)
    assert
      (not
         (equal (of_string "eiaou246801357")
            (unfold_bang (of_string "0123456789") List.hd ( \= []) List.tl
               ['a'; 'e'; 'i'; 'o'; 'u'])));
    ((* . *)
     let cs = Pervasives.ref (of_string "0123456789") in
     do {
       for_each (fun c -> cs.val := delete1 cs.val c) (of_string "02468000");
       assert (equal cs.val (of_string "97531"))
     });
    (let cs = Pervasives.ref (of_string "0123456789") in
     do {
       for_each (fun c -> cs.val := delete1 cs.val c) (of_string "02468");
       assert (not (equal cs.val (of_string "7531")))
     });
    (* . *)
    assert (equal (map Char.uppercase (of_string "aeiou")) (of_string "AEIOU"));
    assert (not (equal (map Char.uppercase (of_string "aeiou")) (of_string "OUAEEEE")));
    (* . *)
    assert (equal (copy (of_string "aeiou")) (of_string "aeiou"));
    (* . *)
    assert (equal (of_list ['x'; 'y']) (of_string "xy"));
    assert (not (equal (of_list ['x'; 'y'; 'z']) (of_string "xy")));
    assert (equal (of_string "xy") (of_list ['x'; 'y']));
    assert (not (equal (of_string "axy") (of_list ['x'; 'y'])));
    (* . *)
    assert (equals [of_list ['x'; 'y']; of_string "xy"]);
    assert (not (equals [of_list ['x'; 'y'; 'z']; of_string "xy"]));
    assert (equals [of_string "xy"; of_list ['x'; 'y']]);
    assert (not (equals [of_string "axy"; of_list ['x'; 'y']]));
    (* . *)
    assert
      (equal (of_string "xy12345") (of_list ~base_cs:(of_string "12345") ['x'; 'y']));
    assert
      (not
         (equal (of_string "y12345") (of_list ~base_cs:(of_string "12345") ['x'; 'y'])));
    (* . *)
    assert (equal (of_string "xy12345") (of_list_bang (of_string "12345") ['x'; 'y']));
    assert
      (not (equal (of_string "y12345") (of_list_bang (of_string "12345") ['x'; 'y'])));
    (* . *)
    assert
      (equal (of_string "aeiou12345") (filter ~base_cs:(of_string "12345") vowel ascii));
    assert
      (not
         (equal (of_string "aeou12345")
            (filter ~base_cs:(of_string "12345") vowel ascii)));
    (* . *)
    assert (equal (of_string "aeiou12345") (filter_bang (of_string "12345") vowel ascii));
    assert
      (not (equal (of_string "aeou12345") (filter_bang (of_string "12345") vowel ascii)));
    (* . *)
    assert
      (equal (of_string "abcdef12345")
         (of_ucs_range ~error:True ~base_cs:(of_string "12345") 97 103));
    assert
      (not
         (equal (of_string "abcef12345")
            (of_ucs_range ~error:True ~base_cs:(of_string "12345") 97 103)));
    assert
      (equal (of_string "abcdef12345")
         (of_ucs_range_bang True (of_string "12345") 97 103));
    assert
      (not
         (equal (of_string "abcef12345")
            (of_ucs_range_bang True (of_string "12345") 97 103)));
    (* . *)
    assert (equals [of_string "x"; of_list ['x']; of_char 'x']);
    assert (not (equals [of_list ['x']; of_string "y"; of_char 'x']));
    (* . *)
    assert (10 = size (intersection [ascii; digit]));
    assert (5 = count vowel ascii);
    (* . *)
    assert (['x'] = to_list (of_char 'x'));
    assert (['X'] <> to_list (of_char 'x'));
    (* . *)
    assert ("x" = to_string (of_char 'x'));
    assert ("X" <> to_string (of_char 'x'));
    (* . *)
    assert (contains (of_string "xyz") 'x');
    assert (not (contains (of_string "xyz") 'a'));
    ((* . *)
    let is_lower c = c = Char.lowercase c
     and is_letter = contains letter in
     do {
       assert (every is_lower (of_string "abcd"));
       assert (not (every is_lower (of_string "abcD")));
       assert (any is_lower (of_string "abcd"));
       assert (not (any is_lower (of_string "ABCD")));
       (* . *)
       assert (contains digit (find is_lower digit));
       assert (is_letter (find is_letter hex_digit));
       assert
         (let c = find (fun c -> is_letter c && is_lower c) hex_digit in
          contains (intersection [letter; lower_case]) c && contains lower_case c);
       assert (try do { ignore (find is_letter digit); False } with [ Not_found -> True ])
     });
    (* . *)
    assert
      (equal (of_string "ABCD")
         (let cs = of_string "abcd" in
          let rec loop cur ans =
            if is_end_of_charset cur then of_list ans
            else loop (cursor_next cs cur) [Char.uppercase (cref cs cur) :: ans]
          in
          loop (cursor cs) []));
    (* . *)
    assert (equal (adjoin (of_string "123") ['x'; 'a']) (of_string "123xa"));
    assert (not (equal (adjoin (of_string "123") ['x'; 'a']) (of_string "123x")));
    assert (equal (adjoin_bang (of_string "123") ['x'; 'a']) (of_string "123xa"));
    assert (not (equal (adjoin_bang (of_string "123") ['x'; 'a']) (of_string "123x")));
    (* . *)
    assert (equal (delete (of_string "123") ['2'; 'a'; '2']) (of_string "13"));
    assert (not (equal (delete (of_string "123") ['2'; 'a'; '2']) (of_string "13a")));
    assert (equal (delete_bang (of_string "123") ['2'; 'a'; '2']) (of_string "13"));
    assert
      (not (equal (delete_bang (of_string "123") ['2'; 'a'; '2']) (of_string "13a")));
    (* . *)
    assert
      (equal (intersection [hex_digit; complement digit]) (of_string "abcdefABCDEF"));
    assert
      (equal (intersection_bang (complement_bang (of_string "0123456789")) [hex_digit])
         (of_string "abcdefABCDEF"));
    (* . *)
    assert
      (equal (union [hex_digit; of_string "abcdefghijkl"])
         (of_string "abcdefABCDEFghijkl0123456789"));
    assert
      (equal (union_bang (of_string "abcdefghijkl") [hex_digit])
         (of_string "abcdefABCDEFghijkl0123456789"));
    (* . *)
    assert
      (equal (difference (of_string "abcdefghijklmn") [hex_digit])
         (of_string "ghijklmn"));
    assert
      (equal (difference_bang (of_string "abcdefghijklmn") [hex_digit])
         (of_string "ghijklmn"));
    (* . *)
    assert (equal (xor [of_string "0123456789"; hex_digit]) (of_string "abcdefABCDEF"));
    assert
      (equal (xor_bang (of_string "0123456789") [hex_digit]) (of_string "abcdefABCDEF"));
    ((* . *)
    let (d, i) = diff_intersection hex_digit [letter] in
     assert (equal d (of_string "0123456789") && equal i (of_string "abcdefABCDEF")));
    let (d, i) =(* . *)
                                  diff_intersection_bang (copy hex_digit) (copy letter) [] in
    assert (equal d (of_string "0123456789") && equal i (of_string "abcdefABCDEF"))
  }
;

value main () =
  let n = if Array.length Sys.argv = 1 then 0 else int_of_string Sys.argv.(1) in
  for i = 0 to n do { test () }
;

if Sys.interactive.val then () else main ();
