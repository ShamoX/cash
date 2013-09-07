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
open Pcaml;
#load "pa_extend.cmo";
#load "q_MLast.cmo";

value latin1_of_char loc x = <:expr< Char.code $x$ >>;

EXTEND
  expr: LEVEL "simple"
  [[ "latin1_of_char" -> <:expr< Char.code >> ]];
END;

value c0 loc () = <:expr< Char.unsafe_chr 0 >>;
value c1 loc () = <:expr< Char.unsafe_chr 1 >>;

EXTEND
  expr: LEVEL "simple"
  [[ "c0" -> <:expr< $c0 loc ()$ >> ]];
END;
EXTEND
  expr: LEVEL "simple"
  [[ "c1" -> <:expr< $c1 loc ()$ >> ]];
END;

EXTEND
  expr: LEVEL "apply"
  [[  "set0"; cs = expr LEVEL "simple"; i = expr LEVEL "simple" ->
    <:expr< $cs$.[$i$] := $c0 loc ()$ >> ]];
END;
EXTEND
  expr: LEVEL "apply"
  [[  "set1"; cs = expr LEVEL "simple"; i = expr LEVEL "simple" ->
    <:expr< $cs$.[$i$] := $c1 loc ()$ >> ]];
END;

EXTEND
  expr: LEVEL "apply"
  [[  "set0c"; cs = expr LEVEL "simple"; i = expr LEVEL "simple" ->
    <:expr< $cs$.[$latin1_of_char loc i$] := $c0 loc ()$ >> ]];
END;
EXTEND
  expr: LEVEL "apply"
  [[  "set1c"; cs = expr LEVEL "simple"; i = expr LEVEL "simple" ->
    <:expr< $cs$.[$latin1_of_char loc i$] := $c1 loc ()$ >> ]];
END;

EXTEND
  expr: LEVEL "apply"
  [[  "is_zero"; cs = expr LEVEL "simple"; i = expr LEVEL "simple" ->
    <:expr< $c0 loc ()$ = $cs$.[$i$] >> ]];
END;
EXTEND
  expr: LEVEL "apply"
  [[  "is_one"; cs = expr LEVEL "simple"; i = expr LEVEL "simple" ->
    <:expr< $c0 loc ()$ <> $cs$.[$i$] >> ]];
END;

EXTEND
  expr: LEVEL "simple"
  [[ "unsafe_char_of_latin1" -> <:expr< Char.unsafe_chr >> ]];
END;

EXTEND
  expr: LEVEL "apply"
  [[  "si"; cs = expr LEVEL "simple"; i = expr LEVEL "simple" ->
    let x = <:expr< $cs$.[$i$] >> in
    <:expr< $latin1_of_char loc x$ >> ]];
END;
