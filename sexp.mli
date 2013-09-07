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
type sexp 'a =
  [ Bool of bool
  | Char of char
  | Eof_object
  | Exact of int
  | Inexact of float
  | List of list 'a
  | String of string
  | Symbol of string
  | Vector of array 'a ]
;

type loc_sexp = { sexp : sexp loc_sexp; loc : Schlex.loc };
type simple = { sexp' : sexp simple };

value display : sexp simple -> unit;
value displayl : sexp loc_sexp -> unit;

value parse : Lexing.lexbuf -> option loc_sexp;
value parse_chan : in_channel -> option loc_sexp;

value read1 : Lexing.lexbuf -> simple;
value read : in_channel -> simple;
