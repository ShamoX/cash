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
{
let symtbl = Hashtbl.create 1013;;

open Schlex;;

(* Warning: redefinition of String. *)
type token = Simple of Schlex.token | String of int * string;;

(* XXX should use Buffer.add_string in rule string. *)
(* Stolen from lexer.ml of ocaml 3.01 (as well as the string parser below). *)
let char_for_backslash =
  function
    'n' -> '\n'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\t'
  | c -> c
;;

(* .. but not this one. *)
let char_for_decimal_code str = str.[0] <- '0'; Char.chr (int_of_string str land 0xFF);;

(* XX difficile de faire réentrant sans pouvoir ajouter des arguments à une rule..
   Mais que se passe-t-il si on instancie le module ? *)
let buffer = Buffer.create 16;;

exception Unterminated_string;;

}

let blank = [' ' '\009' '\010' '\012' '\013']
let non_break = [ ^ ' ' '\009' '\010' '\012' '\013'
                  '"' '(' ')' ]
let sign = ['-' '+']
let binary_literal = ['0'-'1']+
let octal_literal = ['0'-'7']+
let decimal_literal = ['0'-'9']+
let hexadecimal_literal = ['0'-'9' 'a'-'f']+ | ['0'-'9' 'A'-'F']+
let float_literal =
  ['0'-'9']+ ('.' ['0'-'9']* )? (['e' 'E'] ['+' '-']? ['0'-'9']+)?

rule simple_token = parse
  | '#' ['t' 'T']
      { Bool true }
  | '#' ['f' 'F']
      { Bool false }
  | sign ? decimal_literal
      { Int (int_of_string (Lexing.lexeme lexbuf)) }
  | sign ? "#"
      ( ['b' 'B'] binary_literal
      | ['o' 'O'] octal_literal
      | ['d' 'D'] decimal_literal
      | ['b' 'B'] hexadecimal_literal)
      { let s = Lexing.lexeme lexbuf in
        let sharp_idx = String.index s '#' in
        if Char.lowercase s.[succ sharp_idx] = 'd' then s.[succ sharp_idx] <- '0';
        s.[sharp_idx] <- '0';
        Int (int_of_string s) }
  | sign ? float_literal
      { Float (float_of_string (Lexing.lexeme lexbuf)) }
  | '('
      { Open_par }
  | ')'
      { Close_par }
  | non_break +
      {
        let st = Lexing.lexeme lexbuf in
        try Hashtbl.find symtbl st with
          Not_found -> let r = Sym st in Hashtbl.add symtbl st r; r
      }
  | eof
      { Eof }

and string = parse
  | '"'
      { let s = Buffer.contents buffer in Buffer.reset buffer; s }
  | '\\' ("\010" | "\013" | "\013\010") [' ' '\009'] *
      { string lexbuf }
  | '\\' ['\\' '"' 'n' 't' 'b' 'r']
      {
        Buffer.add_char buffer (char_for_backslash (Lexing.lexeme_char lexbuf 1));
        string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      {
        Buffer.add_char buffer (char_for_decimal_code (Lexing.lexeme lexbuf));
        string lexbuf }
  | eof
      { raise End_of_file }
  | _
      { Buffer.add_char buffer (Lexing.lexeme_char lexbuf 0);
        string lexbuf }

and next_token = parse
  | blank +
      { next_token lexbuf }
  | '"'
      {
       let start = Lexing.lexeme_start lexbuf in
       try String (start, string lexbuf) with
         End_of_file -> Stdpp.raise_with_loc (start, succ start) Unterminated_string
      }
  | ""
      { Simple (simple_token lexbuf) }

{

let token lexbuf =
  match next_token lexbuf with
  | Simple token ->
      {token = token; loc = Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf}
  | String (start, str) ->
      {token = Schlex.String str; loc = start, Lexing.lexeme_end lexbuf}

}
