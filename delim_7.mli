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

type termination_kind =
  [ Eof
  | Read of char
  | Full_buffer ]
;

value low_read_delimited_bang :
  ?chan: in_channel -> ?start: int -> ?end_: int -> Charset_14.any_t -> string -> handle_delim ->
    (termination_kind * int);

value read_delimited_bang_split :
  ?chan: in_channel -> ?start: int -> ?end_: int -> Charset_14.any_t -> string ->
    option (int * termination_kind);
value read_delimited_bang :
  ?chan: in_channel -> ?handle_delim: handle_delim -> ?start: int -> ?end_: int ->
    Charset_14.any_t -> string -> option int;

value read_delimited_split : ?chan: in_channel -> Charset_14.any_t -> (string * termination_kind);
value read_delimited :
  ?chan: in_channel -> ?handle_delim: handle_delim -> Charset_14.any_t -> string;

value read_line_split : in_channel -> (string * termination_kind);
value read_line : ?handle_newline: handle_delim -> in_channel -> string;

value read_paragraph_split : in_channel -> (string * string);
value read_paragraph : ?handle_delim: handle_delim -> in_channel -> string;

value skip_char_set : ?chan: in_channel -> Charset_14.any_t -> int;

(* For Rec_field_8. *)
value cs_newline : Charset_14.any_t;
value no_peek : string -> 'a;
value one_line : handle_delim -> in_channel -> string;
