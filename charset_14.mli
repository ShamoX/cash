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
type t = 'a;

type any_t =
  [ Cset of t
  | Str of string
  | Char of char
  | Predicate of char -> bool ]
;

value to_charset : any_t -> t;

value equal : t -> t -> bool;
value equals : list t -> bool;

value not_greater : t -> t -> bool;(* binary not_greaters. *)
value not_greaters : list t -> bool;

value hash : ?bound: int -> t -> int;

type cursor = 'a;

value cursor : t -> cursor;
value cref : t -> cursor -> char;
value cursor_next : t -> cursor -> cursor;
value is_end_of_charset : cursor -> bool;(* XXX is_at_end ? *)

value fold : (char -> 'a -> 'a) -> 'a -> t -> 'a;

value unfold : ?base_cs: t -> ('a -> char) -> ('a -> bool) -> ('a -> 'a) -> 'a -> t;
value unfold_bang : t -> ('a -> char) -> ('a -> bool) -> ('a -> 'a) -> 'a -> t;

value for_each : (char -> unit) -> t -> unit;
value iter : (char -> unit) -> t -> unit;
value map : (char -> char) -> t -> t;

value copy : t -> t;

value of_list_bang : t -> list char -> t;
value of_list : ?base_cs: t -> list char -> t;

value of_string_bang : t -> string -> t;
value of_string : ?base_cs: t -> string -> t;

value of_char_bang : t -> char -> t;
value of_char : ?base_cs: t -> char -> t;

value filter_bang : t -> (char -> bool) -> t -> t;
value filter : ?base_cs: t -> (char -> bool) -> t -> t;

value of_ucs_range : ?error: bool -> ?base_cs: t -> int -> int -> t;
value of_ucs_range_bang : bool -> t -> int -> int -> t;

value size : t -> int;
value count : (char -> bool) -> t -> int;

value to_list : t -> list char;
value to_string : t -> string;

value contains : t -> char -> bool;
value every : (char -> bool) -> t -> bool;
value for_all : (char -> bool) -> t -> bool;
value any : (char -> bool) -> t -> bool;
value exists : (char -> bool) -> t -> bool;
value find : (char -> bool) -> t -> char;

value adjoin1_bang : t -> char -> t;
value adjoin1 : t -> char -> t;

value adjoin_bang : t -> list char -> t;
value adjoin : t -> list char -> t;

value delete1_bang : t -> char -> t;
value delete1 : t -> char -> t;

value delete_bang : t -> list char -> t;
value delete : t -> list char -> t;

value complement_bang : t -> t;
value complement : t -> t;

value union_bang : t -> list t -> t;
value union : list t -> t;

value intersection_bang : t -> list t -> t;
value intersection : list t -> t;

value difference_bang : t -> list t -> t;
value difference : t -> list t -> t;

value xor_bang : t -> list t -> t;
value xor : list t -> t;

value diff_intersection_bang : t -> t -> list t -> (t * t);
value diff_intersection : t -> list t -> (t * t);

(* Not in SRFI-14 *)
value skip : t -> string -> int -> int -> int;
value skip_to : t -> string -> int -> int -> int;

value empty : t;
value full : t;
value lower_case : t;
value upper_case : t;
value title_case : t;
value letter : t;
value digit : t;
value hex_digit : t;
value letter_plus_digit : t;
value punctuation : t;
value symbol : t;
value graphic : t;
value whitespace : t;
value printing : t;
value blank : t;
value iso_control : t;
value ascii : t;
