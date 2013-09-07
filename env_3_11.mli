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
value internal_index : char -> string -> int -> int;
value internal_rindex : char -> string -> int -> int;
(* Versions of Strings_5_1.r?index without optional from. *)

value index_with_pred : (char -> bool) -> string -> int -> int;
(* The character's choice is done by a predicate (rather than equality). *)

value substring : string -> int -> int -> string;
(* substring determined by 2 indices (the 2d one is exclusive) rather than
   index, length as in String.sub. *)

value unwind_protect : (unit -> 'a) -> ('b -> unit) -> 'b -> 'a;

value split_env_string : string -> (string * string);
(* "VAR=val" -> ("VAR", "val"). *)

value getenv : string -> string;
value setenv : ?sval: string -> string -> unit;

value alist_of_env : unit -> list (string * string);

(* For Proc_3_4. *)
value env_array_of_alist : list (string * string) -> array string;

value setenv_from_alist : list (string * string) -> unit;

value alist_delete : 'a -> list ('a * 'b) -> list ('a * 'b);
value alist_update : 'a -> 'b -> list ('a * 'b) -> list ('a * 'b);
value alist_compress : list ('a * bool) -> list ('a * bool);

value with_env : list (string * string) -> (unit -> 'a) -> 'a;
value with_total_env : list (string * string) -> (unit -> 'a) -> 'a;

value add_before : 'a -> 'a -> list 'a -> list 'a;
value add_after : 'a -> 'a -> list 'a -> list 'a;

value last_index : string -> int;
value last_char : string -> char;
(* value last_non_slash_from : string -> int -> int; *)
value last_non_slash : string -> int;

value directory_as_file_name : string -> string;

value ensure_file_name_is_nondirectory : string -> string;

value home_directory : ref string;

value exec_path_list : unit -> list string;
value set_exec_path_list : list string -> unit;
value with_exec_path_list : list string -> (unit -> 'a) -> 'a;
