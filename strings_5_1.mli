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

value index : ?from: int -> string -> char -> option int;
value rindex : ?from: int -> string -> char -> option int;
value substring : string -> int -> int -> string;
value xsubstring : string -> int -> int -> string;

value is_file_name_directory : string -> bool;
value is_file_name_non_directory : string -> bool;

value file_name_as_directory : string -> string;
value directory_as_file_name : string -> string;

value is_file_name_absolute : string -> bool;

value ensure_file_name_is_directory : string -> string;
value ensure_file_name_is_nondirectory : string -> string;

value file_name_directory : string -> string;
value file_name_nondirectory : string -> string;

(* XX. * )
value split_file_name0 : string -> list string; ( **)
value split_file_name : string -> list string;

value file_name_of_path_list : ?dir: string -> list string -> string;

value file_name_extension : string -> string;
value file_name_sans_extension : string -> string;

value parse_file_name : string -> (string * string * string);
value replace_extension : string -> string -> string;

value simplify_file_name : string -> string;
value resolve_file_name : ?dir: string -> string -> string;
value expand_file_name : ?dir: string -> string -> string;

value absolute_file_name : ?dir: string -> string -> string;
value home_dir : ?user: string -> unit -> string;
value home_file : ?user: string -> string -> string;

value substitute_env_vars : string -> string;

value check_substring_spec : string -> int -> int -> string -> unit;
value opt_end : string -> option int -> int;
