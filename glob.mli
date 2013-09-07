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
value glob : list string -> list string;
value glob_quote : string -> string;

type file_match_pattern =
  [ String_pat of string | Regexp_pat of Pcre.regexp | Predicate_pat of string -> bool ]
;

value file_match : ?dot_files: bool -> string -> list file_match_pattern -> list string;
