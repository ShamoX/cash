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

value record_reader :
  ?delims: Charset_14.any_t -> ?elide_delims: bool ->
    ?handle_delim: Delim_7.handle_delim -> unit -> in_channel -> string;

value record_reader_split :
  ?delims: Charset_14.any_t -> ?elide_delims: bool -> unit -> in_channel ->
    (string * string);

type handle_field_delim =
  [ Trim_f
  | Split_f
  | Concat_f ]
and delim_matcher =
  [ Match_proc of string -> int -> (int * int)
  | String of string
  | Charset of Charset_14.any_t
  | Regexp of Pcre.regexp
  | Pattern of string ]
;

value default_field_matcher : delim_matcher;
value field_splitter :
  ?field: delim_matcher -> ?num_fields: int -> unit -> ?start: int -> string ->
    list string;

value default_infix_matcher : delim_matcher;
value infix_splitter :
  ?delim: delim_matcher -> ?num_fields: int -> ?handle_delim: handle_field_delim ->
    unit -> ?start: int -> string -> list string;

value default_suffix_matcher : delim_matcher;
value suffix_splitter :
  ?delim: delim_matcher -> ?num_fields: int -> ?handle_delim: handle_field_delim ->
    unit -> ?start: int -> string -> list string;
value sloppy_suffix_splitter :
  ?delim: delim_matcher -> ?num_fields: int -> ?handle_delim: handle_field_delim ->
    unit -> ?start: int -> string -> list string;

value default_field_parser : ?start: int -> string -> list string;
value gen_field_reader :
  ('record -> 'fields) -> ('chan -> 'record) -> 'chan -> ('record * 'fields);
value field_reader :
  ?field_parser: (?start: int -> string -> list string) ->
    ?rec_reader: (in_channel -> string) -> unit -> in_channel -> (string * list string);
