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

value fold_in_channel : in_channel -> (in_channel -> 'a) -> ('a -> 'b -> 'b) -> 'b -> 'b;

value list_of_in_channel : (in_channel -> 'a) -> in_channel -> list 'a;
value string_of_in_channel : in_channel -> string;
value string_list_of_in_channel : in_channel -> list string;
value sexp_list_of_in_channel : in_channel -> list Sexp.simple;

value run_with_in_channel : (unit -> unit) -> in_channel;
value run_with_out_channel : (unit -> unit) -> out_channel;
value run_with_file : (unit -> unit) -> string;
value run_with_string : (unit -> unit) -> string;
value run_with_strings : (unit -> unit) -> list string;
value run_with_sexp : (unit -> unit) -> Sexp.simple;
value run_with_sexps : (unit -> unit) -> list Sexp.simple;
value run_with_inchan_plus_proc : (unit -> unit) -> (in_channel * Procobj.proc);
value run_with_outchan_plus_proc : (unit -> unit) -> (out_channel * Procobj.proc);
value run_with_collecting :
  list Io_3_2.fd -> (unit -> unit) -> (Unix.process_status * list in_channel);

value char_filter : (char -> char) -> unit -> unit;
value string_filter : ?buflen: int -> (string -> string) -> unit -> unit;

value make_string_in_channel : string -> in_channel;
value make_string_out_channel : unit -> out_channel;
value string_out_channel_output : ?close: bool -> out_channel -> string;
value call_with_string_out_channel : ?close: bool -> (out_channel -> unit) -> string;
