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

value default_value : option 'a -> 'a -> 'a;

value command_line_arguments : ref (option (list string));
value command_line : unit -> list string;
value make_command_line_arguments : unit -> list string;

value arg : ?default: 'a -> list 'a -> int -> 'a;
value arg_star : ?default_thunk: (unit -> 'a) -> list 'a -> int -> 'a;
value argv : ?default: string -> int -> string;
