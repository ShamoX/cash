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
type token =
  [ Bool of bool
  | Int of int
  | Float of float
  | Sym of string
  | String of string
  | Open_par
  | Close_par
  | Eof ]
;

type loc = (int * int);

type loc_tok = { loc : loc; token : token };
