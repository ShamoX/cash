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
type sexp 'a =
  [ Bool of bool
  | Char of char
  | Eof_object
  | Exact of int
  | Inexact of float
  | List of list 'a
  | String of string
  | Symbol of string
  | Vector of array 'a ]
;

type loc_sexp = { sexp : sexp loc_sexp; loc : Schlex.loc };
type simple = { sexp' : sexp simple };

(* Cas général, pas exporté. *)
value display_g sexp_of =
  let rec pr_list l =
    match l with
    [ [] -> ()
    | [sexp] -> display (sexp_of sexp)
    | [sexp :: sexps] -> do { display (sexp_of sexp); print_char ' '; pr_list sexps } ]
  and display =
    fun
    [ Bool b -> do { print_char '#'; print_char (if b then 't' else 'f') }
    | Char c ->
        do {
          print_string "#\\";
          match c with
          [ '\n' -> print_string "newline"
          | ' ' -> print_string "space"
          | '\\' -> print_string "\\"
          | _ -> print_char c ]
        }
    | Eof_object -> ()
    | Exact i -> print_int i
    | Inexact f ->
        let s = string_of_float f in
        do {
          print_string s;
          try ignore (String.index s '.') with [ Not_found -> print_char '.' ]
        }
    | List l -> do { print_char '('; pr_list l; print_char ')' }
    | String s ->
        do {
          print_char '"';
          for i = 0 to String.length s - 1 do {
            do {
              match s.[i] with
              [ '"' | '\\' -> print_char '\\'
              | _ -> () ];
              print_char s.[i]
            }
          };
          print_char '"'
        }
    | Symbol s -> print_string s
    | Vector v ->
        let maxi = pred (Array.length v) in
        do {
          print_string "#(";
          Array.iteri
            (fun i a ->
               do { display (sexp_of a); print_char (if i = maxi then ')' else ' ') })
            v;
          print_char ')'
        } ]
  in
  display
;

(* Cas particuliers  sexp simple et loc_sexp. *)
value displayl = display_g (fun {sexp = sexp} -> sexp);
value display = display_g (fun {sexp' = sexp} -> sexp);

value read mk_sexp =
  let rec read_no_eof =
    parser
    [ [: `{Schlex.token = Schlex.Bool bool; Schlex.loc = loc} :] ->
        mk_sexp (Bool bool) loc
    | [: `{Schlex.token = Schlex.Int int; Schlex.loc = loc} :] -> mk_sexp (Exact int) loc
    | [: `{Schlex.token = Schlex.Float f; Schlex.loc = loc} :] -> mk_sexp (Inexact f) loc
    | [: `{Schlex.token = Schlex.Sym sym; Schlex.loc = loc} :] -> mk_sexp (Symbol sym) loc
    | [: `{Schlex.token = Schlex.String str; Schlex.loc = loc} :] ->
        mk_sexp (String str) loc
    | [: `{Schlex.token = Schlex.Open_par; Schlex.loc = (lopen, _)}; lst = read_list [];
         `{Schlex.token = Schlex.Close_par; Schlex.loc = (_, lclose)} :] ->
        mk_sexp (List lst) (lopen, lclose) ]
  and read_list rev_sublist =
    parser
    [ [: atom = read_no_eof; s :] -> read_list [atom :: rev_sublist] s
    | [: :] -> List.rev rev_sublist ]
  in
  parser
  [ [: a = read_no_eof :] -> a
  | [: :] -> mk_sexp Eof_object (-1, -1) ]
;

value get_loc_tok lbuf () = Atom.token lbuf; (* Pour tracer. *)

value make_tok_stream tokenizer =
  Stream.from
    (fun _ ->
       match tokenizer () with
       [ {Schlex.token = Schlex.Eof} -> None
       | x -> Some x ])
;

value parse lbuf =
  let stream = make_tok_stream (get_loc_tok lbuf) in
  try Some (read (fun s l -> {sexp = s; loc = l}) stream) with exn ->
    let loc =
      match Stream.peek stream with
      [ Some {Schlex.loc = loc} -> loc
      | None -> (Lexing.lexeme_start lbuf, Lexing.lexeme_end lbuf) ]
    in
    Stdpp.raise_with_loc loc exn
;

value parse_chan chan = parse (Lexing.from_channel chan);

(* Version dégraissées des locations. *)

value read1 lbuf = read (fun s _ -> {sexp'=s}) (make_tok_stream (get_loc_tok lbuf));

value read chan = read1 (Lexing.from_channel chan);
