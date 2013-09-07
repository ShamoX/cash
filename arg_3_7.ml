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

(* Unused here, only for the other modules. *)
value default_value opt def =
  match opt with
  [ None -> def
  | Some v -> v ]
;

value command_line_arguments = ref None;

(* XXX revoir si (quand) il y a(ura) un flag -s. *)
value command_line =
  let low_command_line = ref None in
  fun () ->
    match low_command_line.val with
    [ None ->
        let cl = Array.to_list Sys.argv in
        do {
          low_command_line.val := Some cl;
          command_line_arguments.val := Some (List.tl cl);
          cl
        }
    | Some command_line -> command_line ]
;

value make_command_line_arguments () =
  do {
    ignore (command_line ());
    match command_line_arguments.val with
    [ None -> assert False
    | Some args -> args ]
  }
;

value arg_star ?default_thunk arglist n =
  let oops () =
    invalid_arg ("arg_star: argument " ^ string_of_int n ^ " out of bounds")
  in
  if n < 1 then oops ()
  else
    let rec loop al n =
      match al with
      [ [car :: cdr] -> if n = 1 then car else loop cdr (pred n)
      | [] ->
          match default_thunk with
          [ None -> oops ()
          | Some thunk -> thunk () ] ]
    in
    loop arglist n
;

value arg ?default arglist n =
  match default with
  [ None -> arg_star arglist n
  | Some default -> arg_star ~default_thunk:(fun () -> default) arglist n ]
;

value argv ?default n = arg ?default (command_line ()) (succ n);
