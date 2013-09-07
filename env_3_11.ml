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

(* Scsh compatibility implies 0 < from <= String.length str (&& result < from) *)

value internal_index sep str =
  let len = String.length str in
  let rec loop i = if i >= len then -1 else if sep = str.[i] then i else loop (succ i) in
  loop
;

value index_with_pred predicate str =
  let len = String.length str in
  let rec loop i =
    if i >= len then -1 else if predicate str.[i] then i else loop (succ i)
  in
  loop
;

value internal_rindex sep str from =
  loop (pred from) where rec loop i = if i < 0 || sep = str.[i] then i else loop (pred i)
;

value substring s from exclusive_to = String.sub s from (exclusive_to - from);

(* C'est le 1er module qui s'en sert; seule raison de sa présence ici. *)
value unwind_protect thunk handler arg =
  try
    let res = thunk () in
    do { (handler arg : unit); res }
  with exc ->
    do { handler arg; raise exc }
;

(* L'environnement proprement dit. *)

value split_env_string s =
  let i = internal_index '=' s 0 in
  if i < 0 then invalid_arg ("No `=' in environment string " ^ s)
  else (substring s 0 i, substring s (succ i) (String.length s))
;

value alist_of_env () =
  Array.fold_left (fun alist elt -> [split_env_string elt :: alist]) []
    (Unix.environment ())
;

value getenv = Unix.getenv;

external set_environ : array string -> unit = "cash_set_environ";
external set_environ_of_list : list string -> unit = "cash_set_environ_of_list";

value env_list_of_alist alist = List.map (fun (var, val) -> var ^ "=" ^ val) alist;
(* For Proc_3_4: *)
value env_array_of_alist alist = Array.of_list (env_list_of_alist alist);

(* scsh n'implémente pas (var, [vals] malgré la doc. *)
(* Pour ne pas imposer une représentation, on ne supporte pas. *)
value setenv_from_alist alist = set_environ_of_list (env_list_of_alist alist);

(* List.remove_assoc existe, mais filter est tail-recursive. *)
value alist_delete key = List.filter (fun ((k, _) as elt) -> k <> key);

value alist_update1 ((key, _) as key'val) alist = [key'val :: alist_delete key alist];

value alist_update key val = alist_update1 (key, val);

(* ; Remove shadowed entries from ALIST.  Preserves element order.
;; (This version shares no structure.). *) 
value alist_compress alist =
  List.rev
    (List.fold_left
       (fun ans ((k, _) as elt) -> if List.assoc k ans then ans else [elt :: ans]) []
       alist)
;

value setenv ?sval var =
  match sval with
  [ Some sval -> Unix.putenv var sval
  | None -> setenv_from_alist (alist_delete var (alist_of_env ())) ]
;

value with_total_env alist thunk =
  let env = Unix.environment () in
  do { setenv_from_alist alist; unwind_protect thunk set_environ env }
;

value with_env alist_delta =
  with_total_env
    (List.fold_left (fun env varval -> alist_update1 varval env) (alist_of_env ())
       alist_delta)
;

(* add_before n'est pas tail-recursive, on ne va pas faire compliqué comme scsh
   alors que les listes ne peuvent pas être énormes. *)
value add_before elt before =
  loop where rec loop =
    fun
    [ [] -> [elt]
    | [car :: cdr] as list -> if car = before then [elt :: list] else [car :: loop cdr] ]
;

value add_after elt after list = List.rev (add_before elt after (List.rev list));

value last_index fname = pred (String.length fname);
value last_char fname = fname.[pred (String.length fname)];

(* rend -1 si la chaîne ne contient pas de /. *)
value rec last_non_slash_from str i =
  if i >= 0 && str.[i] = '/' then last_non_slash_from str (pred i) else i
;

value last_non_slash str = last_non_slash_from str (last_index str);

(* Ici à cause de home_directory, mais normalement dans strings_5_1. *)

value directory_as_file_name fname =
  let last_index = last_index fname in
  if last_index < 0 then "."
  else
    let last_non_slash = last_non_slash_from fname last_index in
    if last_non_slash >= 0 then
      if last_non_slash = last_index then fname
      else String.sub fname 0 (succ last_non_slash)
    else if last_index = 1 then "//"
    else "/"
;

(* Ici à cause de home_directory, mais normalement dans strings_5_1. *)

value ensure_file_name_is_nondirectory fname =
  if fname = "" then "" else directory_as_file_name fname
;

value home_directory =
  ref
    (ensure_file_name_is_nondirectory
       (try Sys.getenv "HOME" with
        [ Not_found -> (Unix.getpwuid (Unix.getuid ())).Unix.pw_dir ]))
;

value (exec_path_list, set_exec_path_list) =
  let epl = ref None in
  (fun () ->
     match epl.val with
     [ Some thing -> thing
     | None ->
         try
           match Sys.getenv "PATH" with
           [ "" -> []
           | s -> Pcre.split ~pat:":" ~max:max_int s ]
         with
         [ Not_found -> ["/bin"; "/usr/bin"] ] ],
   fun new_val -> epl.val := Some new_val)
;

value with_exec_path_list other_exec_path_list thunk =
  let std_exec_path_list = exec_path_list () in
  do {
    set_exec_path_list other_exec_path_list;
    unwind_protect thunk set_exec_path_list std_exec_path_list
  }
;
