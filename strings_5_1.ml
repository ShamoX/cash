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

(* 5.1.2 first. *)

value opt_end s =
  fun
  [ None -> String.length s
  | Some ends -> ends ]
;

(* Scsh compatibility implies 0 < from <= String.length str (&& result < from) *)

value index ?(from = 0) string char =
  let r = Env_3_11.internal_index char string from in
  if r < 0 then None else Some r
;

value rindex ?from string char =
  let from = opt_end string from in
  let r = Env_3_11.internal_rindex char string from in
  if r < 0 then None else Some r
;

value substring = Env_3_11.substring;

value xsubstring str from upto =
  let positive_from = if from >= 0 then from else succ (from + String.length str)
  and positive_upto = if upto >= 0 then upto else succ (upto + String.length str) in
  substring str positive_from positive_upto
;

(* 5.1.1 Manipulating file_names. *)

value is_file_name_directory fname = fname = "" || '/' = Env_3_11.last_char fname;

value is_file_name_non_directory fname = fname = "" || '/' <> Env_3_11.last_char fname;

(* Le 1er cas serait inutile s'il n'évitait pas de tester String.length fname. *)
value file_name_as_directory fname =
  if fname = "" then "/"
  else if fname = "." then ""
  else if Env_3_11.last_char fname = '/' then fname
  else fname ^ "/"
;

value directory_as_file_name = Env_3_11.directory_as_file_name;

value ensure_file_name_is_directory fname =
  if fname = "" then "" else file_name_as_directory fname
;

value ensure_file_name_is_nondirectory = Env_3_11.ensure_file_name_is_nondirectory;

value is_file_name_absolute fname =
  fname = "" || (let c = fname.[0] in c = '/' || c = '~')
;

(*
;; Returns FNAME's directory component in *directory form.*
*)
value file_name_directory fname =
  let rslash = Env_3_11.internal_rindex '/' fname (String.length fname) in
  if rslash < 0 then ""
  else if Env_3_11.last_non_slash fname < 0 then
    (* ; Posix strangeness: solid slashes are root. *)
    ""
  else String.sub fname 0 (succ rslash)
;

value file_name_nondirectory fname =
  let rslash = Env_3_11.internal_rindex '/' fname (String.length fname) in
  if rslash < 0 then fname
  else if Env_3_11.last_non_slash fname < 0 then(* ; Posix strangeness: solid slashes are root. *)
      fname
  else substring fname (succ rslash) (String.length fname)
;

(*  XX il doit y avoir moyen de faire exactement équivalent en tail_rec, zut. *)
value split_file_name fname =
  let fname = ensure_file_name_is_nondirectory fname in
  let len = String.length fname in
  let rec split start =
    if start >= len then []
    else
      let slash = Env_3_11.internal_index '/' fname start in
      if slash >= 0 then [substring fname start slash :: split (succ slash)]
      else [substring fname start len]
  in
  split 0
;
(* *)

(* L'ordre `naturel' coûte 3 tests en sortie pour ajouter le "" initial pour les
   paths absolus, sauf "/" et "//" (seuls cas où le test 3 rend faux). * )
value split_file_name fname =
  let fname = ensure_file_name_is_nondirectory fname in
  let rec loop last res =
    if last <= 0 then res
    else
      let first = Env_3_11.internal_rindex '/' fname last in
      loop first [substring fname (succ first) last :: res]
  in
  let len = String.length fname in
  let res = loop len [] in
  if len > 0 && fname.[0] = '/' && fname.[pred len] <> '/' then ["" :: res] else res
;
( **)

value file_name_of_path_list ?(dir = ".") pathlist =
  let pathlist =
    match pathlist with
    [ ["" :: _] -> pathlist
    | _ ->
        let root = file_name_as_directory (ensure_file_name_is_nondirectory dir) in
        match pathlist with
        [ [] -> [root]
        | [car :: cdr] -> [root ^ car :: cdr] ] ]
  in
  String.concat "/" pathlist
;

value (file_name_sans_extension, file_name_extension) =
  let extension_index fname =
    let dot = Env_3_11.internal_rindex '.' fname (String.length fname) in
    if dot > 0 && fname.[pred dot] <> '/' then dot else String.length fname
  in
  (fun fname -> String.sub fname 0 (extension_index fname),
   fun fname -> substring fname (extension_index fname) (String.length fname))
;

value replace_extension fname extension = file_name_sans_extension fname ^ extension;

value parse_file_name fname =
  let nd = file_name_nondirectory fname in
  (file_name_directory fname, file_name_sans_extension nd, file_name_extension nd)
;

value resolve_tilde_file_name fname =
  let len = String.length fname in
  if len > 0 && fname.[0] = '~' then
    let homedir_of_tilde end' =
      if end' = 1 then Env_3_11.home_directory.val
      else
        let user = substring fname 1 end' in
        (Unix.getpwnam user).Unix.pw_dir
    in
    let slash = Env_3_11.internal_index '/' fname 1 in
    if slash < 0 then homedir_of_tilde len
    else homedir_of_tilde slash ^ "/" ^ substring fname (succ slash) len
  else fname
;

value resolve_file_name ?(dir = ".") fname =
  let dir = ensure_file_name_is_nondirectory dir
  and fname = ensure_file_name_is_nondirectory fname in
  if fname = "" then "/"
  else
    match fname.[0] with
    [ '/' -> fname
    | '~' -> resolve_tilde_file_name fname
    | _ -> file_name_as_directory dir ^ fname ]
;

(* 
;; _ Remove leading and internal occurrences of dot.  A trailing dot is left
;;   alone, in case the parent is a symlink.
;; _ Remove internal and trailing double_slashes.  A leading double_slash is
;;   left alone, in accordance w/Posix.  However, triple and more leading
;;   slashes are reduced to a single slash, in accordance w/Posix.
;; _ Double_dots are left alone, in case they come after symlinks.
 *)
(* Let XX, YY be: let (slashes, fname) = XX in YY
  ;; First (in XX), we simplify leading multiple slashes:
  ;; 1 or >2 slashes -> /, 2 slashes -> //
   ;;
  ;; Then (between XX and YY), all leading slashes have been pulled off of FNAME.
  ;; Any remaining repeated slashes are fair game for removal.
 *)

value simplify_file_name fname =
  let (slashes, fname) =
    let len = String.length fname in
    if len > 0 && fname.[0] = '/' then
      let j =
        scan_slashes 1 where rec scan_slashes i =
          if i < len && fname.[i] = '/' then scan_slashes (succ i) else i
      in
      if j < 3 then (substring fname 0 j, substring fname j len)
      else ("/", substring fname (pred j) len)
    else ("", fname)
  in
  let path_list = split_file_name fname
  and slashes = [slashes] in
  let ans =
    if path_list = [] then slashes
    else
      let rec loop ans =
        fun
        [ [] -> invalid_arg "loop in simplify_file_name"
        | [elt] -> [elt :: ans]
        | [elt :: path_list] ->
            loop (if elt = "" || elt = "." then ans else ["/"; elt :: ans]) path_list ]
      in
      List.rev (loop slashes path_list)
  in
  String.concat "" ans
;

value expand_file_name ?dir fname = simplify_file_name (resolve_file_name ?dir fname);

value absolute_file_name ?dir fname =
  let fname = ensure_file_name_is_nondirectory fname in
  if fname = "" then "/"
  else
    simplify_file_name
      (if fname.[0] = '/' then fname
       else
         let root =
           match dir with
           [ None -> Unix.getcwd ()
           | Some dir -> dir ]
         in
         file_name_as_directory root ^ fname)
;

value home_dir ?user () =
  match user with
  [ None -> Env_3_11.home_directory.val
  | Some user -> ensure_file_name_is_nondirectory (Unix.getpwnam user).Unix.pw_dir ]
;

value home_file ?user fname = file_name_as_directory (home_dir ?user ()) ^ fname;

(* ;;; Ugh. *)
value substitute_env_vars =
  let getenv s = try Sys.getenv s with [ Not_found -> "" ]
  and non_ident_char =
    fun
    [ 'a'..'z' | 'A'..'Z' | '_' | '0'..'9' -> False
    | _ -> True ]
  in
  let rec loop answer str =
    let len = String.length str in
    if len = 0 then String.concat "" (List.rev answer)
    else
      let dollar_index = Env_3_11.internal_index '$' str 0 in
      if dollar_index < 0 then loop [str :: answer] ""
      else
        let answer = [substring str 0 dollar_index :: answer]
        and str = substring str (succ dollar_index) len
        and len = len - succ dollar_index in
        if len = 0 then loop answer ""
        else if str.[0] <> '{' then
          let i =
            let i = Env_3_11.index_with_pred non_ident_char str 0 in
            if i < 0 then len else i
          in
          loop [getenv (substring str 0 i) :: answer] (substring str i len)
        else
          let close_index = Env_3_11.internal_index '}' str 0 in
          if close_index < 0 then
            invalid_arg "substitute_env_vars: Unbalanced ${ delimiter in string"
          else
            loop [getenv (substring str 1 close_index) :: answer]
              (substring str (succ close_index) len)
  in
  loop []
;

(* Unused here, only for the other modules. *)
value check_substring_spec s start ends caller_name =
  if start < 0 || String.length s < ends || ends < start then
    invalid_arg (caller_name ^ ": bad substring indices")
  else ()
;
