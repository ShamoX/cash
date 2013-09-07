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

value y_or_n_eof_count = 100;

value y_or_n_p ?eof_value question =
  loop y_or_n_eof_count where rec loop count =
    do {
      print_string question;
      print_string " (y/n)? ";
      match try Some (read_line ()) with [ End_of_file -> None ] with
      [ None ->
          do {
            print_newline ();
            if count = 0 then
              match eof_value with
              [ None -> invalid_arg "y_or_n_p"
              | Some thing -> thing ]
            else do {
              print_endline ("I'll only ask another " ^ string_of_int count ^ " times.");
              loop (pred count)
            }
          }
      | Some line ->
          if line = "" then loop count
          else
            match line.[0] with
            [ 'y' -> True
            | 'n' -> False
            | _ -> loop count ] ]
    }
;

(* 3.3 File system. *)

value delete_file = Unix.unlink;
value delete_directory = Unix.rmdir;

(*
;; This procedure nukes FNAME, whatever it may be: directory, file, fifo,
;; symlink.

;; We can't probe FNAME to find out what it is and then do the right delete
;; operation because there's a window in-between the probe and the delete where
;; the file system can be altered -- the probe and delete aren't atomic. In
;; order to deliver on our contract, we have to spin in a funny loop until we
;; win. In practice, the loop will probably never execute more than once.
*)

(* Dans le 1er try, on suppose un fichier; EPERM arrive si fname est un directory. *)
(* Dans le 2d try, on suppose un directory; ENOTDIR provoque le rebouclage. *)
value delete_filesys_object fname =
  loop () where rec loop () =
    (try do { delete_file fname; True } with
     [ Unix.Unix_error Unix.EPERM _ _ -> False
     | Unix.Unix_error Unix.ENOENT _ _ -> True ]) ||
    (try do { delete_directory fname; True } with
     [ Unix.Unix_error Unix.ENOTDIR _ _ -> False
     | Unix.Unix_error Unix.ENOENT _ _ -> True ]) ||
    loop ()
;

value read_symlink = Unix.readlink;

value set_file_mode_fn = Unix.chmod;
value (set_file_mode_fd, set_file_mode_in, set_file_mode_out) =
  let chmod f = Unix.fchmod (Io_3_2.file_descr_of_fd f) in
  (chmod, Io_3_2.call_with_fdes_in chmod, Io_3_2.call_with_fdes_out chmod)
;

value (set_file_owner_fn, set_file_owner_fd, set_file_owner_in, set_file_owner_out) =
  let call_with_null_gid f uid = f uid (-1) in
  let fchown_o f = call_with_null_gid (Unix.fchown (Io_3_2.file_descr_of_fd f))
  and chown_o f = call_with_null_gid (Unix.chown f) in
  let sfo_gen call_with_fdes_fun = call_with_fdes_fun fchown_o in
  (chown_o, fchown_o, sfo_gen Io_3_2.call_with_fdes_in, sfo_gen Io_3_2.call_with_fdes_out)
;

value (set_file_group_fn, set_file_group_fd, set_file_group_in, set_file_group_out) =
  let call_with_null_uid f gid = f (-1) gid in
  let fchown_g f = call_with_null_uid (Unix.fchown (Io_3_2.file_descr_of_fd f))
  and chown_g f = call_with_null_uid (Unix.chown f) in
  let sfg_gen call_with_fdes_fun = call_with_fdes_fun fchown_g in
  (chown_g, fchown_g, sfg_gen Io_3_2.call_with_fdes_in, sfg_gen Io_3_2.call_with_fdes_out)
;

value set_file_times ?times fname =
  let (access, modif) =
    match times with
    [ Some times -> times
    | None -> let time = Unix.time () in (time, time) ]
  in
  Unix.utimes fname access modif
;

external sync_file_fd : int -> unit = "sync_file";
external sync_file_system : unit -> unit = "cash_sync";

value sync_file_out ochan =
  do { flush ochan; Io_3_2.sleazy_call_with_fdes_out sync_file_fd ochan }
;

value truncate_file_fn = Unix.truncate;
value (truncate_file_fd, truncate_file_in, truncate_file_out) =
  let truncate f = Unix.ftruncate (Io_3_2.file_descr_of_fd f) in
  (truncate, Io_3_2.call_with_fdes_in truncate, Io_3_2.call_with_fdes_out truncate)
;

type file_perm = int;
type file_kind =
  Unix.file_kind ==
    [ S_REG
    | S_DIR
    | S_CHR
    | S_BLK
    | S_LNK
    | S_FIFO
    | S_SOCK ]
;
type stats =
  Unix.stats ==
    { st_dev : int;
      st_ino : int;
      st_kind : file_kind;
      st_perm : file_perm;
      st_nlink : int;
      st_uid : int;
      st_gid : int;
      st_rdev : int;
      st_size : int;
      st_atime : float;
      st_mtime : float;
      st_ctime : float }
;

(* file-info & friends passe avant rename-file. *)

value file_info_fn ?(chase = True) = if chase then Unix.stat else Unix.lstat;
value (file_info_fd, file_info_in, file_info_out) =
  let stat fd = Unix.fstat (Io_3_2.file_descr_of_fd fd) in
  (stat, Io_3_2.call_with_fdes_in stat, Io_3_2.call_with_fdes_out stat)
;

value make_file_info_selectors =
  let fi_fn_sel selector ?chase fname = selector (file_info_fn ?chase fname)
  and fi_gen_sel selector file_info_fun file = selector (file_info_fun file) in
  fun selector ->
    (fi_fn_sel selector, fi_gen_sel selector file_info_fd,
     fi_gen_sel selector file_info_in, fi_gen_sel selector file_info_out)
;

value (file_type_fn, file_type_fd, file_type_in, file_type_out) =
  make_file_info_selectors (fun x -> x.Unix.st_kind)
;

value (file_inode_fn, file_inode_fd, file_inode_in, file_inode_out) =
  make_file_info_selectors (fun x -> x.Unix.st_ino)
;

value (file_mode_fn, file_mode_fd, file_mode_in, file_mode_out) =
  make_file_info_selectors (fun x -> x.Unix.st_perm)
;

value (file_nlinks_fn, file_nlinks_fd, file_nlinks_in, file_nlinks_out) =
  make_file_info_selectors (fun x -> x.Unix.st_nlink)
;

value (file_owner_fn, file_owner_fd, file_owner_in, file_owner_out) =
  make_file_info_selectors (fun x -> x.Unix.st_uid)
;

value (file_group_fn, file_group_fd, file_group_in, file_group_out) =
  make_file_info_selectors (fun x -> x.Unix.st_gid)
;

value (file_size_fn, file_size_fd, file_size_in, file_size_out) =
  make_file_info_selectors (fun x -> x.Unix.st_size)
;

value
  (file_last_access_fn, file_last_access_fd, file_last_access_in, file_last_access_out) =
  make_file_info_selectors (fun x -> x.Unix.st_atime)
;

value (file_last_mod_fn, file_last_mod_fd, file_last_mod_in, file_last_mod_out) =
  make_file_info_selectors (fun x -> x.Unix.st_mtime)
;

value
  (file_last_status_change_fn, file_last_status_change_fd, file_last_status_change_in,
   file_last_status_change_out) =
  make_file_info_selectors (fun x -> x.Unix.st_ctime)
;

value make_file_type_checkers =
  let ft_fn_chk kind ?chase fname = file_type_fn ?chase fname = kind
  and ft_gen_chk kind file_type_fun file = file_type_fun file = kind in
  fun kind ->
    (ft_fn_chk kind, ft_gen_chk kind file_type_fd, ft_gen_chk kind file_type_in,
     ft_gen_chk kind file_type_out)
;

value
  (is_file_directory_fn, is_file_directory_fd, is_file_directory_in,
   is_file_directory_out) =
  make_file_type_checkers Unix.S_DIR
;

value (is_file_fifo_fn, is_file_fifo_fd, is_file_fifo_in, is_file_fifo_out) =
  make_file_type_checkers Unix.S_FIFO
;

value (is_file_regular_fn, is_file_regular_fd, is_file_regular_in, is_file_regular_out) =
  make_file_type_checkers Unix.S_REG
;

value (is_file_socket_fn, is_file_socket_fd, is_file_socket_in, is_file_socket_out) =
  make_file_type_checkers Unix.S_SOCK
;

value (is_file_special_fn, is_file_special_fd, is_file_special_in, is_file_special_out) =
  let selector kind =
    match kind with
    [ Unix.S_CHR | Unix.S_BLK -> True
    | _ -> False ]
  in
  let ft_fn ?chase fname = selector (file_type_fn ?chase fname)
  and ft_gen file_type_fun file = selector (file_type_fun file) in
  (ft_fn, ft_gen file_type_fd, ft_gen file_type_in, ft_gen file_type_out)
;

value (is_file_symlink_fn, is_file_symlink_fd, is_file_symlink_in, is_file_symlink_out) =
  let selector kind = kind = Unix.S_LNK in
  let fs_fn fname = selector (file_type_fn ~chase:False fname)
  and fs_gen file_type_fun file = selector (file_type_fun file) in
  (fs_fn, fs_gen file_type_fd, fs_gen file_type_in, fs_gen file_type_out)
;

type existing = [ Existing | Unexisting | Search_denied ];

value (file_not_exists_fn, file_not_exists_fd, file_not_exists_in, file_not_exists_out) =
  let selector file_info_fun file =
    try do { ignore (file_info_fun file); Existing } with
    [ Unix.Unix_error Unix.EACCES _ _ -> Search_denied
    | Unix.Unix_error Unix.ENOENT _ _ | Unix.Unix_error Unix.ENOTDIR _ _ -> Unexisting ]
  in
  let fne_fn ?chase = selector (file_info_fn ?chase)
  and fne_gen file_info_fun = selector file_info_fun in
  (fne_fn, fne_gen file_info_fd, fne_gen file_info_in, fne_gen file_info_out)
;

value
  (is_file_existing_fn, is_file_existing_fd, is_file_existing_in, is_file_existing_out) =
  let bool_of_existing =
    fun
    [ Existing -> True
    | _ -> False ]
  in
  let ife_fn ?chase fn = bool_of_existing (file_not_exists_fn ?chase fn)
  and ife_gen file_not_exists_fun file = bool_of_existing (file_not_exists_fun file) in
  (ife_fn, ife_gen file_not_exists_fd, ife_gen file_not_exists_in,
   ife_gen file_not_exists_out)
;

value errno_error errno syscall data = raise (Unix.Unix_error errno syscall data);

type override = [ Don't | Delete | Query ];

value query_maybe ask =
  fun
  [ Query -> ask ()
  | Delete -> True
  | Don't -> False ]
;

value query_for_delete_maybe op_name fname =
  query_maybe (fun () -> y_or_n_p (op_name ^ ": " ^ fname ^ " already exists. Delete"))
;

(*
;; Unix rename() works backwards from mkdir(), mkfifo(), link(), and symlink()
;; -- it overrides by default, (though insisting on a type match between the old
;; and new object).  So we can't use create-file-thing.  Note that this loop has
;; a tiny atomicity problem -- if someone creates a file *after* we do our
;; existence check, but *before* we do the rename, we could end up overriding
;; it, when the user asked us not to.  That's life in the food chain.
*)
value rename_file ?(override = Don't) old_fname new_fname =
  if file_not_exists_fn new_fname <> Existing ||
     query_for_delete_maybe "rename_file" new_fname override
  then
    Unix.rename old_fname new_fname
  else errno_error Unix.EEXIST "rename_file" new_fname
;

(*
;; (file-not-accessible? perms fd/port/fname)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PERMS is 3 bits, not 9.
;; writeable means (1) file exists & is writeable OR (2) file doesn't exist
;;     and directory is writeable. That is, writeable means writeable or
;;     creatable.
;;
;; There's a Posix call, access(), that checks using the *real* uid, not
;; the effective uid, so that setuid programs can figure out if the luser
;; has perms. file-not-accessible? is defined in terms of the effective uid,
;; so we can't use access().
;;
;; This is a kind of bogus function. The only way to do a real check is to
;; try an open() and see if it flies. Otherwise, there's an obvious atomicity
;; problem. Also, we special case root, saying root always has all perms. But
;; not even root can write on a read-only filesystem, such as a CD ROM. In
;; this case, we'd blithely say the file was writeable -- there's no way to
;; check for a ROFS without doing an open(). We need a euid analog to
;; access(). Ah, well.
;;
;; I also should define a family of real uid perm-checking calls.
;;
;; Return values:
;; #f                   Accessible in at least one of the requested ways.
;; search-denied        Can't stat
;; permission           File exists but is protected
;;                      (also for errno/rofs)
;; no-directory Some directory doesn't exist
;; nonexistent          File itself doesn't exist
;;
;; Otherwise, signals an error.
*)
type accessibility =
  [ Accessible | Unaccessible | Permission | No_directory | Nonexistent ]
;

(* Cas ENOENT:
;; If the file doesn't exist, we usually return 'nonexistent, but we
;; special-case writability for the directory check.
 ;; This string? test *has* to return #t. [ match with Fname doit toujours matcher ]
;; If fd/port/fname is an fd or a port, we wouldn't get an errno/noent error!
;; Just being paranoid...
;; [ XX Est-ce mieux de rendre Nonexistent plutôt que raise Sys_error ??? ]
[ appel récursif à is_file_not_accessible ]:
 ;; OK, check to see if we can create files in the directory. 
 *)
value
  (is_file_not_accessible_fn, is_file_not_accessible_fd, is_file_not_accessible_in,
   is_file_not_accessible_out) =
  let check_it perms file_info fn noent_fun =
    let uid = Proc_state_3_5.user_effective_uid () in
    try
      if uid = 0 then Accessible
      else
        let info = file_info fn in
        let mask =
          if info.Unix.st_uid = uid then perms lsl 6
          else if
            info.Unix.st_gid = Proc_state_3_5.user_effective_gid () ||
            List.mem info.Unix.st_gid
              (Array.to_list (Proc_state_3_5.user_supplementary_gids ())) then
            perms lsl 3
          else perms
        in
        if info.Unix.st_perm land mask = 0 then Permission else Accessible
    with
    [ Unix.Unix_error Unix.EACCES _ _ -> Unaccessible
    | Unix.Unix_error Unix.ENOTDIR _ _ -> No_directory
    | Unix.Unix_error Unix.ENOENT _ _ -> noent_fun perms ]
  in
  let rec ifna_fn perms fn =
    check_it perms file_info_fn fn
      (fun perms ->
         if perms land 2 = 0 then Nonexistent
         else
           ifna_fn 2
             (Strings_5_1.directory_as_file_name (Strings_5_1.file_name_directory fn)))
  and ifna_gen file_info_fun perms file =
    check_it perms file_info_fun file
      (fun _ -> failwith "is_file_not_accessible: OS yields ENOENT on an opened file ?")
  in
  (ifna_fn, ifna_gen file_info_fd, ifna_gen file_info_in, ifna_gen file_info_out)
;

value is_file_not_readable_fn = is_file_not_accessible_fn 4;
value is_file_not_readable_fd = is_file_not_accessible_fd 4;
value is_file_not_readable_in = is_file_not_accessible_in 4;
value is_file_not_readable_out = is_file_not_accessible_out 4;

value is_file_not_writable_fn = is_file_not_accessible_fn 2;
value is_file_not_writable_fd = is_file_not_accessible_fd 2;
value is_file_not_writable_in = is_file_not_accessible_in 2;
value is_file_not_writable_out = is_file_not_accessible_out 2;

value is_file_not_executable_fn = is_file_not_accessible_fn 1;
value is_file_not_executable_fd = is_file_not_accessible_fd 1;
value is_file_not_executable_in = is_file_not_accessible_in 1;
value is_file_not_executable_out = is_file_not_accessible_out 1;

value is_file_readable_fn fn = is_file_not_readable_fn fn = Accessible;
value is_file_readable_fd fd = is_file_not_readable_fd fd = Accessible;
value is_file_readable_in ichan = is_file_not_readable_in ichan = Accessible;
value is_file_readable_out ochan = is_file_not_readable_out ochan = Accessible;

value is_file_writable_fn fn = is_file_not_writable_fn fn = Accessible;
value is_file_writable_fd fd = is_file_not_writable_fd fd = Accessible;
value is_file_writable_in ichan = is_file_not_writable_in ichan = Accessible;
value is_file_writable_out ochan = is_file_not_writable_out ochan = Accessible;

value is_file_executable_fn fn = is_file_not_executable_fn fn = Accessible;
value is_file_executable_fd fd = is_file_not_executable_fd fd = Accessible;
value is_file_executable_in ichan = is_file_not_executable_in ichan = Accessible;
value is_file_executable_out ochan = is_file_not_executable_out ochan = Accessible;

(* Abstract out common code for create-{directory,fifo,hard-link,symlink}. *)

value create_file_thing fname makeit override op_name =
  loop override where rec loop override =
    try makeit fname with
    [ Unix.Unix_error Unix.EEXIST _ _ as err ->
        if query_for_delete_maybe op_name fname override then do {
          ignore (delete_filesys_object fname); loop Delete
        }
        else raise err
    | err -> raise err ]
;

value create_directory ?(perms = 0o777) ?(override = Don't) fname =
  create_file_thing fname (fun dir -> Unix.mkdir dir perms) override "create_directory"
;

value create_fifo ?(perms = 0o777) ?(override = Don't) fname =
  create_file_thing fname (fun fifo -> Unix.mkfifo fifo perms) override "create_fifo"
;

value create_hard_link ?(override = Don't) old_fname new_fname =
  create_file_thing new_fname (fun new_fname -> Unix.link old_fname new_fname) override
    "create_hard_link"
;

value create_symlink ?(override = Don't) old_fname new_fname =
  create_file_thing new_fname (fun new_fname -> Unix.symlink old_fname new_fname) override
    "create_symlink"
;

value fold_input filter init reader source =
  loop init where rec loop elts =
    match try Some (reader source) with [ End_of_file -> None ] with
    [ None -> elts
    | Some elt -> loop (filter elts elt) ]
;

value fold_directory fname_filter init dir =
  let dir_handle = Unix.opendir dir in
  let result =
    fold_input
      (fun entries fn ->
         if fn = "." || fn = ".." then entries else fname_filter entries fn)
      init Unix.readdir dir_handle
  in
  do { Unix.closedir dir_handle; result }
;

(* ; Sorts the filenames by the Unix filename order: dotfiles, then others. *)
value directory_files ?(dot_files = False) dir =
  let fnames =
    fold_directory
      (if dot_files then fun fns fn -> [fn :: fns]
       else fun fns fn -> if fn.[0] = '.' then fns else [fn :: fns])
      [] dir
  in
  if dot_files then
    let (dotfns, ndotfns) = List.partition (fun fn -> fn.[0] = '.') fnames in
    let dotfns = List.sort (fun (a : string) (b : string) -> - compare a b) dotfns
    and ndotfns = List.sort (compare : string -> string -> int) ndotfns in
    List.rev_append dotfns ndotfns
  else List.sort compare fnames
;

value template = ref ("/usr/tmp/" ^ string_of_int (Proc_state_3_5.pid ()) ^ ".", "");

value set_temp_file_template new_template = template.val := new_template;

value with_temp_file_template new_template thunk =
  let old_template = template.val in
  do {
    template.val := new_template;
    Env_3_11.unwind_protect thunk (fun old -> template.val := old) old_template
  }
;

value temp_file_iterate ?(template = template.val) maker =
  loop 0 where rec loop i =
    if i > 1000 then failwith "Can't create temp-file"
    else
      let (pfx, sfx) = template in
      let fname = pfx ^ string_of_int i ^ sfx in
      match try maker fname with [ Unix.Unix_error Unix.EEXIST _ _ -> None ] with
      [ None -> loop (succ i)
      | Some v -> v ]
;

(* ;; Create a new temporary file and return its name.
;; The optional argument specifies the filename prefix to use, and defaults to
;; "/usr/tmp/<pid>.", where <pid> is the current process' id. The procedure
;; scans through the files named <prefix>0, <prefix>1, ... until it finds a
;; filename that doesn't exist in the filesystem. It creates the file with
;; permission #o600, and returns the filename.
 *)
value create_temp_file =
  let flags = [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL] in
  fun ?prefix () ->
    temp_file_iterate
      ?template:(
        match prefix with
        [ None -> None
        | Some prefix -> Some (prefix ^ ".", "") ])
      (fun fname ->
         do {
           ignore (Io_3_2.close_fd (Io_3_2.open_fdes ~perms:0o600 fname flags));
           Some fname
         })
;

(* ;; Roughly equivalent to (pipe).
;; Returns two file ports [iport oport] open on a temp file.  Use this when you
;; may have to buffer large quantities between writing and reading. Note that if
;; the consumer gets ahead of the producer, it won't hang waiting for input, it
;; will just return EOF. To play it safe, make sure that the producer runs to
;; completion before starting the consumer.
;; The temp file is deleted before TEMP-FILE-CHANNEL returns, so as soon as the
;; ports are closed, the file's disk storage is reclaimed.
 *)
(* XX unsafe: should use temp_file_iterate to open O_RDWR, then make 2 channels from the fd. *)
value temp_file_channel () =
  let fname = create_temp_file () in
  let iport = Io_3_2.open_input_file fname in
  let oport = Io_3_2.open_output_file fname in
  do { delete_file fname; (iport, oport) }
;
