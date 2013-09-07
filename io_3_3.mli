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

value y_or_n_eof_count : int;
value y_or_n_p : ?eof_value: bool -> string -> bool;

value errno_error : Unix.error -> string -> string -> 'a;

type override = [ Don't | Delete | Query ];

type file_perm = int;

value create_directory : ?perms: file_perm -> ?override: override -> string -> unit;
value create_fifo : ?perms: file_perm -> ?override: override -> string -> unit;
value create_hard_link : ?override: override -> string -> string -> unit;
value create_symlink : ?override: override -> string -> string -> unit;

value delete_file : string -> unit;
value delete_directory : string -> unit;
value delete_filesys_object : string -> bool;

value read_symlink : string -> string;
value rename_file : ?override: override -> string -> string -> unit;

value set_file_mode_fn : string -> file_perm -> unit;
value set_file_mode_fd : Io_3_2.fd -> file_perm -> unit;
value set_file_mode_in : in_channel -> file_perm -> unit;
value set_file_mode_out : out_channel -> file_perm -> unit;

value set_file_owner_fn : string -> int -> unit;
value set_file_owner_fd : Io_3_2.fd -> int -> unit;
value set_file_owner_in : in_channel -> int -> unit;
value set_file_owner_out : out_channel -> int -> unit;

value set_file_group_fn : string -> int -> unit;
value set_file_group_fd : Io_3_2.fd -> int -> unit;
value set_file_group_in : in_channel -> int -> unit;
value set_file_group_out : out_channel -> int -> unit;

value set_file_times : ?times: (float * float) -> string -> unit;

value sync_file_fd : int -> unit;
value sync_file_out : out_channel -> unit;
value sync_file_system : unit -> unit;

value truncate_file_fn : string -> int -> unit;
value truncate_file_fd : Io_3_2.fd -> int -> unit;
value truncate_file_in : in_channel -> int -> unit;
value truncate_file_out : out_channel -> int -> unit;

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

value file_info_fn : ?chase: bool -> string -> stats;
value file_info_fd : Io_3_2.fd -> stats;
value file_info_in : in_channel -> stats;
value file_info_out : out_channel -> stats;

value file_type_fn : ?chase: bool -> string -> file_kind;
value file_type_fd : Io_3_2.fd -> file_kind;
value file_type_in : in_channel -> file_kind;
value file_type_out : out_channel -> file_kind;

value file_inode_fn : ?chase: bool -> string -> int;
value file_inode_fd : Io_3_2.fd -> int;
value file_inode_in : in_channel -> int;
value file_inode_out : out_channel -> int;

value file_mode_fn : ?chase: bool -> string -> file_perm;
value file_mode_fd : Io_3_2.fd -> file_perm;
value file_mode_in : in_channel -> file_perm;
value file_mode_out : out_channel -> file_perm;

value file_nlinks_fn : ?chase: bool -> string -> int;
value file_nlinks_fd : Io_3_2.fd -> int;
value file_nlinks_in : in_channel -> int;
value file_nlinks_out : out_channel -> int;

value file_owner_fn : ?chase: bool -> string -> int;
value file_owner_fd : Io_3_2.fd -> int;
value file_owner_in : in_channel -> int;
value file_owner_out : out_channel -> int;

value file_group_fn : ?chase: bool -> string -> int;
value file_group_fd : Io_3_2.fd -> int;
value file_group_in : in_channel -> int;
value file_group_out : out_channel -> int;

value file_size_fn : ?chase: bool -> string -> int;
value file_size_fd : Io_3_2.fd -> int;
value file_size_in : in_channel -> int;
value file_size_out : out_channel -> int;

value file_last_access_fn : ?chase: bool -> string -> float;
value file_last_access_fd : Io_3_2.fd -> float;
value file_last_access_in : in_channel -> float;
value file_last_access_out : out_channel -> float;

value file_last_mod_fn : ?chase: bool -> string -> float;
value file_last_mod_fd : Io_3_2.fd -> float;
value file_last_mod_in : in_channel -> float;
value file_last_mod_out : out_channel -> float;

value file_last_status_change_fn : ?chase: bool -> string -> float;
value file_last_status_change_fd : Io_3_2.fd -> float;
value file_last_status_change_in : in_channel -> float;
value file_last_status_change_out : out_channel -> float;

value is_file_directory_fn : ?chase: bool -> string -> bool;
value is_file_directory_fd : Io_3_2.fd -> bool;
value is_file_directory_in : in_channel -> bool;
value is_file_directory_out : out_channel -> bool;

value is_file_fifo_fn : ?chase: bool -> string -> bool;
value is_file_fifo_fd : Io_3_2.fd -> bool;
value is_file_fifo_in : in_channel -> bool;
value is_file_fifo_out : out_channel -> bool;

value is_file_regular_fn : ?chase: bool -> string -> bool;
value is_file_regular_fd : Io_3_2.fd -> bool;
value is_file_regular_in : in_channel -> bool;
value is_file_regular_out : out_channel -> bool;

value is_file_socket_fn : ?chase: bool -> string -> bool;
value is_file_socket_fd : Io_3_2.fd -> bool;
value is_file_socket_in : in_channel -> bool;
value is_file_socket_out : out_channel -> bool;

value is_file_special_fn : ?chase: bool -> string -> bool;
value is_file_special_fd : Io_3_2.fd -> bool;
value is_file_special_in : in_channel -> bool;
value is_file_special_out : out_channel -> bool;

value is_file_symlink_fn : string -> bool;
value is_file_symlink_fd : Io_3_2.fd -> bool;
value is_file_symlink_in : in_channel -> bool;
value is_file_symlink_out : out_channel -> bool;

type accessibility =
  [ Accessible | Unaccessible | Permission | No_directory | Nonexistent ]
;
(*
value is_file_not_accessible_fn : int -> string -> accessibility;
value is_file_not_accessible_fd : int -> Io_3_2.fd -> accessibility;
value is_file_not_accessible_in : int -> in_channel -> accessibility;
value is_file_not_accessible_out : int -> out_channel -> accessibility;
*)
value is_file_not_readable_fn : string -> accessibility;
value is_file_not_readable_fd : Io_3_2.fd -> accessibility;
value is_file_not_readable_in : in_channel -> accessibility;
value is_file_not_readable_out : out_channel -> accessibility;

value is_file_not_writable_fn : string -> accessibility;
value is_file_not_writable_fd : Io_3_2.fd -> accessibility;
value is_file_not_writable_in : in_channel -> accessibility;
value is_file_not_writable_out : out_channel -> accessibility;

value is_file_not_executable_fn : string -> accessibility;
value is_file_not_executable_fd : Io_3_2.fd -> accessibility;
value is_file_not_executable_in : in_channel -> accessibility;
value is_file_not_executable_out : out_channel -> accessibility;

value is_file_readable_fn : string -> bool;
value is_file_readable_fd : Io_3_2.fd -> bool;
value is_file_readable_in : in_channel -> bool;
value is_file_readable_out : out_channel -> bool;

value is_file_writable_fn : string -> bool;
value is_file_writable_fd : Io_3_2.fd -> bool;
value is_file_writable_in : in_channel -> bool;
value is_file_writable_out : out_channel -> bool;

value is_file_executable_fn : string -> bool;
value is_file_executable_fd : Io_3_2.fd -> bool;
value is_file_executable_in : in_channel -> bool;
value is_file_executable_out : out_channel -> bool;

type existing = [ Existing | Unexisting | Search_denied ];

value file_not_exists_fn : ?chase: bool -> string -> existing;
value file_not_exists_fd : Io_3_2.fd -> existing;
value file_not_exists_in : in_channel -> existing;
value file_not_exists_out : out_channel -> existing;

value is_file_existing_fn : ?chase: bool -> string -> bool;
value is_file_existing_fd : Io_3_2.fd -> bool;
value is_file_existing_in : in_channel -> bool;
value is_file_existing_out : out_channel -> bool;

value query_maybe : (unit -> bool) -> override -> bool;

(* fold_input is a folder in the sort of List.fold_left, but instead of
   folding a 'a list, you give it a read function (as read_line), and a source
   (as stdin).  The elts to fold are computed by applying read to the source. *)
value fold_input : ('a -> 'b -> 'a) -> 'a -> ('c -> 'b) -> 'c -> 'a;

(* fold_directory folds the file names of a directory, EXCEPT `.' and `..'. *)
value fold_directory : ('a -> string -> 'a) -> 'a -> string -> 'a;

value directory_files : ?dot_files: bool -> string -> list string;

value create_temp_file : ?prefix: string -> unit -> string;
value set_temp_file_template : (string * string) -> unit;
value with_temp_file_template : (string * string) -> (unit -> 'a) -> 'a;
value temp_file_iterate : ?template: (string * string) -> (string -> option 'a) -> 'a;
value temp_file_channel : unit -> (in_channel * out_channel);
