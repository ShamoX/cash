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

value version = "0.20";

include
  (Unix :
   sig
     type seek_command =
       Unix.seek_command ==
         [ SEEK_SET
         | SEEK_CUR
         | SEEK_END ]
     ;
     type open_flag =
       Unix.open_flag ==
         [ O_RDONLY
         | O_WRONLY
         | O_RDWR
         | O_NONBLOCK
         | O_APPEND
         | O_CREAT
         | O_TRUNC
         | O_EXCL
         | O_NOCTTY
         | O_DSYNC
         | O_SYNC
         | O_RSYNC ]
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
     type process_status =
       Unix.process_status ==
         [ WEXITED of int
         | WSIGNALED of int
         | WSTOPPED of int ]
     ;
     type process_times =
       Unix.process_times ==
         { tms_utime : float; tms_stime : float; tms_cutime : float; tms_cstime : float }
     ;
     type wait_flag =
       Unix.wait_flag ==
         [ WNOHANG
         | WUNTRACED ]
     ;
     type socket_domain =
       Unix.socket_domain ==
         [ PF_UNIX
         | PF_INET ]
     ;
     type socket_type =
       Unix.socket_type ==
         [ SOCK_STREAM
         | SOCK_DGRAM
         | SOCK_RAW
         | SOCK_SEQPACKET ]
     ;
     type inet_addr = Unix.inet_addr;
     type sockaddr =
       Unix.sockaddr ==
         [ ADDR_UNIX of string
         | ADDR_INET of inet_addr and int ]
     ;
     type shutdown_command =
       Unix.shutdown_command ==
         [ SHUTDOWN_RECEIVE
         | SHUTDOWN_SEND
         | SHUTDOWN_ALL ]
     ;
     type msg_flag =
       Unix.msg_flag ==
         [ MSG_OOB
         | MSG_DONTROUTE
         | MSG_PEEK ]
     ;
     type socket_bool_option =
       Unix.socket_bool_option ==
         [ SO_DEBUG
         | SO_BROADCAST
         | SO_REUSEADDR
         | SO_KEEPALIVE
         | SO_DONTROUTE
         | SO_OOBINLINE
         | SO_ACCEPTCONN ]
     ;
     type socket_int_option =
       Unix.socket_int_option ==
         [ SO_SNDBUF
         | SO_RCVBUF
         | SO_ERROR
         | SO_TYPE
         | SO_RCVLOWAT
         | SO_SNDLOWAT ]
     ;
     type socket_optint_option =
       Unix.socket_optint_option ==
         [ SO_LINGER ]
     ;
     type socket_float_option =
       Unix.socket_float_option ==
         [ SO_RCVTIMEO
         | SO_SNDTIMEO ]
     ;
   end);

type itimer =
  Unix.interval_timer ==
    [ ITIMER_REAL
    | ITIMER_VIRTUAL
    | ITIMER_PROF ]
;

type itimer_status =
  Unix.interval_timer_status == { it_interval : float; it_value : float }
;

type fd = int;

(* Comes from Procobj, in fact; but we need it before for Pf_2_3_to_6. *)
include (Proc_3_4 : sig type proc = Proc_3_4.proc; end);

include
  (Pf_2_3_to_6 :
   sig
     value string_of_in_channel : in_channel -> string;
     value string_list_of_in_channel : in_channel -> list string;
     value sexp_list_of_in_channel : in_channel -> list Sexp.simple;
     value list_of_in_channel : (in_channel -> 'a) -> in_channel -> list 'a;
     value fold_in_channel :
       in_channel -> (in_channel -> 'a) -> ('a -> 'b -> 'b) -> 'b -> 'b;
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
       list fd -> (unit -> unit) -> (process_status * list in_channel);
     value char_filter : (char -> char) -> unit -> unit;
     value string_filter : ?buflen: int -> (string -> string) -> unit -> unit;
     value make_string_in_channel : string -> in_channel;
     value make_string_out_channel : unit -> out_channel;
     value string_out_channel_output : ?close: bool -> out_channel -> string;
     value call_with_string_out_channel : ?close: bool -> (out_channel -> unit) -> string;
   end);

include
  (Io_3_2 :
   sig
     value close_fd_after : fd -> (fd -> 'a) -> 'a;
     value close_in_after : in_channel -> (in_channel -> 'a) -> 'a;
     value close_out_after : out_channel -> (out_channel -> 'a) -> 'a;
     value with_stdin : in_channel -> (unit -> 'a) -> 'a;
     value with_stdout : out_channel -> (unit -> 'a) -> 'a;
     value with_stderr : out_channel -> (unit -> 'a) -> 'a;
     value set_stdin : in_channel -> unit;
     value set_stdout : out_channel -> unit;
     value set_stderr : out_channel -> unit;
     value close_fd : fd -> bool;
     value close_in : in_channel -> bool;
     value close_out : out_channel -> bool;
     value stdchans_to_stdio : unit -> unit;
     value stdio_to_stdchans : unit -> unit;
     value with_stdio_chans : (unit -> 'a) -> 'a;
     value in_channel_of_fd : fd -> in_channel;
     value out_channel_of_fd : fd -> out_channel;
     value fd_of_in_channel : in_channel -> fd;
     value fd_of_out_channel : out_channel -> fd;
     value call_with_fdes_in : (fd -> 'a) -> in_channel -> 'a;
     value call_with_fdes_out : (fd -> 'a) -> out_channel -> 'a;
     value move_fd_to_fdes : fd -> fd -> fd;
     value move_in_channel_to_fdes : in_channel -> fd -> in_channel;
     value move_out_channel_to_fdes : out_channel -> fd -> out_channel;
     value dup_fd : ?newfd: fd -> fd -> fd;
     value fdes_of_dup_in : ?newfd: fd -> in_channel -> fd;
     value fdes_of_dup_out : ?newfd: fd -> out_channel -> fd;
     value in_channel_of_dup_fd : ?newfd: fd -> fd -> in_channel;
     value dup_in : ?newfd: fd -> in_channel -> in_channel;
     value in_channel_of_dup_out : ?newfd: fd -> out_channel -> in_channel;
     value out_channel_of_dup_fd : ?newfd: fd -> fd -> out_channel;
     value out_channel_of_dup_in : ?newfd: fd -> in_channel -> out_channel;
     value dup_out : ?newfd: fd -> out_channel -> out_channel;
     value seek_fd : ?whence: seek_command -> fd -> int -> int;
     value seek_in : ?whence: seek_command -> in_channel -> int -> int;
     value seek_out : ?whence: seek_command -> out_channel -> int -> int;
     value tell_fd : fd -> int;
     value tell_in : in_channel -> int;
     value tell_out : out_channel -> int;
     value open_file_out : ?perms: file_perm -> string -> list open_flag -> out_channel;
     value open_file_in : ?perms: file_perm -> string -> list open_flag -> in_channel;
     value open_input_file : ?flags: (list open_flag) -> string -> in_channel;
     value open_output_file :
       ?flags: (list open_flag) -> ?perms: file_perm -> string -> out_channel;
     value with_input_from_file : string -> (unit -> 'a) -> 'a;
     value with_output_to_file : string -> (unit -> 'a) -> 'a;
     value with_errors_to_file : string -> (unit -> 'a) -> 'a;
     value call_with_input_file : string -> (in_channel -> 'a) -> 'a;
     value call_with_output_file : string -> (out_channel -> 'a) -> 'a;
     value call_with_fdes_fn :
       ?perms: file_perm -> string -> list open_flag -> (fd -> 'a) -> 'a;
     value open_fdes : ?perms: file_perm -> string -> list open_flag -> fd;
     value openfile : string -> list open_flag -> file_perm -> Unix.file_descr;
     type fdes_flags =
       [ FD_CLOEXEC ]
     ;
     value fdes_flags_fd : fd -> list fdes_flags;
     value fdes_flags_in : in_channel -> list fdes_flags;
     value fdes_flags_out : out_channel -> list fdes_flags;
     value set_fdes_flags_fd : fd -> list fdes_flags -> unit;
     value set_fdes_flags_in : in_channel -> list fdes_flags -> unit;
     value set_fdes_flags_out : out_channel -> list fdes_flags -> unit;
     value fdes_status_fd : fd -> list open_flag;
     value fdes_status_in : in_channel -> list open_flag;
     value fdes_status_out : out_channel -> list open_flag;
     value set_fdes_status_fd : fd -> list open_flag -> unit;
     value set_fdes_status_in : in_channel -> list open_flag -> unit;
     value set_fdes_status_out : out_channel -> list open_flag -> unit;
     value pipe : unit -> (in_channel * out_channel);
     type error_packet =
       [ Sys__error of string
       | Unix__error of (Unix.error * string * string) ]
     ;
     exception String_io_error of (error_packet * string * string * int * int * int);
     value read_string : ?src: fd -> int -> string;
     value read_string_in : ?src: in_channel -> int -> string;
     value read_string_bang : ?src: fd -> ?start: int -> ?end_: int -> string -> int;
     value read_string_bang_in :
       ?src: in_channel -> ?start: int -> ?end_: int -> string -> int;
     value read_string_partial : ?src: fd -> int -> string;
     value read_string_partial_in : ?src: in_channel -> int -> string;
     value read_string_bang_partial :
       ?src: fd -> ?start: int -> ?end_: int -> string -> int;
     value read_string_bang_partial_in :
       ?src: in_channel -> ?start: int -> ?end_: int -> string -> int;
     value write_string : ?dst: fd -> ?start: int -> ?end_: int -> string -> unit;
     value write_string_out :
       ?dst: out_channel -> ?start: int -> ?end_: int -> string -> unit;
     value write_string_partial : ?dst: fd -> ?start: int -> ?end_: int -> string -> int;
     value write_string_partial_out :
       ?dst: out_channel -> ?start: int -> ?end_: int -> string -> int;
     type selectable =
       [ Nothing
       | Read_in of in_channel
       | Read_fd of fd
       | Write_out of out_channel
       | Write_fd of fd
       | Except_in of in_channel
       | Except_fd of fd ]
     ;
     value select_bang : ?timeout: float -> array selectable -> (int * int * int);
     value select : ?timeout: float -> array selectable -> array selectable;
     type bufpolicy =
       [ Block
       | Line
       | Nobuf ]
     ;
     value set_chan_buffering_in : in_channel -> ?size: int -> bufpolicy -> unit;
     value set_chan_buffering_out : out_channel -> ?size: int -> bufpolicy -> unit;
     value force_output : out_channel -> unit;
     value flush_all_chans : unit -> unit;
   end);

external in_channel_revealed : in_channel -> int = "chan_revealed_count";
external out_channel_revealed : out_channel -> int = "chan_revealed_count";

external release_in_channel_handle : in_channel -> unit = "release_chan_handle";
external release_out_channel_handle : out_channel -> unit = "release_chan_handle";

type file_info =
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

include
  (Io_3_3 :
   sig
     value errno_error : Unix.error -> string -> string -> 'a;
     type override =
       Io_3_3.override ==
         [ Don't
         | Delete
         | Query ]
     ;
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
     value set_file_mode_fd : fd -> file_perm -> unit;
     value set_file_mode_in : in_channel -> file_perm -> unit;
     value set_file_mode_out : out_channel -> file_perm -> unit;
     value set_file_owner_fn : string -> int -> unit;
     value set_file_owner_fd : fd -> int -> unit;
     value set_file_owner_in : in_channel -> int -> unit;
     value set_file_owner_out : out_channel -> int -> unit;
     value set_file_group_fn : string -> int -> unit;
     value set_file_group_fd : fd -> int -> unit;
     value set_file_group_in : in_channel -> int -> unit;
     value set_file_group_out : out_channel -> int -> unit;
     value set_file_times : ?times: (float * float) -> string -> unit;
     value sync_file_fd : int -> unit;
     value sync_file_out : out_channel -> unit;
     value sync_file_system : unit -> unit;
     value truncate_file_fn : string -> int -> unit;
     value truncate_file_fd : fd -> int -> unit;
     value truncate_file_in : in_channel -> int -> unit;
     value truncate_file_out : out_channel -> int -> unit;
     value file_info_fn : ?chase: bool -> string -> file_info;
     value file_info_fd : fd -> file_info;
     value file_info_in : in_channel -> file_info;
     value file_info_out : out_channel -> file_info;
     value file_type_fn : ?chase: bool -> string -> file_kind;
     value file_type_fd : fd -> file_kind;
     value file_type_in : in_channel -> file_kind;
     value file_type_out : out_channel -> file_kind;
     value file_inode_fn : ?chase: bool -> string -> int;
     value file_inode_fd : fd -> int;
     value file_inode_in : in_channel -> int;
     value file_inode_out : out_channel -> int;
     value file_mode_fn : ?chase: bool -> string -> file_perm;
     value file_mode_fd : fd -> file_perm;
     value file_mode_in : in_channel -> file_perm;
     value file_mode_out : out_channel -> file_perm;
     value file_nlinks_fn : ?chase: bool -> string -> int;
     value file_nlinks_fd : fd -> int;
     value file_nlinks_in : in_channel -> int;
     value file_nlinks_out : out_channel -> int;
     value file_owner_fn : ?chase: bool -> string -> int;
     value file_owner_fd : fd -> int;
     value file_owner_in : in_channel -> int;
     value file_owner_out : out_channel -> int;
     value file_group_fn : ?chase: bool -> string -> int;
     value file_group_fd : fd -> int;
     value file_group_in : in_channel -> int;
     value file_group_out : out_channel -> int;
     value file_size_fn : ?chase: bool -> string -> int;
     value file_size_fd : fd -> int;
     value file_size_in : in_channel -> int;
     value file_size_out : out_channel -> int;
     value file_last_access_fn : ?chase: bool -> string -> float;
     value file_last_access_fd : fd -> float;
     value file_last_access_in : in_channel -> float;
     value file_last_access_out : out_channel -> float;
     value file_last_mod_fn : ?chase: bool -> string -> float;
     value file_last_mod_fd : fd -> float;
     value file_last_mod_in : in_channel -> float;
     value file_last_mod_out : out_channel -> float;
     value file_last_status_change_fn : ?chase: bool -> string -> float;
     value file_last_status_change_fd : fd -> float;
     value file_last_status_change_in : in_channel -> float;
     value file_last_status_change_out : out_channel -> float;
     value is_file_directory_fn : ?chase: bool -> string -> bool;
     value is_file_directory_fd : fd -> bool;
     value is_file_directory_in : in_channel -> bool;
     value is_file_directory_out : out_channel -> bool;
     value is_file_fifo_fn : ?chase: bool -> string -> bool;
     value is_file_fifo_fd : fd -> bool;
     value is_file_fifo_in : in_channel -> bool;
     value is_file_fifo_out : out_channel -> bool;
     value is_file_regular_fn : ?chase: bool -> string -> bool;
     value is_file_regular_fd : fd -> bool;
     value is_file_regular_in : in_channel -> bool;
     value is_file_regular_out : out_channel -> bool;
     value is_file_socket_fn : ?chase: bool -> string -> bool;
     value is_file_socket_fd : fd -> bool;
     value is_file_socket_in : in_channel -> bool;
     value is_file_socket_out : out_channel -> bool;
     value is_file_special_fn : ?chase: bool -> string -> bool;
     value is_file_special_fd : fd -> bool;
     value is_file_special_in : in_channel -> bool;
     value is_file_special_out : out_channel -> bool;
     value is_file_symlink_fn : string -> bool;
     value is_file_symlink_fd : fd -> bool;
     value is_file_symlink_in : in_channel -> bool;
     value is_file_symlink_out : out_channel -> bool;
     type accessibility =
       [ Accessible
       | Unaccessible
       | Permission
       | No_directory
       | Nonexistent ]
     ;
     value is_file_not_readable_fn : string -> accessibility;
     value is_file_not_readable_fd : fd -> accessibility;
     value is_file_not_readable_in : in_channel -> accessibility;
     value is_file_not_readable_out : out_channel -> accessibility;
     value is_file_not_writable_fn : string -> accessibility;
     value is_file_not_writable_fd : fd -> accessibility;
     value is_file_not_writable_in : in_channel -> accessibility;
     value is_file_not_writable_out : out_channel -> accessibility;
     value is_file_not_executable_fn : string -> accessibility;
     value is_file_not_executable_fd : fd -> accessibility;
     value is_file_not_executable_in : in_channel -> accessibility;
     value is_file_not_executable_out : out_channel -> accessibility;
     value is_file_readable_fn : string -> bool;
     value is_file_readable_fd : fd -> bool;
     value is_file_readable_in : in_channel -> bool;
     value is_file_readable_out : out_channel -> bool;
     value is_file_writable_fn : string -> bool;
     value is_file_writable_fd : fd -> bool;
     value is_file_writable_in : in_channel -> bool;
     value is_file_writable_out : out_channel -> bool;
     value is_file_executable_fn : string -> bool;
     value is_file_executable_fd : fd -> bool;
     value is_file_executable_in : in_channel -> bool;
     value is_file_executable_out : out_channel -> bool;
     type existing =
       [ Existing
       | Unexisting
       | Search_denied ]
     ;
     value file_not_exists_fn : ?chase: bool -> string -> existing;
     value file_not_exists_fd : fd -> existing;
     value file_not_exists_in : in_channel -> existing;
     value file_not_exists_out : out_channel -> existing;
     value is_file_existing_fn : ?chase: bool -> string -> bool;
     value is_file_existing_fd : fd -> bool;
     value is_file_existing_in : in_channel -> bool;
     value is_file_existing_out : out_channel -> bool;
     value fold_input : ('a -> 'b -> 'a) -> 'a -> ('c -> 'b) -> 'c -> 'a;
     value fold_directory : ('a -> string -> 'a) -> 'a -> string -> 'a;
     value directory_files : ?dot_files: bool -> string -> list string;
     value create_temp_file : ?prefix: string -> unit -> string;
     value set_temp_file_template : (string * string) -> unit;
     value with_temp_file_template : (string * string) -> (unit -> 'a) -> 'a;
     value temp_file_iterate :
       ?template: (string * string) -> (string -> option 'a) -> 'a;
     value temp_file_channel : unit -> (in_channel * out_channel);
   end);

include
  (Glob :
   sig
     value glob : list string -> list string;
     value glob_quote : string -> string;
     type file_match_pattern =
       [ String_pat of string
       | Regexp_pat of Pcre.regexp
       | Predicate_pat of string -> bool ]
     ;
     value file_match : ?dot_files: bool -> string -> list file_match_pattern -> list string;
   end);

include
  (Proc_3_4 :
   sig
     value exec : string -> list string -> unit;
     value exec_path : string -> list string -> 'a;
     value exec_with_env :
       string -> ?env: (list (string * string)) -> list string -> unit;
     value exec_path_with_env :
       string -> ?env: (list (string * string)) -> list string -> 'a;
     value low_exec : string -> ?env: (list (string * string)) -> list string -> unit;
     value exec_path_search : string -> list string -> string;
     value call_terminally : option (unit -> unit) -> option 'a;
     value suspend : unit -> unit;
     value fork : unit -> option proc;
     value fork_child : (unit -> unit) -> proc;
     value low_fork : ?child: (unit -> unit) -> unit -> option proc;
     value fork_with_pipe : unit -> option proc;
     value fork_child_with_pipe : (unit -> unit) -> proc;
     value low_fork_with_pipe : ?child: (unit -> unit) -> unit -> option proc;
     value fork_with_pipe_plus : list (list fd) -> option proc;
     value fork_child_with_pipe_plus : (unit -> 'a) -> list (list fd) -> proc;
     value low_fork_with_pipe_plus :
       ?child: (unit -> 'a) -> list (list fd) -> option proc;
     value pid_of_proc : proc -> int;
     type probe_pid =
       Proc_3_4.probe_pid ==
         [ Probe
         | Create
         | Don't_probe ]
     ;
     value proc_of_pid : ?probe: probe_pid -> int -> option proc;
     type autoreap_policy =
       Proc_3_4.autoreap_policy ==
         [ No_autoreaping
         | Early
         | Late ]
     ;
     value autoreap_policy : ?policy: autoreap_policy -> unit -> autoreap_policy;
     type wait_any =
       Proc_3_4.wait_any ==
         [ None_ready
         | No_children
         | Exited of (proc * process_status) ]
     ;
     value reap_zombies : unit -> bool;
     value wait : ?wflags: (list wait_flag) -> proc -> process_status;
     value wait_pid : ?wflags: (list wait_flag) -> int -> process_status;
     value wait_any : ?wflags: (list wait_flag) -> unit -> wait_any;
     value wait_process_group : ?wflags: (list wait_flag) -> proc -> wait_any;
     value wait_process_group_pgrp : ?wflags: (list wait_flag) -> int -> wait_any;
   end);

exception Child_not_ready = Proc_3_4.Child_not_ready;

value exit = Pervasives.exit;
external low_exit : int -> 'a = "sys_exit";

include
  (Proc_state_3_5 :
   sig
     value umask : unit -> int;
     value set_umask : int -> unit;
     value with_umask : int -> (unit -> 'a) -> 'a;
     value chdir : ?dir:string -> unit -> unit;
     value cwd : unit -> string;
     value with_cwd : string -> (unit -> 'a) -> 'a;
     value pid : unit -> int;
     value parent_pid : unit -> int;
     value process_group : unit -> int;
     value set_process_group : ?proc: proc -> int -> unit;
     value set_process_group_pid : int -> int -> unit;
     type prio =
       Proc_state_3_5.prio ==
         [ Prio_process
         | Prio_pgrp
         | Prio_user ]
     ;
     value set_priority : ?who: proc -> prio -> int -> unit;
     value set_priority_pid : int -> prio -> int -> unit;
     value priority : ?who: proc -> prio -> int;
     value priority_pid : int -> prio -> int;
     value nice : ?proc: proc -> int -> unit;
     value nice_pid : int -> int -> unit;
     value user_login_name : unit -> string;
     value user_uid : unit -> int;
     value user_effective_uid : unit -> int;
     value user_gid : unit -> int;
     value user_effective_gid : unit -> int;
     value user_supplementary_gids : unit -> array int;
     value set_uid : int -> unit;
     value set_gid : int -> unit;
     value process_times : unit -> process_times;
     value cpu_ticks_per_sec : unit -> int;
   end);

include
  (User_group_3_6 :
   sig
     type user_info =
       User_group_3_6.user_info ==
         { ui_name : string;
           ui_uid : int;
           ui_gid : int;
           ui_home_dir : string;
           ui_shell : string }
     ;
     value user_info : int -> user_info;
     value user_info_name : string -> user_info;
     value username_to_uid : string -> int;
     value uid_to_username : int -> string;
     type group_info =
       User_group_3_6.group_info ==
         { gi_name : string; gi_gid : int; gi_members : list string }
     ;
     value group_info : int -> group_info;
     value group_info_name : string -> group_info;
     value groupname_to_gid : string -> int;
     value gid_to_groupname : int -> string;
   end);

include
  (Arg_3_7 :
   sig
     value command_line_arguments : ref (option (list string));
     value command_line : unit -> list string;
     value make_command_line_arguments : unit -> list string;
     value arg : ?default: 'a -> list 'a -> int -> 'a;
     value arg_star : ?default_thunk: (unit -> 'a) -> list 'a -> int -> 'a;
     value argv : ?default: string -> int -> string;
   end);

value system_name = Unix.gethostname;

include
  (Signal_3_9 :
   sig
     value signal_process : proc -> int -> unit;
     value signal_process_pid : int -> int -> unit;
     value signal_process_group : proc -> int -> unit;
     value signal_process_group_pgrp : int -> int -> unit;
     value itimer : ?newstat: itimer_status -> itimer -> itimer_status;
     value pause_until_interrupt : unit -> unit;
     value sleep : int -> unit;
     value sleep_until : float -> unit;
   end);

include
  (Time_3_10 :
   sig
     type date =
       Time_3_10.date ==
         { seconds : int;
           minute : int;
           hour : int;
           month_day : int;
           month : int;
           year : int;
           tz_name : option string;
           tz_secs : option int;
           is_summer : option bool;
           week_day : int;
           year_day : int }
     ;
     value make_date :
       ?tzn: string -> ?tzs: int -> ?summ: bool -> ?wday: int -> ?yday: int -> int ->
         int -> int -> int -> int -> int -> date;
     value time_plus_ticks : unit -> float;
     value ticks_per_sec : unit -> float;
     type time_zone =
       Time_3_10.time_zone ==
         [ Tz_local
         | Tz_secs of int
         | Tz_name of string ]
     ;
     value date : unit -> date;
     value date_of_time : ?tz: time_zone -> float -> date;
     value time : unit -> float;
     value time_of_date : date -> float;
     value string_of_date : date -> string;
     value format_date : string -> date -> string;
   end);

include
  (Env_3_11 :
   sig
     value unwind_protect : (unit -> 'a) -> ('b -> unit) -> 'b -> 'a;
     value getenv : string -> string;
     value setenv : ?sval: string -> string -> unit;
     value alist_of_env : unit -> list (string * string);
     value setenv_from_alist : list (string * string) -> unit;
     value alist_delete : 'a -> list ('a * 'b) -> list ('a * 'b);
     value alist_update : 'a -> 'b -> list ('a * 'b) -> list ('a * 'b);
     value alist_compress : list ('a * bool) -> list ('a * bool);
     value with_env : list (string * string) -> (unit -> 'a) -> 'a;
     value with_total_env : list (string * string) -> (unit -> 'a) -> 'a;
     value add_before : 'a -> 'a -> list 'a -> list 'a;
     value add_after : 'a -> 'a -> list 'a -> list 'a;
     value home_directory : ref string;
     value exec_path_list : unit -> list string;
     value set_exec_path_list : list string -> unit;
     value with_exec_path_list : list string -> (unit -> 'a) -> 'a;
   end);

ignore (exec_path_list ());     (* Start-up time access to $PATH. *)

include
  (Tty_3_12 :
   sig
     value is_tty_fd : fd -> bool;
     value is_tty_in : in_channel -> bool;
     value is_tty_out : out_channel -> bool;
     value tty_file_name_fd : fd -> string;
     value tty_file_name_in : in_channel -> string;
     value tty_file_name_out : out_channel -> string;
     type tty_info =
       { control_chars : string;
         input_flags : nativeint;
         output_flags : nativeint;
         control_flags : nativeint;
         local_flags : nativeint;
         input_speed : int;
         output_speed : int;
         min : int;
         time : int }
     ;
     type tty_chars =
       { delete_char : int;
         delete_line : int;
         eof : int;
         eol : int;
         interrupt : int;
         quit : int;
         suspend : int;
         start : int;
         stop : int;
         delayed_suspend : int;
         delete_word : int;
         discard : int;
         eol2 : int;
         literal_next : int;
         reprint : int;
         status : int }
     ;
     value ttychar : tty_chars;
     value disable_tty_char : char;
     type tty_in =
       { check_parity : nativeint;
         ignore_bad_parity_chars : nativeint;
         mark_parity_errors : nativeint;
         ignore_break : nativeint;
         interrupt_on_break : nativeint;
         seven_bits : nativeint;
         cr_to_nl : nativeint;
         ignore_cr : nativeint;
         nl_to_cr : nativeint;
         input_flow_ctl : nativeint;
         output_flow_ctl : nativeint;
         xon_any : nativeint;
         beep_on_overflow : nativeint;
         lowercase : nativeint }
     ;
     value ttyin : tty_in;
     type tty_out =
       { enable : nativeint;
         nl_to_crnl : nativeint;
         discard_eot : nativeint;
         expand_tabs : nativeint;
         cr_to_nl : nativeint;
         nl_does_cr : nativeint;
         no_col0_cr : nativeint;
         delay_with_fill_char : nativeint;
         fill_with_del : nativeint;
         uppercase : nativeint;
         bs_delay : nativeint;
         bs_delay0 : nativeint;
         bs_delay1 : nativeint;
         cr_delay : nativeint;
         cr_delay0 : nativeint;
         cr_delay1 : nativeint;
         cr_delay2 : nativeint;
         cr_delay3 : nativeint;
         ff_delay : nativeint;
         ff_delay0 : nativeint;
         ff_delay1 : nativeint;
         tab_delay : nativeint;
         tab_delay0 : nativeint;
         tab_delay1 : nativeint;
         tab_delay2 : nativeint;
         tab_delayx : nativeint;
         nl_delay : nativeint;
         nl_delay0 : nativeint;
         nl_delay1 : nativeint;
         vtab_delay : nativeint;
         vtab_delay0 : nativeint;
         vtab_delay1 : nativeint;
         all_delay : nativeint }
     ;
     value ttyout : tty_out;
     type tty_c =
       { char_size : nativeint;
         char_size5 : nativeint;
         char_size6 : nativeint;
         char_size7 : nativeint;
         char_size8 : nativeint;
         enable_parity : nativeint;
         odd_parity : nativeint;
         enable_read : nativeint;
         hup_on_close : nativeint;
         no_modem_sync : nativeint;
         two_stop_bits : nativeint;
         ignore_flags : nativeint;
         cts_output_flow_control : nativeint;
         rts_input_flow_control : nativeint;
         carrier_flow_ctl : nativeint }
     ;
     value ttyc : tty_c;
     type tty_l =
       { canonical : nativeint;
         echo : nativeint;
         echo_delete_lines : nativeint;
         echo_nl : nativeint;
         visual_delete : nativeint;
         enable_signals : nativeint;
         extended : nativeint;
         no_flush_on_interrupt : nativeint;
         ttou_signal : nativeint;
         echo_ctl : nativeint;
         flush_output : nativeint;
         hardcopy_delete : nativeint;
         reprint_unread_chars : nativeint;
         visual_delete_line : nativeint;
         alt_delete_word : nativeint;
         no_kernel_status : nativeint;
         case_map : nativeint }
     ;
     value ttyl : tty_l;
     value make_tty_info :
       nativeint -> nativeint -> nativeint -> nativeint -> int -> int -> int -> int ->
         tty_info;
     value copy_tty_info : tty_info -> tty_info;
     value tty_info_fd : fd -> tty_info;
     value tty_info_in : in_channel -> tty_info;
     value tty_info_out : out_channel -> tty_info;
     value tty_info_fn : string -> tty_info;
     value set_tty_info_now_fd : fd -> tty_info -> unit;
     value set_tty_info_now_in : in_channel -> tty_info -> unit;
     value set_tty_info_now_out : out_channel -> tty_info -> unit;
     value set_tty_info_now_fn : string -> tty_info -> unit;
     value set_tty_info_drain_fd : fd -> tty_info -> unit;
     value set_tty_info_drain_in : in_channel -> tty_info -> unit;
     value set_tty_info_drain_out : out_channel -> tty_info -> unit;
     value set_tty_info_drain_fn : string -> tty_info -> unit;
     value set_tty_info_flush_fd : fd -> tty_info -> unit;
     value set_tty_info_flush_in : in_channel -> tty_info -> unit;
     value set_tty_info_flush_out : out_channel -> tty_info -> unit;
     value set_tty_info_flush_fn : string -> tty_info -> unit;
     value send_tty_break_fd : ?duration: int -> fd -> unit;
     value send_tty_break_in : ?duration: int -> in_channel -> unit;
     value send_tty_break_out : ?duration: int -> out_channel -> unit;
     value send_tty_break_fn : ?duration: int -> string -> unit;
     value drain_tty_fd : fd -> unit;
     value drain_tty_in : in_channel -> unit;
     value drain_tty_out : out_channel -> unit;
     value drain_tty_fn : string -> unit;
     value flush_tty_input_fd : fd -> unit;
     value flush_tty_input_in : in_channel -> unit;
     value flush_tty_input_out : out_channel -> unit;
     value flush_tty_input_fn : string -> unit;
     value flush_tty_output_fd : fd -> unit;
     value flush_tty_output_in : in_channel -> unit;
     value flush_tty_output_out : out_channel -> unit;
     value flush_tty_output_fn : string -> unit;
     value flush_tty_both_fd : fd -> unit;
     value flush_tty_both_in : in_channel -> unit;
     value flush_tty_both_out : out_channel -> unit;
     value flush_tty_both_fn : string -> unit;
     value start_tty_output_fd : fd -> unit;
     value start_tty_output_in : in_channel -> unit;
     value start_tty_output_out : out_channel -> unit;
     value start_tty_output_fn : string -> unit;
     value stop_tty_output_fd : fd -> unit;
     value stop_tty_output_in : in_channel -> unit;
     value stop_tty_output_out : out_channel -> unit;
     value stop_tty_output_fn : string -> unit;
     value start_tty_input_fd : fd -> unit;
     value start_tty_input_in : in_channel -> unit;
     value start_tty_input_out : out_channel -> unit;
     value start_tty_input_fn : string -> unit;
     value stop_tty_input_fd : fd -> unit;
     value stop_tty_input_in : in_channel -> unit;
     value stop_tty_input_out : out_channel -> unit;
     value stop_tty_input_fn : string -> unit;
     value open_control_tty_in : ?flags: (list open_flag) -> string -> in_channel;
     value open_control_tty_out : ?flags: (list open_flag) -> string -> out_channel;
     value become_session_leader : unit -> int;
     value tty_process_group_fd : fd -> int;
     value tty_process_group_in : in_channel -> int;
     value tty_process_group_out : out_channel -> int;
     value tty_process_group_fn : string -> int;
     value set_tty_process_group_fd : fd -> int -> int;
     value set_tty_process_group_in : in_channel -> int -> int;
     value set_tty_process_group_out : out_channel -> int -> int;
     value set_tty_process_group_fn : string -> int -> int;
     value control_tty_file_name : unit -> string;
     value make_pty_generator : unit -> unit -> string;
     value tty_name_of_pty_name : string -> string;
     value pty_name_of_tty_name : string -> string;
     value open_pty : unit -> (in_channel * string);
     value fork_pty_session :
       (unit -> unit) -> (Proc_3_4.proc * in_channel * out_channel * string);
   end);

type protocol_family = socket_domain;
type host_info =
  Unix.host_entry ==
    { h_name : string;
      h_aliases : array string;
      h_addrtype : socket_domain;
      h_addr_list : array inet_addr }
;
type service_info =
  Unix.service_entry ==
    { s_name : string; s_aliases : array string; s_port : int; s_proto : string }
;
type protocol_info =
  Unix.protocol_entry == { p_name : string; p_aliases : array string; p_proto : int }
;

include
  (Network_4 :
   sig
     type socket =
       { family : protocol_family; sock_in : in_channel; sock_out : out_channel }
     ;
     value socket_connect : sockaddr -> socket_type -> unit;
     value sockaddr_of_host_and_port : string -> int -> sockaddr;
     value sockaddr_of_host_and_service : string -> string -> sockaddr;
     value bind_listen_accept_loop_unix : string -> (socket -> sockaddr -> unit) -> unit;
     value bind_listen_accept_loop_port : int -> (socket -> sockaddr -> unit) -> unit;
     value bind_listen_accept_loop_service :
       string -> (socket -> sockaddr -> unit) -> unit;
     value create_socket : ?protocol: int -> protocol_family -> socket_type -> socket;
     value create_socket_pair : socket_type -> (socket * socket);
     value close_socket : socket -> unit;
     value socket_address_of_unix_address : string -> sockaddr;
     value socket_address_of_internet_address : inet_addr -> int -> sockaddr;
     value inet_addr_any : inet_addr;
     value inet_addr_loopback : inet_addr;
     value inet_addr_broadcast : inet_addr;
     value unix_address_of_socket_address : sockaddr -> string;
     value internet_address_of_socket_address : sockaddr -> (inet_addr * int);
     value connect_socket : socket -> sockaddr -> unit;
     value bind_socket : socket -> sockaddr -> unit;
     value listen_socket : socket -> int -> unit;
     value accept_connection : socket -> (socket * sockaddr);
     value socket_local_address : socket -> sockaddr;
     value socket_remote_address : socket -> sockaddr;
     value shutdown_socket : socket -> shutdown_command -> unit;
     value receive_message :
       ?flags: (list msg_flag) -> socket -> int -> (string * sockaddr);
     value receive_message_bang :
       ?start: int -> ?end_: int -> ?flags: (list msg_flag) -> socket -> string ->
         (int * sockaddr);
     value receive_message_partial :
       ?flags: (list msg_flag) -> socket -> int -> (string * sockaddr);
     value receive_message_bang_partial :
       ?start: int -> ?end_: int -> ?flags: (list msg_flag) -> socket -> string ->
         (int * sockaddr);
     value send_message :
       ?start: int -> ?end_: int -> ?flags: (list msg_flag) -> ?sockaddr: sockaddr ->
         socket -> string -> unit;
     value send_message_partial :
       ?start: int -> ?end_: int -> ?flags: (list msg_flag) -> ?sockaddr: sockaddr ->
         socket -> string -> int;
     type protocol_level =
       Network_4.protocol_level ==
         [ SOL_SOCKET ]
     ;
     value socket_option_bool : socket -> protocol_level -> socket_bool_option -> bool;
     value set_socket_option_bool :
       socket -> protocol_level -> socket_bool_option -> bool -> unit;
     value socket_option_int : socket -> protocol_level -> socket_int_option -> int;
     value set_socket_option_int :
       socket -> protocol_level -> socket_int_option -> int -> unit;
     value socket_option_optint :
       socket -> protocol_level -> socket_optint_option -> option int;
     value set_socket_option_optint :
       socket -> protocol_level -> socket_optint_option -> option int -> unit;
     value socket_option_float : socket -> protocol_level -> socket_float_option -> float;
     value set_socket_option_float :
       socket -> protocol_level -> socket_float_option -> float -> unit;
     type herror =
       [ HOST_NOT_FOUND
       | TRY_AGAIN
       | NO_RECOVERY
       | NO_DATA
       | NO_ADDRESS ]
     ;
     exception Netdb_error of herror;
     value host_info_name : string -> host_info;
     value host_info_addr : sockaddr -> host_info;
     type network_info =
       { n_name : string;
         n_aliases : array string;
         n_addrtype : protocol_family;
         n_net : int32 }
     ;
     value network_info_name : string -> network_info;
     value network_info_addr : sockaddr -> network_info;
     value service_info_name : ?protocol: string -> string -> service_info;
     value service_info_port : ?protocol: string -> int -> service_info;
     value protocol_info_name : string -> protocol_info;
     value protocol_info_port : int -> protocol_info;
   end);

include
  (Strings_5_1 :
   sig
     value index : ?from: int -> string -> char -> option int;
     value rindex : ?from: int -> string -> char -> option int;
     value substring : string -> int -> int -> string;
     value xsubstring : string -> int -> int -> string;
     value is_file_name_directory : string -> bool;
     value is_file_name_non_directory : string -> bool;
     value file_name_as_directory : string -> string;
     value directory_as_file_name : string -> string;
     value is_file_name_absolute : string -> bool;
     value ensure_file_name_is_directory : string -> string;
     value file_name_directory : string -> string;
     value file_name_nondirectory : string -> string;
     value split_file_name : string -> list string;
     value file_name_of_path_list : ?dir: string -> list string -> string;
     value file_name_extension : string -> string;
     value file_name_sans_extension : string -> string;
     value parse_file_name : string -> (string * string * string);
     value replace_extension : string -> string -> string;
     value ensure_file_name_is_nondirectory : string -> string;
     value simplify_file_name : string -> string;
     value resolve_file_name : ?dir: string -> string -> string;
     value expand_file_name : ?dir: string -> string -> string;
     value absolute_file_name : ?dir: string -> string -> string;
     value home_dir : ?user: string -> unit -> string;
     value home_file : ?user: string -> string -> string;
     value substitute_env_vars : string -> string;
   end);

include Charset_pred;

include
  (Delim_7 :
   sig
     type handle_delim =
       [ Trim
       | Peek
       | Concat ]
     ;
     type termination_kind =
       [ Eof
       | Read of char
       | Full_buffer ]
     ;
     value read_line_split : in_channel -> (string * termination_kind);
     value read_line : ?handle_newline: handle_delim -> in_channel -> string;
     value read_paragraph_split : in_channel -> (string * string);
     value read_paragraph : ?handle_delim: handle_delim -> in_channel -> string;
     value read_delimited_split :
       ?chan: in_channel -> Charset_14.any_t -> (string * termination_kind);
     value read_delimited :
       ?chan: in_channel -> ?handle_delim: handle_delim -> Charset_14.any_t -> string;
     value read_delimited_bang_split :
       ?chan: in_channel -> ?start: int -> ?end_: int -> Charset_14.any_t -> string ->
         option (int * termination_kind);
     value read_delimited_bang :
       ?chan: in_channel -> ?handle_delim: handle_delim -> ?start: int -> ?end_: int ->
         Charset_14.any_t -> string -> option int;
     value low_read_delimited_bang :
       ?chan: in_channel -> ?start: int -> ?end_: int -> Charset_14.any_t -> string ->
         handle_delim -> (termination_kind * int);
     value skip_char_set : ?chan: in_channel -> Charset_14.any_t -> int;
   end);

include
  (Rec_field_8 :
   sig
     value record_reader :
       ?delims: Charset_14.any_t -> ?elide_delims: bool ->
         ?handle_delim: Delim_7.handle_delim -> unit -> in_channel -> string;
     value record_reader_split :
       ?delims: Charset_14.any_t -> ?elide_delims: bool -> unit -> in_channel ->
         (string * string);
     type handle_field_delim =
       Rec_field_8.handle_field_delim ==
         [ Trim_f
         | Split_f
         | Concat_f ]
     ;
     type delim_matcher =
       Rec_field_8.delim_matcher ==
         [ Match_proc of string -> int -> (int * int)
         | String of string
         | Charset of Charset_14.any_t
         | Regexp of Pcre.regexp
         | Pattern of string ]
     ;
     value field_splitter :
       ?field: delim_matcher -> ?num_fields: int -> unit -> ?start: int -> string ->
         list string;
     value infix_splitter :
       ?delim: delim_matcher -> ?num_fields: int -> ?handle_delim: handle_field_delim ->
         unit -> ?start: int -> string -> list string;
     value suffix_splitter :
       ?delim: delim_matcher -> ?num_fields: int -> ?handle_delim: handle_field_delim ->
         unit -> ?start: int -> string -> list string;
     value sloppy_suffix_splitter :
       ?delim: delim_matcher -> ?num_fields: int -> ?handle_delim: handle_field_delim ->
         unit -> ?start: int -> string -> list string;
     value default_field_matcher : delim_matcher;
     value default_infix_matcher : delim_matcher;
     value default_suffix_matcher : delim_matcher;
     value field_reader :
       ?field_parser: (?start: int -> string -> list string) ->
         ?rec_reader: (in_channel -> string) -> unit -> in_channel ->
         (string * list string);
     value gen_field_reader :
       ('record -> 'fields) -> ('chan -> 'record) -> 'chan -> ('record * 'fields);
     value default_field_parser : ?start: int -> string -> list string;
   end);

(* Protect against closed standard file descriptors. *)
let close_to_dev_null fd flag =
  try ignore (Unix.fstat fd) with
  [ Unix.Unix_error Unix.EBADF _ _ -> ignore (Unix.openfile "/dev/null" [flag] 0) ]
in
do {
  close_to_dev_null Unix.stdin Unix.O_RDONLY;
  close_to_dev_null Unix.stdout Unix.O_WRONLY;
  close_to_dev_null Unix.stderr Unix.O_WRONLY
};
