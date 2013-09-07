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

type fd = int;

(* *Don't* export in cash. *)
external file_descr_of_fd : fd -> Unix.file_descr = "%identity"; (* For Io_3_3. *)
external fd_of_file_descr : Unix.file_descr -> fd = "%identity"; (* For Network_4. *)
(* low_xx_channel_of_fd <=> make-{input,output}-fdport; ça fait ~ comme
   Unix.xx_channel_of_descr, mais met revealed et close_on_exec comme
   install-port.  *)
(* For Network_4. *)
external low_in_channel_of_fd : fd -> int -> in_channel = "in_channel_of_fd";

value with_stdin : in_channel -> (unit -> 'a) -> 'a;
value with_stdout : out_channel -> (unit -> 'a) -> 'a;
value with_stderr : out_channel -> (unit -> 'a) -> 'a;

value set_stdin : in_channel -> unit;
value set_stdout : out_channel -> unit;
value set_stderr : out_channel -> unit;

value stdchans_to_stdio : unit -> unit;
value stdio_to_stdchans : unit -> unit;
value with_stdio_chans : (unit -> 'a) -> 'a;

value in_channel_of_fd : fd -> in_channel;
value out_channel_of_fd : fd -> out_channel;

value fd_of_in_channel : in_channel -> fd;
value fd_of_out_channel : out_channel -> fd;

value evict_chans : fd -> unit;(* DEBUG. *)

value dup_fd : ?newfd: fd -> fd -> fd;
value fdes_of_dup_in : ?newfd: fd -> in_channel -> fd;
value fdes_of_dup_out : ?newfd: fd -> out_channel -> fd;

value in_channel_of_dup_fd : ?newfd: fd -> fd -> in_channel;
value dup_in : ?newfd: fd -> in_channel -> in_channel;
value in_channel_of_dup_out : ?newfd: fd -> out_channel -> in_channel;

value out_channel_of_dup_fd : ?newfd: fd -> fd -> out_channel;
value out_channel_of_dup_in : ?newfd: fd -> in_channel -> out_channel;
value dup_out : ?newfd: fd -> out_channel -> out_channel;

value close_fd : fd -> bool;
value close_in : in_channel -> bool;
value close_out : out_channel -> bool;
value ignoring_close_in : in_channel -> unit;
value ignoring_close_out : out_channel -> unit;

value close_fd_after : fd -> (fd -> 'a) -> 'a;
value close_in_after : in_channel -> (in_channel -> 'a) -> 'a;
value close_out_after : out_channel -> (out_channel -> 'a) -> 'a;

value sleazy_call_with_fdes_fd : (fd -> 'a) -> fd -> 'a;
value sleazy_call_with_fdes_in : (fd -> 'a) -> in_channel -> 'a;
value sleazy_call_with_fdes_out : (fd -> 'a) -> out_channel -> 'a;

(* Scsh uses the inverse order -- don't ask me why I changed it. *)
(* value call_with_fdes_fd : (fd -> 'a) -> fd -> 'a; *)
value call_with_fdes_in : (fd -> 'a) -> in_channel -> 'a;
value call_with_fdes_out : (fd -> 'a) -> out_channel -> 'a;

value move_fd_to_fdes : fd -> fd -> fd;
value move_in_channel_to_fdes : in_channel -> fd -> in_channel;
value move_out_channel_to_fdes : out_channel -> fd -> out_channel;
value ignoring_move_in_channel_to_fdes : in_channel -> fd -> unit;
value ignoring_move_out_channel_to_fdes : out_channel -> fd -> unit;

value flush_all_chans : unit -> unit;

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

value seek_fd : ?whence: seek_command -> fd -> int -> int;
value seek_in : ?whence: seek_command -> in_channel -> int -> int;
value seek_out : ?whence: seek_command -> out_channel -> int -> int;

value tell_fd : fd -> int;
value tell_in : in_channel -> int;
value tell_out : out_channel -> int;

value open_fdes : ?perms: file_perm -> string -> list open_flag -> fd;

value open_file_out : ?perms: file_perm -> string -> list open_flag -> out_channel;
value open_file_in : ?perms: file_perm -> string -> list open_flag -> in_channel;

value open_input_file : ?flags: (list open_flag) -> string -> in_channel;
value open_output_file :
  ?flags: (list open_flag) -> ?perms: file_perm -> string -> out_channel;

value openfile : string -> list open_flag -> file_perm -> Unix.file_descr;

value with_input_from_file : string -> (unit -> 'a) -> 'a;
value with_output_to_file : string -> (unit -> 'a) -> 'a;
value with_errors_to_file : string -> (unit -> 'a) -> 'a;

value call_with_input_file : string -> (in_channel -> 'a) -> 'a;
value call_with_output_file : string -> (out_channel -> 'a) -> 'a;
value call_with_fdes_fn :
  ?perms: file_perm -> string -> list open_flag -> (fd -> 'a) -> 'a;

type fdes_flags = [ FD_CLOEXEC ];
external fdes_flags_fd : fd -> list fdes_flags = "read_fdes_flags";
value fdes_flags_in : in_channel -> list fdes_flags;
value fdes_flags_out : out_channel -> list fdes_flags;

external set_fdes_flags_fd : fd -> list fdes_flags -> unit = "write_fdes_flags";
value set_fdes_flags_in : in_channel -> list fdes_flags -> unit;
value set_fdes_flags_out : out_channel -> list fdes_flags -> unit;

external fdes_status_fd : fd -> list open_flag = "read_file_flags";
value fdes_status_in : in_channel -> list open_flag;
value fdes_status_out: out_channel -> list open_flag;

external set_fdes_status_fd : fd -> list open_flag -> unit = "write_file_flags";
value set_fdes_status_in : in_channel -> list open_flag -> unit;
value set_fdes_status_out : out_channel -> list open_flag -> unit;

value pipe : unit -> (in_channel * out_channel);

type error_packet =
  [ Sys__error of string
  | Unix__error of (Unix.error * string * string) ]
;

exception
  String_io_error of (error_packet * string * string * int * int * int);

value read_string : ?src: fd -> int -> string;
value read_string_bang : ?src: fd -> ?start: int -> ?end_: int -> string -> int;

value read_string_in : ?src: in_channel -> int -> string;
value read_string_bang_in :
  ?src: in_channel -> ?start: int -> ?end_: int -> string -> int;

value read_string_partial : ?src: fd -> int -> string;
value read_string_bang_partial : ?src: fd -> ?start: int -> ?end_: int -> string -> int;

value read_string_partial_in : ?src: in_channel -> int -> string;
value read_string_bang_partial_in :
  ?src: in_channel -> ?start: int -> ?end_: int -> string -> int;

value write_string : ?dst: fd -> ?start: int -> ?end_: int -> string -> unit;
value write_string_out : ?dst: out_channel -> ?start: int -> ?end_: int -> string -> unit;

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
