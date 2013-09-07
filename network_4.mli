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
type socket_domain =
  Unix.socket_domain ==
    [ PF_UNIX
    | PF_INET ]
;
type protocol_family = socket_domain;
type socket_type =
  Unix.socket_type ==
    [ SOCK_STREAM
    | SOCK_DGRAM
    | SOCK_RAW
    | SOCK_SEQPACKET ]
;
type protocol_level =
    [ SOL_SOCKET ]
;

type inet_addr = Unix.inet_addr;
type sockaddr =
  Unix.sockaddr ==
    [ ADDR_UNIX of string
    | ADDR_INET of inet_addr and int ]
;

type socket = { family : protocol_family; sock_in : in_channel; sock_out : out_channel }; 

value socket_address_of_unix_address : string -> sockaddr;
value unix_address_of_socket_address : sockaddr -> string;
value socket_address_of_internet_address : inet_addr -> int -> sockaddr;
value internet_address_of_socket_address : sockaddr -> (inet_addr * int);

value inet_addr_any : inet_addr;
value inet_addr_loopback : inet_addr;
value inet_addr_broadcast : inet_addr;

value create_socket : ?protocol: int -> protocol_family -> socket_type -> socket;
value create_socket_pair : socket_type -> (socket * socket);
value close_socket : socket -> unit;

value connect_socket : socket -> sockaddr -> unit;
value bind_socket : socket -> sockaddr -> unit;
value listen_socket : socket -> int -> unit;
value accept_connection : socket -> (socket * sockaddr);
value socket_local_address : socket -> sockaddr;
value socket_remote_address : socket -> sockaddr;

type shutdown_command =
  Unix.shutdown_command ==
    [ SHUTDOWN_RECEIVE
    | SHUTDOWN_SEND
    | SHUTDOWN_ALL ]
;

value shutdown_socket : socket -> shutdown_command -> unit;

value sockaddr_of_host_and_port : string -> int -> sockaddr;
value sockaddr_of_host_and_service : string -> string -> sockaddr;
value socket_connect : sockaddr -> socket_type -> unit;

value bind_listen_accept_loop_unix : string -> (socket -> sockaddr -> unit) -> unit;
value bind_listen_accept_loop_port : int -> (socket -> sockaddr -> unit) -> unit;
value bind_listen_accept_loop_service : string -> (socket -> sockaddr -> unit) -> unit;

type msg_flag =
  Unix.msg_flag ==
    [ MSG_OOB
    | MSG_DONTROUTE
    | MSG_PEEK ]
;

value receive_message_bang :
  ?start: int -> ?end_: int -> ?flags: (list msg_flag) -> socket -> string ->
    (int * sockaddr);
value receive_message_bang_partial :
  ?start: int -> ?end_: int -> ?flags: (list msg_flag) -> socket -> string ->
    (int * sockaddr);

value receive_message :
  ?flags: (list msg_flag) -> socket -> int -> (string * sockaddr);
value receive_message_partial :
  ?flags: (list msg_flag) -> socket -> int -> (string * sockaddr);

value send_message :
  ?start: int -> ?end_: int -> ?flags: (list msg_flag) -> ?sockaddr: sockaddr ->
    socket -> string -> unit;
value send_message_partial :
  ?start: int -> ?end_: int -> ?flags: (list msg_flag) -> ?sockaddr: sockaddr ->
    socket -> string -> int;

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

value socket_option_bool : socket -> protocol_level -> socket_bool_option -> bool;
value set_socket_option_bool :
  socket -> protocol_level -> socket_bool_option -> bool -> unit;

type socket_int_option =
  Unix.socket_int_option ==
    [ SO_SNDBUF
    | SO_RCVBUF
    | SO_ERROR
    | SO_TYPE
    | SO_RCVLOWAT
    | SO_SNDLOWAT ]
;

value socket_option_int : socket -> protocol_level -> socket_int_option -> int;
value set_socket_option_int :
  socket -> protocol_level -> socket_int_option -> int -> unit;

type socket_optint_option =
  Unix.socket_optint_option ==
    [ SO_LINGER ]
;

value socket_option_optint :
  socket -> protocol_level -> socket_optint_option -> option int;
value set_socket_option_optint :
  socket -> protocol_level -> socket_optint_option -> option int -> unit;

type socket_float_option =
  Unix.socket_float_option ==
    [ SO_RCVTIMEO
    | SO_SNDTIMEO ]
;

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

type host_info =
  Unix.host_entry ==
    { h_name : string;
      h_aliases : array string;
      h_addrtype : protocol_family;
      h_addr_list : array inet_addr }
;

value host_info_name : string -> host_info;
value host_info_addr : sockaddr -> host_info;

type network_info =
  { n_name : string; n_aliases : array string; n_addrtype : protocol_family; n_net : int32 }
;

value network_info_name : string -> network_info;
value network_info_addr : sockaddr -> network_info;

type service_info =
  Unix.service_entry ==
    { s_name : string; s_aliases : array string; s_port : int; s_proto : string }
;
value service_info_name : ?protocol: string -> string -> service_info;
value service_info_port : ?protocol: string -> int -> service_info;

type protocol_info =
  Unix.protocol_entry == { p_name : string; p_aliases : array string; p_proto : int }
;
value protocol_info_name : string -> protocol_info;
value protocol_info_port : int -> protocol_info;
