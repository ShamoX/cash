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
include
  (Unix :
   sig
     type inet_addr = Unix.inet_addr;
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
     (* Tant qu'on ne supporte pas plus qu'Unix, on peut ne pas donner ces types dans
        l'interface, mais il faut les redéclarer ici pour suivre (tracker) toute
        modification dans Unix, sinon, cash_.et_socket_option_* vont sauter. *)
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
     value inet_addr_any : inet_addr;
     type msg_flag =
       Unix.msg_flag ==
         [ MSG_OOB
         | MSG_DONTROUTE
         | MSG_PEEK ]
     ;
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

type socket =
  { family : protocol_family; sock_in : in_channel; sock_out : out_channel }
;

value make_socket_in family ichan =
  {family = family; sock_in = ichan; sock_out = Io_3_2.out_channel_of_dup_in ichan}
;

value make_socket_fd family sock =
  make_socket_in family (Io_3_2.low_in_channel_of_fd (Io_3_2.fd_of_file_descr sock) 0)
;

value socket_address_of_unix_address path =
  if String.length path > 108 then
    invalid_arg ("socket_address_of_unix_address: path too long " ^ path)
  else ADDR_UNIX path
;

value socket_address_of_internet_address host_addr port_num =
  if port_num < 0 || port_num > 0xffff then
    invalid_arg
      ("socket_address_of_internet_address: port out of range " ^ string_of_int port_num)
  else ADDR_INET host_addr port_num
;

value (unix_address_of_socket_address, internet_address_of_socket_address) =
  let error kind =
    invalid_arg (kind ^ "_address_of_socket_address: " ^ kind ^ " socket expected")
  in
  (fun
   [ ADDR_UNIX p -> p
   | _ -> error "unix" ],
   fun
   [ ADDR_INET host_addr port_num -> (host_addr, port_num)
   | _ -> error "internet" ])
;

value create_socket ?(protocol = 0) family socktype =
  make_socket_fd family (Unix.socket family socktype protocol)
;

value create_socket_pair socktype =
  let (s1, s2) = Unix.socketpair PF_UNIX socktype 0 in
  (make_socket_fd PF_UNIX s1, make_socket_fd PF_UNIX s2)
;

value close_socket socket =
  do {
    Io_3_2.ignoring_close_in socket.sock_in; Io_3_2.ignoring_close_out socket.sock_out
  }
;

value inet_addr_loopback = Unix.inet_addr_of_string "127.0.0.1";
(* ; must be masked. *)
value inet_addr_broadcast = Unix.inet_addr_of_string "255.255.255.255";

value check_addr_and_families sock addr msg =
  match (sock.family, addr) with
  [ (PF_UNIX, ADDR_UNIX p) -> ignore (socket_address_of_unix_address p)
  | (PF_INET, ADDR_INET _ _) -> ()
  | _ -> invalid_arg msg ]
;

value file_descr_of_socket sock =
  Io_3_2.file_descr_of_fd (Io_3_2.fd_of_in_channel sock.sock_in)
;

value (connect_socket, bind_socket) =
  let frob_socket name frobber sock addr =
    let () =
      check_addr_and_families sock addr
        (name ^ "_connect: trying to " ^ name ^ " socket to incompatible address")
    in
    frobber (file_descr_of_socket sock) addr
  in
  (frob_socket "connect" Unix.connect, frob_socket "bind" Unix.bind)
;

value listen_socket sock = Unix.listen (file_descr_of_socket sock);

value accept_connection socket =
  let (sock, addr) = Unix.accept (file_descr_of_socket socket) in
  (make_socket_fd socket.family sock, addr)
;

value (socket_local_address, socket_remote_address) =
  let socket_address name get_addr socket =
    if socket.family <> PF_INET then invalid_arg (name ^ ": internet socket expected")
    else get_addr (file_descr_of_socket socket)
  in
  (socket_address "socket_local_address" Unix.getsockname,
   socket_address "socket_remote_address" Unix.getpeername)
;

value shutdown_socket socket = Unix.shutdown (file_descr_of_socket socket);

value socket_connect addr socktype =
  let sock =
    create_socket
      (match addr with
       [ ADDR_INET _ _ -> PF_INET
       | ADDR_UNIX _ -> PF_UNIX ])
      socktype
  in
  try connect_socket sock addr with exc -> do { close_socket sock; raise exc }
;

value fake_sockaddr = ADDR_UNIX "";

value generic_receive_message_bang =
  let myname = "receive_message(_bang)" in
  fun sockfd flags s start end_ ->
    do {
      Strings_5_1.check_substring_spec s start end_ myname;
      let rec loop i addr =
        if i >= end_ then (i - start, addr)
        else
          let (nread, sockaddr) =
            try Unix.recvfrom sockfd s i (end_ - i) flags with
            [ Unix.Unix_error Unix.EINTR _ _ -> (-1, addr)
            | Unix.Unix_error er s1 s2 ->
                (* ;; Give info on partially-read data in error packet. *)
                raise
                  (Io_3_2.String_io_error
                     (Io_3_2.Unix__error (er, s1, s2), myname, s, start, i, end_)) ]
          in
          if nread = 0 then
            let result = i - start in
            if result = 0 then raise End_of_file else (result, sockaddr)
          else loop (i + max 0 nread) sockaddr
      in
      loop start fake_sockaddr
    }
;

value generic_receive_message_bang_partial =
  let myname = "receive_message_(bang)_partial" in
  fun sockfd flags s start end_ ->
    do {
      Strings_5_1.check_substring_spec s start end_ myname;
      if start = end_ then (* ; Vacuous request.. *) (0, fake_sockaddr)
      else
        let rec loop () =
          let ((nread, sockaddr) as result) =
            try Unix.recvfrom sockfd s start (end_ - start) flags with
            [ Unix.Unix_error Unix.EINTR _ _ -> (-1, fake_sockaddr)
            | Unix.Unix_error Unix.EWOULDBLOCK _ _ | Unix.Unix_error Unix.EAGAIN _ _ ->
                (0, fake_sockaddr)
            | Unix.Unix_error er s1 s2 ->
                (* ;; Give info on partially-read data in error packet. *)
                raise
                  (Io_3_2.String_io_error
                     (Io_3_2.Unix__error (er, s1, s2), myname, s, start, start, end_)) ]
          in
          if nread < 0 then loop () else result
        in
        loop ()
    }
;

value (receive_message_bang, receive_message_bang_partial) =
  let receive_message_bang_star generic ?(start = 0) ?end_ ?(flags = []) socket s =
    generic (file_descr_of_socket socket) flags s start (Strings_5_1.opt_end s end_)
  in
  (receive_message_bang_star generic_receive_message_bang,
   receive_message_bang_star generic_receive_message_bang_partial)
;

value (receive_message, receive_message_partial) =
  let receive_message_star receive_bang ?flags socket len =
    let s = String.create len in
    let (nread, sockaddr) = receive_bang ?start:None ?end_:None ?flags socket s in
    (if nread = len then s else String.sub s 0 nread, sockaddr)
  in
  (receive_message_star receive_message_bang,
   receive_message_star receive_message_bang_partial)
;

value generic_send_message =
  let myname = "send_message" in
  fun ?addr sockfd flags s start end_ ->
    do {
      Strings_5_1.check_substring_spec s start end_ myname;
      let rec loop i =
        if i >= end_ then ()
        else
          let nwritten =
            try
              match addr with
              [ None -> Unix.send sockfd s i (end_ - i) flags
              | Some addr -> Unix.sendto sockfd s i (end_ - i) flags addr ]
            with
            [ Unix.Unix_error Unix.EINTR _ _ -> 0
            | Unix.Unix_error er s1 s2 ->
                (* ;; Give info on partially-read data in error packet. *)
                raise
                  (Io_3_2.String_io_error
                     (Io_3_2.Unix__error (er, s1, s2), myname, s, start, i, end_)) ]
          in
          loop (i + nwritten)
      in
      loop start
    }
;

value generic_send_message_partial =
  let myname = "send_message_partial" in
  fun ?addr sockfd flags s start end_ ->
    do {
      Strings_5_1.check_substring_spec s start end_ myname;
      if start = end_ then (* ; Vacuous request.. *) 0
      else
        let rec loop () =
          let nwritten =
            try
              match addr with
              [ None -> Unix.send sockfd s start (end_ - start) flags
              | Some addr -> Unix.sendto sockfd s start (end_ - start) flags addr ]
            with
            [ Unix.Unix_error Unix.EINTR _ _ -> -1
            | Unix.Unix_error Unix.EWOULDBLOCK _ _ | Unix.Unix_error Unix.EAGAIN _ _ -> 0
            | Unix.Unix_error er s1 s2 ->
                (* ;; Give info on partially-read data in error packet. *)
                raise
                  (Io_3_2.String_io_error
                     (Io_3_2.Unix__error (er, s1, s2), myname, s, start, start, end_)) ]
          in
          if nwritten < 0 then loop () else nwritten
        in
        loop ()
    }
;

value (send_message, send_message_partial) =
  let send_message_star generic_send ?(start = 0) ?end_ ?(flags = []) ?sockaddr socket s =
    generic_send ?addr:sockaddr (file_descr_of_socket socket) flags s start
      (Strings_5_1.opt_end s end_)
  in
  (send_message_star generic_send_message, send_message_star generic_send_message_partial)
;

type protocol_level =
  [ SOL_SOCKET ]
;

external low_get_socket_option_bool :
  in_channel -> protocol_level -> socket_bool_option -> bool = "cash_getsockopt_bool";
external low_set_socket_option_bool :
  in_channel -> protocol_level -> socket_bool_option -> bool ->
    unit = "cash_setsockopt_bool";

value socket_option_bool sock = low_get_socket_option_bool sock.sock_in;

value set_socket_option_bool sock = low_set_socket_option_bool sock.sock_in;

external low_get_socket_option_int :
  in_channel -> protocol_level -> socket_int_option -> int = "cash_getsockopt_int";
external low_set_socket_option_int :
  in_channel -> protocol_level -> socket_int_option -> int ->
    unit = "cash_setsockopt_int";

value socket_option_int sock = low_get_socket_option_int sock.sock_in;

value set_socket_option_int sock = low_set_socket_option_int sock.sock_in;

external low_get_socket_option_optint :
  in_channel -> protocol_level -> socket_optint_option -> option int = "cash_getsockopt_optint";
external low_set_socket_option_optint :
  in_channel -> protocol_level -> socket_optint_option -> option int ->
    unit = "cash_setsockopt_optint";

value socket_option_optint sock = low_get_socket_option_optint sock.sock_in;

value set_socket_option_optint sock = low_set_socket_option_optint sock.sock_in;

external low_get_socket_option_float :
  in_channel -> protocol_level -> socket_float_option -> float = "cash_getsockopt_float";
external low_set_socket_option_float :
  in_channel -> protocol_level -> socket_float_option -> float ->
    unit = "cash_setsockopt_float";

value socket_option_float sock = low_get_socket_option_float sock.sock_in;

value set_socket_option_float sock = low_set_socket_option_float sock.sock_in;

type herror =
  [ HOST_NOT_FOUND
  | TRY_AGAIN
  | NO_RECOVERY
  | NO_DATA
  | NO_ADDRESS ]
;

exception Netdb_error of herror;

(* May (re)raise Unix_error or Not_found. *)
external get_herr : string -> herror = "get_h_errno";

type network_info =
  { n_name : string;
    n_aliases : array string;
    n_addrtype : protocol_family;
    n_net : int32 }
;

external getnetbyname : string -> network_info = "unix_getnetbyname";
external getnetbyaddr : inet_addr -> network_info = "unix_getnetbyaddr";


value (host_info_addr, network_info_addr) =
  let db_info myname get_info addr =
    match addr with
    [ ADDR_INET inet_addr _ ->
        try get_info inet_addr with
        [ Not_found -> raise (Netdb_error (get_herr "host_info_addr")) ]
    | _ -> invalid_arg (myname ^ ": internet address expected") ]
  in
  (db_info "host_info_addr" Unix.gethostbyaddr, db_info "network_info_addr" getnetbyaddr)
;

value db_info myname get_info arg =
  try get_info arg with [ Not_found -> raise (Netdb_error (get_herr myname)) ]
;
(* type-checker doesn't generalize service_info_1 args if we do this:
value (host_info_name, network_info_name, service_info_1) =
  (db_info Unix.gethostbyname "host_info_name", db_info getnetbyname "network_info_name",
   fun getserv -> db_info getserv "service_info_name")
; C'est parce que l'ensemble du triplet n'est pas generalisable (seule fun
; getserv l'est); il faudrait qu'il calcule quelles sous expressions le sont ou
; pas; c'est fait pour if/then/else, pas ici. *)

value host_info_name = db_info "host_info_name" Unix.gethostbyname;
value network_info_name = db_info "network_info_name" getnetbyname;

value service_info_name ?(protocol = "") name =
  db_info "service_info_name" Unix.getservbyname name protocol
;
value service_info_port ?(protocol = "") port =
  db_info "service_info_port" Unix.getservbyport port protocol
;

value protocol_info_name = db_info "protocol_info_name" Unix.getprotobyname;
value protocol_info_port = db_info "protocol_info_port" Unix.getprotobynumber;

value sockaddr_of_host_and_port hostname port =
  let hostaddr = (host_info_name hostname).h_addr_list.(0) in
  ADDR_INET hostaddr port
;

value port_of_service service = (service_info_name ~protocol:"tcp" service).s_port;

value sockaddr_of_host_and_service hostname service =
  sockaddr_of_host_and_port hostname (port_of_service service)
;

value bind_listen_accept_loop protocol_family addr proc =
  let sock = create_socket protocol_family SOCK_STREAM in
  do {
    set_socket_option_bool sock SOL_SOCKET SO_REUSEADDR True;
    bind_socket sock addr;
    listen_socket sock 5;
    while True do {
      let (client_socket, client_addr) = accept_connection sock;
      proc client_socket client_addr
    }
  }
;

value bind_listen_accept_loop_unix path =
  bind_listen_accept_loop PF_UNIX (socket_address_of_unix_address path)
;

value bind_listen_accept_loop_port port =
  bind_listen_accept_loop PF_INET (socket_address_of_internet_address inet_addr_any port)
;

value bind_listen_accept_loop_service service =
  bind_listen_accept_loop_port (port_of_service service)
;
