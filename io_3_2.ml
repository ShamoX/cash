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

(* Machinery to get several Caml channels referring to the same C struct channel. *)
type low_chan = 'a;

external ichan_of_low_chan : low_chan -> in_channel = "in_channel_of_channel";
external ochan_of_low_chan : low_chan -> out_channel = "out_channel_of_channel";

type any_channel = 'a;

external anychan_of_ichan : in_channel -> any_channel = "%identity";
external anychan_of_ochan : out_channel -> any_channel = "%identity";
external ichan_of_anychan : any_channel -> in_channel = "%identity"; (* Uh oh. *)
external ochan_of_anychan : any_channel -> out_channel = "%identity";(* I mean: no check. *)

external low_chan_of_any_chan : any_channel -> low_chan = "channel_of_chan";

type fd = int;

external fd_of_file_descr : Unix.file_descr -> fd = "%identity";
external file_descr_of_fd : fd -> Unix.file_descr = "%identity";

(* Shouldn't bump the revealed count. *)
external low_maybe_fdes2chan : fd -> option low_chan = "maybe_fdes2chan";
external low_fd_of_any_channel : any_channel -> fd = "channel_descriptor";

external low_move_chan : fd -> low_chan -> int -> unit = "io_move_chan";
external push_ochan: out_channel -> low_chan -> low_chan = "io_push_chan";
external push_ichan: in_channel -> low_chan -> low_chan = "io_push_chan";
external set_ochan: out_channel -> low_chan -> unit = "io_set_chan";
external set_ichan: in_channel -> low_chan -> unit = "io_set_chan";

(* 3.2.2. Port manipulation and standard ports. *)

value rec low_close_fd fd =
  try do { Unix.close (file_descr_of_fd fd); True } with
  [ Unix.Unix_error Unix.EBADF _ _ -> False
  | Unix.Unix_error Unix.EINTR _ _ -> low_close_fd fd ]
;

(* ;; Don't mess with the revealed count in the port case
   ;; -- just sneakily grab the fdes and run. *)
(* XXX Y'a un paquet d'endroits où on pourrait utiliser ça aussi, e.g. close_XX
   (probablement la plupart des utilisations de low_fd_of_XX_channel).  Décider
   d'une stratégie: on vire sleazy_call_* ou bien on s'en sert partout. XXX *)

value sleazy_call_with_fdes_fd proc (fd : fd) = proc fd;  (* Pretty useless. *)
value sleazy_call_with_fdes_anychan proc chan = proc (low_fd_of_any_channel chan);
value sleazy_call_with_fdes_in proc ichan =
  sleazy_call_with_fdes_anychan proc (anychan_of_ichan ichan)
;
value sleazy_call_with_fdes_out proc ochan =
  sleazy_call_with_fdes_anychan proc (anychan_of_ochan ochan)
;

value (with_stdin, with_stdout, with_stderr) =
  let with_it push_it chan other_any_chan thunk =
    let prev_low_chan = push_it chan (low_chan_of_any_chan other_any_chan) in
    Env_3_11.unwind_protect thunk (fun prev -> ignore (push_it chan prev)) prev_low_chan
  in
  (fun other_chan -> with_it push_ichan stdin (anychan_of_ichan other_chan),
   fun other_chan -> with_it push_ochan stdout (anychan_of_ochan other_chan),
   fun other_chan -> with_it push_ochan stderr (anychan_of_ochan other_chan))
;

value set_stdin other_chan =
  set_ichan stdin (low_chan_of_any_chan (anychan_of_ichan other_chan))
;
value set_stdout other_chan =
  set_ochan stdout (low_chan_of_any_chan (anychan_of_ochan other_chan))
;
value set_stderr other_chan =
  set_ochan stderr (low_chan_of_any_chan (anychan_of_ochan other_chan))
;

value low_dup fd = fd_of_file_descr (Unix.dup (file_descr_of_fd fd));

(* The C level must insure that there's 1 C struct channel by fd. *)
(* (sleazy_call_with_fdes_fd fd low_dup) is inlining of (fdes_of_dup_fd fd) to
   avoid (the MU rule and) cross-recursion between fdes_of_dup_fd and
   evict_chans (and close_fd). *)
value evict_chans fd =
  match low_maybe_fdes2chan fd with
  [ None -> ()
  | Some chan -> low_move_chan (sleazy_call_with_fdes_fd low_dup fd) chan 0 ]
;

external close_any : any_channel -> unit = "caml_close_channel";

value close_fd fd = do { evict_chans fd; low_close_fd fd };

(* To yield a bool: we close the channel's fd in the back of
   Pervasives.close_{in,out}, then call it anyway to clean the corelib structs.
   However, we first have to flush out_channel's.  Once here, we can use
   close_any (caml_close_channel is Pervasives.close_{in,out} without flush) *)
value (close_in, close_out) =
  let close any_chan =
    match
      try Some (low_fd_of_any_channel any_chan) with
      [ Sys_error "Bad file descriptor" -> None ]
    with
    [ (* channel already regularly closed: its fd is -1. *)
      None ->
        False
    | (* But it may have been closed by close_fd, e.g. *)
      Some fd ->
        let res = low_close_fd fd in  (* low_close_fd will tell it. *)
        do {
          close_any any_chan; (* Never raise exception in the current implementation. *)
          res
        } ]
  in
  (fun ichan -> close (anychan_of_ichan ichan),
   fun ochan -> do { flush ochan; close (anychan_of_ochan ochan) })
;

value ignoring_close_in c = ignore (close_in c);
value ignoring_close_out c = ignore (close_out c);

(* ;; (close-after port f)
   ;;   Apply F to PORT. When F returns, close PORT, then return F's result.
   ;;     Does nothing special if you throw out or throw in. *)
(* However, it probably does nothing in case you throw in (before closing).  As
   we can't do that in Caml, we (tentatively) close the thing on exceptions too.
   Let's wait user's returns. 
   Internal close_after was: 
  let close_after closer file consumer =
    let r = consumer file in
    do { ignore (closer file); r }
  in *)

value (close_fd_after, close_in_after, close_out_after) =
  let close_after closer file consumer =
    Env_3_11.unwind_protect (fun () -> consumer file) (fun f -> ignore (closer f)) file
  in
  (fun f -> close_after close_fd f, fun ichan -> close_after close_in ichan,
   fun ochan -> close_after close_out ochan)
;

(* 3.2.3. String ports. *)
(* cf Pf_2_3_to_6. *)

(* 3.2.5. Port-mapping machinery. *)

(* scsh a un 2d arg. delta, mais c'est toujours 1. *)
external incr_revealed_count : low_chan -> unit = "increment_revealed_count";

(* low_xx_channel_of_fd <=> make-{input,output}-fdport; ça fait ~ comme
   Unix.xx_channel_of_descr, mais met revealed et close_on_exec comme
   install-port.  *)
external low_in_channel_of_fd : fd -> int -> in_channel = "in_channel_of_fd";
external low_out_channel_of_fd : fd -> int -> out_channel = "out_channel_of_fd";

value (in_channel_of_fd, out_channel_of_fd) =
  let channel_of_fd low_channel_of_fd chan_of_low_chan fd =
    match low_maybe_fdes2chan fd with
    [ None -> low_channel_of_fd fd 1
    | Some low_chan -> do { incr_revealed_count low_chan; chan_of_low_chan low_chan } ]
  in
  (channel_of_fd low_in_channel_of_fd ichan_of_low_chan,
   channel_of_fd low_out_channel_of_fd ochan_of_low_chan)
;

value fd_of_any_chan chan =
  let fd = low_fd_of_any_channel chan in
  do { incr_revealed_count (low_chan_of_any_chan chan); fd }
;
value fd_of_in_channel ichan = fd_of_any_chan (anychan_of_ichan ichan);
value fd_of_out_channel ochan = fd_of_any_chan (anychan_of_ochan ochan);

external chan_revealed_any : any_channel -> int = "chan_revealed_count";
(* Initialize revealed count on stdchans. *)
if Sys.interactive.val then ()
else
  List.iter
    (fun anychan ->
       do {
         ignore (fd_of_any_chan anychan);
         if chan_revealed_any anychan <> 1 then assert False else ()
       })
    [anychan_of_ichan stdin; anychan_of_ochan stdout; anychan_of_ochan stderr];

(* We call close_fd "; Thus evicting any port there". *)
value (fdes_of_dup_fd, fdes_of_dup_in, fdes_of_dup_out) =
  let fdes_of_dup sleazy_caller_with_fdes ?newfd fd =
    match newfd with
    [ None -> sleazy_caller_with_fdes low_dup fd
    | Some target_fd ->
        do {
          ignore (close_fd target_fd);
          sleazy_caller_with_fdes
            (fun fd ->
               do {
                 Unix.dup2 (file_descr_of_fd fd) (file_descr_of_fd target_fd); target_fd
               })
            fd
        } ]
  in
  (fdes_of_dup sleazy_call_with_fdes_fd,
   fun ?newfd ichan ->
     fdes_of_dup sleazy_call_with_fdes_anychan ?newfd (anychan_of_ichan ichan),
   fun ?newfd ochan ->
     fdes_of_dup sleazy_call_with_fdes_anychan ?newfd (anychan_of_ochan ochan))
;

value dup_fd = fdes_of_dup_fd;

value
  (in_channel_of_dup_fd, out_channel_of_dup_fd, in_channel_of_dup_in,
   out_channel_of_dup_in, in_channel_of_dup_out, out_channel_of_dup_out) =
  let chan_of_dup fdes_of_dup low_channel_of_fd ?newfd chan =
    let fd = fdes_of_dup ?newfd chan in
    low_channel_of_fd fd (if newfd = None then 0 else 1)
  in
  (chan_of_dup fdes_of_dup_fd low_in_channel_of_fd,
   chan_of_dup fdes_of_dup_fd low_out_channel_of_fd,
   chan_of_dup fdes_of_dup_in low_in_channel_of_fd,
   chan_of_dup fdes_of_dup_in low_out_channel_of_fd,
   chan_of_dup fdes_of_dup_out low_in_channel_of_fd,
   chan_of_dup fdes_of_dup_out low_out_channel_of_fd)
;

value dup_in = in_channel_of_dup_in;
value dup_out = out_channel_of_dup_out;

value stdchans_to_stdio () =
  do {
    ignore (fdes_of_dup_in ~newfd:0 stdin);
    ignore (fdes_of_dup_out ~newfd:1 stdout);
    ignore (fdes_of_dup_out ~newfd:2 stderr)
  }
;

value stdio_to_stdchans () =
  do {
    set_stdin (in_channel_of_fd 0);
    set_stdout (out_channel_of_fd 1);
    set_stderr (out_channel_of_fd 2)
  }
;

value with_stdio_chans thunk =
  with_stdin (in_channel_of_fd 0)
    (fun () ->
       with_stdout (out_channel_of_fd 1)
         (fun () -> with_stderr (out_channel_of_fd 2) thunk))
;

external chan_revealed_in : in_channel -> int = "chan_revealed_count";
external chan_revealed_out : out_channel -> int = "chan_revealed_count";

external release_chan_handle : any_channel -> unit = "release_chan_handle";
external release_chan_handle_in : in_channel -> unit = "release_chan_handle";
external release_chan_handle_out : out_channel -> unit = "release_chan_handle";

(* Pretty useless. *)
(* value call_with_fdes_fd op fd_or_chan = op fd_or_chan; *)

value (call_with_fdes_in, call_with_fdes_out) =
  let call_with_fdes op chan =
    let fd = fd_of_any_chan chan in
    Env_3_11.unwind_protect (fun () -> op fd) release_chan_handle chan
  in
  (fun op ichan -> call_with_fdes op (anychan_of_ichan ichan),
   fun op ochan -> call_with_fdes op (anychan_of_ochan ochan))
;

(* ;; Moves an i/o handle FD/PORT to fd TARGET.
;; - If FD/PORT is a file descriptor, this is dup2(); close().
;; - If FD/PORT is a port, this shifts the port's underlying file descriptor
;;   to TARGET, as above, closing the old one. Port's revealed count is
;;   set to 1.
;; TARGET is evicted before the shift -- if there is a port allocated to
;; file descriptor TARGET, it will be shifted to another file descriptor.
 *)

value move_fd_to_fdes fd target_fd =
  do {
    if fd = target_fd then ()
    else do {
      evict_chans target_fd; Unix.dup2 (file_descr_of_fd fd) (file_descr_of_fd target_fd)
    };
    target_fd
  }
;

value (move_in_channel_to_fdes, move_out_channel_to_fdes) =
  let move_any_chan_to_fdes chan target_fd =
    do {
      ignore (move_fd_to_fdes (low_fd_of_any_channel chan) target_fd);
      low_move_chan target_fd (low_chan_of_any_chan chan) 1;
      chan
    }
  in
  (fun ichan target_fd ->
     ichan_of_anychan (move_any_chan_to_fdes (anychan_of_ichan ichan) target_fd),
   fun ochan target_fd ->
     ochan_of_anychan (move_any_chan_to_fdes (anychan_of_ochan ochan) target_fd))
;

value ignoring_move_in_channel_to_fdes och fd = ignore (move_in_channel_to_fdes och fd);
value ignoring_move_out_channel_to_fdes och fd = ignore (move_out_channel_to_fdes och fd);

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
   end);

(* 3.2.6.  Unix I/O. *)

value seek_fd ?(whence = SEEK_SET) fd offset =
  Unix.lseek (file_descr_of_fd fd) offset whence
;
value tell_fd fd = seek_fd ~whence:(SEEK_CUR) fd 0;

value tell_in = Pervasives.pos_in;
value tell_out = Pervasives.pos_out;

value (seek_in, seek_out) =
  let seek chan_pos seek_chan anychan_of ?(whence = SEEK_SET) chan offset =
    let offset =
      match whence with
      [ SEEK_SET -> offset
      | SEEK_CUR -> chan_pos chan + offset
      | SEEK_END ->
          let fd = low_fd_of_any_channel (anychan_of chan) in
          let end_pos = seek_fd ~whence:(Unix.SEEK_END) fd 0 in
          end_pos + offset ]
    in
    do { seek_chan chan offset; offset }
  in
  (seek tell_in Pervasives.seek_in anychan_of_ichan,
   seek tell_out Pervasives.seek_out anychan_of_ochan)
;

value parse_open_mode flags =
  let ((n, what) as r) =
    if List.mem O_RDONLY flags then (1, O_RDONLY) else (0, O_EXCL)
  in
  let ((n, what) as r) =
    if List.mem O_WRONLY flags then (succ n, O_WRONLY) else r
  in
  if List.mem O_RDWR flags then (succ n, O_RDWR) else r
;

value openfile fn mode perms =
  let fd = Unix.openfile fn mode perms in
  do { Unix.set_close_on_exec fd; fd }
;

value open_fdes_internal not_this_mode fn ?(perms = 0o666) fname flags =
  let (n, what) = parse_open_mode flags in
  if n <> 1 ||
     (match not_this_mode with
      [ None -> False
      | Some mode -> mode = what ])
  then
    invalid_arg ("open_" ^ fn ^ ": illegal flags (" ^ fname ^ ")")
  else (fd_of_file_descr (openfile fname flags perms), what)
;

value open_fdes ?perms fname flags =
  let (fd, _) = open_fdes_internal None "fdes" ?perms fname flags in fd
;

value (open_file_out, open_file_in) =
  let open_file not_this_mode name make_chan ?perms fname flags =
    let (fd, what) = open_fdes_internal not_this_mode name ?perms fname flags in
    make_chan fd
  in
  (open_file (Some O_RDONLY) "file_out"
     (fun fd -> Unix.out_channel_of_descr (file_descr_of_fd fd)),
   open_file (Some O_WRONLY) "file_in"
     (fun fd -> Unix.in_channel_of_descr (file_descr_of_fd fd)))
;

value (open_input_file, open_output_file) =
  let open_xput name oflags opener flags fname =
    if fst (parse_open_mode oflags) = 0 then opener fname flags
    else invalid_arg (name ^ "put_file: flags contains an open mode")
  in
  (fun ?(flags = []) ->
     open_xput "open_in" flags (open_file_in ?perms:None) [O_RDONLY :: flags],
    fun ?(flags = [O_CREAT; O_TRUNC]) ?perms ->
     open_xput "open_out" flags (open_file_out ?perms) [O_WRONLY :: flags])
;

value (with_input_from_file, with_output_to_file, with_errors_to_file) =
  let with_file close with_chan chan thunk =
    Env_3_11.unwind_protect (fun () -> with_chan chan thunk) close chan
  in
  (fun fname -> with_file ignoring_close_in with_stdin (open_in fname),
   fun fname -> with_file ignoring_close_out with_stdout (open_out fname),
   fun fname -> with_file ignoring_close_out with_stderr (open_out fname))
;

value call_with_input_file fn = close_in_after (open_in fn);
value call_with_output_file fn = close_out_after (open_out fn);
value call_with_fdes_fn ?perms fn flags = close_fd_after (open_fdes ?perms fn flags);

type fdes_flags = [ FD_CLOEXEC ];
external fdes_flags_fd : fd -> list fdes_flags = "read_fdes_flags";
value fdes_flags_in ichan =
  fdes_flags_fd (low_fd_of_any_channel (anychan_of_ichan ichan))
;
value fdes_flags_out ochan =
  fdes_flags_fd (low_fd_of_any_channel (anychan_of_ochan ochan))
;

external set_fdes_flags_fd : fd -> list fdes_flags -> unit = "write_fdes_flags";
value set_fdes_flags_in ichan flags =
  set_fdes_flags_fd (low_fd_of_any_channel (anychan_of_ichan ichan)) flags
;
value set_fdes_flags_out ochan flags =
  set_fdes_flags_fd (low_fd_of_any_channel (anychan_of_ochan ochan)) flags
;

external fdes_status_fd : fd -> list open_flag = "read_file_flags";
value fdes_status_in ichan =
  fdes_status_fd (low_fd_of_any_channel (anychan_of_ichan ichan))
;
value fdes_status_out ochan =
  fdes_status_fd (low_fd_of_any_channel (anychan_of_ochan ochan))
;

external set_fdes_status_fd : fd -> list open_flag -> unit = "write_file_flags";
value set_fdes_status_in ichan flags =
  set_fdes_status_fd (low_fd_of_any_channel (anychan_of_ichan ichan)) flags
;
value set_fdes_status_out ochan flags =
  set_fdes_status_fd (low_fd_of_any_channel (anychan_of_ochan ochan)) flags
;

value pipe () =
  let (inc, out) = Unix.pipe () in
  (Unix.in_channel_of_descr inc, Unix.out_channel_of_descr out)
;

type error_packet =
  [ Sys__error of string
  | Unix__error of (Unix.error * string * string) ]
;

exception
  String_io_error of (error_packet * string * string * int * int * int);

(* -- Common operations. *)

(* To be called with 3 args. *)
value make_generic_read_write_string_bang_partial_star
  myname zero_handler operator s start end_ io_handle =
  do {
    Strings_5_1.check_substring_spec s start end_ myname;
    if start = end_ then 0
    else
      let rec loop () =
        match operator s start end_ io_handle with
        [ -2 -> 0
        | -1 -> loop ()
        | 0 -> zero_handler ()
        | nchars -> nchars ]
      in
      loop ()
  }
;

(* To be called with 2 args. *)
value unix_partial_io_operator operator opname s start end_ io_handle =
  try operator (file_descr_of_fd io_handle) s start (end_ - start) with
  [ Unix.Unix_error Unix.EWOULDBLOCK _ _ | Unix.Unix_error Unix.EAGAIN _ _ -> -2
  | Unix.Unix_error Unix.EINTR _ _ -> -1
  | Unix.Unix_error er s1 s2 ->
      raise (String_io_error (Unix__error (er, s1, s2), opname, s, start, start, end_)) ]
;

(* To be called with 2 args. *)
value pervasives_partial_io_operator operator opname s start end_ io_handle =
  try operator io_handle s start (end_ - start) with
  [ Sys_blocked_io -> -2
  | Sys_error er ->
      raise (String_io_error (Sys__error er, opname, s, start, start, end_)) ]
;

(* To be called with 2 args. *)
value unix_io_operator operator opname s start i end_ io_handle =
  try operator (file_descr_of_fd io_handle) s i (end_ - i) with
  [ Unix.Unix_error Unix.EINTR _ _ -> -1
  | Unix.Unix_error er s1 s2 ->
      raise (String_io_error (Unix__error (er, s1, s2), opname, s, start, i, end_)) ]
;

(* To be called with 2 args. *)
value pervasives_io_operator operator opname s start i end_ io_handle =
  let error er = raise (String_io_error (Sys__error er, opname, s, start, i, end_)) in
  try operator io_handle s i (end_ - i) with
  [ Sys_blocked_io -> error "Sys_blocked_io"
  | Sys_error er -> error er ]
;

(* -- READ_STRING_*. *)

(* Read zero_handler. *)
value raise_eof () = raise End_of_file;

(* Unix.read version. *)
value generic_read_string_bang_partial =
  let myname = "read_string_bang_partial" in
  make_generic_read_write_string_bang_partial_star myname raise_eof
    (unix_partial_io_operator Unix.read myname)
;

(* Pervasives.input version. *)
value generic_read_string_bang_partial_in =
  let myname = "read_string_bang_partial_in" in
  make_generic_read_write_string_bang_partial_star myname raise_eof
    (pervasives_partial_io_operator Pervasives.input myname)
;

(* To be called with 2 args. *)
value make_generic_read_string_bang_star myname operator s start end_ source =
  do {
    Strings_5_1.check_substring_spec s start end_ myname;
    if start = end_ then 0
    else
      let rec loop i =
        if i >= end_ then i - start
        else
          let nread = operator s start i end_ source in
          if nread > 0 then loop (i + nread)
          else if nread = 0 then
            let result = i - start in
            if result = 0 then raise End_of_file else result
          else loop i
      in
      loop start
  }
;

(* Unix.read version. *)
value generic_read_string_bang =
  let myname = "read_string_bang" in
  make_generic_read_string_bang_star myname (unix_io_operator Unix.read myname)
;

(* Pervasives.input version. *)
value generic_read_string_bang_in =
  let myname = "read_string_bang_in" in
  make_generic_read_string_bang_star myname
    (pervasives_io_operator Pervasives.input myname)
;

value
  (read_string_bang_partial, read_string_bang, read_string_bang_partial_in,
   read_string_bang_in)
  =
  let read_string dflt generic_reader ?(src = dflt) ?(start = 0) ?end_ s =
    generic_reader s start (Strings_5_1.opt_end s end_) src
  in
  (read_string 0 generic_read_string_bang_partial, read_string 0 generic_read_string_bang,
   read_string stdin generic_read_string_bang_partial_in,
   read_string stdin generic_read_string_bang_in)
;

(* XXX ?src : 'a ne passe pas. (PR#1156) *)
value (read_string, read_string_in, read_string_partial, read_string_partial_in) =
  let read (reader : ?src: _ -> ?start: int -> ?end_: int -> string -> int) ?src nbytes =
    let s = String.create nbytes in
    let nread = reader ?src ~end_:nbytes s in
    if nread = nbytes then s else String.sub s 0 nread
  in
  (read read_string_bang, read read_string_bang_in, read read_string_bang_partial,
   read read_string_bang_partial_in)
;

(* Write zero_handler. *)
value impossible () = assert False;

(* Unix.write version. *)
value generic_write_string_partial =
  let myname = "write_string_partial" in
  make_generic_read_write_string_bang_partial_star myname impossible
    (unix_partial_io_operator Unix.write myname)
;

(* io.c (not in Pervasives) output version. *)
external unsafe_output_partial :
  out_channel -> string -> int -> int -> int = "caml_output_partial";

value generic_write_string_partial_out =
  let myname = "write_string_partial_out" in
  make_generic_read_write_string_bang_partial_star myname impossible
    (pervasives_partial_io_operator unsafe_output_partial myname)
;

(* A little too different from make_generic_read_string_bang_star to be merged. *)
(* To be called with 2 args. *)
value make_generic_write_string_star myname operator s start end_ target =
  do {
    Strings_5_1.check_substring_spec s start end_ myname;
    let rec loop i =
      if i < end_ then
        let nwritten = operator s start i end_ target in
        if nwritten = -1 then (* intr *) loop i else loop (i + nwritten)
      else ()
    in
    loop start
  }
;

(* Unix.write version. *)
value generic_write_string =
  let myname = "write_string" in
  make_generic_write_string_star myname (unix_io_operator Unix.write myname)
;

(* io.c (not in Pervasives) output version. *)
value generic_write_string_out =
  let myname = "write_string_out" in
  make_generic_write_string_star myname
    (pervasives_io_operator unsafe_output_partial myname)
;

value (write_string_partial, write_string, write_string_partial_out, write_string_out) =
  let write_string dflt generic_writer ?(dst = dflt) ?(start = 0) ?end_ s =
    generic_writer s start (Strings_5_1.opt_end s end_) dst
  in
  (write_string 0 generic_write_string_partial, write_string 0 generic_write_string,
   write_string stdout generic_write_string_partial_out,
   write_string stdout generic_write_string_out)
;

(* select. *)

type selectable =
  [ Nothing
  | Read_in of in_channel
  | Read_fd of fd
  | Write_out of out_channel
  | Write_fd of fd
  | Except_in of in_channel
  | Except_fd of fd ]
;

external input_buf_empty : in_channel -> bool = "cash_input_buf_empty";
external output_buf_full: out_channel -> bool = "cash_output_buf_full";

value select_bang ?(timeout = (-1.0)) vec =
  let len = Array.length vec in
  (* fd_vec contains the fd's actually passed to Unix.select. *)
  let fdes_vec = Array.make len (file_descr_of_fd (-1)) in
  let add_fd i fd list =
    let fdescr =
      if fd < 0 then invalid_arg ("select_bang: " ^ string_of_int fd)
      else file_descr_of_fd fd
    in
    do { fdes_vec.(i) := fdescr; [fdescr :: list] }
  in
  let (read, write, except) =
    loop [] [] [] len False where rec loop read write except i buf_ready_seen =
      let i = pred i in
      if i < 0 then
        (* no fd to pass to select: if we saw ready buffer(s), no need to select... *)
        if buf_ready_seen && read = [] && write = [] && except = [] then ([], [], [])
        else
          (* .. but if we didn't see one, let select do micro-sleep as user asked. *)
          let timeout = if buf_ready_seen then 0.0 else timeout in
          (* If we saw ready buffer(s), don't wait for other fdescs,
             just get those ready right now. *)
          Unix.select read write except timeout
      else
        match vec.(i) with
        [ Nothing -> loop read write except i buf_ready_seen
        | Read_fd fd -> loop (add_fd i fd read) write except i buf_ready_seen
        | Write_fd fd -> loop read (add_fd i fd write) except i buf_ready_seen
        | Except_fd fd -> loop write read (add_fd i fd except) i buf_ready_seen
        | Read_in ichan ->
            if input_buf_empty ichan then
              loop (add_fd i (low_fd_of_any_channel (anychan_of_ichan ichan)) read) write
                except i buf_ready_seen
            else loop read write except i True
        | Except_in ichan ->
            loop read write
              (add_fd i (low_fd_of_any_channel (anychan_of_ichan ichan)) except) i
              buf_ready_seen
        | Write_out ochan ->
            if output_buf_full ochan then
              loop read (add_fd i (low_fd_of_any_channel (anychan_of_ochan ochan)) write)
                except i buf_ready_seen
            else loop read write except i True ]
  in
  let nr = ref 0
  and nw = ref 0
  and ne = ref 0 in
  let zap_if_not_in list buffer_ready cnt i =
    (* If buffer_ready and fdes = -1, we didn't pass this fdes
       to select but the chan is ok, so don't zap it *)
    let fdes = fdes_vec.(i) in
    if if fdes = file_descr_of_fd (-1) then buffer_ready else List.exists ( \= fdes) list
    then
      incr cnt
    else vec.(i) := Nothing
  in
  do {
    for i = pred len downto 0 do {
      match vec.(i) with
      [ Nothing -> ()
      | Read_in _ -> zap_if_not_in read True nr i
      | Read_fd _ -> zap_if_not_in read False nr i
      | Write_out _ -> zap_if_not_in write True nw i
      | Write_fd _ -> zap_if_not_in write False nw i
      | Except_in _ | Except_fd _ -> zap_if_not_in except False ne i ]
    };
    (nr.val, nw.val, ne.val)
  }
;

value select ?timeout vec =
  let vec = Array.copy vec in
  let (nr, nw, ne) = select_bang ?timeout vec in
  let rvec = Array.make (nr + nw + ne) Nothing in
  do {
    (let rec loop i ri =
       match vec.(i) with
       [ Nothing -> ()
       | _ -> do { rvec.(ri) := vec.(i); loop (succ i) (succ ri) } ]
     in
     loop (Array.length vec) 0);
    rvec
  }
;

(* 3.2.7.  Buffered I/O. *)

type bufpolicy =
  [ Block
  | Line
  | Nobuf ]
;
external set_buffer_size : any_channel -> int -> unit = "cash_set_buffer_size";

value (set_chan_buffering_in, set_chan_buffering_out) =
  let set_chan_buffering chan ?(size = -1) policy =
    let size =
      if size = 0 then 1
      else
        match policy with
        [ Block -> if size = -1 then 0 else size
        | Nobuf -> 1
        | Line -> failwith "Line buffering policy not implemented" ]
    in
    set_buffer_size chan size
  in
  (fun ichan -> set_chan_buffering (anychan_of_ichan ichan),
   fun ochan -> set_chan_buffering (anychan_of_ochan ochan))
;

(* Set stdin unbuffered. *)
set_chan_buffering_in stdin Nobuf;

value force_output = Pervasives.flush;

value flush_all_chans = Pervasives.flush_all;
