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

(* From Io_3_2.ml. *)
type fd = int;

external is_tty_fd : fd -> bool = "unix_isatty";
value is_tty_in = Io_3_2.sleazy_call_with_fdes_in is_tty_fd;
value is_tty_out = Io_3_2.sleazy_call_with_fdes_out is_tty_fd;

external tty_file_name_fd : fd -> string = "unix_tty_file_name";
value tty_file_name_in = Io_3_2.sleazy_call_with_fdes_in tty_file_name_fd;
value tty_file_name_out = Io_3_2.sleazy_call_with_fdes_out tty_file_name_fd;

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

external getttychars : unit -> string = "cash_tty_chars";

value (n_tty_chars, disable_tty_char, ttychar) =
  let chars = getttychars ()
  and int_of_char str i =
    let x = Char.code str.[i] in
    if x < 128 then x else x - 256
  in
  let rec loop i n =
    if i < 0 then n else loop (pred i) (if Char.code chars.[i] <> 255 then succ n else n)
  in
  (loop (pred (String.length chars)) 0,
   chars.[0],
   {delete_char     = int_of_char chars 1;
    delete_line     = int_of_char chars 2;
    eof             = int_of_char chars 3;
    eol             = int_of_char chars 4;
    interrupt       = int_of_char chars 5;
    quit            = int_of_char chars 6;
    suspend         = int_of_char chars 7;
    start           = int_of_char chars 8;
    stop            = int_of_char chars 9;
    delayed_suspend = int_of_char chars 10;
    delete_word     = int_of_char chars 11;
    discard         = int_of_char chars 12;
    eol2            = int_of_char chars 13;
    literal_next    = int_of_char chars 14;
    reprint         = int_of_char chars 15;
    status          = int_of_char chars 16
   })
;

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

external input_flags : unit -> array nativeint = "cash_input_flags";

value ttyin =
  let flags = input_flags () in
  {check_parity            = Array.unsafe_get flags 0;
   ignore_bad_parity_chars = Array.unsafe_get flags 1;
   mark_parity_errors      = Array.unsafe_get flags 2;
   ignore_break            = Array.unsafe_get flags 3;
   interrupt_on_break      = Array.unsafe_get flags 4;
   seven_bits              = Array.unsafe_get flags 5;
   cr_to_nl                = Array.unsafe_get flags 6;
   ignore_cr               = Array.unsafe_get flags 7;
   nl_to_cr                = Array.unsafe_get flags 8;
   input_flow_ctl          = Array.unsafe_get flags 9;
   output_flow_ctl         = Array.unsafe_get flags 10;
   xon_any                 = Array.unsafe_get flags 11;
   beep_on_overflow        = Array.unsafe_get flags 12;
   lowercase               = flags.(13) (* safe_get for the last one. *)
  }
;

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

external output_flags : unit -> array nativeint = "cash_output_flags";

value ttyout =
  let flags = output_flags () in
  {enable               = Array.unsafe_get flags 0;
   nl_to_crnl           = Array.unsafe_get flags 1;
   discard_eot          = Array.unsafe_get flags 2;
   expand_tabs          = Array.unsafe_get flags 3;
   cr_to_nl             = Array.unsafe_get flags 4;
   nl_does_cr           = Array.unsafe_get flags 5;
   no_col0_cr           = Array.unsafe_get flags 6;
   delay_with_fill_char = Array.unsafe_get flags 7;
   fill_with_del        = Array.unsafe_get flags 8;
   uppercase            = Array.unsafe_get flags 9;
   bs_delay             = Array.unsafe_get flags 10;
   bs_delay0            = Array.unsafe_get flags 11;
   bs_delay1            = Array.unsafe_get flags 12;
   cr_delay             = Array.unsafe_get flags 13;
   cr_delay0            = Array.unsafe_get flags 14;
   cr_delay1            = Array.unsafe_get flags 15;
   cr_delay2            = Array.unsafe_get flags 16;
   cr_delay3            = Array.unsafe_get flags 17;
   ff_delay             = Array.unsafe_get flags 18;
   ff_delay0            = Array.unsafe_get flags 19;
   ff_delay1            = Array.unsafe_get flags 20;
   tab_delay            = Array.unsafe_get flags 21;
   tab_delay0           = Array.unsafe_get flags 22;
   tab_delay1           = Array.unsafe_get flags 23;
   tab_delay2           = Array.unsafe_get flags 24;
   tab_delayx           = Array.unsafe_get flags 25;
   nl_delay             = Array.unsafe_get flags 26;
   nl_delay0            = Array.unsafe_get flags 27;
   nl_delay1            = Array.unsafe_get flags 28;
   vtab_delay           = Array.unsafe_get flags 29;
   vtab_delay0          = Array.unsafe_get flags 30;
   vtab_delay1          = Array.unsafe_get flags 31;
   all_delay            = flags.(32) (* safe_get for the last one. *)
  }
;

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

external control_flags : unit -> array nativeint = "cash_control_flags";

value ttyc =
  let flags = control_flags () in
  {char_size               = Array.unsafe_get flags 0;
   char_size5              = Array.unsafe_get flags 1;
   char_size6              = Array.unsafe_get flags 2;
   char_size7              = Array.unsafe_get flags 3;
   char_size8              = Array.unsafe_get flags 4;
   enable_parity           = Array.unsafe_get flags 5;
   odd_parity              = Array.unsafe_get flags 6;
   enable_read             = Array.unsafe_get flags 7;
   hup_on_close            = Array.unsafe_get flags 8;
   no_modem_sync           = Array.unsafe_get flags 9;
   two_stop_bits           = Array.unsafe_get flags 10;
   ignore_flags            = Array.unsafe_get flags 11;
   cts_output_flow_control = Array.unsafe_get flags 12;
   rts_input_flow_control  = Array.unsafe_get flags 13;
   carrier_flow_ctl        = flags.(14) (* safe_get for the last one. *)
  }
;

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

external local_flags : unit -> array nativeint = "cash_local_flags";

value ttyl =
   let flags = local_flags () in
  { canonical             = Array.unsafe_get flags 0;
    echo                  = Array.unsafe_get flags 1;
    echo_delete_lines     = Array.unsafe_get flags 2;
    echo_nl               = Array.unsafe_get flags 3;
    visual_delete         = Array.unsafe_get flags 4;
    enable_signals        = Array.unsafe_get flags 5;
    extended              = Array.unsafe_get flags 6;
    no_flush_on_interrupt = Array.unsafe_get flags 7;
    ttou_signal           = Array.unsafe_get flags 8;
    echo_ctl              = Array.unsafe_get flags 9;
    flush_output          = Array.unsafe_get flags 10;
    hardcopy_delete       = Array.unsafe_get flags 11;
    reprint_unread_chars  = Array.unsafe_get flags 12;
    visual_delete_line    = Array.unsafe_get flags 13;
    alt_delete_word       = Array.unsafe_get flags 14;
    no_kernel_status      = Array.unsafe_get flags 15;
    case_map              = flags.(16) (* safe_get for the last one. *)
  }
;


value make_tty_info iflags oflags cflags lflags ispeed ospeed min time =
  {control_chars = String.make n_tty_chars (Char.chr 0); input_flags = iflags;
   output_flags = oflags; control_flags = cflags; local_flags = lflags;
   input_speed = ispeed; output_speed = ospeed; min = min; time = time}
;

value copy_tty_info info = {(info) with control_chars = String.copy info.control_chars};

value (sleazy_call_with_in_file, sleazy_call_with_out_file) =
  let flags_in = [Io_3_2.O_RDONLY]
  and flags_out = [Io_3_2.O_WRONLY] in
  (fun fn -> Io_3_2.call_with_fdes_fn fn flags_in,
   fun fn -> Io_3_2.call_with_fdes_fn fn flags_out)
;

external tty_info_fd : fd -> tty_info = "cash_tty_info";
value tty_info_in = Io_3_2.sleazy_call_with_fdes_in tty_info_fd;
value tty_info_out = Io_3_2.sleazy_call_with_fdes_out tty_info_fd;
value tty_info_fn fn = sleazy_call_with_in_file fn tty_info_fd;

type how =
  [ NOW
  | DRAIN
  | FLUSH ]
;

external set_tty_info_fd : how -> fd -> tty_info -> unit = "cash_set_ttyinfo";

value set_tty_info_now_fd = set_tty_info_fd NOW;
value set_tty_info_now_in = Io_3_2.sleazy_call_with_fdes_in set_tty_info_now_fd;
value set_tty_info_now_out = Io_3_2.sleazy_call_with_fdes_out set_tty_info_now_fd;
value set_tty_info_now_fn fn = sleazy_call_with_in_file fn set_tty_info_now_fd;

value set_tty_info_drain_fd = set_tty_info_fd DRAIN;
value set_tty_info_drain_in = Io_3_2.sleazy_call_with_fdes_in set_tty_info_drain_fd;
value set_tty_info_drain_out = Io_3_2.sleazy_call_with_fdes_out set_tty_info_drain_fd;
value set_tty_info_drain_fn fn = sleazy_call_with_in_file fn set_tty_info_drain_fd;

value set_tty_info_flush_fd = set_tty_info_fd FLUSH;
value set_tty_info_flush_in = Io_3_2.sleazy_call_with_fdes_in set_tty_info_flush_fd;
value set_tty_info_flush_out = Io_3_2.sleazy_call_with_fdes_out set_tty_info_flush_fd;
value set_tty_info_flush_fn fn = sleazy_call_with_in_file fn set_tty_info_flush_fd;

(* ;;; Send a break on the serial line. *)
value send_tty_break_fd ?(duration = 0) fd =
  Unix.tcsendbreak (Io_3_2.file_descr_of_fd fd) duration
;
value send_tty_break_in ?duration =
  Io_3_2.sleazy_call_with_fdes_in (send_tty_break_fd ?duration)
;
value send_tty_break_out ?duration =
  Io_3_2.sleazy_call_with_fdes_out (send_tty_break_fd ?duration)
;
value send_tty_break_fn ?duration fn =
  sleazy_call_with_in_file fn (send_tty_break_fd ?duration)
;

(* ;;; Drain the main vein. *)
value drain_tty_fd fd = Unix.tcdrain (Io_3_2.file_descr_of_fd fd);
value drain_tty_in = Io_3_2.sleazy_call_with_fdes_in drain_tty_fd;
value drain_tty_out out =
  do { flush out; Io_3_2.sleazy_call_with_fdes_out drain_tty_fd out }
;
value drain_tty_fn fn = sleazy_call_with_out_file fn drain_tty_fd;

(* ;;; Flushing the device queues. *)
value flush_tty_input_fd fd = Unix.tcflush (Io_3_2.file_descr_of_fd fd) Unix.TCIFLUSH;
value flush_tty_input_in = Io_3_2.sleazy_call_with_fdes_in flush_tty_input_fd;
value flush_tty_input_out = Io_3_2.sleazy_call_with_fdes_out flush_tty_input_fd;
value flush_tty_input_fn fn = sleazy_call_with_in_file fn flush_tty_input_fd;

value flush_tty_output_fd fd = Unix.tcflush (Io_3_2.file_descr_of_fd fd) Unix.TCOFLUSH;
value flush_tty_output_in = Io_3_2.sleazy_call_with_fdes_in flush_tty_output_fd;
value flush_tty_output_out = Io_3_2.sleazy_call_with_fdes_out flush_tty_output_fd;
value flush_tty_output_fn fn = sleazy_call_with_out_file fn flush_tty_output_fd;

value flush_tty_both_fd fd = Unix.tcflush (Io_3_2.file_descr_of_fd fd) Unix.TCIOFLUSH;
value flush_tty_both_in = Io_3_2.sleazy_call_with_fdes_in flush_tty_both_fd;
value flush_tty_both_out = Io_3_2.sleazy_call_with_fdes_out flush_tty_both_fd;
value flush_tty_both_fn fn = sleazy_call_with_in_file fn flush_tty_both_fd;

(* ;;; Stopping and starting I/O. *)
value start_tty_output_fd fd = Unix.tcflow (Io_3_2.file_descr_of_fd fd) Unix.TCOON;
value start_tty_output_in = Io_3_2.sleazy_call_with_fdes_in start_tty_output_fd;
value start_tty_output_out = Io_3_2.sleazy_call_with_fdes_out start_tty_output_fd;
value start_tty_output_fn fn = sleazy_call_with_out_file fn start_tty_output_fd;

value stop_tty_output_fd fd = Unix.tcflow (Io_3_2.file_descr_of_fd fd) Unix.TCOOFF;
value stop_tty_output_in = Io_3_2.sleazy_call_with_fdes_in stop_tty_output_fd;
value stop_tty_output_out = Io_3_2.sleazy_call_with_fdes_out stop_tty_output_fd;
value stop_tty_output_fn fn = sleazy_call_with_out_file fn stop_tty_output_fd;

value start_tty_input_fd fd = Unix.tcflow (Io_3_2.file_descr_of_fd fd) Unix.TCION;
value start_tty_input_in = Io_3_2.sleazy_call_with_fdes_in start_tty_input_fd;
value start_tty_input_out = Io_3_2.sleazy_call_with_fdes_out start_tty_input_fd;
value start_tty_input_fn fn = sleazy_call_with_in_file fn start_tty_input_fd;

value stop_tty_input_fd fd = Unix.tcflow (Io_3_2.file_descr_of_fd fd) Unix.TCIOFF;
value stop_tty_input_in = Io_3_2.sleazy_call_with_fdes_in stop_tty_input_fd;
value stop_tty_input_out = Io_3_2.sleazy_call_with_fdes_out stop_tty_input_fd;
value stop_tty_input_fn fn = sleazy_call_with_in_file fn stop_tty_input_fd;

(*; (open-control-tty fname [flags])
;;; Open a control tty, return a port.
;;; This procedure is only guaranteed to work when the process doesn't already
;;; have a control tty -- e.g., right after a (BECOME-PROCESS-LEADER) call.
;;; This limted functionality is about all we can provide portably across BSD,
;;; SunOS, and SVR4. *)
external open_descriptor_out : fd -> out_channel = "caml_open_descriptor_out";
external open_descriptor_in : fd -> in_channel = "caml_open_descriptor_in";
external set_ctty : Unix.file_descr -> fd = "cash_set_control_tty";

(* Scsh marks the channel as revealed?  I don't see why. *)
value (open_control_tty_in, open_control_tty_out) =
  let open_control_tty make_chan flags =
    lp where rec lp tty_name =
      match
        try Some (set_ctty (Io_3_2.openfile tty_name flags 0o666)) with
        [ Unix.Unix_error Unix.EINTR _ _ -> None ]
      with
      [ None -> lp tty_name
      | Some fd -> make_chan fd ]
  in
  (fun ?(flags = [Io_3_2.O_RDWR]) -> open_control_tty open_descriptor_in flags,
   fun ?(flags = [Io_3_2.O_RDONLY]) -> open_control_tty open_descriptor_out flags)
;

value become_session_leader = Unix.setsid;

external tty_process_group_fd : fd -> int = "unix_tcgetpgrp";

value tty_process_group_in = Io_3_2.sleazy_call_with_fdes_in tty_process_group_fd;
value tty_process_group_out = Io_3_2.sleazy_call_with_fdes_out tty_process_group_fd;
value tty_process_group_fn fn = sleazy_call_with_in_file fn tty_process_group_fd;

external set_tty_process_group_fd : fd -> int -> int = "unix_tcsetpgrp";

value set_tty_process_group_in = Io_3_2.sleazy_call_with_fdes_in set_tty_process_group_fd;
value set_tty_process_group_out =
  Io_3_2.sleazy_call_with_fdes_out set_tty_process_group_fd
;
value set_tty_process_group_fn fn = sleazy_call_with_in_file fn set_tty_process_group_fd;

external control_tty_file_name : unit -> string = "unix_control_tty_file_name";

(* ; The following code may in fact be system dependent.
  ;; If so, we'll move it out to the architecture specific directories. *)

value make_pty_generator =
  let letters = "pq"
  and numbers = "0123456789abcdef" in
  let num_letters = String.length letters
  and num_numbers = String.length numbers in
  fun () ->
    let pattern = String.copy "/dev/ptyLN" in  (* ; L=letter N=number. *)
      (* ; Generator's state vars.  The value of the last elt that was generated. *)
    let l_state = ref num_letters
    and n_state = ref 0 in          (* ; (We count backwards to (0,0); n fastest.) *)
    fun () ->
      do {
        if n_state.val = 0 then
          if l_state.val = 0 then raise Not_found
          else do {
            decr l_state;
            n_state.val := pred num_numbers;
            pattern.[8] := letters.[l_state.val]
          }
        else decr n_state;
        pattern.[9] := numbers.[n_state.val];
        String.copy pattern
      }
;

(* ; Map between corresponding pty and tty filenames. *)
value (tty_name_of_pty_name, pty_name_of_tty_name) =
  let pty_tty_name_mapper c name =
    let ans = String.copy name in
    do {
      ans.[5] := c;   (* ; Change X in "/dev/Xtyzz" to CHAR. *)
      ans
    }
  in
  (pty_tty_name_mapper 't', pty_tty_name_mapper 'p')
;

(* ;; (open-pty)
;;; Returns two values: [pty-inport ttyname]
;;; PTY-PORT is a port open on the pty.
;;; TTYNAME is the name of the tty, e.g., "/dev/ttyk4"

;;; Scheme doesn't allow bidirectional ports, so the returned port is an input
;;; port -- however, the underlying file descriptor is opened read+write, and
;;; you can use DUP->OUTPORT to map it to corresponding output ports. *)

value open_pty () =
  loop (make_pty_generator ()) where rec loop next_pty =
    match try Some (next_pty ()) with [ Not_found -> None ] with
    [ None -> failwith "open_pty: could not open new pty"
    | Some pty_name ->
        (* Don't bother with tail recursion: make_pty_generator limits nesting to 32. *)
        try
          let pty = Io_3_2.open_file_in pty_name [Io_3_2.O_RDWR] in
          (pty, tty_name_of_pty_name pty_name)
        with _ ->
          loop next_pty ]
;

(* ;;; (fork-pty-session thunk)
;;; Fork the process with stdio (fd's 0, 1, & 2 and also the current i/o ports)
;;; bound to a tty device. In the parent process, returns four values:
;;;     [process pty-inport pty-outport ttyname]
;;; - PROCESS is a process object for the child.
;;; - PTY-{IN,OUT}PORT are input and output ports open on the controlling pty
;;;   device. PTY-OUTPORT is unbuffered.
;;; - TTYNAME is the name of the child's tty, e.g. "/dev/ttyk4".
;;;
;;; The subprocess is placed in its own session, and the tty device becomes the
;;; control tty for the new session/process-group/process.  The child runs with
;;; stdio hooked up to the tty; the (error-output-port) port is unbuffered.
*)
value fork_pty_session thunk =
  let (pty_in, ttyname) = open_pty () in
  let process =
    Proc_3_4.fork_child
      (fun () ->
         do {
           close_in pty_in;
           ignore (become_session_leader ());
           let tty_in = open_control_tty_in ttyname in
           do {
             ignore (Io_3_2.move_in_channel_to_fdes tty_in 0);
             ignore (Io_3_2.out_channel_of_dup_in ~newfd:1 tty_in);
             Io_3_2.set_chan_buffering_out (Io_3_2.out_channel_of_dup_in ~newfd:2 tty_in)
               Io_3_2.Nobuf
           };
           Io_3_2.with_stdio_chans thunk
         })
  in
  let pty_out = Io_3_2.out_channel_of_dup_in pty_in in
  do {
    Io_3_2.set_chan_buffering_out pty_out Io_3_2.Nobuf;
    (process, pty_in, pty_out, ttyname)
  }
;
