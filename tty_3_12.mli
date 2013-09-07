type fd = int;

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

value ttyout  : tty_out;

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

value set_tty_info_now_fd : Io_3_2.fd -> tty_info -> unit;
value set_tty_info_now_in : in_channel -> tty_info -> unit;
value set_tty_info_now_out : out_channel -> tty_info -> unit;
value set_tty_info_now_fn : string -> tty_info -> unit;

value set_tty_info_drain_fd : Io_3_2.fd -> tty_info -> unit;
value set_tty_info_drain_in : in_channel -> tty_info -> unit;
value set_tty_info_drain_out : out_channel -> tty_info -> unit;
value set_tty_info_drain_fn : string -> tty_info -> unit;

value set_tty_info_flush_fd : Io_3_2.fd -> tty_info -> unit;
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

value open_control_tty_in : ?flags: (list Io_3_2.open_flag) -> string -> in_channel;
value open_control_tty_out : ?flags: (list Io_3_2.open_flag) -> string -> out_channel;

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
