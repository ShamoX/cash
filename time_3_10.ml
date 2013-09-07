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
(* 
;; A TIME is an instant in the history of the universe; it is location
;; independent, barring relativistic effects. It is measured as the number of
;; seconds elapsed since "epoch" -- January 1, 1970 UTC.

;; A DATE is a *local* name for an instant in time -- which instant it names
;; depends on your time zone (February 23, 1994 4:37 pm happens at different
;; moments in Boston and Hong Kong).
*)
type date =
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

value make_date ?tzn ?tzs ?summ ?(wday = 0) ?(yday = 0) s mi h md mo y =
  {seconds = s; minute = mi; hour = h; month_day = md; month = mo; year = y;
   tz_name = tzn; tz_secs = tzs; is_summer = summ; week_day = wday; year_day = yday}
;

value time_plus_ticks = Unix.gettimeofday;
value ticks_per_sec () = 1E6;  (* Hardcoded in gettimeofday.c. *)

type time_zone =
  [ Tz_local
  | Tz_secs of int
  | Tz_name of string ]
;

external tzset : unit -> unit = "cash_tzset";

(* /* NetBSD, SunOS POSIX-noncompliance requires this. */
   `this' is tzset-ing before mktime - I'm quite paranoid here:
   - I do it for localtime too (but not gmtime ?)
   - I (re)tzset after resetting the environment
 *)
value with_tz tzn thunk =
  (* We have to tzset () inside with_total_env... *)
  let r = Env_3_11.with_total_env [("TZ", tzn)] (fun () -> do { tzset (); thunk () }) in
  (* ...and then reset it outside. *)
  let () = tzset () in
  r
;

value make_summer =
  fun
  [ None -> -1
  | Some False -> 0
  | Some True -> 1 ]
;

value time = Unix.time;

external mktime : date -> int -> float = "cash_mktime";

value time_of_date date =
  match date.tz_secs with
  [ Some tzs ->
      (* tzs is Offset from GMT in seconds. *)
      with_tz "UTC0"
        (fun () ->
           (* /* FreeBSD, at least, needs this (0 isdst) or it sulks. */. *)
           let gm_time = mktime date 0 in
           gm_time -. float tzs)
  | None ->
      let make () = mktime date (make_summer date.is_summer) in
      match date.tz_name with
      [ Some tzn -> with_tz tzn make
      | None -> do { tzset (); make () } ] ]
;

(*
;; NAME is a simple time-zone name such as "EST" or "UTC". You get them back
;; from the Unix time functions as the values of the char *tzname[2]
;; standard/dst vector. The problem is that these time are ambiguous.  This
;; function makes them unambiguous by tacking on the UTC offset in Posix format,
;; such as "EST+5". You need to do this for two reasons:
;; 1. Simple time-zone strings are not recognised at all sites.  For example,
;;    HP-UX doesn't understand "EST", but does understand "EST+5"
;; 2. Time zones represented as UTC offsets (e.g., "UTC+5") are returned back
;;    from the Unix time software as just "UTC", which in the example just given
;;    is 5 hours off. Try setting TZ=UTC+5 and running the date(1) program. It
;;    will give you EST time, but print the time zone as "UTC".  Oops.
*)
value format_time_zone name offset =
  if offset = 0 then name
  else
    let (sign, offset) = if offset < 0 then ('+', - offset) else ('-', offset) in
    let offset = offset mod 86400 in
    let h = offset / 3600
    and m = offset mod 3600 / 60
    and s = offset mod 60 in
    if s = 0 then
      if m = 0 then Printf.sprintf "%s%c%d" name sign h
      else Printf.sprintf "%s%c%02d:%02d" name sign h m
    else Printf.sprintf "%s%c%02d:%02d:%02d" name sign h m s
;

(* I assume C's extern tzname[] and timezone. *)
external get_tzname : bool -> string = "cash_tzname";

(* SUS V2 says that tzset sets timezone only as an extension, so Linux (at
   least) doesn't implement it -- I mean: timezone exists, its value is bad.
external get_timezone : unit -> int = "cash_timezone";
  Do it the hard way: substract time from gm_time. *)
value get_timezone localdate time =
  with_tz "UTC0"
    (fun () ->
       let (gm_time, _) = Unix.mktime {(localdate) with Unix.tm_isdst = False} in
       int_of_float (gm_time -. time))
;

value date_of_time =
  let date_of_tm tm tzn tzs =
    {seconds = tm.Unix.tm_sec; minute = tm.Unix.tm_min; hour = tm.Unix.tm_hour;
     month_day = tm.Unix.tm_mday; month = tm.Unix.tm_mon; year = tm.Unix.tm_year;
     tz_name = Some (format_time_zone (Arg_3_7.default_value tzn "UTC") tzs);
     tz_secs = Some tzs; is_summer = Some tm.Unix.tm_isdst; week_day = tm.Unix.tm_wday;
     year_day = tm.Unix.tm_yday}
  in
  fun ?(tz = Tz_local) time ->
    match tz with
    [ Tz_secs secs -> date_of_tm (Unix.gmtime (time +. float secs)) None secs
    | _ ->
        let localdate_tzname () =
          let localdate = Unix.localtime time in
          (* tzname isn't meaningful before calling localtime. *)
          (localdate, get_tzname localdate.Unix.tm_isdst)
        in
        let (localdate, tzname) =
          match tz with
          [ Tz_name tzn -> with_tz tzn localdate_tzname
          | Tz_local -> localdate_tzname ()
          | _ -> assert False ]
        in
        let timezone = get_timezone localdate time in
        date_of_tm localdate (Some tzname) timezone ]
;

value date () = date_of_time (time ());

(*
;; If time-zone is an integer, convert to a Posix-format string of the form:
;;     UTC+hh:mm:ss
*)
value deintegerize_time_zone = format_time_zone "UTC";

external strftime : int -> string -> date -> int -> string = "cash_strftime";

(* It's disgusting how long and tortuous this function is, just to interface to
** the strftime() function. -Olin
-- we approximate more grossly the strftime buffer's length, so are a little
   shorter than Olin's C code

* There's a weird screw case this code is careful to handle. Exhibiting
* classic Unix design (we use the term loosely), strftime()'s error
* return (0) is also a legal return value for some boundary cases.
* For example, if the format string is empty, or it is "%Z" and
* the time-zone is not available, then the result string is 0 chars long.
* We distinguish this case by suffixing an "x" to the format string,
* and flushing the last char in the formatted result.

-- the remark below doesn't concern us, so..
* Don't consider *prefixing* an "x" instead, because then you'd
* probably pass back &result[1] to skip the x, and that would lose --
* the guy we are handing the string to will later pass it to free(),
* so we can't pass back a pointer to anything other than the very front
* of the block.

* Professional programmers sacrifice their pride that others may live.
* Why me? Why Unix?
*)

value format_date fmt =
  let percent_count =
    (* start at 1. *)
    pc_loop 1 (String.length fmt) where rec pc_loop r i =
      let nexti = Env_3_11.internal_index '%' fmt i in
      if nexti < 0 then r else pc_loop (succ r) nexti
  in
  let fmt = "x" ^ fmt in
  fun date ->
    let date =
      match (date.tz_name, date.tz_secs) with
      [ (None, Some tzs) -> {(date) with tz_name = Some (deintegerize_time_zone tzs)}
      | _ -> date ]
    in
    let rec loop len =
      let buflen = len + 5 * percent_count in
      let res = strftime buflen fmt date (make_summer date.is_summer) in
      if res = "" then loop buflen else String.sub res 1 (Env_3_11.last_index res) 
    in
    loop (String.length fmt)
;

value string_of_date = format_date "%a %b %d %H:%M:%S %Y";
