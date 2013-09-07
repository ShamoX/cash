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

value make_date :
  ?tzn: string -> ?tzs: int -> ?summ: bool -> ?wday: int -> ?yday: int -> int -> int ->
    int -> int -> int -> int -> date;

value time_plus_ticks : unit -> float;
value ticks_per_sec : unit -> float;

type time_zone =
  [ Tz_local
  | Tz_secs of int
  | Tz_name of string ]
;

value time : unit -> float;
value time_of_date : date -> float;

value date : unit -> date;
value date_of_time : ?tz: time_zone -> float -> date;

value string_of_date : date -> string;
value format_date : string -> date -> string;
