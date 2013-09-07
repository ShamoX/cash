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

value is_letter : char -> bool;
value is_lower_case : char -> bool;
value is_upper_case : char -> bool;
value is_title_case : char -> bool;
value is_digit : char -> bool;
value is_letter_or_digit : char -> bool;
value is_graphic : char -> bool;
value is_printing : char -> bool;
value is_whitespace : char -> bool;
value is_blank : char -> bool;
value is_iso_control : char -> bool;
value is_punctuation : char -> bool;
value is_hex_digit : char -> bool;
value is_ascii : char -> bool;

value is_alphabetic : char -> bool;
value is_alphanumeric : char -> bool;
value is_numeric : char -> bool;
