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

value is_letter = Charset_14.contains Charset_14.letter;
value is_lower_case = Charset_14.contains Charset_14.lower_case;
value is_upper_case = Charset_14.contains Charset_14.upper_case;
value is_title_case = Charset_14.contains Charset_14.title_case;
value is_digit = Charset_14.contains Charset_14.digit;
value is_letter_or_digit = Charset_14.contains Charset_14.letter_plus_digit;
value is_graphic = Charset_14.contains Charset_14.graphic;
value is_printing = Charset_14.contains Charset_14.printing;
value is_whitespace = Charset_14.contains Charset_14.whitespace;
value is_blank = Charset_14.contains Charset_14.blank;
value is_iso_control = Charset_14.contains Charset_14.iso_control;
value is_punctuation = Charset_14.contains Charset_14.punctuation;
value is_hex_digit = Charset_14.contains Charset_14.hex_digit;
value is_ascii = Charset_14.contains Charset_14.ascii;

value is_alphabetic = is_letter_or_digit;
value is_alphanumeric = is_letter_or_digit;
value is_numeric = is_digit;
