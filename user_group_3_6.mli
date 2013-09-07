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

type user_info =
  { ui_name : string;
    ui_uid : int;
    ui_gid : int;
    ui_home_dir : string;
    ui_shell : string }
;

value user_info : int -> user_info;
value user_info_name : string -> user_info;

value username_to_uid : string -> int;
value uid_to_username : int -> string;

type group_info = { gi_name : string; gi_gid : int; gi_members : list string };
 
value group_info : int -> group_info;
value group_info_name : string -> group_info;

value groupname_to_gid : string -> int;
value gid_to_groupname : int -> string;
