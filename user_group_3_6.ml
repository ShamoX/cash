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

(* User and group databases access. *)

type user_info =
  { ui_name : string;
    ui_uid : int;
    ui_gid : int;
    ui_home_dir : string;
    ui_shell : string }
;

value (user_info, user_info_name) =
  let make p =
    {ui_name = p.Unix.pw_name; ui_uid = p.Unix.pw_uid; ui_gid = p.Unix.pw_gid;
     ui_home_dir = p.Unix.pw_dir; ui_shell = p.Unix.pw_shell}
  in
  (fun uid -> make (Unix.getpwuid uid), fun name -> make (Unix.getpwnam name))
;

value username_to_uid name = (user_info_name name).ui_uid;

value uid_to_username uid = (user_info uid).ui_name;

type group_info = { gi_name : string; gi_gid : int; gi_members : list string };

value (group_info, group_info_name) =
  let make g =
    {gi_name = g.Unix.gr_name; gi_gid = g.Unix.gr_gid;
     gi_members = Array.to_list g.Unix.gr_mem}
  in
  (fun gid -> make (Unix.getgrgid gid), fun name -> make (Unix.getgrnam name))
;

value groupname_to_gid name = (group_info_name name).gi_gid;

value gid_to_groupname gid = (group_info gid).gi_name;
