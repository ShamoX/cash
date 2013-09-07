#!/usr/local/adm/src/caml/bin/cash -r
-I /usr/local/adm/src/cash
!#
open Cash;

(* We see parent after children, so we'll backpatch the parent field. *)
type dir =
  { name : string; subtree_size : int; children : list dir; parent : mutable option dir }
;

value dname n =
  let slash_i = Env_3_11.internal_rindex '/' n (String.length n) in
  if slash_i < 1 then n else String.sub n 0 slash_i
;

value get_children name last =
  loop [last] where rec loop children =
    fun
    [ [last :: roots] when name = dname last.name -> loop [last :: children] roots
    | roots -> (roots, children) ]
;


value load =
  let rex = Pcre.regexp "^\s*(\d+)\s+(\S.*)$"
  and novalues = (-1, "") in
  fun line roots ->
    match
      try
        let substrs = Pcre.extract ~rex ~full_match:False line in
        (int_of_string substrs.(0), substrs.(1))
      with
      [ Not_found ->
          do { prerr_string "Can't understand line:\n"; prerr_endline line; novalues } ]
    with
    [ (-1, _) -> roots
    | (size, name) ->
        match roots with
        [ [last :: roots] when name = dname last.name ->
            let (newroots, children) = get_children name last roots in
            let thisdir =
              {name = name; subtree_size = size; children = children; parent = None}
            in
            do {
              (* Backpatch children. *)
              (let sdir = Some thisdir in
               List.iter (fun child -> child.parent := sdir) children);
              [thisdir :: newroots]
            }
        | _ ->
            (* a new root or a last's brother. *)
            [{name = name; subtree_size = size; children = []; parent = None} ::
             roots] ] ]
;

value print_long total_size size name =
  Printf.printf "\n%3d%%  %6d  %s\n" (100 * size / total_size) size name
;

value print_short total_size subtree_size percent_char size name =
  let sub_percent = size * 100 / subtree_size
  and percent = size * 100 / total_size in
  Printf.printf "    %3d%c %3d%c %6d    %s\n" percent percent_char sub_percent
    percent_char size name
;

value rec do_subtree apply_threshold total_size dir =
  let subtree_size = dir.subtree_size in
  do {
    print_long total_size subtree_size dir.name;
    let dot_size =
      List.fold_left (fun sz child -> sz - child.subtree_size) subtree_size dir.children
    in
    let dot_name = dir.name ^ "/." in
    let dot = {name = dot_name; subtree_size = dot_size; children = []; parent = None} in
    let children =
      List.sort (fun a b -> compare b.subtree_size a.subtree_size)
        (List.filter apply_threshold [dot :: dir.children])
    in
    (* XXX. *)
    let num_components = List.length (split_file_name dir.name) in
    ignore
      (let print_short = print_short total_size subtree_size in
       List.fold_left
         (fun percent_char child ->
            let name = List.nth (split_file_name child.name) num_components in
            do { print_short percent_char child.subtree_size name; ' ' })
         '%' children);
    List.iter (do_subtree apply_threshold total_size) (List.filter (\<> dot) children)
  }
;

value disk_usage_report_formatter threshold threshold_is_percent du_chan =
  (* # Load input, while establishing list of roots. *)
  let root_list = fold_in_channel du_chan read_line load [] in
  (* # Calculate total size. *)
  let total_size = List.fold_left (fun tot root -> tot + root.subtree_size) 0 root_list in
  do {
    (* print headers. *)
    print_string "Tot%  Blocks  Full-pathname\n";
    print_string "    Tot% Sub% Blocks    Child\n";
    let threshold =
      if threshold_is_percent then threshold * total_size / 100 else threshold
    in
    let apply_threshold child = child.subtree_size >= threshold in
    (* Output formatted tree for each root. *)
    List.iter (do_subtree apply_threshold total_size) root_list
  }
;

(* # Prepare file handle. *)
value mkdu use_stdin du_args =
  if use_stdin then do {
    set_chan_buffering_in stdin Block; (* probably a file, anyway. *)
    stdin
  }
  else run_with_in_channel (fun () -> exec_path "du" du_args)
;

value disk_usage_report_formatter_from_sh () =
  let threshold = ref 1
  and threshold_is_percent = ref True
  and use_stdin = ref False
  and du_args = ref [] in
  let spec =
    [("-t",
      Arg.String
        (fun s ->
           let lci = pred (String.length s) in
           do {
             threshold_is_percent.val := s.[lci] = '%';
             threshold.val :=
               int_of_string (if threshold_is_percent.val then String.sub s 0 lci else s)
           }),
      "threshold[%]\n\t Only show directories at or above threshold blocks.\n\t \
       With \"%\", only show directories at or above threshold percent\n\t \
       of the total.  Default is \"1%\"");
     ("-", Arg.Set use_stdin, "\t Read standard input for 'du' listing.")]
  and usage =
    "Usage: durf [-t #[%]] [{du-args|-}]\nLike 'du', but formatted differently."
  in
  do {
    Arg.parse spec (fun arg -> du_args.val := [arg :: du_args.val]) usage;
    disk_usage_report_formatter threshold.val threshold_is_percent.val
      (mkdu use_stdin.val du_args.val)
  }
;

if Sys.interactive.val then() else disk_usage_report_formatter_from_sh ();
