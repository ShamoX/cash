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
#load "q_MLast.cmo";

value parse_quot q =
  let lbuf = Lexing.from_string q in
  let rec loop r =
    match Sexp.parse lbuf with
    [ Some {Sexp.sexp = Sexp.Eof_object} -> List.rev r
    | Some sexp -> loop [sexp :: r]
    | None -> assert False ]
  in
  loop []
;

open Sexp;

type located 'a = { floc : Schlex.loc; form : 'a };
(* floc: read `form location'. *)

type fd = located int;
type fn = located string;
type obj = located string;

type unlocated_fd_or_chan = [ Fd of int | Chan of string ];
type fd_or_chan = located unlocated_fd_or_chan;

type unlocated_redir =
  [ Read of (fd * fn)
  | Trunc of (fd * fn)
  | Object of (fd * obj)
  | Append of (fd * fn)
  | Dup2 of (fd * fd_or_chan)
  | Close of fd_or_chan
  | Stdchans ]
;
type redir = located unlocated_redir;

value noloc : Schlex.loc = (0, 0);

(* Syntaxe bizarre: le ` s'applique aux 2 pattes du pattern... *)
value parse_fname =
  parser
    [: `({sexp = Symbol s} | {sexp = String s} as tok) :] ->
      ({floc = tok.loc; form = s} : fn)
;

value parse_fdes =
  parser [: `{loc = l; sexp = Exact fd} :] -> ({floc = l; form = fd} : fd)
;

value parse_fdes_or_port =
  parser
  [ [: `{loc = l; sexp = Exact fd} :] -> {floc = l; form = Fd fd}
  | [: `({sexp = Symbol s} | {sexp = String s} as tok) :] ->
      ({floc = tok.loc; form = Chan s} : fd_or_chan) ]
;

value parse_redir1 =
  parser
  [ [: `{loc = l; sexp = Symbol "<"}; fn = parse_fname :] ->
      ({floc = (fst l, snd fn.floc); form = Read ({floc = noloc; form = 0}, fn)} : redir)
  | [: `{loc = l; sexp = Symbol ">"}; fn = parse_fname :] ->
      {floc = (fst l, snd fn.floc); form = Trunc ({floc = noloc; form = 1}, fn)}
  | [: `{loc = l; sexp = Symbol "<<"}; fn = parse_fname :] ->
      {floc = (fst l, snd fn.floc); form = Object ({floc = noloc; form = 0}, fn)}
  | [: `({sexp = Symbol ">>"} | {sexp = Symbol ">+"} as tok); fn = parse_fname :] ->
      {floc = (fst tok.loc, snd fn.floc); form = Append ({floc = noloc; form = 1}, fn)}
  | [: `{loc = l; sexp = Symbol "-"}; fd = parse_fdes_or_port :] ->
      {floc = (fst l, snd fd.floc); form = Close fd} ]
;

value parse_redir =
  parser
  [ [: `{loc = l; sexp = Symbol "<"}; fd = parse_fdes; fn = parse_fname :] ->
      ({floc = (fst l, snd fn.floc); form = Read (fd, fn)} : redir)
  | [: `{loc = l; sexp = Symbol ">"}; fd = parse_fdes; fn = parse_fname :] ->
      {floc = (fst l, snd fn.floc); form = Trunc (fd, fn)}
  | [: `{loc = l; sexp = Symbol "<<"}; fd = parse_fdes; fn = parse_fname :] ->
      {floc = (fst l, snd fn.floc); form = Object (fd, fn)}
  | [: `({sexp = Symbol ">>"} | {sexp = Symbol ">+"} as tok); fd = parse_fdes;
       fn = parse_fname :] ->
      {floc = (fst tok.loc, snd fn.floc); form = Append (fd, fn)}
  | [: `{loc = l; sexp = Symbol "="}; fd = parse_fdes; fdp = parse_fdes_or_port :] ->
      {floc = (fst l, snd fdp.floc); form = Dup2 (fd, fdp)}
  | [: `{loc = l} :] -> Stdpp.raise_with_loc l (Stream.Error "bad redirection type")
  | [: :] -> raise (Stream.Error "empty redirection") ]
;

value parse_endlist stream =
  match Stream.peek stream with
  [ None -> ()
  | _ -> raise (Stream.Error "nothing should be left here") ]
;

value parse_check_eol parseur stream =
  let red = parseur stream in do { parse_endlist stream; red }
;

(* Supposedly e(nd_of_loc) is just after the close parenthesis. *)
value parse_list_with_loc list parseur lloc =
  let s = Stream.of_list list in
  try parse_check_eol parseur s with exn ->
    let loc =
      match Stream.peek s with
      [ Some {loc = loc} -> loc
      | None -> let e = snd lloc in (pred e, e) ]
    in
    Stdpp.raise_with_loc loc exn
;

value rec parse_redirs redirs =
  parser
  [ [: `{sexp = List r; loc = loc}; s :] ->
      let parseur =
        match r with
        [ [_; _] -> parse_redir1
        | _ -> parse_redir ]
      in
      parse_redirs [parse_list_with_loc r parseur loc :: redirs] s
  | [: `{sexp = Symbol "stdchans"}; s :] ->
      parse_redirs [{floc = noloc; form = Stdchans} :: redirs] s
  | [: `{loc = loc} :] -> Stdpp.raise_with_loc loc (Stream.Error "redirection expected")
  | [: :] ->
      match redirs with
      [ [] -> {floc = noloc; form = []}
      | [r1 :: _] ->
          let reds = List.rev redirs in
          {floc = (fst (List.hd redirs).floc, snd r1.floc); form = reds} ] ]
;

value redir_ast _ = failwith "redirections not implemented";

type connection = located (list fd);

value parse_connection =
  let rec parse_fd_list fds =
    parser
    [ [: fd = parse_fdes; s :] -> parse_fd_list [fd :: fds] s
    | [: :] ->
        match fds with
        [ [] -> raise (Stream.Error "empty connection")
        | [fd_to :: froms] ->
            let loc =
              match froms with
              [ [] -> fd_to.floc
              | [from1 :: _] -> (fst fd_to.floc, snd from1.floc) ]
            in
            ({floc = loc; form = [fd_to :: List.rev froms]} : connection) ] ]
  in
  parser
    [: `{sexp = List fds; loc = loc} :] -> parse_list_with_loc fds (parse_fd_list []) loc
;

value rec parse_connections connections =
  parser
  [ [: conn = parse_connection; s :] -> parse_connections [conn :: connections] s
  | [: :] ->
      match connections with
      [ [] -> {floc = noloc; form = []}
      | [c1 :: _] ->
          let conns = List.rev connections in
          {floc = (fst (List.hd conns).floc, snd c1.floc); form = conns} ] ]
;

value parse_connection_list =
  parser
    [: `{sexp = List cl; loc = loc} :] ->
      parse_list_with_loc cl (parse_connections []) loc
;

type extended_process_form = { pf : process_form; redirs : located (list redir) }
and complex_pipe =
  { conns : located (list connection); pfs : located (list process_form) }
and unlocated_process_form =
  [ Caml
  | Pipe of list process_form
  | Xpipe of complex_pipe
  | Epf of extended_process_form
  | Program of located (list (located string)) ]
and process_form = located unlocated_process_form
;

value loc_of_caml_code stream =
  let rec last_loc elt =
    if Stream.peek stream <> None then last_loc (Stream.next stream) else snd elt.loc
  in
  match Stream.peek stream with
  [ None -> ((0, 0) : Schlex.loc)
  | Some {loc = (bloc, _)} -> (bloc, last_loc (Stream.next stream)) ]
;

exception Unstringifiable of sexp loc_sexp;

value parse_shell_arg =
  parser
  [ [: `{sexp = Symbol s; loc = loc} | {sexp = String s; loc = loc} :] ->
      {floc = loc; form = s}
  | [: `{sexp = Exact i; loc = loc} :] -> {floc = loc; form = string_of_int i}
  | [: `{sexp = truc; loc = loc} :] ->
      Stdpp.raise_with_loc loc (Unstringifiable truc) ]
;

value rec parse_non_empty_list parse_elt elts =
  parser
  [ [: elt = parse_elt; s :] -> parse_non_empty_list parse_elt [elt :: elts] s
  | [: :] ->
      do {
        assert (elts <> []);
        let result = List.rev elts in
        {floc = (fst (List.hd result).floc, snd (List.hd elts).floc); form = result}
      } ]
;

value rec parse_pf =
  parser [: `{sexp = List pf; loc = loc} :] -> parse_list_with_loc pf parse_inner_pf loc
and parse_pfs pfs = parser [: pf = parse_pf; s :] -> parse_non_empty_list parse_pf [pf] s
and parse_inner_pf =
  parser
  [ [: `{sexp = Symbol "begin"} | {sexp = Symbol "caml"}; s :] ->
      {floc = loc_of_caml_code s; form = Caml}
  | [: `{sexp = Symbol "|"}; pf1 = parse_pf; s :] ->
      let pfs = parse_pfs [pf1] s in
      {(pfs) with form = Pipe pfs.form}
  | [: `{sexp = Symbol "|+"}; cl = parse_connection_list; pf1 = parse_pf; s :] ->
      let pfs = parse_pfs [pf1] s in
      {floc = (fst cl.floc, snd pfs.floc); form = Xpipe {conns = cl; pfs = pfs}}
  | [: `{sexp = Symbol "epf"}; epf = parse_epf :] -> {(epf) with form = Epf epf.form}
  | [: command = parse_shell_arg; s :] ->
      let prog = parse_non_empty_list parse_shell_arg [command] s in
      {(prog) with form = Program prog} ]
and parse_epf =
  parser
    [: pf = parse_pf; r = parse_redirs [] :] ->
      {floc = pf.floc; form = {pf = pf; redirs = r}}
;

value pf_loc_ast q (pf : process_form) =
  match pf.form with
  [ Caml ->
      let (loc, s) =
        match pf.floc with
        [ (0, 0) ->
            let i = Env_3_11.last_index q in
            ((i, i), "")
        | (b, e) -> ((b, e), Env_3_11.substring q b e) ]
      in
      try
        {floc = loc; form = Grammar.Entry.parse Pcaml.expr_eoi (Stream.of_string s)}
      with
      [ Stdpp.Exc_located (bp, ep) exc ->
          let beg = fst loc in
          raise (Stdpp.Exc_located (beg + bp, beg + ep) exc) ]
  | Pipe pfs -> assert False
  | _ -> assert False ]
;

value epf_ast q epf =
  let pfa = (pf_loc_ast q epf.form.pf).form in
  let redsa = List.map redir_ast epf.form.redirs.form in
  let loc = epf.floc in <:expr< do { $list:(redsa @ [pfa])$ }>>
;

value bg_ast q epf =
  let epfa = epf_ast q epf
  and loc = epf.floc in
  <:expr< fork (fun () -> $ epfa $)>>
;

value run_ast q epf =
  let bga = bg_ast q epf
  and loc = epf.floc in
  <:expr< wait ($ bga $)>>
;

value parse_syntactic_form =
  parser
  [ [: `{sexp = Symbol "run"}; epf = parse_epf :] -> (run_ast, epf)
  | [: `{sexp = Symbol "&"}; epf = parse_epf :] -> (bg_ast, epf)
  | [: `{sexp = Symbol "exec-epf"}; epf = parse_epf :] -> (epf_ast, epf)
  | [: `{sexp = sexp; loc = loc} :] ->
      Stdpp.raise_with_loc loc (Stream.Error "Unknown quotation beginning") ]
;

value expander in_expr quote =
  if not in_expr then assert False
  else do {
    let ast = parse_quot quote in
    Sexp.displayl (Sexp.List ast);
    print_newline ();
    let (expand_this_epf, epf) = parse_syntactic_form (Stream.of_list ast) in
    ignore "expand_this_epf quote epf";
    epf
  }
;
