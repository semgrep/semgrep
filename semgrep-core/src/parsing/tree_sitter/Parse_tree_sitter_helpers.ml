(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common
module PI = Parse_info
module G = AST_generic

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A few helpers to help factorize code between the different
 * Parse_xxx_tree_sitter.ml files.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type 'a env = {
  file : Common.filename;
  (* get the charpos (offset) in file given a line x col *)
  conv : (int * int, int) Hashtbl.t;
  extra : 'a;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* mostly a copy-paste of Parse_info.full_charpos_to_pos_large *)
let line_col_to_pos file =
  let chan = open_in_bin file in
  let size = Common2.filesize file + 2 in

  let charpos = ref 0 in
  let line = ref 0 in
  let h = Hashtbl.create size in

  let full_charpos_to_pos_aux () =
    try
      while true do
        let s = input_line chan in
        incr line;

        (* '... +1 do'  cos input_line dont return the trailing \n *)
        for i = 0 to String.length s - 1 + 1 do
          Hashtbl.add h (!line, i) (!charpos + i)
        done;
        charpos := !charpos + String.length s + 1
      done
    with End_of_file ->
      (* bugfix: this is wrong:  Hashtbl.add h (!line, 0) !charpos;
       * because an ident on the last line would get
       * the last charpos.
       *)
      ()
  in
  full_charpos_to_pos_aux ();
  close_in chan;
  h

let token env (tok : Tree_sitter_run.Token.t) =
  let loc, str = tok in
  let h = env.conv in
  let start = loc.Tree_sitter_run.Loc.start in
  (* Parse_info is 1-line based and 0-column based, like Emacs *)
  let line = start.Tree_sitter_run.Loc.row + 1 in
  let column = start.Tree_sitter_run.Loc.column in
  let charpos =
    try Hashtbl.find h (line, column) with Not_found -> -1
    (* TODO? more strict? raise exn? *)
  in
  let file = env.file in
  let tok_loc = { PI.str; charpos; line; column; file } in
  PI.mk_info_of_loc tok_loc

let str env (tok : Tree_sitter_run.Token.t) =
  let _, s = tok in
  (s, token env tok)

(* This is a temporary fix until
 * https://github.com/returntocorp/ocaml-tree-sitter-core/issues/5
 * is fixed.
 *)
let str_if_wrong_content_temporary_fix env (tok : Tree_sitter_run.Token.t) =
  let loc, _wrong_str = tok in

  let file = env.file in
  let h = env.conv in

  let charpos, line, column =
    let pos = loc.Tree_sitter_run.Loc.start in
    (* Parse_info is 1-line based and 0-column based, like Emacs *)
    let line = pos.Tree_sitter_run.Loc.row + 1 in
    let column = pos.Tree_sitter_run.Loc.column in
    try (Hashtbl.find h (line, column), line, column)
    with Not_found ->
      failwith (spf "could not find line:%d x col:%d in %s" line column file)
  in
  let charpos2 =
    let pos = loc.Tree_sitter_run.Loc.end_ in
    (* Parse_info is 1-line based and 0-column based, like Emacs *)
    let line = pos.Tree_sitter_run.Loc.row + 1 in
    let column = pos.Tree_sitter_run.Loc.column in
    try Hashtbl.find h (line, column)
    with Not_found ->
      failwith (spf "could not find line:%d x col:%d in %s" line column file)
  in
  (* Range.t is inclusive, so we need -1 to remove the char at the pos *)
  let charpos2 = charpos2 - 1 in
  let r = { Range.start = charpos; end_ = charpos2 } in
  let str = Range.content_at_range file r in
  let tok_loc = { PI.str; charpos; line; column; file } in
  (str, PI.mk_info_of_loc tok_loc)

let combine_tokens_DEPRECATED env xs =
  match xs with
  | [] -> failwith "combine_tokens: empty list"
  | x :: _xsTODO ->
      let t = token env x in
      t

let debug_sexp_cst_after_error sexp_cst =
  let s = Printexc.get_backtrace () in
  pr2 "Some constructs are not handled yet";
  pr2 "CST was:";
  (* bugfix: do not use CST.dump_tree because it prints on stdout
   * and will mess up our interaction with semgrep python wrapper and
   * also for the parsing_stat CI job.
   *
   * alt: Use Print_sexp.to_stderr of martin
   *)
  pr2 (Sexplib.Sexp.to_string_hum sexp_cst);
  pr2 "Original backtrace:";
  pr2 s

let wrap_parser tree_sitter_parser ast_mapper =
  let res : 'a Tree_sitter_run.Parsing_result.t = tree_sitter_parser () in
  let program =
    match res.program with
    | Some cst ->
        if res.errors <> [] then
          logger#error "Partial errors returned by Tree-sitter parser";
        Some (ast_mapper cst)
    | None -> None
  in
  { res with program }

(* Stuff to put in entry point at the beginning:
   let todo _env _x = failwith "not implemented"

   let todo_any str t any =
     pr2 (AST.show_any any);
     raise (Parse_info.Ast_builder_error (str, t))

   let program =
   ...
   try
     program ...
   with
    (Failure "not implemented") as exn ->
      H.debug_sexp_cst_after_error (CST.sexp_of_program cst);
      raise exn

*)

let parse_number_literal (s, t) =
  match Common2.int_of_string_c_octal_opt s with
  | Some i -> G.Int (Some i, t)
  | None -> (
      match float_of_string_opt s with
      | Some f -> G.Float (Some f, t)
      | None -> G.Int (None, t))
