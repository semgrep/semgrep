(* Yoann Padioleau
 *
 * Copyright (C) 2022 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Parsing Jsonnet.
 *
 * See https://jsonnet.org/ref/spec.html#lexing
 * and https://jsonnet.org/ref/spec.html#abstract_syntax
 *
 * This should really be in ../parsing/ but that would lead to circular
 * dependencies because we would use Parse_jsonnet_tree_sitter.ml which
 * itself use AST_jsonnet.ml.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* copy pasted from Parse_target.ml to avoid circular dependencies
 * between ojsonnet and semgrep_parsing
 *)
let loc_of_tree_sitter_error (err : Tree_sitter_run.Tree_sitter_error.t) =
  let start = err.start_pos in
  {
    Tok.str = err.substring;
    pos =
      {
        charpos = 0;
        (* fake *)
        line = start.row + 1;
        column = start.column;
        file = err.file.name;
      };
  }

let exn_of_loc loc =
  let info = Tok.OriginTok loc in
  Parsing_error.Syntax_error info |> Exception.trace

let error_of_tree_sitter_error (err : Tree_sitter_run.Tree_sitter_error.t) =
  let loc = loc_of_tree_sitter_error err in
  exn_of_loc loc

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_program (file : Fpath.t) : AST_jsonnet.program =
  let res = Parse_jsonnet_tree_sitter.parse file in
  (* similar to Parse_target.run_parser and the TreeSitter case *)
  match (res.program, res.errors) with
  | Some ast, [] -> ast
  | None, [] ->
      failwith "internal error: failed to recover typed tree from treesitter"
  (* we don't care about partial parsing error; we need all the code for
   * ojsonnet; we can't be satisfied by partial code.
   *)
  | (Some _ | None), x :: _xs ->
      let e = error_of_tree_sitter_error x in
      Exception.reraise e
