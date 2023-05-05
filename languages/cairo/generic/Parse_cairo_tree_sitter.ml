(* Romain J
 *
 * Copyright (c) 2023 Semgrep Inc.
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
(* open Common *)
module CST = Tree_sitter_cairo.CST

(* open AST_generic *)
module G = AST_generic
module H = Parse_tree_sitter_helpers
module H2 = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Cairo parser using tree-sitter-lang/semgrep-cairo and converting
 * (mostly directly) to AST_generic.ml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type _env = unit H.env

(*
let token = H.token
let str = H.str
 *)

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-cairo/Boilerplate.ml *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_cairo.Parse.file file)
    (fun _cst ->
      let _env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      failwith "TODO")

let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_cairo.Parse.string str)
    (fun _cst ->
      let file = "<pattern>" in
      let _env = { H.file; conv = Hashtbl.create 0; extra = () } in
      failwith "TODO")
