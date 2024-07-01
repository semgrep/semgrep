(* Yoann Padioleau
 *
 * Copyright (C) 2022 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Fpath_.Operators
open Printf

(*
   Parse OCaml code, extract, and simplify type definitions.
*)

let parse file =
  (* using the tree-sitter OCaml parser, better than the one in pfff *)
  let res = Parse_ocaml_tree_sitter.parse file in
  match res.program with
  | None -> failwith (sprintf "no AST for %s" !!file)
  | Some ast -> ast

(* It returns a list list because it's a list of possibly mutually
 * recursive type definitions. See ast_ml.ml
 *)
let extract_toplevel_typedefs program : AST_ocaml.type_declaration list list =
  program
  |> List_.filter_map (function
       (* less: we could look at iattrs and restrict the boilerplate
        * to type decls with certain attributes (e.g., [@@otarzan]) like
        * for deriving
        *)
       | { AST_ocaml.i = Type (_t, decls); iattrs = _ } -> Some decls
       | _else_ -> None)

let extract_typedefs_from_ml_file file =
  let ast = parse file in
  let typedef_groups = extract_toplevel_typedefs ast in
  typedef_groups
