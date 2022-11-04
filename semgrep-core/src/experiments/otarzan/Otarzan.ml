(* Yoann Padioleau
 *
 * Copyright (C) 2022 r2c
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
open Common
open Ast_ml

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This is a "port" of ocamltarzan[1] to use tree-sitter-ocaml and ast_ml.ml
 * instead of camlp4 to generate OCaml boilerplate code.
 *
 * See the README[2] of ocamltarzan for some old motivations for this
 * kind of metaprogramming technique. Nowaday, you should use
 * ppx deriving[3] or ppx visitors[4] to generate automatically boilerplate
 * code, but there is also a need and place for tools that generate
 * boilerplate that a human can start from and would like to further edit
 * (e.g., for AST_to_IL.ml in semgrep, for AST_to_SAST.ml in deep-semgrep).
 * Otarzan is such a tool.
 *
 * See https://github.com/returntocorp/semgrep/issues/6440 for more
 * context on this work.
 *
 * alternatives:
 *  - use ocamltarzan? but ocamltarzan is a bit difficult to install and
 *    setup (it requires a specific version of OCaml and camlp4). Moreover,
 *    it requires some special comments in the code next to the
 *    type definitions  '(* tarzan *)' (it was written before ppx attributes
 *    existed) so this makes the whole process quite hacky.
 *  - port ocamltarzan to use ppx? ppx is great for generating automatically
 *    some boilerplate code, but this generated code is not easily
 *    accessible and it's not meant to be further edited (it could not
 *    be used for example to help writing AST_to_IL.ml).
 *  - use ATD? This would require to transform our OCaml type definitions
 *    to ATD.
 *  - use compiler-libs? the OCaml AST in compiler-libs is really complex,
 *    and it changes frequently, so using ast_ml.ml seems simpler.
 *
 * Thanks to our work on semgrep, we actually have now easy access to an
 * OCaml parser and a pretty simple AST, so we can just simply write code
 * that traverses the OCaml AST and output code from it.
 *
 * TODO:
 *  - a generate_boilerplate_generic_vs_generic
 *
 * TODO?
 *  - We could define an intermediate AST just for the type definitions,
 *    with just the constructs we handle (e.g., tuples, list, variants).
 *    At the same time we might want to do more complex metaprogramming
 *    so maybe simpler to give access to the full AST.
 *  - we could have based the generator on traversing the generic AST instead
 *    of ast_ml.ml, which would open the way for metaprogramming not only
 *    for generating OCaml boilerplate code but also for the other languages.
 *    We could even generate code in a typed way by not generating strings
 *    but generating code as AST_generic elements (a bit tedious though).
 *    Then we could rely on a general pretty-printer to output the code.
 *
 * TODO LATER:
 *  - at some point we may want to release otarzan as a separate tool. This
 *    would require to make OPAM packages for tree-sitter-ocaml and
 *    Parse_ocaml_tree_sitter.ml
 *
 * references:
 *  [1] https://github.com/aryx/ocamltarzan
 *  [2] https://github.com/aryx/ocamltarzan/blob/master/readme.txt
 *  [3] https://github.com/ocaml-ppx/ppx_deriving
 *  [4] https://gitlab.inria.fr/fpottier/visitors
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let parse file =
  (* using the tree-sitter OCaml parser, better than the one in pfff *)
  let res = Parse_ocaml_tree_sitter.parse file in
  match res.program with
  | None -> failwith (spf "no AST for %s" file)
  | Some ast -> ast

(* It returns a list list because it's a list of possibly mutually
 * recursive type definitions. See ast_ml.ml
 *)
let extract_toplevel_typedefs program : type_declaration list list =
  program
  |> Common.map_filter (function
       (* less: we could look at iattrs and restrict the boilerplate
        * to type decls with certain attributes (e.g., [@@otarzan]) like
        * for deriving
        *)
       | { i = Type (_t, decls); iattrs = _ } -> Some decls
       | _else_ -> None)

let visit_mutually_recursive_type_defs (f : type_declaration list -> string)
    file : string =
  let ast = parse file in
  let types = extract_toplevel_typedefs ast in
  types |> Common.map (fun mutuals -> f mutuals) |> String.concat "\n"

(* TODO: call ocamlformat on the generated code *)
let print_stdout_after_ocamlformat s = pr s

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* port of https://github.com/aryx/ocamltarzan/blob/master/pa/pa_map_todo.ml *)
let generate_boilerplate_map_todo file =
  let body =
    file |> visit_mutually_recursive_type_defs (fun _mutuals -> "TODO")
  in
  let final_str = body in
  print_stdout_after_ocamlformat final_str
