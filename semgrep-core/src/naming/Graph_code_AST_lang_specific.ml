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
module G = Graph_code
module E = Entity_code
module PI = Parse_info
module H = Graph_code_AST_helpers
open AST_generic

(* open Graph_code_AST_env *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Every Lang-specific code should be in this file.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Lang-specific helpers *)
(*****************************************************************************)

let top_parent_and_qualifier ~lang ~readable ~ast :
    G.node * AST_generic.dotted_ident =
  match lang with
  (* in Python, the directory and filename implicitely defines a package *)
  | Lang.Python ->
      let tk = PI.first_loc_of_file readable |> PI.mk_info_of_loc in
      (* ex: from a/b/foo.py, we want to return
       *   - the parent node (a/b/foo, E.File) (without .py extension)
       *   - the qualifiers [a;b;foo]
       *)
      let d, b, e = Common2.dbe_of_filename readable in
      (* coupling: ugly hack for pycheck; we should move out in a hook *)
      let b =
        match (b, e) with
        | s, "pyi" when s =~ "^\\(.*\\)_$" -> Common.matched1 s
        | _ -> b
      in
      let dotted_idents =
        Common.split "/" d @ [ b ] |> List.map (fun s -> (s, tk))
      in
      (* basically replacing "/" with "." *)
      let str = H.str_of_dotted_ident dotted_idents in
      let node = (str, E.File) in
      (node, dotted_idents)
  (* in Java, packages are explicit construct *)
  | Lang.Java -> (
      match ast with
      | {
          s = DirectiveStmt { d = Package (_tk, dotted_idents); d_attrs = _ };
          _;
        }
        :: _ ->
          let str = H.str_of_dotted_ident dotted_idents in
          let node = (str, E.Package) in
          (node, dotted_idents)
      (* can this happen in practice? for test files maybe? *)
      (* for scripts, tests, or entry points? *)
      | _ ->
          let node = (readable, E.File) in
          (node, []))
  (* TODO: in OCaml, it's a flat namespace by default; the path of
   * the module does not matter (when '(wrapped false)' is defined
   * in the dune file), but the filename defines the module name
   * (with some capitalization done automatically if needed)
   *)
  | l -> failwith (spf "language not handled yet: %s" (Lang.to_string l))
