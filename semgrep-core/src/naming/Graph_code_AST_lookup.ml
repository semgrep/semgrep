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
open Graph_code_AST_env
module G = Graph_code
module E = Entity_code
module H = Graph_code_AST_helpers
module T = Type_AST
module AST = AST_generic

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Lookup part of Graph_code_AST.ml
 *
 * alt:
 *  - the complex (but very general) stack lookup in stackgraph of github
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let lookup_dotted_ident_opt (env : env) (xs : AST.dotted_ident) : G.node option
    =
  logger#info "looking up: %s" (xs |> List.map fst |> String.concat ".");
  let g = env.g in
  let rec aux current xs =
    match xs with
    | [] -> Some current
    | (str, _tk) :: xs -> (
        let children = G.children current g in
        let candidates =
          children
          |> List.filter (fun (s2, kind) ->
                 let xs =
                   match kind with
                   | E.Dir -> H.dotted_ident_of_dir s2
                   | _ -> H.dotted_ident_of_entname s2
                 in
                 H.last_ident_of_dotted_ident xs = str)
        in
        match candidates with
        | [] ->
            logger#error "no candidate found for %s at node %s, children = [%s]"
              str (G.string_of_node current)
              (children |> List.map G.string_of_node |> String.concat ",");
            None
        | [ y ] -> aux y xs
        | ys ->
            logger#error "too many candidates for %s, list = [%s]" str
              (ys |> List.map G.string_of_node |> String.concat ",");
            None)
  in
  aux G.root xs
  [@@profile]

(* TODO: actually in some language (e.g., Python, Haskell) you can use
 * definitions defined later in the file, in other (e.g., OCaml, Rust?)
 * you can't and you can even define multiple times the same name, and
 * in other (e.g., C/C++) it depends whether it was "declared" via
 * a prototype before.
 *)
let lookup_local_file_opt (env : env) (id : AST.ident) : G.node option =
  lookup_dotted_ident_opt env (env.file_qualifier @ [ id ])
  [@@profile]

let lookup_type_of_node_opt env node =
  logger#info "lookup type for node %s" (G.string_of_node node);
  Hashtbl.find_opt env.types node

let lookup_type_of_dotted_ident_opt env xs =
  logger#info "looking up type for: %s" (xs |> List.map fst |> String.concat ".");
  let* n = lookup_dotted_ident_opt env xs in
  let* t = lookup_type_of_node_opt env n in
  logger#info "found type for %s = %s" (G.string_of_node n) (T.show t);
  Some t
