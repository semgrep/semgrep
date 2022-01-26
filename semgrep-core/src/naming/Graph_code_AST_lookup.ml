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
module H = Graph_code_AST_helpers

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
(* API *)
(*****************************************************************************)

let lookup_dotted_ident_opt (env : env) (xs : AST_generic.dotted_ident) =
  let g = env.g in
  let rec aux current xs =
    match xs with
    | [] -> Some current
    | (str, _tk) :: xs -> (
        let children = G.children current g in
        let candidates =
          children
          |> List.filter (fun (s2, _kind) ->
                 let xs = H.dotted_ident_of_str s2 in
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
let lookup_local_file_opt env id =
  lookup_dotted_ident_opt env (env.file_qualifier @ [ id ])
  [@@profile]
