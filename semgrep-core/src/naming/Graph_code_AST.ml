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
module E = Entity_code
module H = Graph_code_AST_helpers
module L = Graph_code_AST_lookup
module Lang_specific = Graph_code_AST_lang_specific

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Graph of dependencies for the generic AST.
 * See graph_code.ml, AST_generic.ml, and main_codegraph.ml for more
 * information.
 *
 * related work:
 *  - stackgraph (and scope graph) by github
 *  - LSP server
 *  - Datalog?
 *
 * TODO:
 *  - integrate graph_code_java.ml
 *  - integrate graph_code_cmt.ml
 *  - integrate the other language-specific graph_code_xxx.ml
 *)

[@@@warning "-33"]

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
open Graph_code_AST_env

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* See Graph_code_AST_helpers *)

(*****************************************************************************)
(* Lookup *)
(*****************************************************************************)
(* see Graph_code_AST_lookup *)

(*****************************************************************************)
(* Visitor *)
(*****************************************************************************)
open Graph_code_AST_visitor

let extract_defs_uses ~phase ~g ~ast ~lang ~readable =
  ignore (phase, g, ast, readable);

  let current_parent, current_qualifier =
    Lang_specific.top_parent_and_qualifier ~lang ~readable ~ast
  in

  let env =
    {
      g;
      phase;
      current_parent;
      current_qualifier;
      file_qualifier = current_qualifier;
    }
  in

  (if phase = Defs then
   match current_parent with
   | base, E.File ->
       let dir = Common2.dirname readable in
       G.create_intermediate_directories_if_not_present g dir;
       g |> G.add_node (base, E.File);
       g |> G.add_edge ((dir, E.Dir), (base, E.File)) G.Has
   | str, E.Package ->
       let xs = H.dotted_ident_of_str str in
       H.create_intermediate_packages_if_not_present g G.root xs
   | n -> failwith (spf "top parent not handled yet: %s" (G.string_of_node n)));

  map_program env ast |> ignore

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let verbose = true

(* TODO: to expensive to have all ASTs in memory? use lazy?
 * but then how to free memory between the 2 passes?
 *)
let build ~root lang xs =
  let g = G.create () in
  let stats = G.empty_statistics () in
  G.create_initial_hierarchy g;

  (*  let lookup_fails = Common2.hash_with_default (fun () -> 0) in *)

  (* step1: creating the nodes and 'Has' edges, the defs *)
  logger#info "step1";
  xs
  |> Console.progress ~show:verbose (fun k ->
         List.iter (fun (file, ast) ->
             k ();
             let readable = Common.readable ~root file in
             logger#info "readable: %s" readable;
             extract_defs_uses ~phase:Defs ~g ~ast ~lang ~readable));

  logger#info "step2";
  xs
  |> Console.progress ~show:verbose (fun k ->
         List.iter (fun (file, ast) ->
             k ();
             let readable = Common.readable ~root file in
             extract_defs_uses ~phase:Uses ~g ~ast ~lang ~readable));

  (g, stats)
