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
 *
 * See graph_code.ml (in pfff), AST_generic.ml (in semgrep), and
 * main_codegraph.ml (in codegraph) for more information.
 *
 * On the location of Graph_code_AST.ml:
 * Ideally, this file should be in the deep-semgrep repository, but
 * this would prevent to use it in other OSS projects like codegraph or
 * codecheck, or to use it for the .pyh checker that we want to use on semgrep.
 *
 * In theory, this file should be part of the codegraph repository, but
 * this would add another dependency in deep-semgrep.
 * Moreover, Graph_code_AST.ml leverages Naming_AST.ml and generalizes it
 * to resolve names globally, so it makes sense to have them close together.
 * It's also closer to AST_generic.ml. In any case, codegraph (the
 * dependency explorer GUI) was designed to not be dependent of any
 * language and any graph_code_xxx.ml, and instead rely on the serialized
 * (general) graph_code.marshall on the disk. Thus, codegraph can still work
 * even without a Graph_code_AST.ml (only codegraph_build is impacted,
 * because instead people will have to use for example
 * `semgrep-core -l python -build_codegraph`)
 *
 * Note that even though Graph_code_AST.ml is inside the semgrep repository,
 * its library name is pfff-lang_GENERIC-naming and does not have dependencies to
 * other semgrep modules, so we could easily extract it from semgrep
 * at some point and move it in codegraph for example.
 *
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

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
open Graph_code_AST_env

(* TODO: does not parse well with pfff
type hooks = Graph_code_AST_env.hooks = {
  on_def: (Graph_code.node * AST_generic.definition) -> unit;
}
*)

let default_hooks : hooks =
  {
    on_def_node = (fun _ _ -> ());
    on_extend_edge = (fun _ _ _ -> ());
    on_misc = (fun _ -> ());
  }

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
(* see Graph_code_AST_visitor *)

let extract_defs_uses env ast =
  (if env.phase = Defs then
   match env.current_parent with
   | base, E.File ->
       let dir = Common2.dirname env.readable in
       G.create_intermediate_directories_if_not_present env.g dir;
       env.g |> G.add_node (base, E.File);
       env.g |> G.add_edge ((dir, E.Dir), (base, E.File)) G.Has
   | str, E.Package ->
       let xs = H.dotted_ident_of_str str in
       H.create_intermediate_packages_if_not_present env.g G.root xs
   | n -> failwith (spf "top parent not handled yet: %s" (G.string_of_node n)));

  Graph_code_AST_visitor.map_program env ast |> ignore

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let verbose = true

(* TODO: too expensive to have all ASTs in memory? use lazy?
 * but then how to free memory between the 2 passes?
 *)
let build ~root ~hooks lang xs =
  let g = G.create () in
  let stats = G.empty_statistics () in
  G.create_initial_hierarchy g;

  (*  let lookup_fails = Common2.hash_with_default (fun () -> 0) in *)
  let env_for_file phase file ast =
    let readable = Common.readable ~root file in
    logger#info "readable: %s" readable;
    let current_parent, current_qualifier =
      Lang_specific.top_parent_and_qualifier ~lang ~readable ~ast
    in
    {
      g;
      phase;
      hooks;
      current_parent;
      current_qualifier;
      file_qualifier = current_qualifier;
      readable;
    }
  in

  (* step1: creating the nodes and 'Has' edges, the defs *)
  logger#info "step1: the definitions";
  xs
  |> Console.progress ~show:verbose (fun k ->
         List.iter (fun (file, ast) ->
             k ();
             let env = env_for_file Defs file ast in
             extract_defs_uses env ast));

  logger#info "step2: the uses";
  xs
  |> Console.progress ~show:verbose (fun k ->
         List.iter (fun (file, ast) ->
             k ();
             let env = env_for_file Uses file ast in
             extract_defs_uses env ast));

  (g, stats)
