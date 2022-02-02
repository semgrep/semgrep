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
module G = Graph_code

let logger = Logging.get_logger [ __MODULE__ ]

let save_graph_on_disk = true

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Simple Python interface checker.
 *
 * The term "interface" above is used in the original sense of the word,
 * that is the exposed API of a module (not an interface in the Java sense).
 *
 * IMHO, the most basic software engineering practice is to define
 * interfaces and have the compiler (or whatever tool) making sure this
 * interface is respected. This comes from David Parnas in the 70's.
 * Unfortunately, Python does not support interface, so I had to make a tool
 * to overcome this limimation by offering the possibility to Python
 * programmers to define the interface of a foo.py module in a
 * separate foo_.pyi file (the need for the '_' suffix is explained below).
 *
 *
 * Which extension should we use for python interfaces?
 * -------------------------------------------------------
 * We can't unfortunately use foo.pyi, because this confuses mypy (and pyre)
 * which were not designed with this use-case in mind (see my issue here:
 * https://github.com/facebook/pyre-check/issues/568)
 *
 * We could maybe use foo.pi (python interface), which is even shorter than .pyi
 * or foo.pyh (for python header), to mimic .pyi, but in both cases tools
 * like Emacs, or github are not aware of those extensions and so would not
 * provide color highlighting.
 *
 * We can't use foo.pyi, but we can go over mypy restrictions by using
 * foo_.pyi! That way we get the color highlighting of .pyi mypy interface
 * file, as well as the module typechecking of mypy on those _.pyi files.
 *
 * Where should live this Python interface checker?
 * --------------------------------------------------------
 * In codecheck? but we also need to build the graph_code db, so if we add
 * this pycheck in CI, we will need to build 2 tools: codecheck and codegraph
 *
 * In codegraph? at least this reduces the dependency to one tool in CI,
 * but this still requires to build codegraph, to package it, distribute it,
 * make a docker image, etc.
 *
 * In semgrep? Actually we can leverage all the devops work we did for
 * semgrep (building it, packaging it, distributing it). It's also closer
 * to Graph_code_AST.ml (which is currently also in Semgrep), so this
 * reduces the number of repo you need to modify to add a feature!
 * Finally, we first want to experiment pycheck on the semgrep python codebase,
 * so it's easy to have a target 'make check' in the semgrep/semgrep/Makefile
 * that simply calls semgrep-core -pycheck.
 *
 * We can still design it in a way to not have dependencies to
 * other semgrep modules (like we did for Graph_code_AST.ml) so at some
 * point we can move it elsewhere (e.g., in codecheck or codegraph).
 *
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* less: could generalize again to other languages at some point *)
let parse_program lang file =
  (* for codegraph purpose, we need the toplevel Assign turned into VarDef *)
  let assign_to_vardef = true in
  let ast = Parse_python.parse_program file in
  let ast = Python_to_generic.program ~assign_to_vardef ast in
  Naming_AST.resolve lang ast;
  ast

let rec file_node_parent_of_node n g =
  match n with
  | _, E.File -> Some n
  | _ when n = G.root -> None
  | _ ->
      let n = G.parent n g in
      file_node_parent_of_node n g

let pyi_of_py file =
  let d, b, e = Common2.dbe_of_filename file in
  assert (e = "py");
  (* coupling: note that in Graph_code_AST_lang_specific there is some
   * hack to remove the _ suffix for .pyi file so we get the same
   * entities that with the corresponding .py file
   *)
  let pyi = Common2.filename_of_dbe (d, b ^ "_", "pyi") in
  pyi

let py_of_pyi file =
  let d, b, e = Common2.dbe_of_filename file in
  assert (e = "pyi");
  let b = if b =~ "^\\(.*\\)_$" then Common.matched1 b else b in
  Common2.filename_of_dbe (d, b, "py")

(*****************************************************************************)
(* Build the graphs *)
(*****************************************************************************)

let build_graphs root lang files =
  logger#info "building the graphs";
  let xs = files |> List.map (fun file -> (file, parse_program lang file)) in
  let hooks = Graph_code_AST.default_hooks in
  let gpy, _stats = Graph_code_AST.build ~root ~hooks lang xs in
  if save_graph_on_disk then
    Graph_code.save gpy (Filename.concat root "graph_code.marshall");

  let ys =
    files
    |> Common.map_filter (fun file ->
           let pyi = pyi_of_py file in
           if Sys.file_exists pyi then Some (pyi, parse_program lang pyi)
           else None)
  in
  let hooks = Graph_code_AST.default_hooks in
  let gpyi, _stats = Graph_code_AST.build ~root ~hooks lang ys in
  if save_graph_on_disk then
    Graph_code.save gpyi (Filename.concat root "graph_code_pyi.marshall");

  (gpy, gpyi)

(*****************************************************************************)
(* Check the graphs *)
(*****************************************************************************)

let check_graphs_boundaries_when_pyh gpy gpyi =
  logger#info "checking the graphs";

  let g = gpy in
  let pred = G.mk_eff_use_pred g in

  (* similar to graph_code_checker.ml in pfff/graph_code.
   * alt: iter from .py files which have a .pyh
   *)
  g
  |> G.iter_nodes (fun n_def ->
         G.nodeinfo_opt n_def g
         |> Option.iter (fun _info ->
                let file_node_opt = file_node_parent_of_node n_def g in
                file_node_opt
                |> Option.iter (fun file_node ->
                       if not (G.has_node file_node gpyi) then
                         logger#info "no File node %s found in .pyi"
                           (G.string_of_node file_node)
                       else
                         let users = pred n_def in
                         let file_def = G.file_of_node n_def g in
                         let users_outside =
                           users
                           |> List.filter (fun n ->
                                  try
                                    let file_user = G.file_of_node n g in
                                    file_user <> file_def
                                  with Not_found ->
                                    failwith
                                      (spf "could not find file of %s"
                                         (G.string_of_node n)))
                         in
                         match users_outside with
                         | [] -> ()
                         | x :: _ when not (G.has_node n_def gpyi) ->
                             pr2
                               (spf
                                  "This node\n\
                                   \t %s (in %s)\n\
                                  \ is used outside\n\
                                   \t (e.g., %s)\n\
                                  \ but is not in the _.pyi"
                                  (G.string_of_node n_def) file_def
                                  (G.string_of_node x))
                         | _ -> ())))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* semgrep-core -pycheck *)
let pycheck root =
  (* less: we could generalize to other languages! like typescript *)
  let lang = Lang.Python in
  let files, _skipped =
    Find_target.files_of_dirs_or_files (Some lang) [ root ]
  in
  let files, _skipped =
    Skip_code.filter_files_if_skip_list ~root:[ root ] files
  in
  let files =
    files
    |> Common.exclude (fun file ->
           file =~ ".*\\.pyi" && Sys.file_exists (py_of_pyi file))
  in
  logger#info "processing %d files" (List.length files);

  let gpy, gpyi = build_graphs root lang files in

  (* TODO: check_graphs_py_and_pyh_in_sync gpy gpyi *)
  check_graphs_boundaries_when_pyh gpy gpyi;
  ()
