(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
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

module Flag = Flag_parsing_cpp
module T = Parser_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Graph of dependencies for Objective C. See graph_code.ml and
 * main_codegraph.ml for more information.
 *
 *
 * schema:
 *  Root -> Dir -> File (.m|.h)
 *       -> Dir -> SubDir -> ...
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* for the extract_uses visitor *)
type env = {
  current: Graph_code.node;
  g: Graph_code.graph;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let parse ~show_parse_error file =
  ignore(show_parse_error);
  try
    Common.save_excursion Flag.verbose_lexing false (fun () ->
      Parse_cpp.tokens file
    )
  with
  | Timeout as exn -> Exception.catch_and_reraise exn
  | exn ->
      let e = Exception.catch exn in
      pr2_once (spf "PARSE ERROR with %s, exn = %s" file (Common.exn_to_s exn));
      Exception.reraise e

let add_use_edge env (name, kind) =
  let src = env.current in
  let dst = (name, kind) in
  (match () with
   | _ when G.has_node dst env.g ->
       G.add_edge (src, dst) G.Use env.g

   | _ ->
       G.add_node dst env.g;
       let parent_target = G.not_found in
       pr2 (spf "PB: lookup fail on %s (in %s)"
              (G.string_of_node dst) (G.string_of_node src));
       env.g |> G.add_edge (parent_target, dst) G.Has;
       env.g |> G.add_edge (src, dst) G.Use;
       ()
  )

(*****************************************************************************)
(* Defs *)
(*****************************************************************************)

let extract_defs ~g ~ast ~readable =
  ignore(ast);
  let dir = Common2.dirname readable in
  G.create_intermediate_directories_if_not_present g dir;
  g |> G.add_node (readable, E.File);
  g |> G.add_edge ((dir, E.Dir), (readable, E.File))  G.Has;

  ()

(*****************************************************************************)
(* Uses *)
(*****************************************************************************)
let extract_uses ~g ~ast ~readable =
  let env = {
    current = (readable, E.File);
    g;
  }
  in
  let dir = Common2.dirname readable in
  ast |> List.iter (function
    | T.TInclude (_, file, _) ->
        (match file with
         | s when s =~ "\"\\(.*\\)\"" ->
             let s = Common.matched1 file in
             let final = Filename.concat dir s in
             add_use_edge env (final, E.File)
         | s when s =~ "<\\(.*\\)>" ->
             ()
         | _ -> failwith ("weird include: " ^ file)
        )
    | _ -> ()
  )

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build ?(verbose=true) root files =
  let g = G.create () in
  G.create_initial_hierarchy g;

  (* step1: creating the nodes and 'Has' edges, the defs *)
  if verbose then pr2 "\nstep1: extract defs";
  files |> Console.progress ~show:verbose (fun k ->
    List.iter (fun file ->
      k();
      let readable = Common.readable root file in
      let ast = parse ~show_parse_error:true file in
      extract_defs ~g ~ast ~readable;
    ));

  (* step2: creating the 'Use' edges, the uses *)
  if verbose then pr2 "\nstep2: extract uses";
  files |> Console.progress ~show:verbose (fun k ->
    List.iter (fun file ->
      k();
      let readable = Common.readable root file in
      let ast = parse ~show_parse_error:false file in
      extract_uses ~g ~ast ~readable;
    ));

  g
