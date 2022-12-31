(* Yoann Padioleau
 *
 * Copyright (C) 2014 Facebook
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
open Ast_lisp
module E = Entity_code
module G = Graph_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Graph of dependencies for Lisp. See graph_code.ml and
 * main_codegraph.ml for more information.
 *
 * schema:
 *  Root -> Dir -> File -> Function
 *                      -> Macro
 *                      -> TODO Global
 *
 *       -> Dir -> Subdir -> ...
 *
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type env = {
  g : Graph_code.t;
  phase : phase;
  current : Graph_code.node;
  readable_file : Common.filename;
  at_toplevel : bool;
  (* error reporting *)
  dupes : (Graph_code.node, bool) Hashtbl.t;
  log : string -> unit;
  pr2_and_log : string -> unit;
}

and phase = Defs | Uses

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)

let _hmemo = Hashtbl.create 101

let parse file =
  Common.memoized _hmemo file (fun () ->
      try Parse_lisp.parse_program file with
      | Timeout _ as exn -> Exception.catch_and_reraise exn
      | exn ->
          let e = Exception.catch exn in
          pr2_once
            (spf "PARSE ERROR with %s, exn = %s" file (Exception.to_string e));
          [])

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* dupe: graph_code_c *)
let find_existing_node env s candidates last_resort =
  candidates
  |> Common.find_opt (fun kind -> G.has_node (s, kind) env.g)
  ||| last_resort

let _error s tok = failwith (spf "%s: %s" (Parse_info.string_of_info tok) s)

(*****************************************************************************)
(* Add Node *)
(*****************************************************************************)

let add_node_and_edge_if_defs_mode env (s, kind) tok =
  let node = (s, kind) in
  if env.phase = Defs then (
    if G.has_node node env.g then (
      env.pr2_and_log (spf "DUPE entity: %s" (G.string_of_node node));
      let nodeinfo = G.nodeinfo node env.g in
      let orig_file = nodeinfo.G.pos.Parse_info.file in
      env.log (spf " orig = %s" orig_file);
      env.log (spf " dupe = %s" env.readable_file);
      Hashtbl.replace env.dupes node true)
    else
      let pos = Parse_info.unsafe_token_location_of_info tok in
      let pos = { pos with Parse_info.file = env.readable_file } in
      let nodeinfo = { Graph_code.pos; typ = None; props = [] } in
      env.g |> G.add_node node;
      env.g |> G.add_edge (env.current, node) G.Has;
      env.g |> G.add_nodeinfo node nodeinfo);
  { env with current = node }

(*****************************************************************************)
(* Add edge *)
(*****************************************************************************)
let add_use_edge env (s, kind) tok =
  let src = env.current in
  let dst = (s, kind) in
  match () with
  | _ when not (G.has_node src env.g) ->
      env.pr2_and_log (spf "SRC FAIL: %s, %s" (G.string_of_node src) s)
  (* the normal case *)
  | _ when G.has_node dst env.g -> G.add_edge (src, dst) G.Use env.g
  | _ ->
      env.pr2_and_log
        (spf "Lookup failure on %s (%s)" (G.string_of_node dst)
           (Parse_info.string_of_info tok))

(*****************************************************************************)
(* Defs/Uses *)
(*****************************************************************************)

let rec extract_defs_uses env ast =
  if env.phase = Defs then (
    let dir = Common2.dirname env.readable_file in
    G.create_intermediate_directories_if_not_present env.g dir;
    let node = (env.readable_file, E.File) in
    env.g |> G.add_node node;
    env.g |> G.add_edge ((dir, E.Dir), node) G.Has);
  let env = { env with current = (env.readable_file, E.File) } in
  sexps_toplevel env ast

(* ---------------------------------------------------------------------- *)
(* Toplevels *)
(* ---------------------------------------------------------------------- *)
and sexps_toplevel env xs = xs |> List.iter (sexp_toplevel env)
and sexps env xs = xs |> List.iter (sexp env)
and sexp_toplevel env x = sexp_bis env x
and sexp env x = sexp_bis { env with at_toplevel = false } x

and sexp_bis env x =
  match x with
  | Sexp
      ( _,
        [ Atom (Id ("provide", _)); Special ((Quote, _), Atom (Id (s, t))) ],
        _ ) ->
      let _env = add_node_and_edge_if_defs_mode env (s, E.Module) t in
      ()
  | Sexp (_, Atom (Id ("defmacro", _)) :: Atom (Id (s, t)) :: rest, _)
    when env.at_toplevel ->
      let env = add_node_and_edge_if_defs_mode env (s, E.Macro) t in
      sexps env rest
  (* todo: use Sexp x below because don't handle long symbol like xxx-`S *)
  | Sexp (_, Atom (Id ("defun", _)) :: Atom (Id (s, t)) :: Sexp x :: rest, _)
    when env.at_toplevel ->
      let env = add_node_and_edge_if_defs_mode env (s, E.Function) t in
      sexps env (Sexp x :: rest)
      (*
  | Sexp (_, Atom (Id ("defvar", _))::Atom (Id (s, t))::rest, _) ->
    let env = add_node_and_edge_if_defs_mode env (s, E.Global) t in
    sexps env rest
*)
  | Sexp
      ( _,
        [ Atom (Id ("require", _)); Special ((Quote, _), Atom (Id (s, t))) ],
        _ ) ->
      if env.phase = Uses then
        let node = (s, E.Module) in
        add_use_edge env node t
  | Sexp (_, Atom (Id (s, t)) :: xs, _) ->
      if
        env.phase = Uses
        && (G.has_node (s, E.Macro) env.g || G.has_node (s, E.Function) env.g)
      then (
        let kind =
          find_existing_node env s [ E.Macro; E.Function ] E.Function
        in
        add_use_edge env (s, kind) t;
        sexps env xs)
      else
        (* todo: too many errors for now, do not generate lookup failure for now
         * e.g. all builtins are not taken into account
         *)
        sexps env xs
  (* boilerplate *)
  | Sexp (_, xs, _) -> sexps env xs
  | Atom _ -> ()
  | Special ((special, _t), s) -> (
      match special with
      | Quote -> ()
      | BackQuote ->
          (* todo: should actually evaluate when inside a comma! *)
          ()
      (* hmm but should happen only when in backquote mode *)
      | Comma
      | At ->
          sexp env s)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build ?(verbose = true) root files =
  let g = G.create () in
  G.create_initial_hierarchy g;

  let chan = open_out_bin (Filename.concat root "pfff.log") in

  let env =
    {
      g;
      phase = Defs;
      current = G.pb;
      readable_file = "__filled_later__";
      at_toplevel = true;
      dupes = Hashtbl.create 101;
      log =
        (fun s ->
          output_string chan (s ^ "\n");
          flush chan);
      pr2_and_log =
        (fun s ->
          (*if verbose then *)
          pr2 s;
          output_string chan (s ^ "\n");
          flush chan);
    }
  in

  (* step1: creating the nodes and 'Has' edges, the defs *)
  env.pr2_and_log "\nstep1: extract defs";
  files
  |> Console.progress ~show:verbose (fun k ->
         List.iter (fun file ->
             k ();
             let ast = parse file in
             let readable_file = Common.readable ~root file in
             extract_defs_uses { env with phase = Defs; readable_file } ast));

  (* step2: creating the 'Use' edges *)
  env.pr2_and_log "\nstep2: extract Uses";
  files
  |> Console.progress ~show:verbose (fun k ->
         List.iter (fun file ->
             k ();
             let ast = parse file in
             let readable_file = Common.readable ~root file in
             extract_defs_uses { env with phase = Uses; readable_file } ast));

  g
