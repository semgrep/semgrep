(*s: pfff/lang_GENERIC/analyze/CFG_build.ml *)
(* Yoann Padioleau
 *
 * Copyright (C) 2009, 2010, 2011 Facebook
 * Copyright (C) 2020 r2c
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
open IL
module F = IL (* to be even more similar to controlflow_build.ml *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Control-flow graph generation for the IL.
 *
 * This is mostly a copy-paste (with less cases) of controlflow_build.ml
 *
 * TODO:
 *  - factorize at some point with controlflow_build.ml?
 *  - remove controlflow.ml? now that we have the Il, maybe better to
 *    do any kind of cfg-based analysis on the IL rather than the generic AST.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type [[CFG_build.state]] *)
(* Information passed recursively in stmt or stmt_list below.
 * The graph g is mutable, so most of the work is done by side effects on it.
 * No need to return a new state.
 *)
type state = {
  g: F.cfg;

  (* When there is a 'return' we need to know the exit node to link to *)
  exiti: F.nodei;
}
(*e: type [[CFG_build.state]] *)


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function [[CFG_build.add_arc]] *)
let add_arc (starti, nodei) g =
  g#add_arc ((starti, nodei), F.Direct)
(*e: function [[CFG_build.add_arc]] *)

(*s: function [[CFG_build.add_arc_opt]] *)
let add_arc_opt (starti_opt, nodei) g =
  starti_opt |> Common.do_option (fun starti ->
    g#add_arc ((starti, nodei), F.Direct)
  )
(*e: function [[CFG_build.add_arc_opt]] *)

(*****************************************************************************)
(* Algorithm *)
(*****************************************************************************)

(* The CFG building algorithm works by iteratively visiting the
 * statements in the AST of a function. At each statement,
 * the cfg_stmt function is called, and passed the index of the
 * previous node (if there is one), and returns the index of
 * the created node (if there is one).
 *
 * history:
 * - ver1: old code was returning a nodei, but break has no end, so
 *   cfg_stmt should return a nodei option.
 * - ver2: old code was taking a nodei, but should also take a nodei
 *   option. There can be deadcode in the function.
 *
 * subtle: try/throw. The current algo is not very precise, but
 * it's probably good enough for many analysis.
 *)


let rec (cfg_stmt: state -> F.nodei option -> stmt -> F.nodei option) =
 fun state previ stmt ->

   match stmt.s with
   | Instr x ->
      let newi = state.g#add_node { F.n = F.NInstr x } in
      state.g |> add_arc_opt (previ, newi);
      Some newi

   | If (tok, e, st1, st2) ->
     (* previ -> newi --->  newfakethen -> ... -> finalthen --> lasti -> <rest>
      *                |                                     |
      *                |->  newfakeelse -> ... -> finalelse -|
      *
      * The lasti can be a Join when there is no return in either branch.
      *)
       let newi = state.g#add_node { F.n = F.NCond (tok, e) } in
       state.g |> add_arc_opt (previ, newi);

       let newfakethen = state.g#add_node { F.n = F.TrueNode } in
       let newfakeelse = state.g#add_node { F.n = F.FalseNode } in
       state.g |> add_arc (newi, newfakethen);
       state.g |> add_arc (newi, newfakeelse);

       let finalthen = cfg_stmt_list state (Some newfakethen) st1 in
       let finalelse = cfg_stmt_list state (Some newfakeelse) st2 in

       (match finalthen, finalelse with
       | None, None ->
           (* probably a return in both branches *)
           None
       | Some nodei, None
       | None, Some nodei ->
           Some nodei
       | Some n1, Some n2 ->
           let lasti = state.g#add_node { F.n = F.Join } in
           state.g |> add_arc (n1, lasti);
           state.g |> add_arc (n2, lasti);
           Some lasti
       )


   | Loop (tok, e, st) ->
     (* previ -> newi ---> newfakethen -> ... -> finalthen -
      *             |---|-----------------------------------|
      *                 |-> newfakelse 
      *)
       let newi = state.g#add_node { F.n = NCond (tok, e); } in
       state.g |> add_arc_opt (previ, newi);

       let newfakethen = state.g#add_node { F.n = F.TrueNode } in
       let newfakeelse = state.g#add_node { F.n = F.FalseNode } in
       state.g |> add_arc (newi, newfakethen);
       state.g |> add_arc (newi, newfakeelse);

       let finalthen = cfg_stmt_list state (Some newfakethen) st in
       state.g |> add_arc_opt (finalthen, newi);
       Some newfakeelse

   | Label _ 
   | Goto _
     -> raise Todo

   | Return (tok, e) ->
       let newi = state.g#add_node { F.n = F.NReturn (tok, e); } in
       state.g |> add_arc_opt (previ, newi);
       state.g |> add_arc (newi, state.exiti);
       (* the next statement if there is one will not be linked to
        * this new node *)
       None

   | Try _
   | Throw (_, _)
     -> raise Todo

   | MiscStmt x ->
      let newi = state.g#add_node { F.n = F.NOther x } in
      state.g |> add_arc_opt (previ, newi);
      Some newi


and cfg_stmt_list state previ xs =
  xs |> List.fold_left (fun previ stmt ->
    cfg_stmt state previ stmt
  ) previ

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let (cfg_of_stmts: stmt list -> F.cfg) =
  fun xs ->
  (* yes, I sometimes use objects, and even mutable objects in OCaml ... *)
  let g = new Ograph_extended.ograph_mutable in

  let enteri = g#add_node { F.n = F.Enter } in
  let exiti  = g#add_node { F.n = F.Exit } in

  let newi = enteri in

  let state = {
    g = g;
    exiti = exiti;
  }
  in
  let last_node_opt =
    cfg_stmt_list state (Some newi) xs
  in
  (* maybe the body does not contain a single 'return', so by default
   * connect last stmt to the exit node
   *)
  g |> add_arc_opt (last_node_opt, exiti);
  g

(*e: pfff/lang_GENERIC/analyze/CFG_build.ml *)
