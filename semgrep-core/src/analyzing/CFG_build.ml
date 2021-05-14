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

(* Like IL.label but without the token attached to the ident, this is to allow
 * the label to be used as a key in a map or hash table.
 *)
type label_key = string * G.sid

(*s: type [[CFG_build.state]] *)
(* Information passed recursively in stmt or stmt_list below.
 * The graph g is mutable, so most of the work is done by side effects on it.
 * No need to return a new state.
 *)
type state = {
  g : F.cfg;
  (* When there is a 'return' we need to know the exit node to link to *)
  exiti : F.nodei;
  (* Attaches labels to nodes. *)
  labels : (label_key, F.nodei) Hashtbl.t;
  (* Gotos pending to be resolved. *)
  gotos : (nodei * label_key) list ref;
}

(*e: type [[CFG_build.state]] *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function [[CFG_build.add_arc]] *)
let add_arc (starti, nodei) g = g#add_arc ((starti, nodei), F.Direct)

(*e: function [[CFG_build.add_arc]] *)

(*s: function [[CFG_build.add_arc_opt]] *)
let add_arc_opt (starti_opt, nodei) g =
  starti_opt
  |> Common.do_option (fun starti -> g#add_arc ((starti, nodei), F.Direct))

(*e: function [[CFG_build.add_arc_opt]] *)

let key_of_label ((str, _tok), sid) : label_key = (str, sid)

let add_pending_goto state gotoi label =
  state.gotos := (gotoi, key_of_label label) :: !(state.gotos)

let label_node state labels nodei =
  labels
  |> List.iter (fun label ->
         Hashtbl.add state.labels (key_of_label label) nodei)

let resolve_gotos state =
  !(state.gotos)
  |> List.iter (fun (srci, label_key) ->
         match Hashtbl.find_opt state.labels label_key with
         | None ->
             Common.pr2
             @@ Common.spf "Could not resolve label: %s" (fst label_key)
         | Some dsti -> state.g |> add_arc (srci, dsti));
  state.gotos := []

(*****************************************************************************)
(* Algorithm *)
(*****************************************************************************)

(* The CFG building algorithm works by iteratively visiting the
 * statements in the AST of a function. At each statement,
 * the cfg_stmt function is called, and passed the index of the;
 * previous node (if there is one), and returns a cfg_stmt_result.
 *
 * Function cfg_stmt_list is the one responsible for labeling nodes.
 * We do everything in one pass by collecting the list of gotos and
 * resolving them at the end. Alternatively, we could do it in two
 * passes, with the first pass doing the labeling work.
 *
 * history:
 * - ver1: old code was returning a nodei, but break has no end, so
 *   cfg_stmt should return a nodei option.
 * - ver2: old code was taking a nodei, but should also take a nodei
 *   option. There can be deadcode in the function.
 * - ver3: In order to handle labels/gotos the now return either a
 *   label or a pair nodei * nodei option (entry and exit).
 *
 * subtle: try/throw. The current algo is not very precise, but
 * it's probably good enough for many analysis.
 *)

type cfg_stmt_result =
  (* A label for a label statement. *)
  | CfgLabel of label
  (* The fist (entry) and last (exit) node of the created CFG.
   * Last node is optional; it is None when the execution will not
   * continue (return), or when it may continue with a different
   * statement than the subsequent one (goto).
   *)
  | CfgFirstLast of F.nodei * F.nodei option

let rec cfg_stmt : state -> F.nodei option -> stmt -> cfg_stmt_result =
 fun state previ stmt ->
  match stmt.s with
  | Instr x ->
      let newi = state.g#add_node { F.n = F.NInstr x } in
      state.g |> add_arc_opt (previ, newi);
      CfgFirstLast (newi, Some newi)
  | If (tok, e, st1, st2) -> (
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

      match (finalthen, finalelse) with
      | None, None ->
          (* probably a return in both branches *)
          CfgFirstLast (newi, None)
      | Some nodei, None | None, Some nodei -> CfgFirstLast (newi, Some nodei)
      | Some n1, Some n2 ->
          let lasti = state.g#add_node { F.n = F.Join } in
          state.g |> add_arc (n1, lasti);
          state.g |> add_arc (n2, lasti);
          CfgFirstLast (newi, Some lasti) )
  | Loop (tok, e, st) ->
      (* previ -> newi ---> newfakethen -> ... -> finalthen -
       *             |---|-----------------------------------|
       *                 |-> newfakelse
       *)
      let newi = state.g#add_node { F.n = NCond (tok, e) } in
      state.g |> add_arc_opt (previ, newi);

      let newfakethen = state.g#add_node { F.n = F.TrueNode } in
      let newfakeelse = state.g#add_node { F.n = F.FalseNode } in
      state.g |> add_arc (newi, newfakethen);
      state.g |> add_arc (newi, newfakeelse);

      let finalthen = cfg_stmt_list state (Some newfakethen) st in
      state.g |> add_arc_opt (finalthen, newi);
      CfgFirstLast (newi, Some newfakeelse)
  | Label label -> CfgLabel label
  | Goto (tok, label) ->
      let newi = state.g#add_node { F.n = F.NGoto (tok, label) } in
      state.g |> add_arc_opt (previ, newi);
      add_pending_goto state newi label;
      CfgFirstLast (newi, None)
  | Return (tok, e) ->
      let newi = state.g#add_node { F.n = F.NReturn (tok, e) } in
      state.g |> add_arc_opt (previ, newi);
      state.g |> add_arc (newi, state.exiti);
      (* the next statement if there is one will not be linked to
       * this new node *)
      CfgFirstLast (newi, None)
  | Try (try_st, catches, finally_st) ->
      (* TODO: This is not a proper CFG for try-catch-finally... but
       * it's probably "good enough" for now! *)
      (* previ -> newi -> try --> catch1 -|
       *                      |->  ...   -|
       *                      |-> catchN -|
       *                      |-----------|-> newfakefinally -> finally
       *)
      let newi = state.g#add_node { F.n = F.TrueNode } in
      state.g |> add_arc_opt (previ, newi);
      let finaltry = cfg_stmt_list state (Some newi) try_st in
      let newfakefinally = state.g#add_node { F.n = F.TrueNode } in
      state.g |> add_arc_opt (finaltry, newfakefinally);
      catches
      |> List.iter (fun (_, catch_st) ->
             let finalcatch = cfg_stmt_list state finaltry catch_st in
             state.g |> add_arc_opt (finalcatch, newfakefinally));
      let finalfinally = cfg_stmt_list state (Some newfakefinally) finally_st in
      CfgFirstLast (newi, finalfinally)
  | Throw (_, _) -> cfg_todo state previ stmt
  | MiscStmt x ->
      let newi = state.g#add_node { F.n = F.NOther x } in
      state.g |> add_arc_opt (previ, newi);
      CfgFirstLast (newi, Some newi)
  | FixmeStmt _ -> cfg_todo state previ stmt

and cfg_todo state previ stmt =
  let newi = state.g#add_node { F.n = F.NTodo stmt } in
  state.g |> add_arc_opt (previ, newi);
  CfgFirstLast (newi, Some newi)

and cfg_stmt_list state previ xs =
  let lasti_opt =
    xs
    |> List.fold_left
         (fun (previ, labels) stmt ->
           (* We don't create special nodes for labels in the CFG; instead,
            * we assign them to the entry nodes of the labeled statements.
            *)
           match cfg_stmt state previ stmt with
           | CfgFirstLast (firsti, lasti) ->
               label_node state labels firsti;
               (lasti, [])
           | CfgLabel label -> (previ, label :: labels))
         (previ, [])
    |> fst
  in
  lasti_opt

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let (cfg_of_stmts : stmt list -> F.cfg) =
 fun xs ->
  (* yes, I sometimes use objects, and even mutable objects in OCaml ... *)
  let g = new Ograph_extended.ograph_mutable in

  let enteri = g#add_node { F.n = F.Enter } in
  let exiti = g#add_node { F.n = F.Exit } in

  let newi = enteri in

  let state = { g; exiti; labels = Hashtbl.create 2; gotos = ref [] } in
  let last_node_opt = cfg_stmt_list state (Some newi) xs in
  (* Must wait until all nodes have been labeled before resolving gotos. *)
  resolve_gotos state;
  (* maybe the body does not contain a single 'return', so by default
   * connect last stmt to the exit node
   *)
  g |> add_arc_opt (last_node_opt, exiti);
  g

(*e: pfff/lang_GENERIC/analyze/CFG_build.ml *)
