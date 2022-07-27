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

(* Information passed recursively in stmt or stmt_list below.
 * The graph g is mutable, so most of the work is done by side effects on it.
 * No need to return a new state.
 *)
type state = {
  g : (F.node, F.edge) Ograph_extended.ograph_mutable;
  (* We keep this so we can connect to unreachable nodes. *)
  enteri : F.nodei;
  (* When there is a 'return' we need to know the exit node to link to *)
  exiti : F.nodei;
  (* Attaches labels to nodes. *)
  labels : (label_key, F.nodei) Hashtbl.t;
  (* Gotos pending to be resolved, a list of Goto nodes and the label
   * to which they are jumping. *)
  gotos : (nodei * label_key) list ref;
  (* If we are inside a Try, this is the start of the handlers. *)
  try_catches_opt : F.nodei option;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let add_arc (starti, nodei) g = g#add_arc ((starti, nodei), F.Direct)

let add_arc_from_opt (starti_opt, nodei) g =
  starti_opt
  |> Option.iter (fun starti -> g#add_arc ((starti, nodei), F.Direct))

let add_arc_opt_to_opt (starti_opt, nodei_opt) g =
  starti_opt
  |> Option.iter (fun starti ->
         nodei_opt
         |> Option.iter (fun nodei -> g#add_arc ((starti, nodei), F.Direct)))

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

let rec cfg_stmt : Lang.t -> state -> F.nodei option -> stmt -> cfg_stmt_result
    =
 fun lang state previ stmt ->
  match stmt.s with
  | Instr x -> (
      let newi = state.g#add_node { F.n = F.NInstr x } in
      state.g |> add_arc_from_opt (previ, newi);
      match x.i with
      | Call _ ->
          (* If we are inside a try-catch, we consider the possibility of this call
           * raising an exception, then we add a jump to catch-blocks. This could
           * lead to some false positives when running taint rules (since it's a
           * may-analysis), but they are probably rare. For constant propagation
           * this should reduce false positives (since it's a must-analysis).
           * Ideally we should have a preceeding analysis that infers which calls
           * may (or may not) raise exceptions. *)
          state.g |> add_arc_opt_to_opt (Some newi, state.try_catches_opt);
          CfgFirstLast (newi, Some newi)
      | AssignAnon (_, Lambda (_, fdef)) ->
          (* Lambdas are treated as statement blocks, kind of like IF-THEN blocks,
           * the CFG does NOT capture the actual flow of data that goes into the
           * lambda through its parameters, and back into the surrounding definition
           * through its `return' statement.
           *
           * previ -> newi -> ... (lambda body) -> finallambda -> lasti
           *               \__________________________________/
           *
           * TODO: In order to handle lambdas properly, we could either inline
           * them, or "lift" them and then use an inter-procedural analysis.
           *)
          let lasti = state.g#add_node { F.n = F.Join } in
          let finallambda =
            cfg_stmt_list lang
              { state with exiti = lasti }
              (Some newi) fdef.fbody
          in
          state.g |> add_arc (newi, lasti);
          state.g |> add_arc_from_opt (finallambda, lasti);
          CfgFirstLast (newi, Some lasti)
      | __else__ -> CfgFirstLast (newi, Some newi))
  | If (tok, e, st1, st2) -> (
      (* previ -> newi --->  newfakethen -> ... -> finalthen --> lasti -> <rest>
       *                |                                     |
       *                |->  newfakeelse -> ... -> finalelse -|
       *
       * The lasti can be a Join when there is no return in either branch.
       *)
      let newi = state.g#add_node { F.n = F.NCond (tok, e) } in
      state.g |> add_arc_from_opt (previ, newi);

      let newfakethen = state.g#add_node { F.n = F.TrueNode } in
      let newfakeelse = state.g#add_node { F.n = F.FalseNode } in
      state.g |> add_arc (newi, newfakethen);
      state.g |> add_arc (newi, newfakeelse);

      let finalthen = cfg_stmt_list lang state (Some newfakethen) st1 in
      let finalelse = cfg_stmt_list lang state (Some newfakeelse) st2 in

      match (finalthen, finalelse) with
      | None, None ->
          (* probably a return in both branches *)
          CfgFirstLast (newi, None)
      | Some nodei, None
      | None, Some nodei ->
          CfgFirstLast (newi, Some nodei)
      | Some n1, Some n2 ->
          let lasti = state.g#add_node { F.n = F.Join } in
          state.g |> add_arc (n1, lasti);
          state.g |> add_arc (n2, lasti);
          CfgFirstLast (newi, Some lasti))
  | Loop (tok, e, st) ->
      (* previ -> newi ---> newfakethen -> ... -> finalthen -
       *             |---|-----------------------------------|
       *                 |-> newfakelse
       *)
      let newi = state.g#add_node { F.n = NCond (tok, e) } in
      state.g |> add_arc_from_opt (previ, newi);

      let newfakethen = state.g#add_node { F.n = F.TrueNode } in
      let newfakeelse = state.g#add_node { F.n = F.FalseNode } in
      state.g |> add_arc (newi, newfakethen);
      state.g |> add_arc (newi, newfakeelse);

      let finalthen = cfg_stmt_list lang state (Some newfakethen) st in
      state.g |> add_arc_from_opt (finalthen, newi);
      CfgFirstLast (newi, Some newfakeelse)
  | Label label -> CfgLabel label
  | Goto (tok, label) ->
      let newi = state.g#add_node { F.n = F.NGoto (tok, label) } in
      state.g |> add_arc_from_opt (previ, newi);
      add_pending_goto state newi label;
      CfgFirstLast (newi, None)
  | Return (tok, e) ->
      let newi = state.g#add_node { F.n = F.NReturn (tok, e) } in
      state.g |> add_arc_from_opt (previ, newi);
      state.g |> add_arc (newi, state.exiti);
      CfgFirstLast (newi, None)
  | Try (try_st, catches, finally_st) ->
      (* previ ->
       * newi ->
       * try -> catchesi --> catch1 -|
       *                 |->  ...   -|
       *                 |-> catchN -|
       *                 |-----------|-> newfakefinally -> finally
       *
       *)
      let newi = state.g#add_node { F.n = NOther (Noop "try") } in
      state.g |> add_arc_from_opt (previ, newi);
      let catchesi = state.g#add_node { F.n = NOther (Noop "catch") } in
      let state' = { state with try_catches_opt = Some catchesi } in
      let finaltry = cfg_stmt_list lang state' (Some newi) try_st in
      state.g |> add_arc_from_opt (finaltry, catchesi);
      let newfakefinally = state.g#add_node { F.n = NOther (Noop "finally") } in
      state.g |> add_arc (catchesi, newfakefinally);
      catches
      |> List.iter (fun (_, catch_st) ->
             let finalcatch =
               cfg_stmt_list lang state (Some catchesi) catch_st
             in
             state.g |> add_arc_from_opt (finalcatch, newfakefinally));
      let finalfinally =
        cfg_stmt_list lang state (Some newfakefinally) finally_st
      in
      (* If we're inside another Try then we assume that we could propagate up
       * some unhandled exception. Otherwise we assume that we handled everything.
       * THINK: Alternatively, we could add an arc to the exit node. *)
      state.g |> add_arc_opt_to_opt (finalfinally, state.try_catches_opt);
      CfgFirstLast (newi, finalfinally)
  | Throw (tok, e) ->
      let newi = state.g#add_node { F.n = F.NThrow (tok, e) } in
      state.g |> add_arc_from_opt (previ, newi);
      (match state.try_catches_opt with
      | None -> state.g |> add_arc (newi, state.exiti)
      | Some catchesi -> state.g |> add_arc (newi, catchesi));
      CfgFirstLast (newi, None)
  (* Any DefStmts which are FuncDefs are reified as proper Func nodes
     within the CFG, which contain their own smaller CFGs.
  *)
  | FuncStmt { fdef; ent; body } -> (
      let cfg = cfg_of_stmts lang body in
      let newi = state.g#add_node { F.n = NFunc { fdef; cfg; ent } } in
      (* If there is no previous node, we add an edge from the entrance node.
         Why? This is so that the dataflow can run on unreachable functions.
      *)
      match previ with
      | None ->
          state.g |> add_arc (state.enteri, newi);
          CfgFirstLast (newi, Some newi)
      | _ ->
          state.g |> add_arc_from_opt (previ, newi);
          CfgFirstLast (newi, Some newi))
  | ClassStmt stmts ->
      let cfg = cfg_of_stmts lang stmts in
      let newi = state.g#add_node { F.n = NClass cfg } in
      state.g |> add_arc_from_opt (previ, newi);
      CfgFirstLast (newi, Some newi)
  | ModuleStmt stmts ->
      let cfg = cfg_of_stmts lang stmts in
      let newi = state.g#add_node { F.n = NModule cfg } in
      state.g |> add_arc_from_opt (previ, newi);
      CfgFirstLast (newi, Some newi)
  | MiscStmt x ->
      let newi = state.g#add_node { F.n = F.NOther x } in
      state.g |> add_arc_from_opt (previ, newi);
      CfgFirstLast (newi, Some newi)
  | FixmeStmt _ -> cfg_todo state previ stmt

and cfg_todo state previ stmt =
  let newi = state.g#add_node { F.n = F.NTodo stmt } in
  state.g |> add_arc_from_opt (previ, newi);
  CfgFirstLast (newi, Some newi)

and cfg_stmt_list lang state previ xs =
  let lasti_opt, labels =
    xs
    |> List.fold_left
         (fun (previ, labels) stmt ->
           (* We don't create special nodes for labels in the CFG; instead,
            * we assign them to the entry nodes of the labeled statements.
            *)
           match cfg_stmt lang state previ stmt with
           | CfgFirstLast (firsti, lasti) ->
               label_node state labels firsti;
               (lasti, [])
           | CfgLabel label -> (previ, label :: labels))
         (previ, [])
  in
  match labels with
  | l :: ls ->
      (* If we have labels at the end of our list of stmt, we create a dummy
       * node to assign them to. This happens when there are labels at the end
       * of a function's body, for example:
       *
       *     void foo(x)
       *     {
       *       if (x > 0) goto label;
       *       bar();
       *       label:
       *     }
       *
       * Such labels may be in the original sources, or they may be introduced
       * by the AST-to-IL translation.
       *)
      let dummyi = state.g#add_node { n = NOther (Noop "return") } in
      label_node state (l :: ls) dummyi;
      state.g |> add_arc_from_opt (lasti_opt, dummyi);
      Some dummyi
  | _ -> lasti_opt

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

and (cfg_of_stmts : Lang.t -> stmt list -> F.cfg) =
 fun lang xs ->
  (* yes, I sometimes use objects, and even mutable objects in OCaml ... *)
  let g = new Ograph_extended.ograph_mutable in

  let enteri = g#add_node { F.n = F.Enter } in
  let exiti = g#add_node { F.n = F.Exit } in

  let newi = enteri in

  let state =
    {
      g;
      enteri;
      exiti;
      labels = Hashtbl.create 2;
      gotos = ref [];
      try_catches_opt = None;
    }
  in
  let last_node_opt = cfg_stmt_list lang state (Some newi) xs in
  (* Must wait until all nodes have been labeled before resolving gotos. *)
  resolve_gotos state;
  (* maybe the body does not contain a single 'return', so by default
   * connect last stmt to the exit node
   *)
  g |> add_arc_from_opt (last_node_opt, exiti);
  CFG.make g enteri
