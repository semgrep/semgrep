(* Yoann Padioleau
 *
 * Copyright (C) 2009, 2010, 2011 Facebook
 * Copyright (C) 2020 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
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

let logger = Logging.get_logger [ __MODULE__ ]

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
  (* When there is a 'return' we need to know the exit node to link to *)
  exiti : F.nodei;
  (* Attaches labels to nodes. *)
  labels : (label_key, F.nodei) Hashtbl.t;
  (* Gotos pending to be resolved, a list of Goto nodes and the label
   * to which they are jumping. *)
  gotos : (nodei * label_key) list ref;
  (* If we are inside a Try, this is the start of the handlers. *)
  try_catches_opt : F.nodei option;
  (* Lambdas are always assigned to a variable in IL, this table records the
   * name-to-lambda mapping. Whenever a lambda is fetched, we look it up here
   * and we generate the lambda's CFG right at the use site.
   *
   * Why at the use site? So that the fixpoint function will visit them in the
   * right order. Then we can propagate taint from e.g. an object receiving
   * a method call, to a lambda being passed to that method. Previously, we
   * always inserted the lambdas CFGs preceding their use, so taint propagation
   * could not happen.
   *)
  lambdas : (name, IL.function_definition) Hashtbl.t;
  (* If a lambda is never used, we just insert its CFG at declaration site. *)
  unused_lambdas : (name, nodei * nodei) Hashtbl.t;
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

let lval_is_lambda state l =
  match l with
  | { base = Var name; rev_offset = [] } ->
      let* fdef = Hashtbl.find_opt state.lambdas name in
      Some (name, fdef)
  | { base = Var _ | VarSpecial _ | Mem _; rev_offset = _ } ->
      (* Lambdas are only assigned to plain variables without any offset. *)
      None

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
      let new_ = F.NInstr x in
      let newi = state.g#add_node { F.n = new_ } in
      state.g |> add_arc_from_opt (previ, newi);
      let lasti =
        match x.i with
        | Call _ -> (
            (* If we are inside a try-catch, we consider the possibility of this call
               * raising an exception, then we add a jump to catch-blocks. This could
               * lead to some false positives when running taint rules (since it's a
               * may-analysis), but they are probably rare. For constant propagation
               * this should reduce false positives (since it's a must-analysis).
               * Ideally we should have a preceeding analysis that infers which calls
               * may (or may not) raise exceptions. *)
            state.g |> add_arc_opt_to_opt (Some newi, state.try_catches_opt);
            match build_cfg_for_lambdas_in state newi new_ with
            | Some lasti -> lasti
            | None -> newi)
        | AssignAnon ({ base = Var name; rev_offset = [] }, Lambda fdef) ->
            (* Just in case the lambda CFG needs be inserted here later on (if the
             * lambda is never dereferenced) then we have to insert a JOIN node here,
             * see cfg_lambda. *)
            let lasti = state.g#add_node { F.n = F.Join } in
            state.g |> add_arc (newi, lasti);
            Hashtbl.add state.lambdas name fdef;
            Hashtbl.add state.unused_lambdas name (newi, lasti);
            lasti
        | __else__ -> newi
      in
      CfgFirstLast (newi, Some lasti)
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

      let finalthen = cfg_stmt_list state (Some newfakethen) st1 in
      let finalelse = cfg_stmt_list state (Some newfakeelse) st2 in

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

      let finalthen = cfg_stmt_list state (Some newfakethen) st in
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
      let finaltry = cfg_stmt_list state' (Some newi) try_st in
      state.g |> add_arc_from_opt (finaltry, catchesi);
      let newfakefinally = state.g#add_node { F.n = NOther (Noop "finally") } in
      state.g |> add_arc (catchesi, newfakefinally);
      catches
      |> List.iter (fun (_, catch_st) ->
             let finalcatch = cfg_stmt_list state (Some catchesi) catch_st in
             state.g |> add_arc_from_opt (finalcatch, newfakefinally));
      let finalfinally = cfg_stmt_list state (Some newfakefinally) finally_st in
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
  | MiscStmt x ->
      let newi = state.g#add_node { F.n = F.NOther x } in
      state.g |> add_arc_from_opt (previ, newi);
      CfgFirstLast (newi, Some newi)
  | FixmeStmt _ -> cfg_todo state previ stmt

and cfg_lambda state previ joini fdef =
  (* Lambdas are treated as statement blocks, the CFG does NOT capture the actual
     * flow of data that goes into the lambda through its parameters, and back
     * into the surrounding definition through its `return' statement. We won't
     * do inter-procedural analysis here, it is a DeepSemgrep thing.
     *
     * previ -> NLambda params -> ... (lambda body) -> finallambda -> lasti
     *
     * alt: We could inline lambdas perhaps?
  *)
  let newi = state.g#add_node { F.n = NLambda fdef.fparams } in
  state.g |> add_arc (previ, newi);
  let finallambda =
    cfg_stmt_list { state with exiti = joini } (Some newi) fdef.fbody
  in
  state.g |> add_arc_from_opt (finallambda, joini)

and build_cfg_for_lambdas_in state previ n =
  (* We look for all lambdas being fetched within node `n`, and insert their CFGs
   * right there.
   *
   * THINK: Where do we need to call this? Clearly we need to check `NInstr` nodes
   *   since lambdas will typically be dereferenced in `Call` instructions, either
   *   as the function being called, or as arguments to a higher-order function. *)
  let lambda_names, lambda_fdefs =
    IL_helpers.rlvals_of_node n
    |> Common.map_filter (lval_is_lambda state)
    |> List.split
  in
  if lambda_fdefs <> [] then (
    (* We translate the set of lambdas used in the node as like an IF-ELSEIF-ELSE,
     * block, with a fall-through case. This is meant to reduce FPs, but we should
     * revisit it if we are proven wrong. Note that we insert the lambda's CFG at
     * the site where they occur in the code, but they are not necessarily being
     * run there (often they are arguments to high-order functions). If the node
     * has multiple lambdas, it will typically be a call to a function that takes
     * multiple lambdas as arguments.
     *
     * previ ->  (lambda 1)     /-> lasti
     *       \-> ...         ->/
     *       \-> (lambda N) ->/
     *       \_______________/
     *)
    let lasti = state.g#add_node { F.n = F.Join } in
    state.g |> add_arc (previ, lasti);
    lambda_fdefs |> List.iter (fun fdef -> cfg_lambda state previ lasti fdef);
    lambda_names
    |> List.iter (fun name -> Hashtbl.remove state.unused_lambdas name);
    Some lasti)
  else None

and cfg_todo state previ stmt =
  let newi = state.g#add_node { F.n = F.NTodo stmt } in
  state.g |> add_arc_from_opt (previ, newi);
  CfgFirstLast (newi, Some newi)

and cfg_stmt_list state previ xs =
  let lasti_opt, labels =
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
  | [] -> lasti_opt

let build_cfg_of_unused_lambdas state =
  (* For those lambdas that are not dereferenced, we insert their CFG
   * at the declaration site. *)
  state.unused_lambdas
  |> Hashtbl.iter (fun name (starti, lasti) ->
         match Hashtbl.find_opt state.lambdas name with
         | None ->
             logger#error "Cannot find the definition of a lambda";
             ()
         | Some fdef -> cfg_lambda state starti lasti fdef);
  Hashtbl.clear state.unused_lambdas

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

  let state =
    {
      g;
      exiti;
      labels = Hashtbl.create 10;
      gotos = ref [];
      try_catches_opt = None;
      lambdas = Hashtbl.create 10;
      unused_lambdas = Hashtbl.create 10;
    }
  in
  let last_node_opt = cfg_stmt_list state (Some newi) xs in
  (* Must wait until all nodes have been labeled before resolving gotos. *)
  resolve_gotos state;
  build_cfg_of_unused_lambdas state;
  (* maybe the body does not contain a single 'return', so by default
   * connect last stmt to the exit node
   *)
  g |> add_arc_from_opt (last_node_opt, exiti);
  CFG.make g enteri exiti
