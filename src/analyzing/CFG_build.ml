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
module Log = Log_analyzing.Log
module F = IL (* to be even more similar to controlflow_build.ml *)
module G = AST_generic

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

let tags = Logs_.create_tags [ "CFG_build" ]

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
  opt_tok : Tok.t option;
  (* An optional token to point to the function/entity for which we are
   * constructing a CFG. *)
  g : (F.node, F.edge) Ograph_extended.ograph_mutable;
  (* When there is a 'return' we need to know the exit node to link to *)
  exiti : F.nodei;
  (* Attaches labels to nodes. *)
  labels : (label_key, F.nodei) Hashtbl.t;
  (* Gotos pending to be resolved, a list of Goto nodes and the label
   * to which they are jumping. *)
  gotos : (nodei * label_key) list ref;
  (* Destination node that a throw node should go to.
   * Alt: we could make this non-optional and always add an arc to the
   * exit node when an exception is thrown outside a try statement,
   * but this may cause too many arcs in the CFG because all function
   * calls would then have this arc. This can causes performance issues
   * and there is no significant benefit of having these extra arcs.
   *)
  throw_destination : F.nodei option;
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
  (* If a lambda is never used, we just insert its CFG at the end of
     its parent function. *)
  unused_lambdas : (name, unit) Hashtbl.t;
}

type fdef_cfg = { fparams : name list; fcfg : cfg }

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
             let loc_str =
               match state.opt_tok with
               | None -> ""
               | Some tok -> spf " (%s)" (Tok.stringpos_of_tok tok)
             in
             Log.warn (fun m ->
                 m ~tags "Could not resolve label: %s%s" (fst label_key) loc_str)
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
  (* The tuple includes
   * 1) the first (entry),
   * 2) the last (exit) node of the created CFG, and
   * 3) an indicator whether there may be a Throw nested in the stmt.
   *
   * Last node is optional; it is None when the execution will not
   * continue (return), or when it may continue with a different
   * statement than the subsequent one (goto).
   *)
  | CfgFirstLast of F.nodei * F.nodei option * bool

let rec cfg_stmt : state -> F.nodei option -> stmt -> cfg_stmt_result =
 fun state previ stmt ->
  match stmt.s with
  | Instr x ->
      let new_ = F.NInstr x in
      let newi = state.g#add_node (IL.mk_node new_) in
      state.g |> add_arc_from_opt (previ, newi);
      let lasti, throws =
        match x.i with
        | New _
        | Call _ -> (
            (* If we are inside a try-catch, we consider the possibility of this call
               * raising an exception, then we add a jump to catch-blocks. This could
               * lead to some false positives when running taint rules (since it's a
               * may-analysis), but they are probably rare. For constant propagation
               * this should reduce false positives (since it's a must-analysis).
               * Ideally we should have a preceeding analysis that infers which calls
               * may (or may not) raise exceptions. *)
            state.g |> add_arc_opt_to_opt (Some newi, state.throw_destination);
            match build_cfg_for_lambdas_in state (Some newi) new_ with
            | Some lasti -> (lasti, true)
            | None -> (newi, true))
        | AssignAnon ({ base = Var name; rev_offset = [] }, Lambda fdef) ->
            (* Just in case the lambda CFG needs be inserted here later on (if the
             * lambda is never dereferenced) then we have to insert a JOIN node here,
             * see cfg_lambda. *)
            let lasti = state.g#add_node (IL.mk_node F.Join) in
            state.g |> add_arc (newi, lasti);
            Hashtbl.add state.lambdas name fdef;
            Hashtbl.add state.unused_lambdas name ();
            (lasti, false)
        | __else__ -> (newi, false)
      in
      CfgFirstLast (newi, Some lasti, throws)
  | If (tok, e, st1, st2) -> (
      (* previ -> newi --->  newfakethen -> ... -> finalthen --> lasti -> <rest>
       *                |                                     |
       *                |->  newfakeelse -> ... -> finalelse -|
       *
       * The lasti can be a Join when there is no return in either branch.
       *)
      let newi = state.g#add_node (IL.mk_node (F.NCond (tok, e))) in
      state.g |> add_arc_from_opt (previ, newi);

      let newfakethen = state.g#add_node (IL.mk_node (F.TrueNode e)) in
      let newfakeelse = state.g#add_node (IL.mk_node (F.FalseNode e)) in
      state.g |> add_arc (newi, newfakethen);
      state.g |> add_arc (newi, newfakeelse);

      let finalthen, then_throws = cfg_stmt_list state (Some newfakethen) st1 in
      let finalelse, else_throws = cfg_stmt_list state (Some newfakeelse) st2 in
      let throws = then_throws || else_throws in
      match (finalthen, finalelse) with
      | None, None ->
          (* probably a return in both branches *)
          CfgFirstLast (newi, None, throws)
      | Some nodei, None
      | None, Some nodei ->
          CfgFirstLast (newi, Some nodei, throws)
      | Some n1, Some n2 ->
          let lasti = state.g#add_node (IL.mk_node F.Join) in
          state.g |> add_arc (n1, lasti);
          state.g |> add_arc (n2, lasti);
          CfgFirstLast (newi, Some lasti, throws))
  | Loop (tok, e, st) ->
      (* previ -> newi ---> newfakethen -> ... -> finalthen -
       *             |---|-----------------------------------|
       *                 |-> newfakelse
       *)
      let newi = state.g#add_node (IL.mk_node (NCond (tok, e))) in
      state.g |> add_arc_from_opt (previ, newi);

      let newfakethen = state.g#add_node (IL.mk_node (F.TrueNode e)) in
      let newfakeelse = state.g#add_node (IL.mk_node (F.FalseNode e)) in
      state.g |> add_arc (newi, newfakethen);
      state.g |> add_arc (newi, newfakeelse);

      let finalthen, throws = cfg_stmt_list state (Some newfakethen) st in
      state.g |> add_arc_from_opt (finalthen, newi);
      CfgFirstLast (newi, Some newfakeelse, throws)
  | Label label -> CfgLabel label
  | Goto (tok, label) ->
      let newi = state.g#add_node (IL.mk_node (F.NGoto (tok, label))) in
      state.g |> add_arc_from_opt (previ, newi);
      add_pending_goto state newi label;
      CfgFirstLast (newi, None, false)
  | Return (tok, e) ->
      let new_ = F.NReturn (tok, e) in
      let newi = state.g#add_node (IL.mk_node new_) in
      state.g |> add_arc_from_opt (previ, newi);
      let lasti =
        match build_cfg_for_lambdas_in state (Some newi) new_ with
        | Some lasti -> lasti
        | None -> newi
      in
      state.g |> add_arc (lasti, state.exiti);
      CfgFirstLast (lasti, None, false)
  | Try (try_st, catches, else_st, finally_st) ->
      (* previ ->
       * newi ->
       * try -> catchesi --> catch1 -|
       *                 |->  ...   -|
       *                 |-> catchN -|
       *                 |-----------|
       *     -> elsei    --> else    |-> newfakefinally -> finally
       *)
      let newi = state.g#add_node (IL.mk_node (NOther (Noop "try"))) in
      let catchesi = state.g#add_node (IL.mk_node (NOther (Noop "catch"))) in
      let elsei = state.g#add_node (IL.mk_node (NOther (Noop "else"))) in
      let newfakefinally =
        state.g#add_node (IL.mk_node (NOther (Noop "finally")))
      in

      (* From prev to try. *)
      state.g |> add_arc_from_opt (previ, newi);

      (* Inside try may go to catches. *)
      let try_state = { state with throw_destination = Some catchesi } in
      let finaltry, try_may_throw =
        cfg_stmt_list try_state (Some newi) try_st
      in

      (* We do not directly connect the end of try with catchesi but whenever we
       * encounter a Call or a Throw inside a Try, we add an arc.
       * But if there is a path that can reach the end of try, we add an arc
       * to elsei. *)
      state.g |> add_arc_from_opt (finaltry, elsei);

      (* In the else clause, if an exception is thrown, go to the finally
       * clause, or if there is no finally clause, propagate the exception.
       *)
      let has_finally =
        match finally_st with
        | [] -> false
        | _some_list_ -> true
      in
      let throw_destination =
        if has_finally then Some newfakefinally
          (* If there is no `finally` then we throw to the inherited destination
           * (from an outer `try`) if any, or to the function's exit node otherwise. *)
        else Some (state.throw_destination ||| state.exiti)
      in
      let else_state = { state with throw_destination } in
      let finalelse, else_may_throw =
        cfg_stmt_list else_state (Some elsei) else_st
      in

      state.g |> add_arc_from_opt (finalelse, newfakefinally);

      (* In case of uncaught exceptions. *)
      state.g |> add_arc_opt_to_opt (Some catchesi, throw_destination);

      (* Same reasoning as the else clause above for the catch clauses. *)
      let catch_state = { state with throw_destination } in
      let catch_may_throw =
        catches
        |> List.fold_left
             (fun may_throw (_, catch_st) ->
               let finalcatch, throws =
                 cfg_stmt_list catch_state (Some catchesi) catch_st
               in
               state.g |> add_arc_from_opt (finalcatch, newfakefinally);
               may_throw || throws)
             false
      in

      (* Inside the finally clause, exceptions get propagated. *)
      let finally_state = state in
      let finalfinally, finally_may_throw =
        cfg_stmt_list finally_state (Some newfakefinally) finally_st
      in

      (* Also propagate any throws that may have happened inside this
       * try-catch-else-finally statement at the end of the finally clause.
       *)
      let may_throw =
        try_may_throw || else_may_throw || catch_may_throw || finally_may_throw
      in
      if may_throw then
        state.g |> add_arc_opt_to_opt (finalfinally, state.throw_destination);

      (* For now, just always assume dataflow may continue after the try-catch-else-finally
       * clause. It's possible that short-circuit the dataflow, but it may be tricky
       * to get it right.
       *)
      CfgFirstLast (newi, finalfinally, may_throw)
  | Throw (tok, e) ->
      let newi = state.g#add_node (IL.mk_node (F.NThrow (tok, e))) in
      state.g |> add_arc_from_opt (previ, newi);
      state.g |> add_arc_opt_to_opt (previ, state.throw_destination);
      CfgFirstLast (newi, None, true)
  | MiscStmt x ->
      let newi = state.g#add_node (IL.mk_node (F.NOther x)) in
      state.g |> add_arc_from_opt (previ, newi);
      CfgFirstLast (newi, Some newi, false)
  | FixmeStmt _ -> cfg_todo state previ stmt

and cfg_lambda state previ joini (fdef : IL.function_definition) =
  (* Lambdas are treated as statement blocks, the CFG does NOT capture the actual
     * flow of data that goes into the lambda through its parameters, and back
     * into the surrounding definition through its `return' statement. We won't
     * do inter-procedural analysis here, it is a DeepSemgrep thing.
     *
     * previ -> NLambda params -> ... (lambda body) -> finallambda -> lasti
     *
     * alt: We could inline lambdas perhaps?
  *)
  let newi = state.g#add_node (IL.mk_node (NLambda fdef.fparams)) in
  state.g |> add_arc_from_opt (previ, newi);
  let finallambda, _ignore_throws_in_lambda_ =
    cfg_stmt_list
      { state with throw_destination = None }
      (Some newi) fdef.IL.fbody
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
    |> List_.filter_map (lval_is_lambda state)
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
    let lasti = state.g#add_node (IL.mk_node F.Join) in
    state.g |> add_arc_from_opt (previ, lasti);
    lambda_fdefs |> List.iter (fun fdef -> cfg_lambda state previ lasti fdef);
    lambda_names
    |> List.iter (fun name -> Hashtbl.remove state.unused_lambdas name);
    Some lasti)
  else None

and cfg_todo state previ stmt =
  let newi = state.g#add_node (IL.mk_node (F.NTodo stmt)) in
  state.g |> add_arc_from_opt (previ, newi);
  CfgFirstLast (newi, Some newi, false)

and cfg_stmt_list state previ xs =
  let lasti_opt, labels, may_throw =
    xs
    |> List.fold_left
         (fun (previ, labels, may_throw) stmt ->
           (* We don't create special nodes for labels in the CFG; instead,
            * we assign them to the entry nodes of the labeled statements.
            *)
           match cfg_stmt state previ stmt with
           | CfgFirstLast (firsti, lasti, throws) ->
               label_node state labels firsti;
               (lasti, [], may_throw || throws)
           | CfgLabel label -> (previ, label :: labels, may_throw))
         (previ, [], false)
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
      let dummyi = state.g#add_node (IL.mk_node (NOther (Noop "return"))) in
      label_node state (l :: ls) dummyi;
      state.g |> add_arc_from_opt (lasti_opt, dummyi);
      (Some dummyi, may_throw)
  | [] -> (lasti_opt, may_throw)

let build_cfg_of_unused_lambdas state previ nexti =
  state.unused_lambdas
  |> Hashtbl.iter (fun name _ ->
         match Hashtbl.find_opt state.lambdas name with
         | None ->
             let tok = snd name.ident in
             Log.warn (fun m ->
                 m ~tags "Cannot find the definition of a lambda (%s)"
                   (Tok.stringpos_of_tok tok));
             ()
         | Some fdef -> cfg_lambda state previ nexti fdef);
  Hashtbl.clear state.unused_lambdas

(*****************************************************************************)
(* Marking nodes *)
(*****************************************************************************)

let mark_at_exit_nodes cfg =
  let rec loop nodei =
    let node = cfg.CFG.graph#nodes#find nodei in
    match node.n with
    (* Visit ancestor for exit, noop, goto, and join nodes. *)
    | Exit
    | NOther (Noop _)
    | NGoto _
    | Join ->
        CFG.predecessors cfg nodei |> List.iter (fun (predi, _) -> loop predi)
    (* These can be at-exit nodes. *)
    | NInstr _
    | NReturn _
    | NThrow _
    | NLambda _
    | NTodo _ ->
        node.at_exit <- true
    (* Whereas these cannot. *)
    | NOther _
    | NCond _
    | TrueNode _
    | FalseNode _
    | Enter ->
        ()
  in
  loop cfg.exit

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let (cfg_of_stmts : ?tok:Tok.t -> stmt list -> F.cfg) =
 fun ?tok xs ->
  (* yes, I sometimes use objects, and even mutable objects in OCaml ... *)
  let g = new Ograph_extended.ograph_mutable in

  let enteri = g#add_node (IL.mk_node F.Enter) in
  let exiti = g#add_node (IL.mk_node F.Exit) in

  let newi = enteri in

  let state =
    {
      opt_tok = tok;
      g;
      exiti;
      labels = Hashtbl.create 10;
      gotos = ref [];
      throw_destination = None;
      lambdas = Hashtbl.create 10;
      unused_lambdas = Hashtbl.create 10;
    }
  in
  let last_node_opt, _ignore_may_throw_ = cfg_stmt_list state (Some newi) xs in
  (* Must wait until all nodes have been labeled before resolving gotos. *)
  resolve_gotos state;
  (* Previously, we used to insert the CFGs of unused lambdas at the
     declaration site. However, this approach triggered some false
     positives. For example, consider the following code:
     ```
     void incorrect(int *p) {
       auto f1 = [&p]() {
         source(p);
       };
       auto f2 = [&p]() {
         sink(p);
       };
     }
     ```
     In this code, there's no actual control flow between the source
     and sink, and the lambdas are never even called. But when we
     inserted their CFGs at the declaration site, it incorrectly
     indicated a taint finding. To prevent these types of false
     positives while still scanning the body of unused lambdas, we now
     insert their CFGs in parallel at the end of their parent
     function, right after all other statements and just before the
     end node. *)
  build_cfg_of_unused_lambdas state last_node_opt exiti;
  (* maybe the body does not contain a single 'return', so by default
   * connect last stmt to the exit node
   *)
  g |> add_arc_from_opt (last_node_opt, exiti);
  CFG.make g enteri exiti

let cfg_of_fdef lang ?ctx fdef =
  let cfg_of_fdef () =
    let fparams, fstmts = AST_to_IL.function_definition lang ?ctx fdef in
    let fcfg = cfg_of_stmts ~tok:(snd fdef.fkind) fstmts in
    mark_at_exit_nodes fcfg;
    { fparams; fcfg }
  in

  if Implicit_return.lang_supports_implicit_return lang then (
    (* We need to build the CFG here first because the analysis
     * visits the CFG to determine returning nodes.
     *)
    (* TODO: Could we instead just transform the CFG directly ? *)
    let _fparams, fstmts = AST_to_IL.function_definition lang ?ctx fdef in
    Implicit_return.mark_implicit_return_nodes (cfg_of_stmts fstmts);

    (* Rebuild CFG after marking the return nodes, so that all
     * implicit returns become explicit.
     *)
    cfg_of_fdef ())
  else cfg_of_fdef ()
