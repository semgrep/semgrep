(* Yoann Padioleau
 *
 * Copyright (C) 2009, 2010, 2011 Facebook
 * Copyright (C) 2019 r2c
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
open AST_generic
module Ast = AST_generic
module F = Controlflow
module H = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Information passed recursively in cfg_stmt or cfg_stmt_list below.
 * The graph g is mutable, so most of the work is done by side effects on it.
 * No need to return a new state.
 *)
type state = {
  g : F.flow;
  (* When there is a 'return' we need to know the exit node to link to *)
  exiti : F.nodei;
  (* Sometimes when there is a 'continue' or 'break' we must know where
   * to jump and so we must know the node index for the end of the loop.
   * The same kind of information is needed for 'switch' or 'try/throw'.
   *
   * Because loops can be inside switch or try, and vice versa, you need
   * a stack of context.
   *)
  ctx : context Common.stack; (* todo: labels for goto *)
}

and context =
  | NoCtx
  | LoopCtx of F.nodei (* head *) * F.nodei (* end *)
  | SwitchCtx of F.nodei (* end *)
  | TryCtx of F.nodei

(* the first catch *)

(* xleroy's error style *)
type error = error_kind * Parse_info.t option

and error_kind = NoEnclosingLoop | DynamicBreak

exception Error of error

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let add_arc (starti, nodei) g = g#add_arc ((starti, nodei), F.Direct)

let add_arc_opt (starti_opt, nodei) g =
  starti_opt
  |> Common.do_option (fun starti -> g#add_arc ((starti, nodei), F.Direct))

(*
 * When there is a 'break', 'continue', or 'throw', we need to look up in the
 * stack of contexts whether there is an appropriate one. In the case
 * of 'break/continue', because some languages allow statements like
 * 'break 2;', we also need to know how many upper contexts we need to
 * look for.
 *)
let (lookup_some_ctx :
      ?level:int ->
      ctx_filter:(context -> 'a option) ->
      context list ->
      'a option) =
 fun ?(level = 1) ~ctx_filter xs ->
  let rec aux depth xs =
    match xs with
    | [] -> None
    | x :: xs -> (
        match ctx_filter x with
        | None -> aux depth xs
        | Some a -> if depth = level then Some a else aux (depth + 1) xs )
  in
  aux 1 xs

let info_opt any =
  match Visitor_AST.ii_of_any any with [] -> None | x :: _xs -> Some x

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
let rec (cfg_stmt : state -> F.nodei option -> stmt -> F.nodei option) =
 fun state previ stmt ->
  let i () = info_opt (S stmt) in

  match stmt.s with
  | Label _ | Goto _ -> raise Todo
  | Block (_, stmts, _) -> cfg_stmt_list state previ stmts
  | For _ | While _ ->
      (* previ -> newi ---> newfakethen -> ... -> finalthen -
       *             |---|-----------------------------------|
       *                 |-> newfakelse
       *)
      let node, stmt =
        match stmt.s with
        | While (_, e, stmt) -> (F.WhileHeader e, stmt)
        | For (_, forheader, stmt) ->
            ( ( match forheader with
              | ForClassic _ -> raise Todo
              | ForEach (pat, _, e) -> F.ForeachHeader (pat, e)
              | ForEllipsis _ -> raise Todo
              | ForIn _ -> raise Todo ),
              stmt )
        | _ -> raise Impossible
      in

      let newi = state.g#add_node { F.n = node; i = i () } in
      state.g |> add_arc_opt (previ, newi);

      let newfakethen = state.g#add_node { F.n = F.TrueNode; i = None } in
      let newfakeelse = state.g#add_node { F.n = F.FalseNode; i = None } in
      state.g |> add_arc (newi, newfakethen);
      state.g |> add_arc (newi, newfakeelse);

      let state =
        { state with ctx = LoopCtx (newi, newfakeelse) :: state.ctx }
      in
      let finalthen = cfg_stmt state (Some newfakethen) stmt in
      state.g |> add_arc_opt (finalthen, newi);
      Some newfakeelse
  (* this was a tentative by jiao to work with dataflow_php.ml but it
     has some regression so I've commented it out
     | While (t1, e, colon_stmt) ->
       (* previ -> newi ---> newfakethen -> ... -> finalthen
        *             |--|---------------------------------|
        *                |-> newfakelse -> <rest>
        *)
         let node = F.WhileHeader (Ast.unparen e) in

         let newi = state.g#add_node { F.n = node; i=i() } in
         state.g |> add_arc_opt (previ, newi);

         let newfakethen = state.g#add_node { F.n = F.TrueNode;i=None } in
         let newfakeelse = state.g#add_node { F.n = F.FalseNode;i=None } in
         state.g |> add_arc (newi, newfakethen);
         state.g |> add_arc (newi, newfakeelse);

         let state = { state with
           ctx = LoopCtx (newi, newfakeelse)::state.ctx;
         }
         in
         let finalthen = cfg_colon_stmt state (Some newfakethen) colon_stmt in
         (* let's loop *)
         state.g |> add_arc_opt (finalthen, newi);
         Some newfakeelse

     | For (t1, t2, e1, t3, e2, t4, e5, t6, colon_stmt) ->
       (* previ -> e1i ->newi -> e2i --> newfakethen -> ... -> finalthen -> e5i
        *                  |--------|----------------------------------------|
        *                           |-> newfakelse -> <rest>
        *)
         let exprs = Ast.uncomma e1 in
         let e1i = List.fold_left (cfg_expr state F.SpecialMaybeUnused)
           previ exprs in

         let node = F.ForHeader in
         let newi = state.g#add_node { F.n = node; i=i() } in
         state.g |> add_arc_opt (e1i, newi);

         let exprs = Ast.uncomma e2 in
         let e2i = List.fold_left (cfg_expr state F.Normal)
           (Some newi) exprs in

         let newfakethen = state.g#add_node { F.n = F.TrueNode;i=None } in
         let newfakeelse = state.g#add_node { F.n = F.FalseNode;i=None } in
         state.g |> add_arc_opt (e2i, newfakethen);
         state.g |> add_arc_opt (e2i, newfakeelse);

         (* todo: the head should not be newi but the node just before
          * the increment, see tests/php/controlflow/continue_for.php
          *)
         let state = { state with
           ctx = LoopCtx (newi, newfakeelse)::state.ctx;
         }
         in
         let finalthen = cfg_colon_stmt state (Some newfakethen) colon_stmt in

         let exprs = Ast.uncomma e5 in
         let e5i = List.fold_left (cfg_expr state F.Normal) finalthen exprs in

         state.g |> add_arc_opt (e5i, newi);
         Some newfakeelse

     | Foreach (t1, t2, e1, t3, v_arrow_opt, t4, colon_stmt) ->
       (* previ -> e1i ->newi ---> newfakethen -> ... -> finalthen
        *                  |---|----------------------------------|
        *                      |-> newfakelse -> <rest>
        *)
         let e1i = cfg_expr state F.Normal previ e1 in

         let names =
           match v_arrow_opt with
           | ForeachVar var -> [var]
           | ForeachArrow (var1, _, var2) ->
             [var1;var2]
           | ForeachList (_, xs) ->
             failwith "Warning: list foreach"
         in
         let node = F.ForeachHeader names in
         let newi = state.g#add_node { F.n = node; i=i() } in
         state.g |> add_arc_opt (e1i, newi);

         let newfakethen = state.g#add_node { F.n = F.TrueNode;i=None } in
         let newfakeelse = state.g#add_node { F.n = F.FalseNode;i=None } in
         state.g |> add_arc (newi, newfakethen);
         state.g |> add_arc (newi, newfakeelse);

         let state = { state with
           ctx = LoopCtx (newi, newfakeelse)::state.ctx;
         }
         in
         let finalthen =
           cfg_colon_stmt state (Some newfakethen) colon_stmt
         in
         state.g |> add_arc_opt (finalthen, newi);
         Some newfakeelse
  *)
  (* This time, we may return None, for instance if return in body of dowhile
   * (whereas While can't return None). But if we return None, certainly
   * sign of buggy code.
   *)
  | DoWhile (_, st, e) -> (
      (* previ -> doi ---> ... ---> finalthen (opt) ---> taili
       *          |--------- newfakethen ----------------| |-> newfakelse <rest>
       *)
      let doi = state.g#add_node { F.n = F.DoHeader; i = i () } in
      state.g |> add_arc_opt (previ, doi);

      let taili = state.g#add_node { F.n = F.DoWhileTail e; i = None } in
      let newfakethen = state.g#add_node { F.n = F.TrueNode; i = None } in
      let newfakeelse = state.g#add_node { F.n = F.FalseNode; i = None } in
      state.g |> add_arc (taili, newfakethen);
      state.g |> add_arc (taili, newfakeelse);
      state.g |> add_arc (newfakethen, doi);

      let state =
        { state with ctx = LoopCtx (taili, newfakeelse) :: state.ctx }
      in
      let finalthen = cfg_stmt state (Some doi) st in
      match finalthen with
      | None ->
          (* weird, probably wrong code *)
          None
      | Some finalthen ->
          state.g |> add_arc (finalthen, taili);
          Some newfakeelse )
  | If (_, e, st_then, st_else) -> (
      (* previ -> newi --->  newfakethen -> ... -> finalthen --> lasti -> <rest>
       *                |                                     |
       *                |->  newfakeelse -> ... -> finalelse -|
       *
       * Can generate either special nodes for elseif, or just consider
       * elseif as syntactic sugar that translates into regular ifs, which
       * is what I do for now.
       * The lasti can be a Join when there is no return in either branch.
       *)
      let newi = state.g#add_node { F.n = F.IfHeader e; i = i () } in
      state.g |> add_arc_opt (previ, newi);

      let newfakethen = state.g#add_node { F.n = F.TrueNode; i = None } in
      let newfakeelse = state.g#add_node { F.n = F.FalseNode; i = None } in
      state.g |> add_arc (newi, newfakethen);
      state.g |> add_arc (newi, newfakeelse);

      let finalthen = cfg_stmt state (Some newfakethen) st_then in
      let finalelse =
        match st_else with
        | None -> None
        | Some st -> cfg_stmt state (Some newfakeelse) st
      in

      match (finalthen, finalelse) with
      | None, None ->
          (* probably a return in both branches *)
          None
      | Some nodei, None | None, Some nodei -> Some nodei
      | Some n1, Some n2 ->
          let lasti = state.g#add_node { F.n = F.Join; i = None } in
          state.g |> add_arc (n1, lasti);
          state.g |> add_arc (n2, lasti);
          Some lasti )
  | Return (_, e, _) ->
      let newi = state.g#add_node { F.n = F.Return e; i = i () } in
      state.g |> add_arc_opt (previ, newi);
      state.g |> add_arc (newi, state.exiti);
      (* the next statement if there is one will not be linked to
       * this new node *)
      None
  | Continue (_, _TODOlabelid, _) | Break (_, _TODOlabelid, _) ->
      let is_continue, node =
        match stmt.s with
        | Continue _ -> (true, F.Continue)
        | Break _ -> (false, F.Break)
        | _ -> raise Impossible
      in

      (*
       let depth =
         match e with
         | None -> 1
         | Some e ->
             (match intvalue_of_expr e with
             | Some i -> i
             | None ->
                 (* a dynamic variable ? *)
                 raise (Error (DynamicBreak, t1))
             )
       in
*)
      let newi = state.g#add_node { F.n = node; i = i () } in
      state.g |> add_arc_opt (previ, newi);

      let nodei_to_jump_to =
        state.ctx
        |> lookup_some_ctx ~level:1 ~ctx_filter:(function
             | LoopCtx (headi, endi) ->
                 if is_continue then Some headi else Some endi
             | SwitchCtx endi ->
                 (* it's ugly but PHP allows to 'continue' inside 'switch' (even
                  * when the switch is not inside a loop) in which case
                  * it has the same semantic than 'break'.
                  *)
                 Some endi
             | TryCtx _ | NoCtx -> None)
      in
      ( match nodei_to_jump_to with
      | Some nodei -> state.g |> add_arc (newi, nodei)
      | None -> raise (Error (NoEnclosingLoop, i ())) );
      None
  | Switch (_, e, cases_and_body) ->
      let newi = state.g#add_node { F.n = F.SwitchHeader e; i = i () } in
      state.g |> add_arc_opt (previ, newi);

      (* note that if all cases have return, then we will remove
       * this endswitch node later.
       *)
      let endi = state.g#add_node { F.n = F.SwitchEnd; i = None } in

      (* if no default: then must add path from start to end directly
       * todo? except if the cases cover the full spectrum ?
       *)
      if
        not
          ( cases_and_body
          |> List.exists (function
               | CasesAndBody (cases, _body) ->
                   cases
                   |> List.exists (function
                        | Ast.Default _ -> true
                        | _ -> false)
               | CaseEllipsis _ -> raise Impossible) )
      then state.g |> add_arc (newi, endi);
      (* let's process all cases *)
      let last_stmt_opt = cfg_cases (newi, endi) state None cases_and_body in
      state.g |> add_arc_opt (last_stmt_opt, endi);

      (* remove endi if for instance all branches contained a return *)
      if (state.g#predecessors endi)#null then (
        (* coupling: make sure Dataflow.new_node_array handle that case *)
        state.g#del_node endi;
        None )
      else Some endi
  (*
   * Handling try part 1. See the case for Throw below and the
   * cfg_catches function for the second part.
   *
   * Any function call in the body of the try could potentially raise
   * an exception, so should we add edges to the catch nodes ?
   * In the same way any function call could potentially raise
   * a divide by zero or call exit().
   * For now we don't add all those edges. We do it only for explicit throw.
   *
   * todo? Maybe later the CFG could be extended with information
   * computed by a global bottom-up analysis (so that we would add certain
   * edges)
   *
   * todo? Maybe better to just add edges for all the nodes in the body
   * of the try to all the catches ?
   *
   * So for now, we mostly consider catches as a serie of elseifs,
   * and add some goto to be conservative at a few places. For instance
   *
   *   try {
   *     ...;
   *   } catch (E1 $x) {
   *     throw $x;
   *   } catch (E2 $x) {
   *     ...
   *   }
   *   ...
   *
   * is rougly considered as this code:
   *
   *   <tryheader> {
   *    if(true) goto catchstart;
   *    else {
   *      ...;
   *      goto tryend;
   *    }
   *   }
   *   <catchstart>
   *   if (is E1) {
   *     goto exit; /* or next handler if nested try */
   *   } elseif (is E2) {
   *     ...
   *     goto tryend;
   *   } else {
   *     goto exit; /* or next handler if nested try */
   *   }
   *
   *   <tryend>
   *)
  | Try (_, body, catches, _finallys) ->
      (* TODO Task #3622443: Update the logic below to account for "finally"
         clauses *)
      let newi = state.g#add_node { F.n = F.TryHeader; i = i () } in
      let catchi = state.g#add_node { F.n = F.CatchStart; i = None } in
      state.g |> add_arc_opt (previ, newi);

      (* may have to delete it later if nobody connected to it *)
      let endi = state.g#add_node { F.n = F.TryEnd; i = None } in

      (* for now we add a direct edge between the try and catch,
       * as even the first statement in the body of the try could
       * be a function raising internally an exception.
       *
       * I just don't want certain analysis like the deadcode-path
       * to report that the code in catch are never executed. I want
       * the catch nodes to have at least one parent. So I am
       * kind of conservative.
       *)
      state.g |> add_arc (newi, catchi);

      let state' = { state with ctx = TryCtx catchi :: state.ctx } in

      let last_stmt_opt = cfg_stmt state' (Some newi) body in
      state.g |> add_arc_opt (last_stmt_opt, endi);

      (* note that we use state, not state' here, as we want the possible
       * throws inside catches to be themselves link to a possible surrounding
       * try.
       *)
      let last_false_node = cfg_catches state catchi endi catches in

      (* we want to connect the end of the catch list with
       * the next handler, if try are nested, or to the exit if
       * there is no more handler in this context
       *)
      let nodei_to_jump_to =
        state.ctx
        |> lookup_some_ctx ~ctx_filter:(function
             | TryCtx nextcatchi -> Some nextcatchi
             | LoopCtx _ | SwitchCtx _ | NoCtx -> None)
      in
      ( match nodei_to_jump_to with
      | Some nextcatchi -> state.g |> add_arc (last_false_node, nextcatchi)
      | None -> state.g |> add_arc (last_false_node, state.exiti) );

      (* if nobody connected to endi erase the node. For instance
       * if have only return in the try body.
       *)
      if (state.g#predecessors endi)#null then (
        state.g#del_node endi;
        None )
      else Some endi
  (*
   * For now we don't do any fancy analysis to statically detect
   * which exn handler a throw should go to. The argument of throw can
   * be static as in 'throw new ExnXXX' but it could also be dynamic. So for
   * now we just branch to the first catch and make edges between
   * the different catches in cfg_catches below
   * (which is probably what is done at runtime by the PHP interpreter).
   *
   * todo? Again maybe later the CFG could be sharpened with
   * path sensitive analysis to be more precise (so that we would remove
   * certain edges)
   *)
  | Throw (_, e, _) ->
      let newi = state.g#add_node { F.n = F.Throw e; i = i () } in
      state.g |> add_arc_opt (previ, newi);

      let nodei_to_jump_to =
        state.ctx
        |> lookup_some_ctx ~ctx_filter:(function
             | TryCtx catchi -> Some catchi
             | LoopCtx _ | SwitchCtx _ | NoCtx -> None)
      in
      ( match nodei_to_jump_to with
      | Some catchi -> state.g |> add_arc (newi, catchi)
      | None ->
          (* no enclosing handler, branch to exit node of the function *)
          state.g |> add_arc (newi, state.exiti) );
      None
  (* TODO? should create a OtherStmtWithStmtFooter and arc to it? *)
  | OtherStmtWithStmt (op, eopt, st) ->
      let header = F.OtherStmtWithStmtHeader (op, eopt) in
      let newi = state.g#add_node { F.n = header; i = i () } in
      state.g |> add_arc_opt (previ, newi);
      cfg_stmt state (Some newi) st
  | WithUsingResource (_, stmts1, stmts2) ->
      cfg_stmt_list state previ [ stmts1; stmts2 ]
  (* for dataflow purpose, such definitions are really the same than
   * an assignment. Liveness analysis does not make any difference between
   * a definition and an assign.
   *)
  | DefStmt (ent, VarDef def) ->
      cfg_simple_node state previ (exprstmt (H.vardef_to_assign (ent, def)))
  (* just to factorize code, a nested func is really like a lambda *)
  | DefStmt (ent, FuncDef def) ->
      let resolved = Some (Local, Ast.sid_TODO) in
      cfg_simple_node state previ
        (exprstmt (H.funcdef_to_lambda (ent, def) resolved))
  (* TODO: we should process lambdas! and generate an arc to its
   * entry that then go back here! After all most lambdas are used for
   * callbacks and they sure can be called just after they have been
   * defined. It would be better to exactly determine when, but as a first
   * approximation we can at least create an arc! This could find
   * tainting-related bugs that occur inside a single function, even
   * if the source is in the function and the sink in the callback!
   * (see daghan example in js-permissions slides on xhr.open).
   * Note that DefStmt are not the only form of lambdas ... you can have
   * lambdas inside expressions too! (need a proper instr type really)
   *)
  | DefStmt _ | ExprStmt _ | Assert _ | DirectiveStmt _ | OtherStmt _ ->
      cfg_simple_node state previ stmt
  | DisjStmt _ -> raise Impossible

(* only in sgrep context in a pattern *)
and cfg_stmt_list state previ xs =
  xs |> List.fold_left (fun previ stmt -> cfg_stmt state previ stmt) previ

(*
 * Creating the CFG nodes and edges for the cases of a switch.
 *
 * PHP allows to write code like  case X: case Y: ... This is
 * parsed as a [Case (X, []); Case (Y, ...)] which means
 * the statement list of the X case is empty. In this situation we just
 * want to link the node for X directly to the node for Y.
 *
 * So cfg_cases works like cfg_stmt by optionally taking the index of
 * the previous node (here for instance the node of X), and optionally
 * returning a node (if the case contains a break, then this will be
 * None)
 *)
and (cfg_cases :
      F.nodei * F.nodei ->
      state ->
      F.nodei option ->
      Ast.case_and_body list ->
      F.nodei option) =
 fun (switchi, endswitchi) state previ cases ->
  let state = { state with ctx = SwitchCtx endswitchi :: state.ctx } in

  cases
  |> List.fold_left
       (fun previ case_and_body ->
         match case_and_body with
         | CasesAndBody (cases, stmt) ->
             let node =
               (* TODO: attach expressions there!!! *)
               match cases with [ Default _ ] -> F.Default | _ -> F.Case
             in

             let i () = info_opt (S stmt) in

             let newi = state.g#add_node { F.n = node; i = i () } in
             state.g |> add_arc_opt (previ, newi);
             (* connect SwitchHeader to Case node *)
             state.g |> add_arc (switchi, newi);

             (* the stmts can contain 'break' that will be linked to the endswitch *)
             cfg_stmt state (Some newi) stmt
         | CaseEllipsis _ -> raise Impossible)
       previ

(*
 * Creating the CFG nodes and edges for the catches of a try.
 *
 * We will conside catch(Exn $e) as a kind of if, with a TrueNode for
 * the case the thrown exn matched the specified class,
 * and FalseNode otherwise.
 *
 * cfg_catches takes the nodei of the previous catch nodes (or false node
 * of the previous catch node), process the catch body, and return
 * a new False Node.
 *)
and (cfg_catches : state -> F.nodei -> F.nodei -> Ast.catch list -> F.nodei) =
 fun state previ tryendi catches ->
  catches
  |> List.fold_left
       (fun previ catch ->
         let _t, _pattern, stmt = catch in

         let i () = info_opt (S stmt) in

         let newi = state.g#add_node { F.n = F.Catch; i = i () } in
         state.g |> add_arc (previ, newi);
         (*
     let ei = cfg_var_def state (Some newi) name in
*)
         let ei = Some newi in

         let truei = state.g#add_node { F.n = F.TrueNode; i = None } in
         let falsei = state.g#add_node { F.n = F.FalseNode; i = None } in

         state.g |> add_arc_opt (ei, truei);
         state.g |> add_arc_opt (ei, falsei);

         (* the stmts can contain 'throw' that will be linked to an upper try or
          * exit node *)
         let last_stmt_opt = cfg_stmt state (Some truei) stmt in
         state.g |> add_arc_opt (last_stmt_opt, tryendi);

         (* we chain the catches together, like elseifs *)
         falsei)
       previ

(*
 * Creating the CFG nodes and edges for the simple nodes.
 *)
and cfg_simple_node state previ stmt =
  let i = info_opt (S stmt) in

  let simple_node =
    match F.simple_node_of_stmt_opt stmt with
    | Some x -> x
    | None -> raise Impossible
    (* see caller of cfg_simple_node *)
  in
  let newi = state.g#add_node { F.n = F.SimpleNode simple_node; i } in
  state.g |> add_arc_opt (previ, newi);
  Some newi

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let (control_flow_graph_of_stmts : parameter list -> stmt list -> F.flow) =
 fun params xs ->
  (* yes, I sometimes use objects, and even mutable objects in OCaml ... *)
  let g = new Ograph_extended.ograph_mutable in

  let enteri = g#add_node { F.n = F.Enter; i = None } in
  let exiti = g#add_node { F.n = F.Exit; i = None } in

  let newi =
    params
    |> List.fold_left
         (fun previ param ->
           let node_kind = F.SimpleNode (F.Parameter param) in
           let i = info_opt (Pa param) in
           let parami = g#add_node { F.n = node_kind; i } in
           g |> add_arc (previ, parami);
           parami)
         enteri
  in

  let state =
    {
      g;
      exiti;
      ctx = [ NoCtx ] (* could also remove NoCtx and use an empty list *);
    }
  in
  let last_node_opt = cfg_stmt_list state (Some newi) xs in
  (* maybe the body does not contain a single 'return', so by default
   * connect last stmt to the exit node
   *)
  g |> add_arc_opt (last_node_opt, exiti);
  g

let (cfg_of_func : function_definition -> F.flow) =
 fun def ->
  let params = def.fparams in
  (* less: could create a node with function name ? *)
  control_flow_graph_of_stmts params [ def.fbody ]

(* alias *)
let cfg_of_stmts = control_flow_graph_of_stmts

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

let string_of_error_kind error_kind =
  match error_kind with
  | NoEnclosingLoop -> "No enclosing loop found for break or continue"
  | DynamicBreak -> "Dynamic break/continue are not supported"

(* note that the output is emacs compile-mode compliant *)
let string_of_error (error_kind, info) =
  match info with
  | None -> spf "NOLOC: FLOW %s" (string_of_error_kind error_kind)
  | Some info ->
      let info = Parse_info.token_location_of_info info in
      spf "%s:%d:%d: FLOW %s" info.Parse_info.file info.Parse_info.line
        info.Parse_info.column
        (string_of_error_kind error_kind)

let (report_error : error -> unit) = fun err -> pr2 (string_of_error err)
