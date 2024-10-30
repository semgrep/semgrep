open AST_generic
module Log = Log_analyzing.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Support for pattern-when, allowing for control flow-related comparisons.
 * This code is behind -path_sensitive flag which sets hook_path_sensitive below.
 *
 * Example usage:
 *
 * rules:
 * - id: patten-when
 *   match:
 *   pattern: foo($X);
 *   where:
 *    - comparison: $X == 0
 * ...
 *
 * if (x == NULL) {
 *    // ok
 *    foo(x);
 * } else {
 *    // ruleid: patten-when
 *    foo(x);
 * }
 *
 *)
(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* facts: a type defined in AST_generic. it represents known truths about
 * an expression, derived from control flow constructs.
 *
 * ctx: a type used only in this file. it's a stack of facts representing
 * the state (facts we know) at different levels in a cfg (hd of list =
 * top of stack).
 *
 * for example, if we have the following code:
 *
 * if (x == 0) {
 *    // A
 *    ...
 *    if (y == 0) {
 *      // B
 *      ...
 *    }
 * }
 *
 * the facts would be x == 0 and y == 0. the ctx at A would be [[x == 0]],
 * the ctx at B would be [[y == 0]; [x == 0]].
 *)
type ctx = facts list

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* if the -path_sensitive flag is enabled, the ref below will be set to
 * true. the functions in this file will only execute if both
 * -deep_intra_file and -path_sensitive are enabled.
 *)
let hook_path_sensitive = ref false

(* this visitor is used for annotating an expression and its subexpressions
 * with the same list of facts.
 *
 * for example, if we have the following code:
 *
 * if (x == 0) {
 *    x + x
 * }
 *
 * we will call the visitor with the fact x == 0 and recursively
 * annotate x + x and the two x's with the fact.
 *)
class ['self] tag_fact_visitor =
  object
    inherit ['self] AST_generic.iter_no_id_info as super

    method! visit_expr facts expr =
      expr.facts <- facts;
      super#visit_expr facts expr
  end

(* retrieves the facts at the current level (flattened list of the ctx).
 *
 * ctx should never be empty since a Join node is always paired with an NCond node.
 *)
let facts_at_current_level (ctx : ctx) : facts option =
  match ctx with
  | _ :: _ -> Some (List_.flatten ctx)
  | _ ->
      Log.warn (fun m -> m "impossible: ctx is empty");
      None

(* returns a new context with the current level (head) removed. *)
let parent_ctx (ctx : ctx) : ctx option =
  match ctx with
  | _ :: tl -> Some tl
  | _ ->
      Log.warn (fun m -> m "impossible: ctx is empty");
      None

(* annotates an expression and its subexpressions with the same facts *)
let annotate_expr_with_facts =
  let v = new tag_fact_visitor in
  fun facts e -> v#visit_expr facts e

(* update the facts of the current node to inherit the facts of
   its parent node *)
let inherit_facts (node : IL.node) facts =
  match node.n with
  | TrueNode e
  | FalseNode e
  | NCond (_, e)
  | NReturn (_, e)
  | NThrow (_, e) -> (
      match e with
      | { eorig = SameAs e; _ } -> annotate_expr_with_facts facts e
      | _ -> ())
  | NInstr { iorig = SameAs e; _ } -> annotate_expr_with_facts facts e
  | _ -> ()

(* this function add facts onto nodes in a cfg based on conditional constructs by
 * keeping track of a context, called ctx, which stores a list of facts representing
 * the state of facts at different levels (see type definition above).
 *
 * in the cfg, any conditional construct is deconstructed into NCond, TrueNode, FalseNode,
 * Join, and other nodes.
 *
 * when we encounter a TrueNode/ FalseNode (i.e. going down a level),
 * we will combine the new assumptions (based on the guard) and the existing facts
 * to create a new fact list, which is pushed into the context.
 *
 * when we encounter a Join, we know that we are going outside of the current conditional
 * construct (i.e. going up a level), so we will pop the top-level facts from the context.
 *
 * for the other nodes, we make them inherit their parent's facts in the cfg and
 * continue propagating the facts via the context.
 *)
let rec annotate_facts_of_node (cfg : IL.cfg) nodei (ctx : ctx) visited =
  let node = cfg.graph#nodes#find nodei in
  if Set_.mem nodei !visited then (* skip if already processed *) ()
  else (
    visited := Set_.add nodei !visited;
    match facts_at_current_level ctx with
    | None -> ()
    | Some facts -> (
        let process_successors ctx =
          List.iter
            (fun (i, _) -> annotate_facts_of_node cfg i ctx visited)
            (CFG.successors cfg nodei)
        in
        match node.n with
        | Join -> (
            (* pop up one level when joined.
             *
             * for example, if we have the following code:
             *
             * if (x == 0) {
             *    // A - ctx: [[x == 0]]
             *    ...
             *    if (y == 0) {
             *        // B - ctx: [[y == 0]; [x == 0]]
             *        ...
             *    } else {
             *        ...
             *    }
             *    // C - ctx: [[x == 0]]
             *    ...
             * }
             *
             * we would want to pop the hd of the ctx at point C
             * (where the then and else branch join) so that we
             * can go back to the ctx at point A.
             *)
            match parent_ctx ctx with
            | None -> ()
            | Some parent_ctx -> process_successors parent_ctx)
        (* condition in the form id <op> expr e.g.
         *
         * if (x == 0) {
         *    ...
         * }
         *)
        | TrueNode { e = Operator ((op, _), args); _ } -> (
            match args with
            (* id <op> expr
             *
             * TODO: same code as FalseNode (but negated) and should be
             * refactored.
             *)
            | [
             Unnamed { e = Fetch { base = Var { ident; id_info; _ }; _ }; _ };
             Unnamed { e = _; eorig = SameAs e };
            ] -> (
                let name = Id (ident, id_info) in
                match op with
                | Eq
                | PhysEq ->
                    process_successors ([ Equal (name, e) ] :: ctx)
                | NotEq
                | NotPhysEq ->
                    process_successors ([ NotEqual (name, e) ] :: ctx)
                | _ -> process_successors ([] :: ctx))
            | _ -> process_successors ([] :: ctx))
        | FalseNode { e = Operator ((op, _), args); _ } -> (
            match args with
            (* id <op> expr
             *
             * TODO: same code as TrueNode (but negated) and should be
             * refactored.
             *)
            | [
             Unnamed { e = Fetch { base = Var { ident; id_info; _ }; _ }; _ };
             Unnamed { e = _; eorig = SameAs e };
            ] -> (
                let name = Id (ident, id_info) in
                match op with
                | Eq
                | PhysEq ->
                    process_successors ([ NotEqual (name, e) ] :: ctx)
                | NotEq
                | NotPhysEq ->
                    process_successors ([ Equal (name, e) ] :: ctx)
                | _ -> process_successors ([] :: ctx))
            | _ -> process_successors ([] :: ctx))
        | TrueNode _
        | FalseNode _ ->
            process_successors ([] :: ctx)
        (* TODO: Need to revisit this after a major refactoring of how we handle lambdas
         *       in Pro PR #2477 (that PR also removed 'NLambda' nodes).
         *
         * based on the assumption in CFG_build.cfg_lambda that we don't actual traverse
         * into the lambda and perform inter-procedural analysis, we keep the
         * same ctx when we encounter NLambda.
         *
         * however, if the assumption no longer holds, the current implementation
         * cannot handle the NLamda case.
         *
         * for example, if a lambda function takes x as an argument but x also
         * exists outside the function, the facts known about the outer x differ
         * from the x bound within the lambda function.
         *
         * here's a more concrete example:
         *
         * def foo():
         *    x = 0
         *    if (x == 0):
         *        double = lambda x: x + x
         *        double(1)
         *        return
         *
         * in which the argument x of double should not inherit the facts we know
         * of the outer x.
         *
         * we also cannot simply clear the ctx because if we do, we cannot
         * revert it back to the original ctx after exiting the lambda.
         *)
        | NCond _
        | NInstr _
        | Enter
        | Exit
        | NGoto _
        | NReturn _
        | NThrow _
        | NOther _
        | NTodo _ ->
            inherit_facts node facts;
            process_successors ctx))

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let annotate_facts (cfg : IL.cfg) =
  if !hook_path_sensitive then
    let visited = ref Set_.empty in
    annotate_facts_of_node cfg cfg.entry [ [] ] visited

let facts_satisfy_e (mvars : Metavariable.bindings) (facts : facts) (e : expr) =
  !hook_path_sensitive
  &&
  (* checks if the expression in the fact (fe) matches the expression in the when expression (e). *)
  let fact_expr_matches_when_expr (fe : expr) (e : expr) =
    match (e.e, fe.e) with
    (* literals *)
    | L l1, L l2 -> equal_literal l1 l2
    (* identifiers *)
    | N n1, N n2 -> equal_name n1 n2
    | _ -> false
  in
  (* checks if there exists a fact which satisfies the when condition.
   *
   * facts are implicitly conjunctions, so a when condition is true
   * if any of the facts attached to an expression are true.
   * therefore, we check for existence, not for all.
   *
   * a when condition should be in the form id <op> e,
   * where id consists of two parts (e_id and e_id_info)
   * and e is the expr.
   *
   * e.g. for the condition x == 0, the id would be x, op would
   * be ==, and e would be 0.
   *)
  let match_op_en_e op e_id e_id_info_option e =
    List.exists
      (fun f ->
        match (f, op) with
        | Equal (Id (f_id, f_id_info), fe), Eq
        | NotEqual (Id (f_id, f_id_info), fe), NotEq -> (
            equal_ident f_id e_id
            && fact_expr_matches_when_expr fe e
            &&
            match e_id_info_option with
            | None ->
                (* this is the case where the id in the condition is not a metavariable,
                 * so we ignore the id_info. for more, see the long comment below regarding
                 * conditions without metavariables.
                 *)
                true
            | Some e_id_info -> equal_id_info f_id_info e_id_info)
        | _ -> false)
      facts
  in
  match e.e with
  (* when conditions should be in the form of id <op> expr,
   * e.g. x == 0.
   *)
  | Call ({ e = IdSpecial (Op op, _); _ }, (_, args, _)) -> (
      match args with
      (* id <op> expr *)
      | [ Arg { e = N (Id (((en, _) as e_id), _)); _ }; Arg e ] ->
          if Mvar.is_metavar_name en || Mvar.is_metavar_ellipsis en then
            try
              let v = List.assoc en mvars in
              match v with
              | Id (e_id, Some e_id_info)
              | N (Id (e_id, e_id_info)) ->
                  match_op_en_e op e_id (Some e_id_info) e
              | _ -> false
            with
            | Not_found -> true
          else (
            (* although metavariable-comparison/ comparison aren't usually used
             * for conditions without metavariables, there are some use
             * cases where being able to specify a condition without
             * metavariables would be useful (e.g., the example
             * fclose-return-condition-taint.c in the test suite).
             *
             * here, we are trying to handle the case for when the condition is
             * in the form id <op> expr, but the id is not a metavariable.
             *
             * since id is not a metavariable, and is just a part of the python
             * expression parsed as a condition, its id_info will not match that of
             * any variables in the code. so, when dealing with this type
             * of ids, we ignore their id_infos when matching the condition
             * with the facts.
             *
             * if this behavior causes any confusion, it can be rolled back.
             *)
            Log.debug (fun m -> m "condition for when: without metavariables");
            match_op_en_e op e_id None e)
      | _ ->
          Log.debug (fun m -> m "not a condition for when: too many arguments");
          false)
  | _ ->
      Log.debug (fun m -> m "not a condition for when");
      false

let hook_annotate_facts = ref None
let hook_facts_satisfy_e = ref None

let with_pro_hooks f =
  Common.save_excursion hook_annotate_facts (Some annotate_facts) (fun () ->
      Common.save_excursion hook_facts_satisfy_e (Some facts_satisfy_e) f)
