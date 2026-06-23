(* Yoann Padioleau
 *
 * Copyright (C) 2020 Semgrep Inc.
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
module G = AST_generic
module H = AST_generic_helpers

[@@@warning "-40-42"]

(** Generic AST to IL Translation

  Please refer to the preample in {!IL} for the IL design.

  Expression Translation ({!expr}):
  - Literals: Direct translation with origin tracking
  - Variables: Become {!lval} fetches with scope resolution
  - Calls: Creation of pre-statements for arguments, followed by call and storage of results in a temporary variable
  - Assignments: Support simple, destructuring, and record patterns
  - Conditionals, including Switch:
    - Become if-statements with fresh temporary variables
    - condition ORing and ANDing are converted to nested ifs.
  - Loops:
    - For/Foreach/DoWhile/While: converted all to Loop,
    - Foreach, converted to a Loop and 2 new special
    - Continue/Break: converted to goto

  Statement Translation ({!stmt}):
  - Control flow: If/loops/switch become explicit jumps and labels
  - Declarations: Variables/functions extracted and sequenced
  - Exceptions: Try-catch-finally with explicit throw destinations

  Pattern Translation (pattern):
   - Creates synthetic assignments for destructuring

  Type Translation (type_):
   - Analyzes embedded expressions (e.g., array sizes)
*)

let locate ?tok s : string =
  let opt_loc =
    try Option.map Tok.stringpos_of_tok tok with
    | Tok.NoTokenLocation _ -> None
  in
  match opt_loc with
  | Some loc -> spf "%s: %s" loc s
  | None -> s

let log_debug ?tok msg : unit = Log.debug (fun m -> m "%s" (locate ?tok msg))
let log_warning ?tok msg : unit = Log.warn (fun m -> m "%s" (locate ?tok msg))
let log_error ?tok msg : unit = Log.err (fun m -> m "%s" (locate ?tok msg))

(*****************************************************************************)
(* Configs *)
(*****************************************************************************)

(* NOTE "yield as return":

  In Python the `yield` statement functions similarly to a `return` statement
  but with the added capability of saving the function's state. While this
  analogy isn't entirely precise, we currently treat it as a return statement
  for simplicity's sake. *)
let lang_treat_yield_as_return lang =
  match lang with
  | Lang.Python -> true
  | __else__ -> false

(* NOTE "yield in for-comprehension"

  In Scala the `yield` statement is part of a "for-comprehension" such as:

      for (x <- xs) yield x+1

  which is equivalent to:

      xs.map(x => x + 1)

  So `yield` specifies the mapping of elements in the input collections. And
  we translate `yield` as an accumulator variable that stores such mappings.
  *)
let lang_has_yield_in_for_comprehension lang =
  match lang with
  | Lang.Scala -> true
  | __else__ -> false

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
module IdentSet = Sets.String_set

type ctx = { entity_names : IdentSet.t }

type env = {
  lang : Lang.t;
      (** stmts hidden inside expressions that we want to move out of 'exp',
          usually simple Instr, but can be also If when handling Conditional expr. *)
  stmts : stmt list ref;
      (** When entering a loop, we create two labels, one to jump to if a Continue stmt is found
          and another to jump to if a Break stmt is found. Since PHP supports breaking an arbitrary
          number of loops up, we keep a stack of break labels instead of just one. *)
  break_labels : label list;
  cont_label : label option;
  yield_var : IL.name option;
      (** When translating a yield in a for-comprehension, this variable holds
        the "collection" that is being built. See NOTE "yield in for-comprehension". *)
  ctx : ctx;
}

let empty_ctx : ctx = { entity_names = IdentSet.empty }

let empty_env (lang : Lang.t) : env =
  {
    stmts = ref [];
    break_labels = [];
    cont_label = None;
    yield_var = None;
    ctx = empty_ctx;
    lang;
  }

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

exception Fixme of fixme_kind * G.any

let sgrep_construct any_generic : 'a =
  raise (Fixme (Sgrep_construct, any_generic))

let todo any_generic : 'a = raise (Fixme (ToDo, any_generic))
let impossible any_generic : 'a = raise (Fixme (Impossible, any_generic))

let log_fixme kind gany : unit =
  let toks = AST_generic_helpers.ii_of_any gany in
  let tok = List_.hd_opt toks in
  match kind with
  | ToDo ->
      log_warning ?tok
        "Unsupported construct(s) may affect the accuracy of dataflow analyses"
  | Sgrep_construct ->
      log_error ?tok "Cannot translate Semgrep construct(s) into IL"
  | Impossible ->
      log_error ?tok "Impossible happened during AST-to-IL translation"

let fixme_exp ?partial kind gany eorig : exp =
  log_fixme kind (any_of_orig eorig);
  { e = FixmeExp (kind, gany, partial); eorig }

let fixme_instr kind gany eorig : instr =
  log_fixme kind (any_of_orig eorig);
  { i = FixmeInstr (kind, gany); iorig = eorig }

let fixme_stmt kind gany : stmt list =
  log_fixme kind gany;
  [ { s = FixmeStmt (kind, gany) } ]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let fresh_var ?(str = "_tmp") _env tok : IL.name =
  let tok =
    (* We don't want "fake" auxiliary variables to have non-fake tokens, otherwise
       we confuse ourselves! E.g. during taint-tracking we don't want to add these
       variables to the taint trace. *)
    if Tok.is_fake tok then tok else Tok.fake_tok tok str
  in
  let i = G.SId.mk () in
  { ident = (str, tok); sid = i; id_info = G.empty_id_info () }

let fresh_label ?(label = "_label") _env tok : IL.label =
  let i = G.SId.mk () in
  ((label, tok), i)

let fresh_lval ?str env tok : lval =
  let var = fresh_var ?str env tok in
  { base = Var var; rev_offset = [] }

let var_of_id_info id id_info : name =
  let sid =
    match !(id_info.G.id_resolved) with
    | Some (_resolved, sid) -> sid
    | None ->
        let id_str, id_tok = id in
        let msg = spf "the ident '%s' is not resolved" id_str in
        log_debug ~tok:id_tok msg;
        G.SId.unsafe_default
  in
  { ident = id; sid; id_info }

let var_of_name name : name =
  match name with
  | G.Id (id, id_info) -> var_of_id_info id id_info
  | G.IdQualified { G.name_last = id, _typeargsTODO; name_info = id_info; _ } ->
      var_of_id_info id id_info
  | G.IdSpecial _ -> todo (G.E (G.N name |> G.e))

(** [lval_is_just_var lval = Some name] iff [lval = { base = Var name; rev_offset = [] }] *)
let lval_is_just_var = function
  | { base = Var name; rev_offset = [] } -> Some name
  | { base = Var _; rev_offset = _ :: _ }
  | { base = VarSpecial _ | Mem _; rev_offset = _ } ->
      None

(** [exp_is_just_var exp = Some name] iff [exp = Fetch { base = Var name; rev_offset = [] }] *)
let exp_is_just_var = function
  | Fetch lval -> lval_is_just_var lval
  | __else__ -> None

let lval_of_id_info _env id id_info : lval =
  let var = var_of_id_info id id_info in
  { base = Var var; rev_offset = [] }

(* TODO: use also qualifiers? *)

let lval_of_id_qualified env
    { G.name_last = id, _typeargsTODO; name_info = id_info; _ } : lval =
  lval_of_id_info env id id_info

let lval_of_base base : lval = { base; rev_offset = [] }

(* TODO: should do first pass on body to get all labels and assign
 * a gensym to each.
 *)
let label_of_label _env lbl : label = (lbl, G.SId.unsafe_default)
let lookup_label _env lbl : label = (lbl, G.SId.unsafe_default)
let mk_e e eorig : exp = { e; eorig }
let mk_i i iorig : instr = { i; iorig }
let mk_s s : stmt = { s }

let mk_unit tok eorig : exp =
  let unit = G.Unit tok in
  mk_e (Literal unit) eorig

let add_instr env instr : unit = Stack_.push (mk_s (Instr instr)) env.stmts

(** Create an auxiliary variable for an expression.
    If [force] is [false] and the expression itself is already a variable then
    it will not create an auxiliary variable but just return that. *)
let mk_aux_var ?(force = false) ?str env tok exp : name * lval =
  match exp.e with
  | Fetch ({ base = Var var; rev_offset = []; _ } as lval) when not force ->
      (var, lval)
  | __else__ ->
      let var = fresh_var ?str env tok in
      let lval = lval_of_base (Var var) in
      add_instr env (mk_i (Assign (lval, exp)) NoOrig);
      (var, lval)

(* THINK: Allow arbitrary l-values while keeping simple variables for lambdas. *)
let lval_of_ret env tok ret =
  match ret with
  | `Var (name, orig) ->
      (* When we pass a name through '`Var' we also need to pair it with the
      proper 'orig' for the assignment. Otherwise, e.g. given this var-def:

          foo.bar.Sink bad = sink(source());

      We would pass `bad` to the 'map_expr' for `sink(source())`, and when
      we introduce the instruction `bad = sink(...)` this would have the
      expression `sink(source())` as orig, instead of having the full var-def
      as orig as it should have. This can affect taint best-matches computation.

      See 'Taint_spec_match.Best_matches'.
      See 'OSS/tests/rules/taint_best_fit_sink11'. *)
      (lval_of_base (Var name), Some orig)
  | `Tmp -> (fresh_lval env tok, None)
  | `Void ->
      (* The Void case should not happen, but this is not guaranteed by the
        type system, so we do handle it here. If it does the safer thing to
        do is to just create a fresh variable. *)
      (fresh_lval ~str:"_FIXME_expected_void" env tok, None)

let add_call env tok eorig ~ret call : exp =
  let mk_call res = AssignCall (res, { c = call; corig = eorig }) in
  match ret with
  | `Void ->
      add_instr env (mk_i (mk_call None) eorig);
      mk_unit tok NoOrig
  | (`Tmp | `Var _) as ret ->
      let lval, ret_orig = lval_of_ret env tok ret in
      add_instr env (mk_i (mk_call (Some lval)) (ret_orig ||| eorig));
      mk_e (Fetch lval) NoOrig

let add_stmt env st : unit = Stack_.push st env.stmts
let add_stmts env xs : unit = xs |> List.iter (add_stmt env)

let add_nested_def env tok lval def_kind eorig : unit =
  match lval_is_just_var lval with
  | Some name ->
      (* Avoid creating tmps if possible. *)
      let entity : IL.entity = { name = EN name; attrs = []; tparams = None } in
      let def = (entity, def_kind) in
      add_stmt env (mk_s (NestedDef def))
  | None ->
      let tmp = fresh_var env tok in
      let entity : IL.entity = { name = EN tmp; attrs = []; tparams = None } in
      let def = (entity, def_kind) in
      add_stmt env (mk_s (NestedDef def));
      add_instr env
        (mk_i
           (Assign (lval, mk_e (Fetch (lval_of_base (Var tmp))) NoOrig))
           eorig)

let pop_stmts env : stmt list =
  let xs = List.rev !(env.stmts) in
  env.stmts := [];
  xs

let with_pre_stmts env f : stmt list * 'a =
  let saved_stmts = !(env.stmts) in
  env.stmts := [];
  let r = f env in
  let f_stmts = pop_stmts env in
  env.stmts := saved_stmts;
  (f_stmts, r)

let bracket_keep f (t1, x, t2) : _ * _ * _ = (t1, f x, t2)

let ident_of_entity_opt ent : (G.ident * G.id_info) option =
  match ent.G.name with
  | G.EN (G.Id (i, pinfo)) -> Some (i, pinfo)
  (* TODO: use name_middle? name_top? *)
  | G.EN (G.IdQualified { name_last = i, _topt; name_info = pinfo; _ }) ->
      Some (i, pinfo)
  | G.EN (G.IdSpecial _)
  | G.EDynamic _ ->
      None
  (* TODO *)
  | G.EPattern _
  | G.OtherEntity _ ->
      None

let name_of_entity ent : name option =
  match ident_of_entity_opt ent with
  | Some (i, pinfo) ->
      let name = var_of_id_info i pinfo in
      Some name
  | _____else_____ -> None

let composite_of_container ~g_expr : G.container_operator -> IL.composite_kind =
  function
  | Array -> CArray
  | List -> CList
  | Tuple -> CTuple
  | Set -> CSet
  | Dict -> impossible (E g_expr)

let mk_unnamed_args (exps : IL.exp list) : exp argument list =
  List.map (fun x -> Unnamed x) exps

let is_hcl lang : bool =
  match lang with
  | Lang.Terraform -> true
  | _ -> false

let mk_class_constructor_name (ty : G.type_) cons_id_info : G.name option =
  match ty with
  | { t = TyN (G.Id (id, _)); _ }
  | { t = TyExpr { e = G.N (G.Id (id, _)); _ }; _ }
  (* FIXME: JS parser produces this ^ although it should be parsed as a 'TyN'. *)
    when Option.is_some !(cons_id_info.G.id_resolved) ->
      Some (G.Id (id, cons_id_info))
  | __else__ -> None

let add_entity_name ctx ident : ctx =
  { entity_names = IdentSet.add (H.str_of_ident ident) ctx.entity_names }

let build_ctx lang ast : ctx =
  (* At this point we only use the ctx on Ruby, to disambiguate
     variable accesses from zero-argument calls. *)
  if lang =*= Lang.Ruby then (
    let ctx = ref empty_ctx in
    Visit_function_defs.visit
      (fun opt_ent _fdef ->
        match opt_ent with
        | Some { name = EN (Id (n, _)); _ } -> ctx := add_entity_name !ctx n
        | __else__ -> ())
      ast;
    !ctx)
  else empty_ctx

let def_expr_evaluates_to_value (lang : Lang.t) : bool =
  match lang with
  | Elixir -> true
  | _else_ -> false

let is_constructor env ret_ty id_info : bool =
  match id_info.G.id_resolved.contents with
  | Some (G.GlobalName (ls, _), _) -> (
      env.lang =*= Lang.Python
      && List.length ls >= 3 (* Module + Class + __init__ *)
      && (match List_.last_opt ls with
        | Some "__init__" -> true
        | _ -> false)
      &&
      match ret_ty with
      (* It would be nice if we can check that this type actually
         corresponds to a class, but I am uncertain if this is
         possible. Istead we just check if it is a nominal typed.
         TODO could we somehow guarentee this type is a class? *)
      | { G.t = G.TyN _; _ } -> true
      | _ -> false)
  | _ -> false

type compile_pattern_matching_fn =
  env ->
  cond_with_pre_stmts:(env -> G.condition -> stmt list * exp) ->
  stmt_expr_with_pre_stmts:(env -> G.stmt -> stmt list * exp) ->
  G.condition ->
  G.case_and_body list ->
  stmt list * exp
(** Hook for Pro pattern matching compilation.

    The implementation is provided by Pro code via Pro_AST_to_IL.with_pro_hooks.
    If not set, pattern matching features will not be available (OSS limitation). *)

let hook_compile_pattern_matching : compile_pattern_matching_fn option Hook.t =
  Hook.create None

let is_ruby_parenless_call env lval =
  (* In Ruby, a bare identifier is ambiguous. It could be a variable
   access or a zero-argument method call. Emit as a call iff the name
   matches a known entity. *)
  match (env.lang, lval) with
  | Lang.Ruby, { base = Var { ident; _ }; _ } ->
      IdentSet.mem (H.str_of_ident ident) env.ctx.entity_names
  | _ -> false

let is_zero_arity_function (g_expr : G.expr) : bool =
  match g_expr.e with
  | G.N name
  | G.DotAccess (_, _, G.FN name) -> (
      match name with
      | G.Id (_, { id_type = { contents = Some ty }; _ })
      | G.IdQualified { name_info = { id_type = { contents = Some ty }; _ }; _ }
        -> (
          match ty with
          | { t = G.TyFun ([], _); _ } -> true
          | _ -> false)
      | _ -> false)
  | _ -> false

(* Some languages allow function calls without parentheses. So a simple identifier
   or field access is ambiguous. We only need to consider the case of zero-arity
   function calls, as the AST correctly encodes the other cases as function calls. *)
let should_emit_call_without_parens (env : env) (g_expr : G.expr) (lval : lval)
    : bool =
  match env.lang with
  | Lang.Ruby ->
      (* N.B Ruby is not a Pro language, so we shouldn't rely on typing info *)
      is_ruby_parenless_call env lval
  | lang when Lang.allows_call_without_parens lang ->
      is_zero_arity_function g_expr
  | _ -> false

(*****************************************************************************)
(* lvalue *)
(*****************************************************************************)

let rec map_lval env eorig : lval =
  match eorig.G.e with
  | G.N n -> map_name env eorig n
  | G.DotAccess (e1orig, tok, field) ->
      let offset' =
        match field with
        | G.FN (G.Id (id, idinfo)) -> Dot (var_of_id_info id idinfo)
        | G.FN name ->
            let attr = map_expr env (G.N name |> G.e) in
            Index attr
        | G.FDynamic e2orig ->
            let attr = map_expr env e2orig in
            Index attr
      in
      let offset' = { o = offset'; oorig = SameAs eorig } in
      let lv1 = map_nested_lval env tok e1orig in
      { lv1 with rev_offset = offset' :: lv1.rev_offset }
  | G.ArrayAccess (e1orig, (_, e2orig, _)) ->
      let tok = G.fake "[]" in
      let lv1 = map_nested_lval env tok e1orig in
      let e2 = map_expr env e2orig in
      let offset' = { o = Index e2; oorig = SameAs eorig } in
      { lv1 with rev_offset = offset' :: lv1.rev_offset }
  | G.DeRef (_, e1orig) ->
      let e1 = map_expr env e1orig in
      lval_of_base (Mem e1)
  | _ -> todo (G.E eorig)

and map_nested_lval env tok e_gen : lval =
  match map_expr env e_gen with
  | { e = Fetch lval; _ } -> lval
  | rhs ->
      let fresh = fresh_lval env tok in
      add_instr env (mk_i (Assign (fresh, rhs)) (related_exp e_gen));
      fresh

and map_name env eorig : G.name -> lval = function
  | G.Id (("_", tok), _) ->
      (* wildcard *)
      fresh_lval env tok
  | G.Id (id, id_info) ->
      let lval = lval_of_id_info env id id_info in
      lval
  | G.IdQualified qualified_info ->
      let lval = lval_of_id_qualified env qualified_info in
      lval
  | G.IdSpecial ((G.This, tok), _) -> lval_of_base (VarSpecial (This, tok))
  | _ -> todo (G.E eorig)

(*****************************************************************************)
(* Pattern *)
(*****************************************************************************)

(* TODO: This code is very similar to that of `assign`. Actually, we should not
 * be dealing with patterns in the LHS of `Assign`, those are supposed to be
 * `LetPattern`s. *)

and map_pattern env pat : lval * stmt list =
  match pat with
  | G.PatWildcard tok ->
      let lval = fresh_lval env tok in
      (lval, [])
  | G.PatId (id, id_info) ->
      let lval = lval_of_id_info env id id_info in
      (lval, [])
  | G.PatList (_tok1, pats, tok2)
  | G.PatTuple (_tok1, pats, tok2) ->
      (* P1, ..., Pn *)
      let tmp = fresh_var env tok2 in
      let tmp_lval = lval_of_base (Var tmp) in
      (* Pi = tmp[i] *)
      let ss =
        pats
        |> List.mapi (fun i pat_i ->
            let eorig = Related (G.P pat_i) in
            let index_i = Literal (G.Int (Parsed_int.of_int i)) in
            let offset_i =
              { o = Index { e = index_i; eorig }; oorig = NoOrig }
            in
            let lval_i = { base = Var tmp; rev_offset = [ offset_i ] } in
            map_pattern_assign_statements env
              (mk_e (Fetch lval_i) eorig)
              ~eorig pat_i)
        |> List_.flatten
      in
      (tmp_lval, ss)
  | G.PatTyped (pat1, ty) ->
      map_type_ env ty |> ignore;
      map_pattern env pat1
  | _ -> todo (G.P pat)

and _TODO_catch_exn env exn : lval * stmt list =
  match exn with
  | G.CatchPattern pat -> map_pattern env pat
  | G.CatchParam { pname = Some id; pinfo = id_info; _ } ->
      let lval = lval_of_id_info env id id_info in
      (lval, [])
  | _ -> todo (G.Ce exn)

and map_pattern_assign_statements env ?(eorig = NoOrig) exp pat : stmt list =
  try
    let lval, ss = map_pattern env pat in
    [ mk_s (Instr (mk_i (Assign (lval, exp)) eorig)) ] @ ss
  with
  | Fixme (kind, any_generic) -> fixme_stmt kind any_generic

(*****************************************************************************)
(* Exceptions *)
(*****************************************************************************)

and map_try_catch_else_finally env ~try_st ~catches ~opt_else ~opt_finally :
    stmt list =
  let try_stmt = map_stmt env try_st in
  let catches_stmt_rev =
    List.fold_left
      (fun acc (ctok, exn, catch_st) ->
        (* TODO: Handle exn properly. *)
        let name = fresh_var env ctok in
        let todo_pattern = fixme_stmt ToDo (G.Ce exn) in
        let catch_stmt = map_stmt env catch_st in
        (name, todo_pattern @ catch_stmt) :: acc)
      [] catches
  in
  let else_stmt =
    match opt_else with
    | None -> []
    | Some (_tok, else_st) -> map_stmt env else_st
  in
  let finally_stmt =
    match opt_finally with
    | None -> []
    | Some (_tok, finally_st) -> map_stmt env finally_st
  in
  [ mk_s (Try (try_stmt, List.rev catches_stmt_rev, else_stmt, finally_stmt)) ]

(*****************************************************************************)
(* Assign *)
(*****************************************************************************)

and map_assign_rhs env ~ret rhs =
  match rhs with
  | `Ge grhs -> map_expr env ~ret grhs
  | `IL irhs -> irhs

and map_assign_rhs_minimizing_tmps env lval rhs eorig =
  match lval_is_just_var lval with
  | Some name -> (
      (* If the l-value is a simple variable, then should not create
         tmps to store the result of evaluating the RHS, simply store
         it in that variable! *)
      let rhs_exp = map_assign_rhs env ~ret:(`Var (name, eorig)) rhs in
      match exp_is_just_var rhs_exp.e with
      | Some name' when IL.equal_name name name' ->
          (* Do not create `foo = foo` assignments.
            (Note that `map_expr` sometimes introduces an assignment and returns
            a `Fetch` of the lval.) *)
          ()
      | Some _ (* different name *)
      | None ->
          add_instr env (mk_i (Assign (lval, rhs_exp)) eorig))
  | None ->
      (* Not a simple variable so we introduce a tmp. *)
      let rhs_exp = map_assign_rhs env ~ret:`Tmp rhs in
      add_instr env (mk_i (Assign (lval, rhs_exp)) eorig)

and map_assign ?eorig env ~g_expr lhs tok rhs : exp =
  let eorig = Option.value eorig ~default:(SameAs g_expr) in
  match lhs.G.e with
  | G.N _
  | G.DotAccess _
  | G.ArrayAccess _
  | G.DeRef _ -> (
      try
        let lval = map_lval env lhs in
        map_assign_rhs_minimizing_tmps env lval rhs eorig;
        mk_e (Fetch lval) (SameAs lhs)
      with
      | Fixme (kind, any_generic) ->
          (* lval translation failed, we use a fresh lval instead *)
          let fixme_var = fresh_var ~str:"_FIXME" env tok in
          let fixme_lval = lval_of_base (Var fixme_var) in
          let rhs_exp = map_assign_rhs env ~ret:(`Var (fixme_var, eorig)) rhs in
          add_instr env (mk_i (Assign (fixme_lval, rhs_exp)) eorig);
          fixme_exp kind any_generic (related_exp g_expr))
  | G.Container (((G.Tuple | G.List | G.Array) as ckind), (tok1, lhss, tok2)) ->
      (* TODO: handle cases like [a, b, ...rest] = e *)
      (* E1, ..., En = RHS *)
      (* tmp = RHS*)
      let tmp = fresh_var env tok2 in
      let tmp_lval = lval_of_base (Var tmp) in
      let rhs_exp = map_assign_rhs env ~ret:(`Var (tmp, eorig)) rhs in
      add_instr env (mk_i (Assign (tmp_lval, rhs_exp)) eorig);
      (* Ei = tmp[i] *)
      let tup_elems =
        lhss
        |> List.mapi (fun i lhs_i ->
            let index_i = Literal (G.Int (Parsed_int.of_int i)) in
            let offset_i =
              {
                o = Index { e = index_i; eorig = related_exp lhs_i };
                oorig = NoOrig;
              }
            in
            let lval_i = { base = Var tmp; rev_offset = [ offset_i ] } in
            map_assign ~eorig:(related_exp lhs_i) env ~g_expr lhs_i tok1
              (`IL { e = Fetch lval_i; eorig = related_exp lhs_i }))
      in
      (* (E1, ..., En) *)
      mk_e
        (Composite
           (composite_of_container ~g_expr ckind, (tok1, tup_elems, tok2)))
        (related_exp lhs)
  | G.Record (tok1, fields, tok2) ->
      let rhs_exp = map_assign_rhs env ~ret:`Tmp rhs in
      map_assign_to_record env (tok1, fields, tok2) rhs_exp (related_exp lhs)
  | _ ->
      (* We don't support the LHS but we at least try to translate the RHS. *)
      let fixme_lval = fresh_lval ~str:"_FIXME" env tok in
      map_assign_rhs_minimizing_tmps env fixme_lval rhs eorig;
      add_instr env (fixme_instr ToDo (G.E g_expr) (related_exp g_expr));
      fixme_exp ToDo (G.E g_expr) (related_exp lhs)

(** Assignments of the form

      {x1: p1, ..., xN: pN} = RHS

  where `xi` are field names, and `pi` are patterns.

  In the simplest case, where the patterns are variables
  v1, ..., VN, this becomes:

      tmp = RHS
      v1 = tmp.x1
      ...
       vN = tmp.xN
 *)
and map_assign_to_record env (tok1, fields, tok2) rhs_exp lhs_orig : exp =
  let tmp, _tmp_lval = mk_aux_var env tok1 rhs_exp in
  let rec do_fields acc_rev_offsets fs =
    fs |> List.map (do_field acc_rev_offsets)
  and do_field acc_rev_offsets f =
    match f with
    | G.F
        {
          s =
            G.DefStmt
              ( { name = EN (G.Id (id1, ii1)); _ },
                G.FieldDefColon
                  { vinit = Some { e = G.N (G.Id (id2, ii2)); _ }; _ } );
          _;
        } ->
        (* fld = var ----> var := tmp. ... <accumulated offsets> ... .fld *)
        let tok = snd id1 in
        let fldi = var_of_id_info id1 ii1 in
        let offset = { o = Dot fldi; oorig = NoOrig } in
        let vari = var_of_id_info id2 ii2 in
        let vari_lval = lval_of_base (Var vari) in
        let ei =
          mk_e
            (Fetch { base = Var tmp; rev_offset = offset :: acc_rev_offsets })
            (related_tok tok)
        in
        add_instr env (mk_i (Assign (vari_lval, ei)) (related_tok tok));
        Field (fldi, mk_e (Fetch vari_lval) (related_tok tok))
    | G.F
        {
          s =
            G.DefStmt
              ( { name = EN (G.Id (id1, ii1)); _ },
                G.FieldDefColon
                  { vinit = Some { e = G.Record (_, fields, _); _ }; _ } );
          _;
        } ->
        (* fld = { ... }, nested record pattern, we recurse. *)
        let tok = snd id1 in
        let fldi = var_of_id_info id1 ii1 in
        let offset = { o = Dot fldi; oorig = NoOrig } in
        let fields = do_fields (offset :: acc_rev_offsets) fields in
        Field (fldi, mk_e (RecordOrDict fields) (related_tok tok))
    | field ->
        (* TODO: What other patterns could be nested ? *)
        (* __FIXME_AST_to_IL__: FixmeExp ToDo *)
        let xi = ("__FIXME_AST_to_IL_assign_to_record__", tok1) in
        let xn =
          {
            ident = xi;
            sid = G.SId.unsafe_default;
            id_info = G.empty_id_info ();
          }
        in
        let ei = fixme_exp ToDo (G.Fld field) (related_tok tok1) in
        let tmpi = fresh_var env tok2 in
        let tmpi_lval = lval_of_base (Var tmpi) in
        add_instr env (mk_i (Assign (tmpi_lval, ei)) (related_tok tok1));
        Field (xn, mk_e (Fetch tmpi_lval) (Related (G.Fld field)))
  in
  let fields : field_or_entry list = do_fields [] fields in
  (* {x1: E1, ..., xN: En} *)
  mk_e (RecordOrDict fields) lhs_orig

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)
(* less: we could pass in an optional lval that we know the caller want
 * to assign into, which would avoid creating useless fresh_var intermediates.
 *)
(* We set `void` to `true` when the value of the expression is being discarded, in
 * which case, for certain expressions and in certain languages, we assume that the
 * expression has side-effects. See translation of operators below. *)

and map_expr_aux env ?(ret = `Tmp) g_expr : exp =
  let eorig = SameAs g_expr in
  match g_expr.G.e with
  | G.Call
      ( { e = G.Special (G.Op ((G.And | G.Or) as op), tok); _ },
        (_, arg0 :: args, _) )
    when ret <> `Void ->
      map_expr_lazy_op env op tok arg0 args eorig
  (* args_with_pre_stmts *)
  | G.Call ({ e = G.Special (G.Op op, tok); _ }, args) -> (
      let args = map_arguments env (Tok.unbracket args) in
      if ret <> `Void then mk_e (Operator ((op, tok), args)) eorig
      else
        (* The operation's result is not being used, so it may have side-effects.
         * We then assume this is just syntax sugar for a method call. E.g. in
         * Ruby `s << "hello"` is syntax sugar for `s.<<("hello")` and it mutates
         * the string `s` appending "hello" to it. *)
        match args with
        | [] -> impossible (G.E g_expr)
        | obj :: args' ->
            let obj_var, _obj_lval =
              mk_aux_var env tok (IL_helpers.exp_of_arg obj)
            in
            let method_name = fresh_var env tok ~str:(Tok.content_of_tok tok) in
            let offset = { o = Dot method_name; oorig = NoOrig } in
            let method_lval = { base = Var obj_var; rev_offset = [ offset ] } in
            let method_ = { e = Fetch method_lval; eorig = related_tok tok } in
            add_call env tok eorig ~ret (Call (method_, args')))
  | G.Call
      ( ({
           e =
             G.N
               (G.IdSpecial (((G.This | G.Super | G.Self | G.Parent), tok), _));
           _;
         } as e),
        args ) ->
      map_call_generic env ~ret tok eorig e args
  | G.Call
      ({ e = G.Special (G.IncrDecr (incdec, _prepostIGNORE), tok); _ }, args)
    -> (
      (* in theory in expr() we should return each time a list of pre-instr
       * and a list of post-instrs to execute before and after the use
       * of the expression. However this complicates the interface of 'expr()'.
       * Right now, for the pre-instr we agglomerate them instead in env
       * and use them in 'expr_with_pre_instr()' below, but for the post
       * we dont. Anyway, for our static analysis purpose it should not matter.
       * We don't do fancy path-sensitive-evaluation-order-sensitive analysis.
       *)
      match Tok.unbracket args with
      | [ G.Arg e ] ->
          let lval = map_lval env e in
          (* TODO: This `lval` should have a new svalue ref given that we
           * are translating `lval++` as `lval = lval + 1`. *)
          let lvalexp = mk_e (Fetch lval) (related_exp e) in
          let op =
            ( (match incdec with
              | G.Incr -> G.Plus
              | G.Decr -> G.Minus),
              tok )
          in
          let one = G.Int (Parsed_int.of_int 1) in
          let one_exp = mk_e (Literal one) (related_tok tok) in
          let opexp =
            mk_e
              (Operator (op, [ Unnamed lvalexp; Unnamed one_exp ]))
              (related_tok tok)
          in
          add_instr env (mk_i (Assign (lval, opexp)) eorig);
          lvalexp
      | _ -> impossible (G.E g_expr))
  | G.Call
      ( {
          e =
            G.DotAccess
              ( obj,
                tok,
                G.FN
                  (G.Id
                     (("concat", _), { G.id_resolved = { contents = None }; _ }))
              );
          _;
        },
        args ) ->
      (* obj.concat(args) *)
      (* NOTE: Often this will be string concatenation but not necessarily! *)
      let obj_arg' = Unnamed (map_expr env obj) in
      let args' = map_arguments env (Tok.unbracket args) in
      let res =
        match env.lang with
        (* Ruby's concat method is side-effectful and updates the object. *)
        (* TODO: The lval in the LHs should have a differnt svalue than the
         * one in the RHS. *)
        | Lang.Ruby -> (
            try map_lval env obj with
            | Fixme _ -> fresh_lval ~str:"_FIXME" env tok)
        | _ -> fresh_lval env tok
      in
      add_instr env
        (mk_i
           (AssignCall
              ( Some res,
                {
                  c = CallSpecial ((Concat, tok), obj_arg' :: args');
                  corig = eorig;
                } ))
           eorig);
      mk_e (Fetch res) eorig
  (* todo: if the xxx_to_generic forgot to generate Eval *)
  | G.Call
      ( {
          e =
            G.N
              (G.Id (("eval", tok), { G.id_resolved = { contents = None }; _ }));
          _;
        },
        args ) ->
      let lval = fresh_lval env tok in
      let special = (Eval, tok) in
      let args = map_arguments env (Tok.unbracket args) in
      add_instr env
        (mk_i
           (AssignCall
              (Some lval, { c = CallSpecial (special, args); corig = eorig }))
           eorig);
      mk_e (Fetch lval) (related_tok tok)
  | G.Call ({ e = G.Special (G.InterpolatedElement, _); _ }, (_, [ G.Arg e ], _))
    ->
      (* G.InterpolatedElement is useful for matching certain patterns against
       * interpolated strings, but we do not have an use for it yet during
       * semantic analysis, so in the IL we just unwrap the expression. *)
      map_expr env e
  | G.New (tok, ty, _cons_id_info, args) ->
      (* HACK: Fall-through case where we don't know to what variable the allocated
       * object is being assigned to. See HACK(new), we expect to intercept `New`
       * already in 'stmt_aux'.
       *)
      let lval = fresh_lval env tok in
      let args = map_arguments env (Tok.unbracket args) in
      add_instr env (mk_i (New (lval, map_type_ env ty, None, args)) eorig);
      mk_e (Fetch lval) NoOrig
  | G.Call ({ e = G.Special ((_, tok) as spec); _ }, args) -> (
      let args = map_arguments env (Tok.unbracket args) in
      try
        let special = map_call_special env spec in
        add_call env tok eorig ~ret (CallSpecial (special, args))
      with
      | Fixme (kind, any_generic) ->
          let fixme = fixme_exp kind any_generic (related_exp g_expr) in
          add_call env tok eorig ~ret (Call (fixme, args)))
  | G.Call (e, args) ->
      let tok = G.fake "call" in
      map_call_generic env ~ret tok eorig e args
  | G.L lit -> mk_e (Literal lit) eorig
  | G.DotAccess ({ e = N (Id (("var", _), _)); _ }, _, FN (Id ((s, t), id_info)))
    when is_hcl env.lang ->
      (* We need to change all uses of a variable, which looks like a DotAccess, to a name which
         reads the same. This is so that our parameters to our function can properly be recognized
         as tainted by the taint engine.
      *)
      map_expr_aux env (G.N (Id (("var." ^ s, t), id_info)) |> G.e)
  | G.N (IdSpecial ((spec, tok), _)) -> (
      let opt_var_special =
        match spec with
        | G.This -> Some This
        | G.Super -> Some Super
        | G.Self -> Some Self
        | G.Parent -> Some Parent
        | G.Cls -> None
      in
      match opt_var_special with
      | Some var_special ->
          let lval = lval_of_base (VarSpecial (var_special, tok)) in
          mk_e (Fetch lval) eorig
      | None -> impossible (G.E g_expr))
  | G.N (Id _)
  | G.N (IdQualified _)
  | G.DotAccess (_, _, _)
  | G.ArrayAccess (_, _)
  | G.DeRef (_, _) ->
      let lval = map_lval env g_expr in
      let exp = mk_e (Fetch lval) eorig in
      (* TODO: Does this really need to be a name (?) in the first place? Why
         can't this be call? Syntactic ambiguity? *)
      if should_emit_call_without_parens env g_expr lval then
        let tok = G.fake "call" in
        add_call env tok eorig ~ret (Call (exp, []))
      else exp
  | G.Assign
      ( ({ e = G.N obj; _ } as obj_e),
        _,
        ({ e = G.New (_tok, ty, cons_id_info, args); _ } as new_exp) ) ->
      (* x = new T(args) -- initialization without declaration *)
      (* HACK(new): Because of field-sensitivity hacks, we need to know to which
       * variable are we assigning the `new` object, so we intercept the assignment. *)
      let obj' = var_of_name obj in
      let lval, ss =
        mk_class_construction env obj' new_exp ty cons_id_info args
      in
      add_stmts env ss;
      mk_e (Fetch lval) (SameAs obj_e)
  | G.Assign
      (* x = ClassName(args ...) in Python *)
      (* Identified and treated as x = New ClassName(args ...) to support
         field sensitivity. See HACK(new) *)
      ( ({
           e =
             G.N
               (G.Id ((_, _), { id_type = { contents = Some ret_ty }; _ }) as
                obj);
           _;
         } as obj_e),
        _,
        ({
           e =
             G.Call
               ( {
                   e =
                     ( G.N (Id (_, id_info))
                     (* Module paths are currently parsed into
                        dotaccess so m.ClassName() is completely
                        valid. *)
                     | G.DotAccess (_, _, FN (Id (_, id_info))) );
                   _;
                 },
                 args );
           _;
         } as origin_exp) )
    when is_constructor env ret_ty id_info
         (* ClassName has been resolved to __init__ by the pro engine. *) ->
      let obj' = var_of_name obj in
      let lval, ss =
        mk_class_construction env obj' origin_exp ret_ty id_info args
      in
      add_stmts env ss;
      mk_e (Fetch lval) (SameAs obj_e)
  | G.Assign (e1, tok, e2) -> map_assign env ~g_expr e1 tok (`Ge e2)
  | G.AssignOp (e1, (G.Eq, tok), e2) ->
      (* AsssignOp(Eq) is used to represent plain assignment in some languages,
       * e.g. Go's `:=` is represented as `AssignOp(Eq)`, and C#'s assignments
       * are all represented this way too. *)
      map_assign env ~g_expr e1 tok (`Ge e2)
  | G.AssignOp (e1, op, e2) ->
      let exp = map_expr env e2 in
      let lval = map_lval env e1 in
      let lvalexp = mk_e (Fetch lval) (SameAs e1) in
      let opexp =
        mk_e
          (Operator (op, [ Unnamed lvalexp; Unnamed exp ]))
          (related_tok (snd op))
      in
      add_instr env (mk_i (Assign (lval, opexp)) eorig);
      lvalexp
  | G.LetPattern (pat, e) ->
      let exp = map_expr env e in
      add_stmts env (map_pattern_assign_statements env ~eorig exp pat);
      mk_unit (G.fake "()") NoOrig
  | G.Seq xs -> (
      match List.rev xs with
      | [] -> impossible (G.E g_expr)
      | last :: xs ->
          let xs = List.rev xs in
          xs
          |> List.iter (fun e ->
              let _eIGNORE = map_expr env e in
              ());
          map_expr env last)
  | G.Record fields -> map_record env fields
  | G.Container (G.Dict, xs) -> map_dict env xs g_expr
  | G.Container (kind, xs) ->
      let xs = bracket_keep (List.map (map_expr env)) xs in
      let kind = map_composite_kind ~g_expr kind in
      mk_e (Composite (kind, xs)) eorig
  | G.Comprehension (kind, (b1, (e, xs), b2)) ->
      let loop = map_comp_to_nested_loop env e xs in
      let kind = map_composite_kind ~g_expr kind in
      mk_e (Composite (kind, (b1, [ loop ], b2))) eorig
  | G.Lambda fdef ->
      (* We encode lambdas as nested function definitions. And, if we got a
        `var = <lambda>` declaration/assignment, we reuse `var` and avoid
        "tmp"s.
        *)
      let lval, ret_orig = lval_of_ret env (snd fdef.fkind) ret in
      let fdef =
        (* NOTE(config.stmts): This is a recursive call to
           `function_definition` and we need to pass it a fresh
           `stmts` ref list. If we reuse the same `stmts` ref list,
           then whatever `stmts` we have accumulated so far, will
           "magically" appear in the body of this lambda in the final
           IL representation. This can happen e.g. when translating
           `foo(bar(), (x) => { ... })`, because the instruction added
           to `stmts` by the translation of `bar()` is still present
           when traslating `(x) => { ... }`. *)
        map_function_definition { env with stmts = ref [] } fdef
      in
      let tok = snd fdef.fkind in
      add_nested_def env tok lval (FuncDef fdef) (ret_orig ||| eorig);
      mk_e (Fetch lval) eorig
  | G.AnonClass cdef ->
      (* TODO: should use def.ckind *)
      let tok = Common2.fst3 cdef.G.cbody in
      let lval = fresh_lval env tok in
      let cdef = map_class_definition env cdef in
      add_nested_def env tok lval (ClassDef cdef) eorig;
      mk_e (Fetch lval) eorig
  | G.Special _ -> impossible (G.E g_expr)
  | G.SliceAccess (_, _) -> todo (G.E g_expr)
  (* e1 ? e2 : e3 ==>
   *  pre: lval = e1;
   *       if(lval) { lval = e2 } else { lval = e3 }
   *  exp: lval
   *)
  | G.Conditional (e1_gen, e2_gen, e3_gen) ->
      let tok = G.fake "conditional" in
      let lval = fresh_lval env tok in

      (* not sure this is correct *)
      let before = pop_stmts env in
      let e1 = map_expr env e1_gen in
      let ss_for_e1 = pop_stmts env in
      let e2 = map_expr env e2_gen in
      let ss_for_e2 = pop_stmts env in
      let e3 = map_expr env e3_gen in
      let ss_for_e3 = pop_stmts env in

      add_stmts env before;
      add_stmts env ss_for_e1;
      add_stmt env
        (mk_s
           (If
              ( tok,
                e1,
                ss_for_e2 @ [ mk_s (Instr (mk_i (Assign (lval, e2)) NoOrig)) ],
                ss_for_e3 @ [ mk_s (Instr (mk_i (Assign (lval, e3)) NoOrig)) ]
              )));
      mk_e (Fetch lval) eorig
  | G.Await (tok, e1orig) ->
      let e1 = map_expr env e1orig in
      let tmp = fresh_lval env tok in
      add_instr env
        (mk_i
           (AssignCall
              ( Some tmp,
                {
                  c = CallSpecial ((Await, tok), [ Unnamed e1 ]);
                  corig = eorig;
                } ))
           eorig);
      mk_e (Fetch tmp) NoOrig
  | G.Yield (tok, e1orig_opt, _) ->
      let yield_args =
        match e1orig_opt with
        | None -> []
        | Some e1orig -> [ map_expr env e1orig ]
      in
      let yield_lval =
        let* yield_var = env.yield_var in
        (* We are inside a for-comprehension ('env.yield_var' is set). We
          interpret `yield E` as `yield_var[*] = yield E`, that is, mapping an
          arbitrary element of the resulting collection. We probably should
          have the RHS to be just `E`, but `yield E` is convenient here like
          in case `E` is a tuple, and `yield E` is also the default anyways
          when there is no 'yield_var'. See NOTE "yield in for-comprehension". *)
        let any_index =
          mk_e (Fetch (fresh_lval ~str:"__any__" env tok)) NoOrig
        in
        Some
          {
            base = Var yield_var;
            rev_offset = [ { o = Index any_index; oorig = NoOrig } ];
          }
      in
      add_instr env
        (mk_i
           (AssignCall
              ( yield_lval,
                {
                  c = CallSpecial ((Yield, tok), mk_unnamed_args yield_args);
                  corig = eorig;
                } ))
           eorig);
      mk_unit tok NoOrig
  | G.Ref (tok, e1orig) ->
      let e1 = map_expr env e1orig in
      let tmp = fresh_lval env tok in
      add_instr env
        (mk_i
           (AssignCall
              ( Some tmp,
                { c = CallSpecial ((Ref, tok), [ Unnamed e1 ]); corig = eorig }
              ))
           eorig);
      mk_e (Fetch tmp) NoOrig
  | G.Constructor (cname, (tok1, esorig, tok2)) ->
      let cname = var_of_name cname in
      let es = esorig |> List.map (fun eiorig -> map_expr env eiorig) in
      mk_e (Composite (Constructor cname, (tok1, es, tok2))) eorig
  | G.RegexpTemplate ((l, e, r), _opt) ->
      mk_e (Composite (Regexp, (l, [ map_expr env e ], r))) NoOrig
  | G.Xml xml -> map_xml_expr env ~ret eorig xml
  | G.Cast (typ, _, e) ->
      let e = map_expr env e in
      mk_e (Cast (typ, e)) eorig
  | G.Alias (_alias, e) -> map_expr env e
  | G.LocalImportAll (_module, _tk, e) ->
      (* TODO: what can we do with _module? *)
      map_expr env e
  | G.Ellipsis _
  | G.TypedMetavar (_, _, _)
  | G.DisjExpr (_, _)
  | G.DeepEllipsis _
  | G.DotAccessEllipsis _ ->
      sgrep_construct (G.E g_expr)
  | G.StmtExpr st -> map_stmt_expr env ~g_expr st
  | G.OtherExpr ((str, tok), xs) ->
      let es =
        xs
        |> List.map (fun x ->
            match x with
            | G.E e1orig -> map_expr env e1orig
            | __else__ -> fixme_exp ToDo x (related_tok tok))
      in
      let other_expr = mk_e (Composite (CTuple, (tok, es, tok))) eorig in
      let _, tmp = mk_aux_var ~str env tok other_expr in
      let partial = mk_e (Fetch tmp) (related_tok tok) in
      fixme_exp ToDo (G.E g_expr) (related_tok tok) ~partial
  | G.RawExpr _ -> todo (G.E g_expr)

and map_expr env ?ret e_gen : exp =
  try map_expr_aux env ?ret e_gen with
  | Fixme (kind, any_generic) -> fixme_exp kind any_generic (related_exp e_gen)

and map_expr_opt env tok : G.expr option -> exp = function
  | None ->
      let void = G.Unit tok in
      mk_e (Literal void) (related_tok tok)
  | Some e -> map_expr env e

and map_expr_lazy_op env op tok arg0 args eorig : exp =
  let arg0' = map_argument env arg0 in
  let args' : exp argument list =
    (* Consider A && B && C, side-effects in B must only take effect `if A`,
     * and side-effects in C must only take effect `if A && B`. *)
    args
    |> List.fold_left_map
         (fun cond argi ->
           let ssi, argi' = map_arg_with_pre_stmts env argi in
           if ssi <> [] then add_stmt env (mk_s @@ If (tok, cond, ssi, []));
           let condi =
             mk_e (Operator ((op, tok), [ Unnamed cond; argi' ])) eorig
           in
           (condi, argi'))
         (IL_helpers.exp_of_arg arg0')
    |> snd
  in
  mk_e (Operator ((op, tok), arg0' :: args')) eorig

and map_call_generic env ~ret tok eorig e args : exp =
  let e = map_expr env e in
  (* In theory, instrs in args could have side effect on the value in 'e',
   * but we will agglomerate all those instrs in the environment and
   * the caller will call them in sequence (see expr_with_pre_instr).
   * In theory, we should not execute those instrs before getting the
   * value in 'e' in the caller, but for our static analysis purpose
   * we should not care about those edge cases. That would require
   * to return in expr multiple arguments and thread things around; Not
   * worth it.
   *)
  let args = map_arguments env (Tok.unbracket args) in
  add_call env tok eorig ~ret (Call (e, args))

and map_call_special _env (x, tok) =
  ( (match x with
    | G.Op _
    | G.IncrDecr _
    | G.InterpolatedElement ->
        impossible (G.E (G.Special (x, tok) |> G.e))
    (* should be intercepted before *)
    | G.Eval -> Eval
    | G.Typeof -> Typeof
    | G.Instanceof -> Instanceof
    | G.Sizeof -> Sizeof
    | G.ConcatString _kindopt -> Concat
    | G.Spread -> SpreadFn
    | G.Require -> Require
    | G.EncodedString _
    | G.Defined
    | G.HashSplat
    | G.ForOf
    | G.NextArrayIndex ->
        todo (G.E (G.Special (x, tok) |> G.e))),
    tok )

and map_composite_kind ~g_expr : G.container_operator -> composite_kind =
  function
  | G.Array -> CArray
  | G.List -> CList
  | G.Dict -> impossible (E g_expr)
  | G.Set -> CSet
  | G.Tuple -> CTuple

(* TODO: dependency of order between arguments for instr? *)

and map_arguments env xs : exp argument list = xs |> List.map (map_argument env)

and map_argument env arg : exp argument =
  match arg with
  | G.Arg e -> Unnamed (map_expr env e)
  | G.ArgKwd (id, e)
  | G.ArgKwdOptional (id, e) ->
      Named (id, map_expr env e)
  | G.ArgType { t = TyExpr e; _ } -> Unnamed (map_expr env e)
  | __else__ ->
      let any = G.Ar arg in
      Unnamed (fixme_exp ToDo any (Related any))

and map_comp_to_nested_loop env e xs : exp =
  (* (e FOR pat IN iter IF cond) ->

      FOR pat IN iter {
          IF cond {
              tmp = e;
          }
      }
      tmp

      The more faithful translation would involve "adding" elements to a
      collection rather than reassigning a scalar value as we do here. However,
      for the purposes of the dataflow analyses we are doing, this ought to
      suffice.
  *)
  let ss, e' = map_expr_with_pre_stmts env e in
  let fresh = fresh_lval env (G.fake "comp") in
  let assign = mk_s (Instr (mk_i (Assign (fresh, e')) (related_exp e))) in
  let rec fold_comps env = function
    | [] -> [ assign ]
    | comp :: stmts -> (
        match comp with
        | G.CompFor (t_for, pat, t_in, iter) ->
            let cont_label, break_label, st_env =
              mk_break_continue_labels env t_for
            in
            let cont_label_s = [ mk_s (Label cont_label) ] in
            let body_stmts = fold_comps st_env stmts in
            map_for_each_aux env t_for pat t_in iter body_stmts cont_label_s
            @ [ mk_s (Label break_label) ]
        | G.CompIf (t_if, e) ->
            let ss, e' = map_expr_with_pre_stmts env e in
            let st = fold_comps env stmts in
            ss @ [ mk_s (If (t_if, e', st, [])) ])
  in
  fold_comps env xs @ ss |> add_stmts env;
  mk_e (Fetch fresh) (related_exp e)

and map_record env ((_tok, origfields, _) as record_def) : exp =
  let e_gen = G.Record record_def |> G.e in
  let fields =
    origfields
    |> List.filter_map (function
      | G.F
          {
            s =
              G.DefStmt
                ( { G.name = G.EN (G.Id (id, id_info)); tparams = None; _ },
                  def_kind );
            _;
          } as forig ->
          let field_name = var_of_id_info id id_info in
          let field_def =
            match def_kind with
            (* TODO: Consider what to do with vtype. *)
            | G.VarDef { G.vinit = Some fdeforig; _ }
            | G.FieldDefColon { G.vinit = Some fdeforig; _ } ->
                map_expr env fdeforig
            (* Some languages such as javascript allow function
                  definitions in object literal syntax. *)
            | G.FuncDef fdef ->
                let tok = snd fdef.fkind in
                let lval = fresh_lval env tok in
                (* See NOTE(config.stmts)! *)
                let fdef =
                  map_function_definition { env with stmts = ref [] } fdef
                in
                let forig = Related (G.Fld forig) in
                add_nested_def env tok lval (FuncDef fdef) forig;
                mk_e (Fetch lval) forig
            | ___else___ -> todo (G.E e_gen)
          in
          Some (Field (field_name, field_def))
      | G.F
          {
            s =
              G.ExprStmt
                ( {
                    e = Call ({ e = Special (Spread, _); _ }, (_, [ Arg e ], _));
                    _;
                  },
                  _ );
            _;
          } ->
          Some (Spread (map_expr env e))
      | G.F
          {
            s =
              G.ExprStmt
                ( ({
                     e =
                       Call
                         ( { e = N (Id (id, id_info)); _ },
                           (_, [ Arg { e = Record fields; _ } ], _) );
                     _;
                   } as prior_expr),
                  _ );
            _;
          }
        when is_hcl env.lang ->
          (* This is an inner block of the form
                someblockhere {
                  s {
                    <args>
                  }
                }

                We want this to be understood as a record of { <args> } being bound to
                the name `s`.

                So we just translate it to a field defining `s = <record>`.

                We don't actually really care for it to be specifically defining the name `s`.
                we just want it in there at all so that we can use it as a sink.
             *)
          let field_name = var_of_id_info id id_info in
          let field_expr = map_record env fields in
          (* We need to use the entire `prior_expr` here, or the range won't be quite
                right (we'll leave out the identifier)
             *)
          Some
            (Field (field_name, { field_expr with eorig = SameAs prior_expr }))
      | _ when is_hcl env.lang ->
          (* For HCL constructs such as `lifecycle` blocks within a module call, the
                IL translation engine will brick the whole record if it is encountered.
                To avoid this, we will just ignore any unrecognized fields for HCL specifically.
             *)
          log_warning "Skipping HCL record field during IL translation";
          None
      | G.F _ -> todo (G.E e_gen))
  in
  mk_e (RecordOrDict fields) (SameAs e_gen)

and map_dict env (_, orig_entries, _) orig : exp =
  let entries =
    orig_entries
    |> List.map (fun orig_entry ->
        match orig_entry.G.e with
        | G.Container (G.Tuple, (_, [ korig; vorig ], _)) ->
            let ke = map_expr env korig in
            let ve = map_expr env vorig in
            Entry (ke, ve)
        | __else__ -> todo (G.E orig))
  in
  mk_e (RecordOrDict entries) (SameAs orig)

and map_xml_expr env ~ret eorig xml : exp =
  let tok, jsx_name =
    match xml.G.xml_kind with
    | G.XmlClassic (tok, name, _, _)
    | G.XmlSingleton (tok, name, _) ->
        (tok, Some name)
    | G.XmlFragment (tok, _) -> (tok, None)
  in
  let body =
    xml.G.xml_body
    |> List.filter_map (function
      | G.XmlExpr (tok, Some eorig, _) ->
          let exp = map_expr env eorig in
          let _, lval = mk_aux_var env tok exp in
          Some (mk_e (Fetch lval) (SameAs eorig))
      | G.XmlXml xml' ->
          let eorig' = SameAs (G.Xml xml' |> G.e) in
          Some (map_xml_expr env ~ret:`Tmp eorig' xml')
      | G.XmlExpr (_, None, _)
      | G.XmlText _ ->
          None)
  in
  match jsx_name with
  | Some jsx_name when Lang.is_js env.lang ->
      (* Model `<Foo x={y}>{bar}</Foo>` as `Foo({x: y, children: [bar])`
       *
       * Technically, this should be modeled as `React.createElement(Foo, {x:
       * y}, bar)`, and we should then correctly track taint through the call to
       * `React.createElement`. But realistically we can shortcut that and just
       * model it as a direct call.
       *
       * This works for functional components, which are standard practice these
       * days. In order to correctly model older kinds of React components,
       * we'll need to do more work. *)
      let name_eorig = G.N jsx_name |> G.e in
      let name_lval = map_name env name_eorig jsx_name in
      let e = mk_e (Fetch name_lval) (SameAs name_eorig) in
      let fields =
        xml.G.xml_attrs
        |> List.filter_map (function
          | G.XmlAttr (id, tok, eorig) ->
              (* e.g. <Foo x={y}/> *)
              let attr_name =
                {
                  ident = id;
                  sid = G.SId.unsafe_default;
                  id_info = G.empty_id_info ();
                }
              in
              let e = map_expr env eorig in
              let _, lval = mk_aux_var env tok e in
              let e = mk_e (Fetch lval) (SameAs eorig) in
              Some (Field (attr_name, e))
          | G.XmlAttrExpr (_l, eorig, _r) ->
              let e = map_expr env eorig in
              Some (Spread e)
          | G.XmlEllipsis _ ->
              (* Should never encounter this in a target *)
              None)
      in
      let body_exp =
        mk_e
          (Composite (CArray, Tok.unsafe_fake_bracket body))
          (Related (G.Xmls xml.G.xml_body))
      in
      let children_field_name =
        {
          ident = ("children", G.fake "children");
          sid = G.SId.unsafe_default;
          id_info = G.empty_id_info ();
        }
      in
      let fields = Field (children_field_name, body_exp) :: fields in
      let fields_orig =
        let attrs = xml.G.xml_attrs |> List.map (fun attr -> G.XmlAt attr) in
        let body = G.Xmls xml.G.xml_body in
        Related (G.Anys (body :: attrs))
      in
      let record = mk_e (RecordOrDict fields) fields_orig in
      let args = [ Unnamed record ] in
      add_call env tok eorig ~ret (Call (e, args))
  | Some _
  | None ->
      let attrs =
        xml.G.xml_attrs
        |> List.filter_map (function
          | G.XmlAttr (_, tok, eorig)
          | G.XmlAttrExpr (tok, eorig, _) ->
              let exp = map_expr env eorig in
              let _, lval = mk_aux_var env tok exp in
              Some (mk_e (Fetch lval) (SameAs eorig))
          | _ -> None)
      in
      mk_e
        (Composite (CTuple, (tok, List.rev_append attrs body, tok)))
        (Related (G.Xmls xml.G.xml_body))

(* 'is_returned' iff the result of evaluating 'st' is the result of the function
  being translated

  TODO: We should treat all statements the same way and don't have this duplication.
  TODO: We should also re-think implicit-return analysis... maybe it should be
    integrated into AST_to_IL ??
 *)
and map_stmt_expr env ?(is_returned = false) ?g_expr st : exp =
  let todo () =
    match g_expr with
    | None -> fixme_exp ToDo (G.E (G.e (G.StmtExpr st))) (Related (G.S st))
    | Some e_gen -> fixme_exp ToDo (G.E e_gen) (Related (G.S st))
  in
  match st.G.s with
  | G.ExprStmt (eorig, tok) ->
      (* How do we end up here? For example, in the Scala code below we have
          "Yield(StmtExpr(Block[...; ExprStmt(Id tainted)]))":

            for (x <- xs) yield {
              val tainted = source(x)
              tainted
            }
       *)
      if is_returned then
        (* The 'is_returned' path will not go through 'map_expr_stmt' where we
          have some code adding a dummy `tmp = E` so that 'eorig' can be later
          marked by 'Implicit_return.markmark_first_instr_ancestor', so we mark
          'eorig' here. *)
        eorig.is_implicit_return <- true;
      let e = map_expr env eorig in
      if eorig.is_implicit_return then (
        mk_s (Return (tok, e)) |> add_stmt env;
        map_expr_opt env tok None)
      else e
  | G.OtherStmt
      ( OS_Delete,
        ( [ (G.Tk tok as atok); G.E eorig ]
        | [ (G.Tk tok as atok); G.Tk _; G.Tk _; G.E eorig ] (* delete[] *) ) )
    ->
      let e = map_expr env eorig in
      let special = (Delete, tok) in
      add_instr env
        (mk_i
           (AssignCall
              ( None,
                {
                  c = CallSpecial (special, [ Unnamed e ]);
                  corig = Related atok;
                } ))
           (Related atok));
      mk_unit tok (Related atok)
  | G.If (tok, cond, st1, opt_st2) ->
      (* if cond then e1 else e2
       * -->
       * if cond {
       *   tmp = e1;
       * }
       * else {
       *   tmp = e2;
       * }
       * tmp
       *
       * TODO: Look at RIL (used by Diamondblack Ruby) for insiration,
       *       see https://www.cs.umd.edu/~mwh/papers/ril.pdf.
       *)
      let ss, e' = map_cond_with_pre_stmts env cond in
      let pre_a1, e1 = map_stmt_expr_with_pre_stmts env ~is_returned st1 in
      let pre_a2, e2 =
        match opt_st2 with
        | Some st2 -> map_stmt_expr_with_pre_stmts env ~is_returned st2
        | None ->
            (* Coming from OCaml-land we would not expect this to happen... but
             * we got some Ruby examples from r2c's SR team where there is an `if`
             * expression without an `else`... anyways, if it happens we translate
             * what we can, and we fill-in the `else` with a "fixme" node. *)
            ([], fixme_exp ToDo (G.Tk tok) (Related (G.S st)))
      in
      let fresh = fresh_lval env tok in
      let a1 = mk_s (Instr (mk_i (Assign (fresh, e1)) (related_tok tok))) in
      let a2 = mk_s (Instr (mk_i (Assign (fresh, e2)) (related_tok tok))) in
      add_stmts env
        (ss @ [ mk_s (If (tok, e', pre_a1 @ [ a1 ], pre_a2 @ [ a2 ])) ]);
      let eorig =
        match g_expr with
        | None -> related_exp (G.e (G.StmtExpr st))
        | Some e_gen -> SameAs e_gen
      in
      mk_e (Fetch fresh) eorig
  | G.For (tok, for_header, st)
    when lang_has_yield_in_for_comprehension env.lang ->
      let yield_var = fresh_var ~str:"__yield" env tok in
      let ss =
        map_for { env with yield_var = Some yield_var } tok for_header st
      in
      add_stmts env ss;
      let orig = Related (G.Tk tok) in
      let e = mk_e (Fetch (lval_of_base (Var yield_var))) orig in
      if is_returned then mk_s (Return (G.fake "return", e)) |> add_stmt env;
      e
  | G.Switch (_tok, Some scrutinee, branches) when Lang.(equal env.lang Scala)
    -> (
      match Hook.get hook_compile_pattern_matching with
      | Some compile_fn ->
          let ss, e =
            compile_fn env ~cond_with_pre_stmts:map_cond_with_pre_stmts
              ~stmt_expr_with_pre_stmts:
                (map_stmt_expr_with_pre_stmts ~is_returned)
              scrutinee branches
          in
          add_stmts env ss;
          e
      | None ->
          map_stmt env st |> add_stmts env;
          todo ())
  | G.Block (t, block, _) -> (
      (* See 'AST_generic.stmt_to_expr' *)
      match List.rev block with
      | st :: rev_sts ->
          rev_sts |> List.rev |> List.concat_map (map_stmt env) |> add_stmts env;
          map_stmt_expr env ~is_returned st
      | [] -> mk_unit t (Related (G.S st)))
  | G.Return (t, eorig, _) ->
      mk_s (Return (t, map_expr_opt env t eorig)) |> add_stmt env;
      map_expr_opt env t None
  | G.DefStmt (ent, G.VarDef { G.vinit = Some e; vtype = opt_ty; vtok = _ })
    when def_expr_evaluates_to_value env.lang ->
      map_type_opt env opt_ty;
      (* We may end up here due to Elixir_to_elixir's parsing. Other languages
       * such as Ruby, Julia, and C seem to result in Assignments, not DefStmts.
       *)
      let e = map_expr env e in
      let lv = map_lval_of_ent env ent in
      mk_i (Assign (lv, e)) (Related (G.S st)) |> add_instr env;
      mk_e (Fetch lv) (related_exp (G.e (G.StmtExpr st)))
  | __else__ ->
      (* In any case, let's make sure the statement is in the IL translation
       * so that e.g. taint can do its job. *)
      map_stmt env st |> add_stmts env;
      todo ()

(*****************************************************************************)
(* Exprs and instrs *)
(*****************************************************************************)

and map_lval_of_ent env ent : lval =
  match ent.G.name with
  | G.EN (G.Id (id, idinfo)) -> lval_of_id_info env id idinfo
  | G.EN name -> map_lval env (G.N name |> G.e)
  | G.EDynamic eorig -> map_lval env eorig
  | G.EPattern (PatId (id, id_info)) ->
      map_lval env (G.N (Id (id, id_info)) |> G.e)
  | G.EPattern _ -> (
      let any = G.En ent in
      log_fixme ToDo any;
      let toks = AST_generic_helpers.ii_of_any any in
      match toks with
      | [] -> raise Impossible
      | x :: _ -> fresh_lval env x)
  | G.OtherEntity _ -> (
      let any = G.En ent in
      log_fixme ToDo any;
      let toks = AST_generic_helpers.ii_of_any any in
      match toks with
      | [] -> raise Impossible
      | x :: _ -> fresh_lval env x)

and map_expr_with_pre_stmts env ?ret e : stmt list * exp =
  with_pre_stmts env (fun env -> map_expr env ?ret e)

and map_stmt_expr_with_pre_stmts env ?is_returned st : stmt list * exp =
  with_pre_stmts env (fun env -> map_stmt_expr env ?is_returned st)

(* alt: could use H.cond_to_expr and reuse expr_with_pre_stmts *)
and map_cond_with_pre_stmts env cond : stmt list * exp =
  with_pre_stmts env (fun env ->
      match cond with
      | G.Cond e -> map_expr env e
      | G.OtherCond
          ( todok,
            [
              (Def (ent, VarDef { G.vinit = Some e; vtype = opt_ty; vtok = _ })
               as def);
            ] ) ->
          map_type_opt env opt_ty;
          (* e.g. C/C++: `if (const char *tainted_or_null = source("PATH"))` *)
          let e' = map_expr env e in
          let lv = map_lval_of_ent env ent in
          add_instr env (mk_i (Assign (lv, e')) (Related def));
          mk_e (Fetch lv) (Related (G.TodoK todok))
      | G.OtherCond (categ, xs) ->
          let e = G.OtherExpr (categ, xs) |> G.e in
          log_fixme ToDo (G.E e);
          map_expr env e)

and map_arg_with_pre_stmts env arg : stmt list * exp argument =
  with_pre_stmts env (fun env -> map_argument env arg)

and map_args_with_pre_stmts env args : stmt list * exp argument list =
  with_pre_stmts env (fun env -> map_arguments env args)

and map_expr_with_pre_stmts_opt env tok eopt : stmt list * exp =
  match eopt with
  | None -> ([], map_expr_opt env tok None)
  | Some e -> map_expr_with_pre_stmts env e

and map_for_var_or_expr_list env xs : stmt list =
  xs
  |> List.concat_map (function
    | G.ForInitExpr e ->
        let ss, _eIGNORE = map_expr_with_pre_stmts env e in
        ss
    | G.ForInitVar (ent, vardef) -> (
        (* copy paste of VarDef case in stmt *)
        match vardef with
        | { G.vinit = Some e; vtype = opt_ty; vtok = _ } ->
            let ss1, e' = map_expr_with_pre_stmts env e in
            let ss2 = map_type_opt_with_pre_stmts env opt_ty in
            let lv = map_lval_of_ent env ent in
            ss1 @ ss2
            @ [ mk_s (Instr (mk_i (Assign (lv, e')) (Related (G.En ent)))) ]
        | _ -> []))

(*****************************************************************************)
(* Parameters *)
(*****************************************************************************)
and map_param ~map_default : G.parameter -> param = function
  | G.Param { pname = Some i; pinfo; pdefault; _ } ->
      Param
        {
          pname = var_of_id_info i pinfo;
          pdefault = Option.map map_default pdefault;
        }
  | G.ParamPattern pat -> PatternParam pat
  | G.ParamRest (_tok, { pname = Some i; pinfo; pdefault = None; _ }) ->
      (* Not expecting default values for ParamRest/varargs. *)
      ParamRest (var_of_id_info i pinfo)
  | G.Param { pname = None; _ }
  | G.ParamRest (_, _)
  | G.ParamHashSplat (_, _)
  | G.ParamEllipsis _
  | G.ParamReceiver _
  | G.OtherParam (_, _) ->
      FixmeParam (* TODO *)

(** Lowers parameters, including default value expressions. *)
and map_parameters env params : param list =
  (* HACK: TODO: For languages like Python where defaults are evaluated at
     definition time rather than at each call site, re-running the default
     initializer at each call site is semantically wrong. In practice this
     doesn't matter for taint analysis since we only track final variables. *)
  let map_default e =
    let dinit, dexp = with_pre_stmts env (fun env' -> map_expr env' e) in
    { dinit; dexp }
  in
  params |> Tok.unbracket |> List.map (map_param ~map_default)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)

and map_type_ env (ty : G.type_) : G.type_ =
  (* Expressions inside types also need to be analyzed.
   *
   * E.g., in C we need to be able to do const prop here:
   *
   *     int x = 3;
   *     int arr[x]; // should match 'int arr[3]'
   *)
  let exps =
    match ty.t with
    | G.TyArray ((_, Some e, _), _)
    | G.TyExpr e ->
        [ map_expr env e ]
    | __TODO__ -> []
  in
  let tok = G.fake "type" in
  exps
  |> List.iter (fun e ->
      (* We add a fake assignment for dataflow analysis to reach these
       * expressions occurring inside types. *)
      mk_aux_var ~force:true ~str:"_type" env tok e |> ignore);
  ty

and map_type_with_pre_stmts env ty : stmt list * G.type_ =
  with_pre_stmts env (fun env -> map_type_ env ty)

and map_type_opt env opt_ty =
  opt_ty |> Option.iter (fun ty -> map_type_ env ty |> ignore)

and map_type_opt_with_pre_stmts env opt_ty =
  let ss, () = with_pre_stmts env (fun env -> map_type_opt env opt_ty) in
  ss

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)

(* NOTE: There should not be direct calls to 'expr' from here on, instead
 * use 'expr_with_pre_stmts' or other '*_pre_stmts*' functions. Just so that
 * we don't forget about 'env.stmts'! *)

and no_switch_fallthrough : Lang.t -> bool = function
  | Go
  | Ruby
  | Rust
  | Scala ->
      true
  | _ -> false

(** [add_break_label env label] adds an existing [label] to the top of the
[break_labels] list. *)
and add_break_label env label =
  { env with break_labels = label :: env.break_labels }

and mk_break_continue_labels env tok : label * label * env =
  let cont_label = fresh_label ~label:"__loop_continue" env tok in
  let break_label = fresh_label ~label:"__loop_break" env tok in
  let st_env =
    {
      env with
      break_labels = break_label :: env.break_labels;
      cont_label = Some cont_label;
    }
  in
  (cont_label, break_label, st_env)

and mk_switch_break_label env tok : label * stmt list * env =
  let break_label = fresh_label ~label:"__switch_break" env tok in
  let switch_env =
    { env with break_labels = break_label :: env.break_labels }
  in
  (break_label, [ mk_s (Label break_label) ], switch_env)

and implicit_return env eorig tok : stmt list =
  (* We always expect a value from an expression that is implicitly
   * returned, so void is set to false here.
   *)
  let ss, e = map_expr_with_pre_stmts ~ret:`Tmp env eorig in
  let ret = mk_s (Return (tok, e)) in
  ss @ [ ret ]

and map_expr_stmt env (eorig : G.expr) tok : IL.stmt list =
  (* optimize? pass context to expr when no need for return value? *)
  let ss, e = map_expr_with_pre_stmts ~ret:`Void env eorig in

  (* Some expressions may return unit, and if we call mk_aux_var below, not only
   * is it extraneous, but it also interferes with implicit return analysis.
   *
   * For example,
   *   call f()
   *   tmp = unit
   * interferes with implicit return analysis, because the analysis walks
   * backwards from the exit node to mark the first instr node it sees on each
   * path.
   *
   * If we have
   *   call f()
   *   tmp = unit
   * then `unit` will be marked as a returning expression when we actually
   * want to mark `f()`, so we must avoid creating `tmp = unit` following
   * a function call that doesn't expect results.
   *)
  (match e.e with
  | Literal (G.Unit _) -> ()
  | _else_ -> mk_aux_var env tok e |> ignore);

  let ss' = pop_stmts env in
  match ss @ ss' with
  | [] ->
      (* This case may happen when we have a function like
       *
       *   function some_function(some_var) {
       *     some_var
       *   }
       *
       * the `some_var` will not show up in the CFG. Neither expr_with_pre_stmts
       * nor mk_aux_var will cause nodes to be created.
       *
       * This is typically OK, because it doesn't make sense to write
       * `some_var` for side-effects.
       *
       * The issue is that for some languages
       * when `some_var` is the last evaluated expression in the function,
       * `some_var` is also implicitly returned from the function. In this case
       * `some_var` actually means `return some_var`, so there should be a return
       * node in the CFG.
       *
       * We'd like to always create an IL node here as a fake "no-op" assignment
       *   tmp = some_var
       * because we'd like to mark some_var's eorig as an implicit return node
       * so later we can convert
       *   some_var
       * to
       *   return some_var
       * when some_var is marked as an implicit return node.
       *
       * If some_var isn't a returning expression, we have created an unneeded node
       * but it doesn't affect correctness.
       *)
      let var = fresh_var env tok in
      let lval = lval_of_base (Var var) in
      let fake_i = mk_i (Assign (lval, e)) NoOrig in
      [ mk_s (Instr fake_i) ]
  | ss'' -> ss''

and mk_class_construction env obj origin_exp ty cons_id_info args :
    lval * stmt list =
  (* We encode `obj = new T(args)` as `obj = new obj.T(args)` so that taint
     analysis knows that the reciever when calling `T` is the variable
     `obj`. It's kinda hacky but works for now. *)
  let lval = lval_of_base (Var obj) in
  let ss1, args' = map_args_with_pre_stmts env (Tok.unbracket args) in
  let opt_cons =
    let* cons = mk_class_constructor_name ty cons_id_info in
    let cons' = var_of_name cons in
    let cons_exp =
      mk_e
        (Fetch { lval with rev_offset = [ { o = Dot cons'; oorig = NoOrig } ] })
        (SameAs (G.N cons |> G.e))
      (* THINK: ^^^^^ We need to construct a `SameAs` eorig here because Pro
       * looks at the eorig, but maybe it shouldn't? *)
    in
    Some cons_exp
  in
  let ss2, ty = map_type_with_pre_stmts env ty in
  ( lval,
    ss1 @ ss2
    @ [
        mk_s
          (Instr (mk_i (New (lval, ty, opt_cons, args')) (SameAs origin_exp)));
      ] )

and map_stmt_aux env st : stmt list =
  match st.G.s with
  | G.ExprStmt (eorig, tok) -> (
      match eorig with
      | { is_implicit_return = true; _ } -> implicit_return env eorig tok
      (* See NOTE "yield as return". *)
      | { e = Yield (_, Some e, _); _ } when lang_treat_yield_as_return env.lang
        ->
          implicit_return env e tok
      | _ -> map_expr_stmt env eorig tok)
  | G.DefStmt
      ( { name = EN obj; _ },
        G.VarDef
          {
            G.vinit =
              Some ({ e = G.New (_tok, ty, cons_id_info, args); _ } as new_exp);
            _;
          } ) ->
      (* T x = new T(args) *)
      (* HACK(new): Because of field-sensitivity hacks, we need to know to which
       * variable are we assigning the `new` object, so we intercept the assignment. *)
      let obj' = var_of_name obj in
      mk_class_construction env obj' new_exp ty cons_id_info args |> snd
  | G.DefStmt (ent, G.VarDef { G.vinit = Some e; vtype = opt_ty; vtok = _ }) ->
      let sorig = Related (G.S st) in
      let lv = map_lval_of_ent env ent in
      let ss1, needs_assign_st =
        match lval_is_just_var lv with
        | Some name -> (
            let ss, e' =
              map_expr_with_pre_stmts env ~ret:(`Var (name, sorig)) e
            in
            match exp_is_just_var e'.e with
            | Some name' when IL.equal_name name name' ->
                (* e' = Fetch lv so no need for na extra assignment *)
                (ss, None)
            | Some _ (* different name *)
            | None ->
                (ss, Some e'))
        | None ->
            let ss, e' = map_expr_with_pre_stmts env e in
            (ss, Some e')
      in
      let assign_st =
        match needs_assign_st with
        | Some e' -> [ mk_s (Instr (mk_i (Assign (lv, e')) sorig)) ]
        | None -> []
      in
      let ss2 = map_type_opt_with_pre_stmts env opt_ty in
      ss1 @ ss2 @ assign_st
  | G.DefStmt (_ent, G.VarDef { G.vinit = None; vtype = Some ty; vtok = _ }) ->
      (* We want to analyze any expressions in 'ty'. *)
      let ss, _ = map_type_with_pre_stmts env ty in
      ss
  | G.DefStmt ((ent, _) as def) ->
      let use_nested_def = H.definition_is_func def && H.entity_is_local ent in
      let def = map_definition env def in
      if use_nested_def then [ mk_s (NestedDef def) ]
      else [ mk_s (MiscStmt (DefStmt def)) ]
  | G.DirectiveStmt dir -> [ mk_s (MiscStmt (DirectiveStmt dir)) ]
  | G.Block xs -> (
      let any_to_stmt s =
        match s with
        (* Intended only to be used for the ForOrElse and WhileOrElse statements. *)
        | G.S s -> s
        | _ -> impossible s
      in
      let xs = Tok.unbracket xs in
      match List.map (fun x -> x.G.s) xs with
      | [
       G.For (tok, G.ForEach (pat, tok2, e), main_st);
       G.OtherStmt (G.OS_ForOrElse, else_st);
      ] ->
          (* Python:
            for <pat> in <e>:
                <main_st>
            else:
                <else_st>
           *)
          let else_st = else_st |> List.map any_to_stmt in
          map_for_each env tok (pat, tok2, e) main_st (Some else_st)
      | [ G.While (tok, e, main_st); G.OtherStmt (G.OS_WhileOrElse, else_st) ]
        ->
          (* Python:
            while <e>:
                <main_st>
            else:
                <else_st>
           *)
          let else_st = else_st |> List.map any_to_stmt in
          map_while_aux env tok e main_st (Some else_st)
      | __else__ -> List.concat_map (map_stmt env) xs)
  | G.If (tok, cond, st1, st2) ->
      let ss, e' = map_cond_with_pre_stmts env cond in
      let st1 = map_stmt env st1 in
      let st2 = List.concat_map (map_stmt env) (st2 |> Option.to_list) in
      ss @ [ mk_s (If (tok, e', st1, st2)) ]
  | G.Switch (tok, switch_expr_opt, cases_and_bodies) ->
      let ss, translate_cases =
        match switch_expr_opt with
        | Some switch_expr ->
            let ss, switch_expr' = map_cond_with_pre_stmts env switch_expr in
            ( ss,
              map_switch_expr_and_cases_to_exp env tok
                (H.cond_to_expr switch_expr)
                switch_expr' )
        | None -> ([], map_cases_to_exp env tok)
      in
      let break_label, break_label_s, switch_env =
        mk_switch_break_label env tok
      in
      let jumps, bodies =
        map_cases_and_bodies_to_stmts switch_env tok break_label translate_cases
          cases_and_bodies
      in
      ss @ jumps @ bodies @ break_label_s
  | G.While (tok, e, st) -> map_while_aux env tok e st None
  | G.DoWhile (tok, st, e) ->
      let cont_label, break_label, st_env = mk_break_continue_labels env tok in
      let cont_label_s = [ mk_s (Label cont_label) ] in
      let break_label_s = [ mk_s (Label break_label) ] in
      let st = map_stmt st_env st in
      let ss, e' = map_expr_with_pre_stmts env e in
      st @ ss
      @ [ mk_s (Loop (tok, e', st @ cont_label_s @ ss)) ]
      @ break_label_s
  | G.For (tok, for_header, st) ->
      let ss = map_for env tok for_header st in
      ss
  (* TODO: repeat env work of controlflow_build.ml *)
  | G.Continue (tok, lbl_ident, _) -> (
      match lbl_ident with
      | G.LNone -> (
          match env.cont_label with
          | None -> impossible (G.Tk tok)
          | Some lbl -> [ mk_s (Goto (tok, lbl)) ])
      | G.LId lbl -> [ mk_s (Goto (tok, label_of_label env lbl)) ]
      | G.LInt _
      | G.LDynamic _ ->
          todo (G.S st))
  | G.Break (tok, lbl_ident, _) -> (
      match lbl_ident with
      | G.LNone -> (
          match env.break_labels with
          | [] -> impossible (G.Tk tok)
          | lbl :: _ -> [ mk_s (Goto (tok, lbl)) ])
      | G.LId lbl -> [ mk_s (Goto (tok, label_of_label env lbl)) ]
      | G.LInt (i, _) -> (
          match List.nth_opt env.break_labels i with
          | None -> impossible (G.Tk tok)
          | Some lbl -> [ mk_s (Goto (tok, lbl)) ])
      | G.LDynamic _ -> impossible (G.Tk tok))
  | G.Label (lbl, st) ->
      let lbl = label_of_label env lbl in
      let st = map_stmt env st in
      [ mk_s (Label lbl) ] @ st
  | G.Goto (tok, lbl, _sc) ->
      let lbl = lookup_label env lbl in
      [ mk_s (Goto (tok, lbl)) ]
  | G.Return (tok, eopt, _) ->
      let ss, e = map_expr_with_pre_stmts_opt env tok eopt in
      ss @ [ mk_s (Return (tok, e)) ]
  | G.Assert (tok, args, _) ->
      let ss, args = map_args_with_pre_stmts env (Tok.unbracket args) in
      let special = (Assert, tok) in
      (* less: wrong e? would not be able to match on Assert, or
       * need add sorig:
       *)
      ss
      @ [
          mk_s
            (Instr
               (mk_i
                  (AssignCall
                     ( None,
                       {
                         c = CallSpecial (special, args);
                         corig = Related (G.S st);
                       } ))
                  (Related (G.S st))));
        ]
  | G.Throw (tok, e, _) ->
      let ss, e = map_expr_with_pre_stmts env e in
      ss @ [ mk_s (Throw (tok, e)) ]
  | G.OtherStmt (G.OS_ThrowNothing, [ G.Tk tok ]) ->
      (* Python's `raise` without arguments *)
      let eorig = related_tok tok in
      let todo_exp = fixme_exp ToDo (G.Tk tok) eorig in
      [ mk_s (Throw (tok, todo_exp)) ]
  | G.OtherStmt
      (G.OS_ThrowFrom, [ G.E from; G.S ({ s = G.Throw _; _ } as throw_stmt) ])
    ->
      (* Python's `raise E1 from E2` *)
      let todo_stmt = fixme_stmt ToDo (G.E from) in
      todo_stmt @ map_stmt env throw_stmt
  | G.OtherStmt (OS_Go, [ G.E ({ e = G.Call _; _ } as eorig) ]) ->
      (* Translate the goroutine call as a plain function call.
         Goroutines' return value, if any, is dismissed. The call
         is effectively void. *)
      let ss, _unit = map_expr_with_pre_stmts env ~ret:`Tmp eorig in
      (* This statement is there to document the approximation *)
      let todo_stmt =
        fixme_stmt ToDo (G.TodoK ("goroutine", G.fake "goroutine"))
      in
      todo_stmt @ ss
  | G.Try (_tok, try_st, catches, opt_else, opt_finally) ->
      map_try_catch_else_finally env ~try_st ~catches ~opt_else ~opt_finally
  | G.WithUsingResource (_, stmt1, stmt2) ->
      let stmt1 = List.concat_map (map_stmt env) stmt1 in
      let stmt2 = map_stmt env stmt2 in
      stmt1 @ stmt2
  | G.DisjStmt _ -> sgrep_construct (G.S st)
  | G.OtherStmtWithStmt (G.OSWS_With, [ G.E manager_as_pat ], body) ->
      let opt_pat, manager =
        (* Extract <manager> and <pat> from `with <manager> as <pat>`;
         * <manager> is an expression that evaluates to a context manager,
         * <pat> is optional. *)
        match manager_as_pat.G.e with
        | G.LetPattern (pat, manager) -> (Some pat, manager)
        | _ -> (None, manager_as_pat)
      in
      map_python_with_stmt env manager opt_pat body
  (* Java: synchronized (E) S *)
  | G.OtherStmtWithStmt (G.OSWS_Block _, [ G.E objorig ], stmt1) ->
      (* TODO: Restrict this to a syncrhonized block ? *)
      let ss, _TODO_obj = map_expr_with_pre_stmts env objorig in
      ss @ map_stmt env stmt1
  (* Rust: unsafe block *)
  | G.OtherStmtWithStmt (G.OSWS_Block ("Unsafe", tok), [], stmt1) ->
      let todo_stmt = fixme_stmt ToDo (G.TodoK ("unsafe_block", tok)) in
      todo_stmt @ map_stmt env stmt1
  | G.OtherStmt (OS_Async, [ G.S stmt1 ]) ->
      let todo_stmt = fixme_stmt ToDo (G.TodoK ("async", G.fake "async")) in
      todo_stmt @ map_stmt env stmt1
  | G.OtherStmt _
  | G.OtherStmtWithStmt _ ->
      todo (G.S st)
  | G.RawStmt _ -> todo (G.S st)

and map_while_aux env tok e main_st else_st =
  let cont_label, break_label, main_env = mk_break_continue_labels env tok in
  let cont_label_s = [ mk_s (Label cont_label) ] in
  let break_label_s = [ mk_s (Label break_label) ] in
  let ss, e' = map_cond_with_pre_stmts env e in
  let main_st = map_stmt main_env main_st in
  let else_st =
    match else_st with
    | None -> []
    | Some else_st ->
        let else_env = add_break_label env break_label in
        List.concat_map (map_stmt else_env) else_st
  in
  ss
  @ [ mk_s (Loop (tok, e', main_st @ cont_label_s @ ss)) ]
  @ else_st @ break_label_s

(* THINK: Hanndle 'ForOrElse' here too? *)
and map_for env tok for_header st =
  match for_header with
  | G.ForEach (pat, tok2, e) -> map_for_each env tok (pat, tok2, e) st None
  | G.MultiForEach [] -> map_stmt env st
  | G.MultiForEach (FEllipsis _ :: _) -> sgrep_construct (G.S st)
  | G.MultiForEach (FECond (fr, tok2, e) :: for_eachs) ->
      let loop = G.For (tok, G.MultiForEach for_eachs, st) |> G.s in
      let st = G.If (tok2, Cond e, loop, None) |> G.s in
      map_for_each env tok fr st None
  | G.MultiForEach (FE fr :: for_eachs) ->
      map_for_each env tok fr
        (G.For (tok, G.MultiForEach for_eachs, st) |> G.s)
        None
  | G.ForClassic (xs, eopt1, eopt2) ->
      let cont_label, break_label, st_env = mk_break_continue_labels env tok in
      let cont_label_s = [ mk_s (Label cont_label) ] in
      let break_label_s = [ mk_s (Label break_label) ] in
      let ss1 = map_for_var_or_expr_list env xs in
      let st = map_stmt st_env st in
      let ss2, cond =
        match eopt1 with
        | None ->
            let vtrue = G.Bool (true, tok) in
            ([], mk_e (Literal vtrue) (related_tok tok))
        | Some e -> map_expr_with_pre_stmts env e
      in
      let next =
        match eopt2 with
        | None -> []
        | Some e ->
            let ss, _eIGNORE = map_expr_with_pre_stmts env e in
            ss
      in
      let ss =
        ss1 @ ss2
        @ [ mk_s (Loop (tok, cond, st @ cont_label_s @ next @ ss2)) ]
        @ break_label_s
      in
      ss
  | G.ForEllipsis _ -> sgrep_construct (G.S st)

and map_for_each env tok (pat, tok2, e) main_st else_st =
  let cont_label, break_label, st_env = mk_break_continue_labels env tok in
  let cont_label_s = [ mk_s (Label cont_label) ] in
  let break_label_s = [ mk_s (Label break_label) ] in
  let stmts = map_stmt st_env main_st in
  let main_st = map_for_each_aux env tok pat tok2 e stmts cont_label_s in
  let else_st =
    match else_st with
    | None -> []
    | Some else_st ->
        let else_env = add_break_label env break_label in
        let else_stmts = List.concat_map (map_stmt else_env) else_st in
        else_stmts @ break_label_s
  in
  main_st @ else_st @ break_label_s

and map_for_each_aux env tok pat tok2 e stmts cont_label_s =
  let ss, e' =
    match e.e with
    | Call ({ e = Special (ForOf, _); _ }, (_, [ Arg e ], _)) ->
        (* JS: for (let x of E) *)
        map_expr_with_pre_stmts env e
    | __else__ -> map_expr_with_pre_stmts env e
  in
  let next_lval = fresh_lval env tok2 in
  let hasnext_lval = fresh_lval env tok2 in
  let hasnext_call =
    mk_s
      (Instr
         (mk_i
            (AssignCall
               ( Some hasnext_lval,
                 {
                   c = CallSpecial ((ForeachHasNext, tok2), [ Unnamed e' ]);
                   corig = related_tok tok2;
                 } ))
            (related_tok tok2)))
  in
  let next_call =
    mk_s
      (Instr
         (mk_i
            (AssignCall
               ( Some next_lval,
                 {
                   c = CallSpecial ((ForeachNext, tok2), [ Unnamed e' ]);
                   corig = related_tok tok2;
                 } ))
            (related_tok tok2)))
  in
  (* same semantic? or need to take Ref? or pass lval
   * directly in next_call instead of using intermediate next_lval?
   *)
  let assign_st =
    map_pattern_assign_statements env
      (mk_e (Fetch next_lval) (related_tok tok2))
      ~eorig:(related_tok tok2) pat
  in
  let cond = mk_e (Fetch hasnext_lval) (related_tok tok2) in

  (ss @ [ hasnext_call ])
  @ [
      mk_s
        (Loop
           ( tok,
             cond,
             [ next_call ] @ assign_st @ stmts @ cont_label_s
             @ [ (* ss @ ?*) hasnext_call ] ));
    ]

(* TODO: Maybe this and the following function could be merged *)
and map_switch_expr_and_cases_to_exp env tok switch_expr_orig switch_expr cases
    : stmt list * exp =
  (* If there is a scrutinee, the cases are expressions we need to check for equality with the scrutinee  *)
  let ss, es =
    List.fold_left
      (fun (ss, es) -> function
        | G.Case (tok, G.PatLiteral l) ->
            ( ss,
              {
                e =
                  Operator
                    ( (G.Eq, tok),
                      [
                        Unnamed { e = Literal l; eorig = related_tok tok };
                        Unnamed switch_expr;
                      ] );
                eorig = related_tok tok;
              }
              :: es )
        | G.Case (tok, G.OtherPat (_, [ E c ]))
        | G.CaseEqualExpr (tok, c) ->
            let c_ss, c' = map_expr_with_pre_stmts env c in
            ( ss @ c_ss,
              {
                e = Operator ((G.Eq, tok), [ Unnamed c'; Unnamed switch_expr ]);
                eorig = related_tok tok;
              }
              :: es )
        | G.Default tok ->
            (* Default should only ever be the final case, and cannot be part of a list of
               `Or`ed together cases. It's handled specially in cases_and_bodies_to_stmts
            *)
            impossible (G.Tk tok)
        | G.Case (tok, _) ->
            (ss, fixme_exp ToDo (G.Tk tok) (related_tok tok) :: es)
        | G.OtherCase ((_todo_categ, tok), _any) ->
            (ss, fixme_exp ToDo (G.Tk tok) (related_tok tok) :: es))
      ([], []) cases
  in
  ( ss,
    {
      e = Operator ((Or, tok), mk_unnamed_args es);
      eorig = SameAs switch_expr_orig;
    } )

and map_cases_to_exp env tok cases : stmt list * exp =
  (* If we have no scrutinee, the cases are boolean expressions, so we Or them together *)
  let ss, es =
    List.fold_left
      (fun (ss, es) -> function
        | G.Case (tok, G.PatLiteral l) ->
            (ss, { e = Literal l; eorig = related_tok tok } :: es)
        | G.Case (_, G.OtherPat (_, [ E c ]))
        | G.CaseEqualExpr (_, c) ->
            let c_ss, c' = map_expr_with_pre_stmts env c in
            (ss @ c_ss, c' :: es)
        | G.Default tok ->
            (* Default should only ever be the final case, and cannot be part of a list of
               `Or`ed together cases. It's handled specially in cases_and_bodies_to_stmts
            *)
            impossible (G.Tk tok)
        | G.Case (tok, _) ->
            (ss, fixme_exp ToDo (G.Tk tok) (related_tok tok) :: es)
        | G.OtherCase ((_, tok), _) ->
            (ss, fixme_exp ToDo (G.Tk tok) (related_tok tok) :: es))
      ([], []) cases
  in
  (ss, { e = Operator ((Or, tok), mk_unnamed_args es); eorig = related_tok tok })

and map_cases_and_bodies_to_stmts env tok break_label translate_cases :
    G.case_and_body list -> stmt list * stmt list = function
  | [] -> ([ mk_s (Goto (tok, break_label)) ], [])
  | G.CaseEllipsis tok :: _ -> sgrep_construct (G.Tk tok)
  | [ G.CasesAndBody ([ G.Default dtok ], body) ] ->
      let label = fresh_label ~label:"__switch_default" env tok in
      ([ mk_s (Goto (dtok, label)) ], mk_s (Label label) :: map_stmt env body)
  | G.CasesAndBody (cases, body) :: xs ->
      let jumps, bodies =
        map_cases_and_bodies_to_stmts env tok break_label translate_cases xs
      in
      let label = fresh_label ~label:"__switch_case" env tok in
      let case_ss, case = translate_cases cases in
      let jump =
        mk_s (IL.If (tok, case, [ mk_s (Goto (tok, label)) ], jumps))
      in
      let body = mk_s (Label label) :: map_stmt env body in
      let break_if_no_fallthrough =
        if no_switch_fallthrough env.lang then
          [ mk_s (Goto (tok, break_label)) ]
        else []
      in
      (case_ss @ [ jump ], body @ break_if_no_fallthrough @ bodies)

and map_stmt env st =
  try
    let pre, post = with_pre_stmts env (fun env -> map_stmt_aux env st) in
    pre @ post
  with
  | Fixme (kind, any_generic) -> fixme_stmt kind any_generic

and map_function_body env fbody : stmt list =
  match fbody with
  | G.FBExpr { e = G.StmtExpr st; _ } ->
      (* HACK: We handle these StmtExprs directly, so we can pass
          ~is_returned:true. This allows us to handle e.g. Scala's for-yield
          which is not covered by implicit-return analysis at the moment. It
          is also not clear to me if the ideal way of translating for-yield
          would be through `is_implicit_return`.

          Current implicit-return analysis mostly works for simple expression
          nodes and not for statements. In Scala the `yield` expression may not
          be something we want to mark as implicitly returned, but perhaps the
          `for` loop itself ??
          *)
      let ss, _ = map_stmt_expr_with_pre_stmts env ~is_returned:true st in
      ss
  | _ ->
      let body_stmt = H.funcbody_to_stmt fbody in
      map_stmt env body_stmt

(* We keep it really simple, very far from what would be the proper translation
 * (see https://www.python.org/dev/peps/pep-0343/):
 *
 *     with MANAGER as PAT:
 *         BODY
 *
 * ~>
 *
 *     PAT = MANAGER
 *     BODY
 *
 * Previously we used this more accurate (yet not 100% accurate) translation:
 *
 *     mgr = MANAGER
 *     value = type(mgr).__enter__(mgr)
 *     try:
 *         PAT = value
 *         BODY
 *     finally:
 *         type(mgr).__exit__(mgr)
 *
 * but to be honest we had no use for all that extra complexity, and this
 * translated prevented symbolic propagation to match e.g.
 * `Session(...).execute(...)` against:
 *
 *   with Session(engine) as s:
 *       s.execute("<query>")
 *)

and map_python_with_stmt env manager opt_pat body : stmt list =
  (* mgr = MANAGER *)
  let mgr = fresh_lval env G.sc in
  let ss_def_mgr =
    let ss_mk_mgr, manager' = map_expr_with_pre_stmts env manager in
    ss_mk_mgr @ [ mk_s (Instr (mk_i (Assign (mgr, manager')) NoOrig)) ]
  in
  (* PAT = mgr *)
  let ss_def_pat =
    match opt_pat with
    | None -> []
    | Some pat ->
        map_pattern_assign_statements env (mk_e (Fetch mgr) NoOrig)
          ~eorig:NoOrig pat
  in
  ss_def_mgr @ ss_def_pat @ map_stmt env body

(*****************************************************************************)
(* Defs *)
(*****************************************************************************)

and map_function_definition env fdef : function_definition =
  let fparams = map_parameters env fdef.G.fparams in
  let fbody = map_function_body env fdef.G.fbody in
  { fkind = fdef.fkind; fparams; frettype = fdef.G.frettype; fbody }

and map_class_params env (cparams : G.parameters) :
    class_field list * fixme_field list =
  (* Class parameters, like those in Scala, become class fields. *)
  cparams |> Tok.unbracket
  |> List.partition_map (function
    | G.Param { pname = Some id; pinfo; ptype; pattrs; pdefault } ->
        let entity =
          {
            IL.name = EN (var_of_id_info id pinfo);
            attrs = pattrs;
            tparams = None;
          }
        in
        let vinit = Option.map (map_expr env) pdefault in
        let vdef = { IL.vtype = ptype; vinit } in
        Left (entity, vdef)
    | ( G.Param { pname = None; _ }
      | G.ParamPattern _ | G.ParamRest _ | G.ParamHashSplat _
      | G.ParamEllipsis _ | G.ParamReceiver _ | G.OtherParam _ ) as param ->
        Right (G.Pa param))

and map_class_body env (cbody : G.field list G.bracket) :
    class_field list * class_method list * fixme_field list =
  (* Separate "fields" into fields (class variables) and methods, and use
    "fixme"s for what we don't know how to handle yet. *)
  let body_fields = cbody |> Tok.unbracket in
  let rev_cfields, rev_cmethods, rev_cfixmes =
    body_fields
    |> List.fold_left
         (fun (fields_acc, methods_acc, fixmes_acc) field ->
           match field with
           | G.F
               {
                 s =
                   G.DefStmt
                     ( ent,
                       (G.VarDef { G.vtype; vinit; vtok = _ } :
                         G.definition_kind) );
                 _;
               } ->
               (* Field (class variable) *)
               let vinit = Option.map (map_expr env) vinit in
               let vdef = { IL.vtype; vinit } in
               let entity = map_entity env ent in
               ((entity, vdef) :: fields_acc, methods_acc, fixmes_acc)
           | G.F { s = G.DefStmt (ent, G.FuncDef fdef); _ } ->
               (* Method *)
               let entity = map_entity env ent in
               let fdef = map_function_definition env fdef in
               (fields_acc, (entity, fdef) :: methods_acc, fixmes_acc)
           | G.F _ as other_field ->
               (fields_acc, methods_acc, G.Fld other_field :: fixmes_acc))
         ([], [], [])
  in
  (List.rev rev_cfields, List.rev rev_cmethods, List.rev rev_cfixmes)

and map_class_definition env (cdef : G.class_definition) : class_definition =
  let G.{ ckind; cextends; cimplements; cmixins; cparams; cbody } = cdef in
  let param_fields, param_fixmes = map_class_params env cparams in
  let cfields, cmethods, body_fixmes = map_class_body env cbody in
  {
    ckind;
    cextends;
    cimplements;
    cmixins;
    cfields = param_fields @ cfields;
    cmethods;
    cfixmes = param_fixmes @ body_fixmes;
  }

and map_entity env ({ name; attrs; tparams } as entity : G.entity) : IL.entity =
  let map_entity_name _env (name : G.entity_name) : IL.entity_name =
    match name with
    | EN name -> (
        match name with
        | G.Id (id, id_info)
        | G.IdQualified { name_last = id, _; name_info = id_info; _ } ->
            EN (var_of_id_info id id_info)
        | G.IdSpecial _ -> FixmeEntity (G.En entity))
    | EDynamic _
    | EPattern _
    | OtherEntity _ ->
        FixmeEntity (G.En entity)
  in
  { name = map_entity_name env name; attrs; tparams }

and map_definition env (def : G.definition) : definition =
  let entity, def_kind = def in
  let entity = map_entity env entity in
  let def_kind = map_definition_kind env def_kind in
  (entity, def_kind)

and map_definition_kind env (def : G.definition_kind) : definition_kind =
  match def with
  | VarDef _ ->
      (* This should be handled in 'stmt_aux' *)
      todo (G.Dk def)
  | FuncDef fdef -> FuncDef (map_function_definition env fdef)
  | ClassDef cdef -> ClassDef (map_class_definition env cdef)
  | ModuleDef _ (* TODO *)
  | FieldDefColon _
  | EnumEntryDef _
  | TypeDef _
  | MacroDef _
  | Signature _
  | UseOuterDecl _
  | OtherDef _ ->
      todo (G.Dk def)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let function_definition lang ?ctx fdef : function_definition =
  let env = { (empty_env lang) with ctx = ctx ||| empty_ctx } in
  map_function_definition env fdef

let stmt lang st : stmt list =
  let env = empty_env lang in
  map_stmt env st

let expr lang e : exp =
  let env = empty_env lang in
  map_expr env e

let program lang prog : program =
  let env = empty_env lang in
  prog |> List.concat_map (map_stmt env)
