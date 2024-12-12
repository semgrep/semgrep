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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* AST generic to IL translation.
 *
 * todo:
 *  - a lot ...
 *)

let locate ?tok s =
  let opt_loc =
    try Option.map Tok.stringpos_of_tok tok with
    | Tok.NoTokenLocation _ -> None
  in
  match opt_loc with
  | Some loc -> spf "%s: %s" loc s
  | None -> s

let log_debug ?tok msg = Log.debug (fun m -> m "%s" (locate ?tok msg))
let log_warning ?tok msg = Log.warn (fun m -> m "%s" (locate ?tok msg))
let log_error ?tok msg = Log.err (fun m -> m "%s" (locate ?tok msg))

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
module IdentSet = Set.Make (String)

type ctx = { entity_names : IdentSet.t }

type env = {
  lang : Lang.t;
  (* stmts hidden inside expressions that we want to move out of 'exp',
   * usually simple Instr, but can be also If when handling Conditional expr.
   *)
  stmts : stmt list ref;
  (* When entering a loop, we create two labels, one to jump to if a Continue stmt is found
     and another to jump to if a Break stmt is found. Since PHP supports breaking an arbitrary
     number of loops up, we keep a stack of break labels instead of just one
  *)
  break_labels : label list;
  cont_label : label option;
  ctx : ctx;
}

let empty_ctx = { entity_names = IdentSet.empty }

let empty_env (lang : Lang.t) : env =
  {
    stmts = ref [];
    break_labels = [];
    cont_label = None;
    ctx = empty_ctx;
    lang;
  }

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

exception Fixme of fixme_kind * G.any

let sgrep_construct any_generic = raise (Fixme (Sgrep_construct, any_generic))
let todo any_generic = raise (Fixme (ToDo, any_generic))
let impossible any_generic = raise (Fixme (Impossible, any_generic))

let log_fixme kind gany =
  let toks = AST_generic_helpers.ii_of_any gany in
  let tok = Common2.hd_opt toks in
  match kind with
  | ToDo ->
      log_warning ?tok
        "Unsupported construct(s) may affect the accuracy of dataflow analyses"
  | Sgrep_construct ->
      log_error ?tok "Cannot translate Semgrep construct(s) into IL"
  | Impossible ->
      log_error ?tok "Impossible happened during AST-to-IL translation"

let fixme_exp ?partial kind gany eorig =
  log_fixme kind (any_of_orig eorig);
  { e = FixmeExp (kind, gany, partial); eorig }

let fixme_instr kind gany eorig =
  log_fixme kind (any_of_orig eorig);
  { i = FixmeInstr (kind, gany); iorig = eorig }

let fixme_stmt kind gany =
  log_fixme kind gany;
  [ { s = FixmeStmt (kind, gany) } ]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let fresh_var ?(str = "_tmp") _env tok =
  let tok =
    (* We don't want "fake" auxiliary variables to have non-fake tokens, otherwise
       we confuse ourselves! E.g. during taint-tracking we don't want to add these
       variables to the taint trace. *)
    if Tok.is_fake tok then tok else Tok.fake_tok tok str
  in
  let i = G.SId.mk () in
  { ident = (str, tok); sid = i; id_info = G.empty_id_info () }

let fresh_label ?(label = "_label") _env tok =
  let i = G.SId.mk () in
  ((label, tok), i)

let fresh_lval ?str env tok =
  let var = fresh_var ?str env tok in
  { base = Var var; rev_offset = [] }

let var_of_id_info id id_info =
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

let var_of_name name =
  match name with
  | G.Id (id, id_info) -> var_of_id_info id id_info
  | G.IdQualified { G.name_last = id, _typeargsTODO; name_info = id_info; _ } ->
      var_of_id_info id id_info

let lval_of_id_info _env id id_info =
  let var = var_of_id_info id id_info in
  { base = Var var; rev_offset = [] }

(* TODO: use also qualifiers? *)
let lval_of_id_qualified env
    { G.name_last = id, _typeargsTODO; name_info = id_info; _ } =
  lval_of_id_info env id id_info

let lval_of_base base = { base; rev_offset = [] }

(* TODO: should do first pass on body to get all labels and assign
 * a gensym to each.
 *)
let label_of_label _env lbl = (lbl, G.SId.unsafe_default)
let lookup_label _env lbl = (lbl, G.SId.unsafe_default)
let mk_e e eorig = { e; eorig }
let mk_i i iorig = { i; iorig }
let mk_s s = { s }

let mk_unit tok eorig =
  let unit = G.Unit tok in
  mk_e (Literal unit) eorig

let add_instr env instr = Stack_.push (mk_s (Instr instr)) env.stmts

(* Create an auxiliary variable for an expression.
 *
 * If 'force' is 'false' and the expression itself is already a variable then
 * it will not create an auxiliary variable but just return that. *)
let mk_aux_var ?(force = false) ?str env tok exp =
  match exp.e with
  | Fetch ({ base = Var var; rev_offset = []; _ } as lval) when not force ->
      (var, lval)
  | __else__ ->
      let var = fresh_var ?str env tok in
      let lval = lval_of_base (Var var) in
      add_instr env (mk_i (Assign (lval, exp)) NoOrig);
      (var, lval)

let add_call env tok eorig ~void mk_call =
  if void then (
    add_instr env (mk_i (mk_call None) eorig);
    mk_unit tok NoOrig)
  else
    let lval = fresh_lval env tok in
    add_instr env (mk_i (mk_call (Some lval)) eorig);
    mk_e (Fetch lval) NoOrig

let add_stmt env st = Stack_.push st env.stmts
let add_stmts env xs = xs |> List.iter (add_stmt env)

let pop_stmts env =
  let xs = List.rev !(env.stmts) in
  env.stmts := [];
  xs

let with_pre_stmts env f =
  let saved_stmts = !(env.stmts) in
  env.stmts := [];
  let r = f env in
  let f_stmts = pop_stmts env in
  env.stmts := saved_stmts;
  (f_stmts, r)

let bracket_keep f (t1, x, t2) = (t1, f x, t2)

let ident_of_entity_opt ent =
  match ent.G.name with
  | G.EN (G.Id (i, pinfo)) -> Some (i, pinfo)
  (* TODO: use name_middle? name_top? *)
  | G.EN (G.IdQualified { name_last = i, _topt; name_info = pinfo; _ }) ->
      Some (i, pinfo)
  | G.EDynamic _ -> None
  (* TODO *)
  | G.EPattern _
  | G.OtherEntity _ ->
      None

let name_of_entity ent =
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

let mk_unnamed_args (exps : IL.exp list) = List_.map (fun x -> Unnamed x) exps

let is_hcl lang =
  match lang with
  | Lang.Terraform -> true
  | _ -> false

let mk_class_constructor_name (ty : G.type_) cons_id_info =
  match ty with
  | { t = TyN (G.Id (id, _)); _ }
  | { t = TyExpr { e = G.N (G.Id (id, _)); _ }; _ }
  (* FIXME: JS parser produces this ^ although it should be parsed as a 'TyN'. *)
    when Option.is_some !(cons_id_info.G.id_resolved) ->
      Some (G.Id (id, cons_id_info))
  | __else__ -> None

let add_entity_name ctx ident =
  { entity_names = IdentSet.add (H.str_of_ident ident) ctx.entity_names }

let def_expr_evaluates_to_value (lang : Lang.t) =
  match lang with
  | Elixir -> true
  | _else_ -> false

let is_constructor env ret_ty id_info =
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

(*****************************************************************************)
(* lvalue *)
(*****************************************************************************)

let rec lval env eorig =
  match eorig.G.e with
  | G.N n -> name env n
  | G.IdSpecial (G.This, tok) -> lval_of_base (VarSpecial (This, tok))
  | G.DotAccess (e1orig, tok, field) ->
      let offset' =
        match field with
        | G.FN (G.Id (id, idinfo)) -> Dot (var_of_id_info id idinfo)
        | G.FN name ->
            let attr = expr env (G.N name |> G.e) in
            Index attr
        | G.FDynamic e2orig ->
            let attr = expr env e2orig in
            Index attr
      in
      let offset' = { o = offset'; oorig = SameAs eorig } in
      let lv1 = nested_lval env tok e1orig in
      { lv1 with rev_offset = offset' :: lv1.rev_offset }
  | G.ArrayAccess (e1orig, (_, e2orig, _)) ->
      let tok = G.fake "[]" in
      let lv1 = nested_lval env tok e1orig in
      let e2 = expr env e2orig in
      let offset' = { o = Index e2; oorig = SameAs eorig } in
      { lv1 with rev_offset = offset' :: lv1.rev_offset }
  | G.DeRef (_, e1orig) ->
      let e1 = expr env e1orig in
      lval_of_base (Mem e1)
  | _ -> todo (G.E eorig)

and nested_lval env tok e_gen : lval =
  match expr env e_gen with
  | { e = Fetch lval; _ } -> lval
  | rhs ->
      let fresh = fresh_lval env tok in
      add_instr env (mk_i (Assign (fresh, rhs)) (related_exp e_gen));
      fresh

and name env = function
  | G.Id (("_", tok), _) ->
      (* wildcard *)
      fresh_lval env tok
  | G.Id (id, id_info) ->
      let lval = lval_of_id_info env id id_info in
      lval
  | G.IdQualified qualified_info ->
      let lval = lval_of_id_qualified env qualified_info in
      lval

(*****************************************************************************)
(* Pattern *)
(*****************************************************************************)

(* TODO: This code is very similar to that of `assign`. Actually, we should not
 * be dealing with patterns in the LHS of `Assign`, those are supposed to be
 * `LetPattern`s. *)
and pattern env pat =
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
        |> List_.mapi (fun i pat_i ->
               let eorig = Related (G.P pat_i) in
               let index_i = Literal (G.Int (Parsed_int.of_int i)) in
               let offset_i =
                 { o = Index { e = index_i; eorig }; oorig = NoOrig }
               in
               let lval_i = { base = Var tmp; rev_offset = [ offset_i ] } in
               pattern_assign_statements env
                 (mk_e (Fetch lval_i) eorig)
                 ~eorig pat_i)
        |> List_.flatten
      in
      (tmp_lval, ss)
  | G.PatTyped (pat1, ty) ->
      type_ env ty |> ignore;
      pattern env pat1
  | _ -> todo (G.P pat)

and _catch_exn env exn =
  match exn with
  | G.CatchPattern pat -> pattern env pat
  | G.CatchParam { pname = Some id; pinfo = id_info; _ } ->
      let lval = lval_of_id_info env id id_info in
      (lval, [])
  | _ -> todo (G.Ce exn)

and pattern_assign_statements env ?(eorig = NoOrig) exp pat =
  try
    let lval, ss = pattern env pat in
    [ mk_s (Instr (mk_i (Assign (lval, exp)) eorig)) ] @ ss
  with
  | Fixme (kind, any_generic) -> fixme_stmt kind any_generic

(*****************************************************************************)
(* Exceptions *)
(*****************************************************************************)
and try_catch_else_finally env ~try_st ~catches ~opt_else ~opt_finally =
  let try_stmt = stmt env try_st in
  let catches_stmt_rev =
    List.fold_left
      (fun acc (ctok, exn, catch_st) ->
        (* TODO: Handle exn properly. *)
        let name = fresh_var env ctok in
        let todo_pattern = fixme_stmt ToDo (G.Ce exn) in
        let catch_stmt = stmt env catch_st in
        (name, todo_pattern @ catch_stmt) :: acc)
      [] catches
  in
  let else_stmt =
    match opt_else with
    | None -> []
    | Some (_tok, else_st) -> stmt env else_st
  in
  let finally_stmt =
    match opt_finally with
    | None -> []
    | Some (_tok, finally_st) -> stmt env finally_st
  in
  [ mk_s (Try (try_stmt, List.rev catches_stmt_rev, else_stmt, finally_stmt)) ]

(*****************************************************************************)
(* Assign *)
(*****************************************************************************)
and assign env ~g_expr lhs tok rhs_exp =
  let eorig = SameAs g_expr in
  match lhs.G.e with
  | G.N _
  | G.DotAccess _
  | G.ArrayAccess _
  | G.DeRef _ -> (
      try
        let lval = lval env lhs in
        add_instr env (mk_i (Assign (lval, rhs_exp)) eorig);
        mk_e (Fetch lval) (SameAs lhs)
      with
      | Fixme (kind, any_generic) ->
          (* lval translation failed, we use a fresh lval instead *)
          let fixme_lval = fresh_lval ~str:"_FIXME" env tok in
          add_instr env (mk_i (Assign (fixme_lval, rhs_exp)) eorig);
          fixme_exp kind any_generic (related_exp g_expr))
  | G.Container (((G.Tuple | G.List | G.Array) as ckind), (tok1, lhss, tok2)) ->
      (* TODO: handle cases like [a, b, ...rest] = e *)
      (* E1, ..., En = RHS *)
      (* tmp = RHS*)
      let tmp = fresh_var env tok2 in
      let tmp_lval = lval_of_base (Var tmp) in
      add_instr env (mk_i (Assign (tmp_lval, rhs_exp)) eorig);
      (* Ei = tmp[i] *)
      let tup_elems =
        lhss
        |> List_.mapi (fun i lhs_i ->
               let index_i = Literal (G.Int (Parsed_int.of_int i)) in
               let offset_i =
                 {
                   o = Index { e = index_i; eorig = related_exp lhs_i };
                   oorig = NoOrig;
                 }
               in
               let lval_i = { base = Var tmp; rev_offset = [ offset_i ] } in
               assign env ~g_expr lhs_i tok1
                 { e = Fetch lval_i; eorig = related_exp lhs_i })
      in
      (* (E1, ..., En) *)
      mk_e
        (Composite
           (composite_of_container ~g_expr ckind, (tok1, tup_elems, tok2)))
        (related_exp lhs)
  | G.Record (tok1, fields, tok2) ->
      assign_to_record env (tok1, fields, tok2) rhs_exp (related_exp lhs)
  | _ ->
      add_instr env (fixme_instr ToDo (G.E g_expr) (related_exp g_expr));
      fixme_exp ToDo (G.E g_expr) (related_exp lhs)

and assign_to_record env (tok1, fields, tok2) rhs_exp lhs_orig =
  (* Assignments of the form
   *
   *     {x1: p1, ..., xN: pN} = RHS
   *
   * where `xi` are field names, and `pi` are patterns.
   *
   * In the simplest case, where the patterns are variables
   * v1, ..., VN, this becomes:
   *
   *     tmp = RHS
   *     v1 = tmp.x1
   *     ...
   *     vN = tmp.xN
   *)
  let tmp, _tmp_lval = mk_aux_var env tok1 rhs_exp in
  let rec do_fields acc_rev_offsets fs =
    fs |> List_.map (do_field acc_rev_offsets)
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
and expr_aux env ?(void = false) g_expr =
  let eorig = SameAs g_expr in
  match g_expr.G.e with
  | G.Call
      ( { e = G.IdSpecial (G.Op ((G.And | G.Or) as op), tok); _ },
        (_, arg0 :: args, _) )
    when not void ->
      expr_lazy_op env op tok arg0 args eorig
  (* args_with_pre_stmts *)
  | G.Call ({ e = G.IdSpecial (G.Op op, tok); _ }, args) -> (
      let args = arguments env (Tok.unbracket args) in
      if not void then mk_e (Operator ((op, tok), args)) eorig
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
            add_call env tok eorig ~void (fun res -> Call (res, method_, args'))
      )
  | G.Call
      ( ({ e = G.IdSpecial ((G.This | G.Super | G.Self | G.Parent), tok); _ } as
         e),
        args ) ->
      call_generic env ~void tok eorig e args
  | G.Call
      ({ e = G.IdSpecial (G.IncrDecr (incdec, _prepostIGNORE), tok); _ }, args)
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
          let lval = lval env e in
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
      let obj_arg' = Unnamed (expr env obj) in
      let args' = arguments env (Tok.unbracket args) in
      let res =
        match env.lang with
        (* Ruby's concat method is side-effectful and updates the object. *)
        (* TODO: The lval in the LHs should have a differnt svalue than the
         * one in the RHS. *)
        | Lang.Ruby -> (
            try lval env obj with
            | Fixme _ -> fresh_lval ~str:"Fixme" env tok)
        | _ -> fresh_lval env tok
      in
      add_instr env
        (mk_i (CallSpecial (Some res, (Concat, tok), obj_arg' :: args')) eorig);
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
      let args = arguments env (Tok.unbracket args) in
      add_instr env (mk_i (CallSpecial (Some lval, special, args)) eorig);
      mk_e (Fetch lval) (related_tok tok)
  | G.Call
      ({ e = G.IdSpecial (G.InterpolatedElement, _); _ }, (_, [ G.Arg e ], _))
    ->
      (* G.InterpolatedElement is useful for matching certain patterns against
       * interpolated strings, but we do not have an use for it yet during
       * semantic analysis, so in the IL we just unwrap the expression. *)
      expr env e
  | G.New (tok, ty, _cons_id_info, args) ->
      (* HACK: Fall-through case where we don't know to what variable the allocated
       * object is being assigned to. See HACK(new), we expect to intercept `New`
       * already in 'stmt_aux'.
       *)
      let lval = fresh_lval env tok in
      let args = arguments env (Tok.unbracket args) in
      add_instr env (mk_i (New (lval, type_ env ty, None, args)) eorig);
      mk_e (Fetch lval) NoOrig
  | G.Call ({ e = G.IdSpecial spec; _ }, args) -> (
      let tok = snd spec in
      let args = arguments env (Tok.unbracket args) in
      try
        let special = call_special env spec in
        add_call env tok eorig ~void (fun res ->
            CallSpecial (res, special, args))
      with
      | Fixme (kind, any_generic) ->
          let fixme = fixme_exp kind any_generic (related_exp g_expr) in
          add_call env tok eorig ~void (fun res -> Call (res, fixme, args)))
  | G.Call (e, args) ->
      let tok = G.fake "call" in
      call_generic env ~void tok eorig e args
  | G.L lit -> mk_e (Literal lit) eorig
  | G.DotAccess ({ e = N (Id (("var", _), _)); _ }, _, FN (Id ((s, t), id_info)))
    when is_hcl env.lang ->
      (* We need to change all uses of a variable, which looks like a DotAccess, to a name which
         reads the same. This is so that our parameters to our function can properly be recognized
         as tainted by the taint engine.
      *)
      expr_aux env (G.N (Id (("var." ^ s, t), id_info)) |> G.e)
  | G.N _
  | G.DotAccess (_, _, _)
  | G.ArrayAccess (_, _)
  | G.DeRef (_, _) ->
      let lval = lval env g_expr in
      let exp = mk_e (Fetch lval) eorig in
      let ident_function_call_hack exp =
        (* Taking into account Ruby's ability to allow function calls without
         * parameters or parentheses, we are conducting a check to determine
         * if a function with the same name as the identifier exists, specifically
         * for Ruby. *)
        match lval with
        | { base = Var { ident; _ }; _ }
          when env.lang =*= Lang.Ruby
               && IdentSet.mem (H.str_of_ident ident) env.ctx.entity_names ->
            let tok = G.fake "call" in
            add_call env tok eorig ~void (fun res -> Call (res, exp, []))
        | _ -> exp
      in
      ident_function_call_hack exp
  (* x = ClassName(args ...) in Python *)
  (* ClassName has been resolved to __init__ by the pro engine. *)
  (* Identified and treated as x = New ClassName(args ...) to support
     field sensitivity. See HACK(new) *)
  | G.Assign
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
    when is_constructor env ret_ty id_info ->
      let obj' = var_of_name obj in
      let lval, ss =
        mk_class_construction env obj' origin_exp ret_ty id_info args
      in
      add_stmts env ss;
      mk_e (Fetch lval) (SameAs obj_e)
  | G.Assign (e1, tok, e2) ->
      let exp = expr env e2 in
      assign env ~g_expr e1 tok exp
  | G.AssignOp (e1, (G.Eq, tok), e2) ->
      (* AsssignOp(Eq) is used to represent plain assignment in some languages,
       * e.g. Go's `:=` is represented as `AssignOp(Eq)`, and C#'s assignments
       * are all represented this way too. *)
      let exp = expr env e2 in
      assign env ~g_expr e1 tok exp
  | G.AssignOp (e1, op, e2) ->
      let exp = expr env e2 in
      let lval = lval env e1 in
      let lvalexp = mk_e (Fetch lval) (SameAs e1) in
      let opexp =
        mk_e
          (Operator (op, [ Unnamed lvalexp; Unnamed exp ]))
          (related_tok (snd op))
      in
      add_instr env (mk_i (Assign (lval, opexp)) eorig);
      lvalexp
  | G.LetPattern (pat, e) ->
      let exp = expr env e in
      add_stmts env (pattern_assign_statements env ~eorig exp pat);
      mk_unit (G.fake "()") NoOrig
  | G.Seq xs -> (
      match List.rev xs with
      | [] -> impossible (G.E g_expr)
      | last :: xs ->
          let xs = List.rev xs in
          xs
          |> List.iter (fun e ->
                 let _eIGNORE = expr env e in
                 ());
          expr env last)
  | G.Record fields -> record env fields
  | G.Container (G.Dict, xs) -> dict env xs g_expr
  | G.Container (kind, xs) ->
      let xs = bracket_keep (List_.map (expr env)) xs in
      let kind = composite_kind ~g_expr kind in
      mk_e (Composite (kind, xs)) eorig
  | G.Comprehension _ -> todo (G.E g_expr)
  | G.Lambda fdef ->
      let lval = fresh_lval env (snd fdef.fkind) in
      let fdef =
        (* NOTE(config.stmts): This is a recursive call to
         * `function_definition` and we need to pass it a fresh
         * `stmts` ref list. If we reuse the same `stmts` ref list,
         * then whatever `stmts` we have accumulated so far, will
         * "magically" appear in the body of this lambda in the final
         * IL representation. This can happen e.g. when translating
         * `foo(bar(), (x) => { ... })`, because the instruction added
         * to `stmts` by the translation of `bar()` is still present
         * when traslating `(x) => { ... }`. *)
        function_definition { env with stmts = ref [] } fdef
      in
      add_instr env (mk_i (AssignAnon (lval, Lambda fdef)) eorig);
      mk_e (Fetch lval) eorig
  | G.AnonClass def ->
      (* TODO: should use def.ckind *)
      let tok = Common2.fst3 def.G.cbody in
      let lval = fresh_lval env tok in
      add_instr env (mk_i (AssignAnon (lval, AnonClass def)) eorig);
      mk_e (Fetch lval) eorig
  | G.IdSpecial (spec, tok) -> (
      let opt_var_special =
        match spec with
        | G.This -> Some This
        | G.Super -> Some Super
        | G.Self -> Some Self
        | G.Parent -> Some Parent
        | _ -> None
      in
      match opt_var_special with
      | Some var_special ->
          let lval = lval_of_base (VarSpecial (var_special, tok)) in
          mk_e (Fetch lval) eorig
      | None -> impossible (G.E g_expr))
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
      let e1 = expr env e1_gen in
      let ss_for_e1 = pop_stmts env in
      let e2 = expr env e2_gen in
      let ss_for_e2 = pop_stmts env in
      let e3 = expr env e3_gen in
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
      let e1 = expr env e1orig in
      let tmp = fresh_lval env tok in
      add_instr env
        (mk_i (CallSpecial (Some tmp, (Await, tok), [ Unnamed e1 ])) eorig);
      mk_e (Fetch tmp) NoOrig
  | G.Yield (tok, e1orig_opt, _) ->
      let yield_args =
        match e1orig_opt with
        | None -> []
        | Some e1orig -> [ expr env e1orig ]
      in
      add_instr env
        (mk_i
           (CallSpecial (None, (Yield, tok), mk_unnamed_args yield_args))
           eorig);
      mk_unit tok NoOrig
  | G.Ref (tok, e1orig) ->
      let e1 = expr env e1orig in
      let tmp = fresh_lval env tok in
      add_instr env
        (mk_i (CallSpecial (Some tmp, (Ref, tok), [ Unnamed e1 ])) eorig);
      mk_e (Fetch tmp) NoOrig
  | G.Constructor (cname, (tok1, esorig, tok2)) ->
      let cname = var_of_name cname in
      let es = esorig |> List_.map (fun eiorig -> expr env eiorig) in
      mk_e (Composite (Constructor cname, (tok1, es, tok2))) eorig
  | G.RegexpTemplate ((l, e, r), _opt) ->
      mk_e (Composite (Regexp, (l, [ expr env e ], r))) NoOrig
  | G.Xml xml -> xml_expr env ~void eorig xml
  | G.Cast (typ, _, e) ->
      let e = expr env e in
      mk_e (Cast (typ, e)) eorig
  | G.Alias (_alias, e) -> expr env e
  | G.LocalImportAll (_module, _tk, e) ->
      (* TODO: what can we do with _module? *)
      expr env e
  | G.Ellipsis _
  | G.TypedMetavar (_, _, _)
  | G.DisjExpr (_, _)
  | G.DeepEllipsis _
  | G.DotAccessEllipsis _ ->
      sgrep_construct (G.E g_expr)
  | G.StmtExpr st -> stmt_expr env ~g_expr st
  | G.OtherExpr ((str, tok), xs) ->
      let es =
        xs
        |> List_.map (fun x ->
               match x with
               | G.E e1orig -> expr env e1orig
               | __else__ -> fixme_exp ToDo x (related_tok tok))
      in
      let other_expr = mk_e (Composite (CTuple, (tok, es, tok))) eorig in
      let _, tmp = mk_aux_var ~str env tok other_expr in
      let partial = mk_e (Fetch tmp) (related_tok tok) in
      fixme_exp ToDo (G.E g_expr) (related_tok tok) ~partial
  | G.RawExpr _ -> todo (G.E g_expr)

and expr env ?void e_gen =
  try expr_aux env ?void e_gen with
  | Fixme (kind, any_generic) -> fixme_exp kind any_generic (related_exp e_gen)

and expr_opt env tok = function
  | None ->
      let void = G.Unit tok in
      mk_e (Literal void) (related_tok tok)
  | Some e -> expr env e

and expr_lazy_op env op tok arg0 args eorig =
  let arg0' = argument env arg0 in
  let args' : exp argument list =
    (* Consider A && B && C, side-effects in B must only take effect `if A`,
       * and side-effects in C must only take effect `if A && B`. *)
    args
    |> List.fold_left_map
         (fun cond argi ->
           let ssi, argi' = arg_with_pre_stmts env argi in
           if ssi <> [] then add_stmt env (mk_s @@ If (tok, cond, ssi, []));
           let condi =
             mk_e (Operator ((op, tok), [ Unnamed cond; argi' ])) eorig
           in
           (condi, argi'))
         (IL_helpers.exp_of_arg arg0')
    |> snd
  in
  mk_e (Operator ((op, tok), arg0' :: args')) eorig

and call_generic env ?(void = false) tok eorig e args =
  let e = expr env e in
  (* In theory, instrs in args could have side effect on the value in 'e',
   * but we will agglomerate all those instrs in the environment and
   * the caller will call them in sequence (see expr_with_pre_instr).
   * In theory, we should not execute those instrs before getting the
   * value in 'e' in the caller, but for our static analysis purpose
   * we should not care about those edge cases. That would require
   * to return in expr multiple arguments and thread things around; Not
   * worth it.
   *)
  let args = arguments env (Tok.unbracket args) in
  add_call env tok eorig ~void (fun res -> Call (res, e, args))

and call_special _env (x, tok) =
  ( (match x with
    | G.Op _
    | G.IncrDecr _
    | G.This
    | G.Super
    | G.Self
    | G.Cls
    | G.Parent
    | G.InterpolatedElement ->
        impossible (G.E (G.IdSpecial (x, tok) |> G.e))
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
        todo (G.E (G.IdSpecial (x, tok) |> G.e))),
    tok )

and composite_kind ~g_expr = function
  | G.Array -> CArray
  | G.List -> CList
  | G.Dict -> impossible (E g_expr)
  | G.Set -> CSet
  | G.Tuple -> CTuple

(* TODO: dependency of order between arguments for instr? *)
and arguments env xs = xs |> List_.map (argument env)

and argument env arg =
  match arg with
  | G.Arg e -> Unnamed (expr env e)
  | G.ArgKwd (id, e)
  | G.ArgKwdOptional (id, e) ->
      Named (id, expr env e)
  | G.ArgType { t = TyExpr e; _ } -> Unnamed (expr env e)
  | __else__ ->
      let any = G.Ar arg in
      Unnamed (fixme_exp ToDo any (Related any))

and record env ((_tok, origfields, _) as record_def) =
  let e_gen = G.Record record_def |> G.e in
  let fields =
    origfields
    |> List_.filter_map (function
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
                   expr env fdeforig
               (* Some languages such as javascript allow function
                  definitions in object literal syntax. *)
               | G.FuncDef fdef ->
                   let lval = fresh_lval env (snd fdef.fkind) in
                   (* See NOTE(config.stmts)! *)
                   let fdef =
                     function_definition { env with stmts = ref [] } fdef
                   in
                   let forig = Related (G.Fld forig) in
                   add_instr env (mk_i (AssignAnon (lval, Lambda fdef)) forig);
                   mk_e (Fetch lval) forig
               | ___else___ -> todo (G.E e_gen)
             in
             Some (Field (field_name, field_def))
         | G.F
             {
               s =
                 G.ExprStmt
                   ( {
                       e =
                         Call
                           ({ e = IdSpecial (Spread, _); _ }, (_, [ Arg e ], _));
                       _;
                     },
                     _ );
               _;
             } ->
             Some (Spread (expr env e))
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
             let field_expr = record env fields in
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

and dict env (_, orig_entries, _) orig =
  let entries =
    orig_entries
    |> List_.map (fun orig_entry ->
           match orig_entry.G.e with
           | G.Container (G.Tuple, (_, [ korig; vorig ], _)) ->
               let ke = expr env korig in
               let ve = expr env vorig in
               Entry (ke, ve)
           | __else__ -> todo (G.E orig))
  in
  mk_e (RecordOrDict entries) (SameAs orig)

and xml_expr env ~void eorig xml =
  let tok, jsx_name =
    match xml.G.xml_kind with
    | G.XmlClassic (tok, name, _, _)
    | G.XmlSingleton (tok, name, _) ->
        (tok, Some name)
    | G.XmlFragment (tok, _) -> (tok, None)
  in
  let body =
    xml.G.xml_body
    |> List_.filter_map (function
         | G.XmlExpr (tok, Some eorig, _) ->
             let exp = expr env eorig in
             let _, lval = mk_aux_var env tok exp in
             Some (mk_e (Fetch lval) (SameAs eorig))
         | G.XmlXml xml' ->
             let eorig' = SameAs (G.Xml xml' |> G.e) in
             Some (xml_expr env ~void:false eorig' xml')
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
      let name_eorig = SameAs (G.N jsx_name |> G.e) in
      let name_lval = name env jsx_name in
      let e = mk_e (Fetch name_lval) name_eorig in
      let fields =
        xml.G.xml_attrs
        |> List_.filter_map (function
             | G.XmlAttr (id, tok, eorig) ->
                 (* e.g. <Foo x={y}/> *)
                 let attr_name =
                   {
                     ident = id;
                     sid = G.SId.unsafe_default;
                     id_info = G.empty_id_info ();
                   }
                 in
                 let e = expr env eorig in
                 let _, lval = mk_aux_var env tok e in
                 let e = mk_e (Fetch lval) (SameAs eorig) in
                 Some (Field (attr_name, e))
             | G.XmlAttrExpr (_l, eorig, _r) ->
                 let e = expr env eorig in
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
        let attrs = xml.G.xml_attrs |> List_.map (fun attr -> G.XmlAt attr) in
        let body = G.Xmls xml.G.xml_body in
        Related (G.Anys (body :: attrs))
      in
      let record = mk_e (RecordOrDict fields) fields_orig in
      let args = [ Unnamed record ] in
      add_call env tok eorig ~void (fun res -> Call (res, e, args))
  | Some _
  | None ->
      let attrs =
        xml.G.xml_attrs
        |> List_.filter_map (function
             | G.XmlAttr (_, tok, eorig)
             | G.XmlAttrExpr (tok, eorig, _) ->
                 let exp = expr env eorig in
                 let _, lval = mk_aux_var env tok exp in
                 Some (mk_e (Fetch lval) (SameAs eorig))
             | _ -> None)
      in
      mk_e
        (Composite (CTuple, (tok, List.rev_append attrs body, tok)))
        (Related (G.Xmls xml.G.xml_body))

and stmt_expr env ?g_expr st =
  let todo () =
    match g_expr with
    | None -> todo (G.E (G.e (G.StmtExpr st)))
    | Some e_gen -> todo (G.E e_gen)
  in
  match st.G.s with
  | G.ExprStmt (eorig, tok) ->
      let e = expr env eorig in
      if eorig.is_implicit_return then (
        mk_s (Return (tok, e)) |> add_stmt env;
        expr_opt env tok None)
      else e
  | G.OtherStmt
      ( OS_Delete,
        ( [ (G.Tk tok as atok); G.E eorig ]
        | [ (G.Tk tok as atok); G.Tk _; G.Tk _; G.E eorig ] (* delete[] *) ) )
    ->
      let e = expr env eorig in
      let special = (Delete, tok) in
      add_instr env
        (mk_i (CallSpecial (None, special, [ Unnamed e ])) (Related atok));
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
      let ss, e' = cond_with_pre_stmts env cond in
      let pre_a1, e1 = stmt_expr_with_pre_stmts env st1 in
      let pre_a2, e2 =
        match opt_st2 with
        | Some st2 -> stmt_expr_with_pre_stmts env st2
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
  | G.Block (_, block, _) -> (
      (* See 'AST_generic.stmt_to_expr' *)
      match List.rev block with
      | st :: rev_sts ->
          rev_sts |> List.rev |> List.concat_map (stmt env) |> add_stmts env;
          stmt_expr env st
      | __else__ -> todo ())
  | G.Return (t, eorig, _) ->
      mk_s (Return (t, expr_opt env t eorig)) |> add_stmt env;
      expr_opt env t None
  | G.DefStmt (ent, G.VarDef { G.vinit = Some e; vtype = opt_ty; vtok = _ })
    when def_expr_evaluates_to_value env.lang ->
      type_opt env opt_ty;
      (* We may end up here due to Elixir_to_elixir's parsing. Other languages
       * such as Ruby, Julia, and C seem to result in Assignments, not DefStmts.
       *)
      let e = expr env e in
      let lv = lval_of_ent env ent in
      mk_i (Assign (lv, e)) (Related (G.S st)) |> add_instr env;
      mk_e (Fetch lv) (related_exp (G.e (G.StmtExpr st)))
  | __else__ ->
      (* In any case, let's make sure the statement is in the IL translation
       * so that e.g. taint can do its job. *)
      stmt env st |> add_stmts env;
      todo ()

(*****************************************************************************)
(* Exprs and instrs *)
(*****************************************************************************)
and lval_of_ent env ent =
  match ent.G.name with
  | G.EN (G.Id (id, idinfo)) -> lval_of_id_info env id idinfo
  | G.EN name -> lval env (G.N name |> G.e)
  | G.EDynamic eorig -> lval env eorig
  | G.EPattern (PatId (id, id_info)) -> lval env (G.N (Id (id, id_info)) |> G.e)
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

and expr_with_pre_stmts env ?void e =
  with_pre_stmts env (fun env -> expr env ?void e)

and stmt_expr_with_pre_stmts env st =
  with_pre_stmts env (fun env -> stmt_expr env st)

(* alt: could use H.cond_to_expr and reuse expr_with_pre_stmts *)
and cond_with_pre_stmts env cond =
  with_pre_stmts env (fun env ->
      match cond with
      | G.Cond e -> expr env e
      | G.OtherCond
          ( todok,
            [
              (Def (ent, VarDef { G.vinit = Some e; vtype = opt_ty; vtok = _ })
               as def);
            ] ) ->
          type_opt env opt_ty;
          (* e.g. C/C++: `if (const char *tainted_or_null = source("PATH"))` *)
          let e' = expr env e in
          let lv = lval_of_ent env ent in
          add_instr env (mk_i (Assign (lv, e')) (Related def));
          mk_e (Fetch lv) (Related (G.TodoK todok))
      | G.OtherCond (categ, xs) ->
          let e = G.OtherExpr (categ, xs) |> G.e in
          log_fixme ToDo (G.E e);
          expr env e)

and arg_with_pre_stmts env arg =
  with_pre_stmts env (fun env -> argument env arg)

and args_with_pre_stmts env args =
  with_pre_stmts env (fun env -> arguments env args)

and expr_with_pre_stmts_opt env tok eopt =
  match eopt with
  | None -> ([], expr_opt env tok None)
  | Some e -> expr_with_pre_stmts env e

and for_var_or_expr_list env xs =
  xs
  |> List.concat_map (function
       | G.ForInitExpr e ->
           let ss, _eIGNORE = expr_with_pre_stmts env e in
           ss
       | G.ForInitVar (ent, vardef) -> (
           (* copy paste of VarDef case in stmt *)
           match vardef with
           | { G.vinit = Some e; vtype = opt_ty; vtok = _ } ->
               let ss1, e' = expr_with_pre_stmts env e in
               let ss2 = type_opt_with_pre_stmts env opt_ty in
               let lv = lval_of_ent env ent in
               ss1 @ ss2
               @ [ mk_s (Instr (mk_i (Assign (lv, e')) (Related (G.En ent)))) ]
           | _ -> []))

(*****************************************************************************)
(* Parameters *)
(*****************************************************************************)
and parameters _env params : param list =
  params |> Tok.unbracket
  |> List_.map (function
       | G.Param { pname = Some i; pinfo; pdefault; _ } ->
           Param { pname = var_of_id_info i pinfo; pdefault }
       | G.ParamPattern pat -> PatternParam pat
       | G.Param { pname = None; _ }
       | G.ParamRest (_, _)
       | G.ParamHashSplat (_, _)
       | G.ParamEllipsis _
       | G.ParamReceiver _
       | G.OtherParam (_, _) ->
           FixmeParam (* TODO *))

(*****************************************************************************)
(* Type *)
(*****************************************************************************)

and type_ env (ty : G.type_) : G.type_ =
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
        [ expr env e ]
    | __TODO__ -> []
  in
  let tok = G.fake "type" in
  exps
  |> List.iter (fun e ->
         (* We add a fake assignment for dataflow analysis to reach these
          * expressions occurring inside types. *)
         mk_aux_var ~force:true ~str:"_type" env tok e |> ignore);
  ty

and type_with_pre_stmts env ty = with_pre_stmts env (fun env -> type_ env ty)

and type_opt env opt_ty =
  opt_ty |> Option.iter (fun ty -> type_ env ty |> ignore)

and type_opt_with_pre_stmts env opt_ty =
  let ss, () = with_pre_stmts env (fun env -> type_opt env opt_ty) in
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
  | Rust ->
      true
  | _ -> false

and mk_break_continue_labels env tok =
  let cont_label = fresh_label ~label:"__loop_continue" env tok in
  let break_label = fresh_label ~label:"__loop_break" env tok in
  let st_env =
    {
      env with
      break_labels = break_label :: env.break_labels;
      cont_label = Some cont_label;
    }
  in
  let cont_label_s = [ mk_s (Label cont_label) ] in
  let break_label_s = [ mk_s (Label break_label) ] in
  (cont_label_s, break_label_s, st_env)

and mk_switch_break_label env tok =
  let break_label = fresh_label ~label:"__switch_break" env tok in
  let switch_env =
    { env with break_labels = break_label :: env.break_labels }
  in
  (break_label, [ mk_s (Label break_label) ], switch_env)

and implicit_return env eorig tok =
  (* We always expect a value from an expression that is implicitly
   * returned, so void is set to false here.
   *)
  let ss, e = expr_with_pre_stmts ~void:false env eorig in
  let ret = mk_s (Return (tok, e)) in
  ss @ [ ret ]

and expr_stmt env (eorig : G.expr) tok : IL.stmt list =
  (* optimize? pass context to expr when no need for return value? *)
  let ss, e = expr_with_pre_stmts ~void:true env eorig in

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
  let ss1, args' = args_with_pre_stmts env (Tok.unbracket args) in
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
  let ss2, ty = type_with_pre_stmts env ty in
  ( lval,
    ss1 @ ss2
    @ [
        mk_s
          (Instr (mk_i (New (lval, ty, opt_cons, args')) (SameAs origin_exp)));
      ] )

and stmt_aux env st =
  match st.G.s with
  | G.ExprStmt (eorig, tok) -> (
      match eorig with
      | { is_implicit_return = true; _ } -> implicit_return env eorig tok
      (* Python's yield statement functions similarly to a return
         statement but with the added capability of saving the
         function's state. While this analogy isn't entirely precise,
         we currently treat it as a return statement for simplicity's
         sake. *)
      | { e = Yield (_, Some e, _); _ } when env.lang =*= Lang.Python ->
          implicit_return env e tok
      | _ -> expr_stmt env eorig tok)
  | G.DefStmt
      ( { name = EN obj; _ },
        G.VarDef
          {
            G.vinit =
              Some ({ e = G.New (_tok, ty, cons_id_info, args); _ } as new_exp);
            _;
          } ) ->
      (* x = new T(args) *)
      (* HACK(new): Because of field-sensitivity hacks, we need to know to which
       * variable are we assigning the `new` object, so we intercept the assignment. *)
      let obj' = var_of_name obj in
      mk_class_construction env obj' new_exp ty cons_id_info args |> snd
  | G.DefStmt (ent, G.VarDef { G.vinit = Some e; vtype = opt_ty; vtok = _ }) ->
      let ss1, e' = expr_with_pre_stmts env e in
      let lv = lval_of_ent env ent in
      let ss2 = type_opt_with_pre_stmts env opt_ty in
      ss1 @ ss2 @ [ mk_s (Instr (mk_i (Assign (lv, e')) (Related (G.S st)))) ]
  | G.DefStmt (_ent, G.VarDef { G.vinit = None; vtype = Some ty; vtok = _ }) ->
      (* We want to analyze any expressions in 'ty'. *)
      let ss, _ = type_with_pre_stmts env ty in
      ss
  | G.DefStmt def -> [ mk_s (MiscStmt (DefStmt def)) ]
  | G.DirectiveStmt dir -> [ mk_s (MiscStmt (DirectiveStmt dir)) ]
  | G.Block xs -> xs |> Tok.unbracket |> List.concat_map (stmt env)
  | G.If (tok, cond, st1, st2) ->
      let ss, e' = cond_with_pre_stmts env cond in
      let st1 = stmt env st1 in
      let st2 = List.concat_map (stmt env) (st2 |> Option.to_list) in
      ss @ [ mk_s (If (tok, e', st1, st2)) ]
  | G.Switch (tok, switch_expr_opt, cases_and_bodies) ->
      let ss, translate_cases =
        match switch_expr_opt with
        | Some switch_expr ->
            let ss, switch_expr' = cond_with_pre_stmts env switch_expr in
            ( ss,
              switch_expr_and_cases_to_exp env tok
                (H.cond_to_expr switch_expr)
                switch_expr' )
        | None -> ([], cases_to_exp env tok)
      in
      let break_label, break_label_s, switch_env =
        mk_switch_break_label env tok
      in
      let jumps, bodies =
        cases_and_bodies_to_stmts switch_env tok break_label translate_cases
          cases_and_bodies
      in
      ss @ jumps @ bodies @ break_label_s
  | G.While (tok, e, st) ->
      let cont_label_s, break_label_s, st_env =
        mk_break_continue_labels env tok
      in
      let ss, e' = cond_with_pre_stmts env e in
      let st = stmt st_env st in
      ss @ [ mk_s (Loop (tok, e', st @ cont_label_s @ ss)) ] @ break_label_s
  | G.DoWhile (tok, st, e) ->
      let cont_label_s, break_label_s, st_env =
        mk_break_continue_labels env tok
      in
      let st = stmt st_env st in
      let ss, e' = expr_with_pre_stmts env e in
      st @ ss
      @ [ mk_s (Loop (tok, e', st @ cont_label_s @ ss)) ]
      @ break_label_s
  | G.For (tok, G.ForEach (pat, tok2, e), st) ->
      for_each env tok (pat, tok2, e) st
  | G.For (_, G.MultiForEach [], st) -> stmt env st
  | G.For (_, G.MultiForEach (FEllipsis _ :: _), _) -> sgrep_construct (G.S st)
  | G.For (tok, G.MultiForEach (FECond (fr, tok2, e) :: for_eachs), st) ->
      let loop = G.For (tok, G.MultiForEach for_eachs, st) |> G.s in
      let st = G.If (tok2, Cond e, loop, None) |> G.s in
      for_each env tok fr st
  | G.For (tok, G.MultiForEach (FE fr :: for_eachs), st) ->
      for_each env tok fr (G.For (tok, G.MultiForEach for_eachs, st) |> G.s)
  | G.For (tok, G.ForClassic (xs, eopt1, eopt2), st) ->
      let cont_label_s, break_label_s, st_env =
        mk_break_continue_labels env tok
      in
      let ss1 = for_var_or_expr_list env xs in
      let st = stmt st_env st in
      let ss2, cond =
        match eopt1 with
        | None ->
            let vtrue = G.Bool (true, tok) in
            ([], mk_e (Literal vtrue) (related_tok tok))
        | Some e -> expr_with_pre_stmts env e
      in
      let next =
        match eopt2 with
        | None -> []
        | Some e ->
            let ss, _eIGNORE = expr_with_pre_stmts env e in
            ss
      in
      ss1 @ ss2
      @ [ mk_s (Loop (tok, cond, st @ cont_label_s @ next @ ss2)) ]
      @ break_label_s
  | G.For (_, G.ForEllipsis _, _) -> sgrep_construct (G.S st)
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
      let st = stmt env st in
      [ mk_s (Label lbl) ] @ st
  | G.Goto (tok, lbl, _sc) ->
      let lbl = lookup_label env lbl in
      [ mk_s (Goto (tok, lbl)) ]
  | G.Return (tok, eopt, _) ->
      let ss, e = expr_with_pre_stmts_opt env tok eopt in
      ss @ [ mk_s (Return (tok, e)) ]
  | G.Assert (tok, args, _) ->
      let ss, args = args_with_pre_stmts env (Tok.unbracket args) in
      let special = (Assert, tok) in
      (* less: wrong e? would not be able to match on Assert, or
       * need add sorig:
       *)
      ss
      @ [
          mk_s
            (Instr (mk_i (CallSpecial (None, special, args)) (Related (G.S st))));
        ]
  | G.Throw (tok, e, _) ->
      let ss, e = expr_with_pre_stmts env e in
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
      todo_stmt @ stmt_aux env throw_stmt
  | G.Try (_tok, try_st, catches, opt_else, opt_finally) ->
      try_catch_else_finally env ~try_st ~catches ~opt_else ~opt_finally
  | G.WithUsingResource (_, stmt1, stmt2) ->
      let stmt1 = List.concat_map (stmt env) stmt1 in
      let stmt2 = stmt env stmt2 in
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
      python_with_stmt env manager opt_pat body
  (* Java: synchronized (E) S *)
  | G.OtherStmtWithStmt (G.OSWS_Block _, [ G.E objorig ], stmt1) ->
      (* TODO: Restrict this to a syncrhonized block ? *)
      let ss, _TODO_obj = expr_with_pre_stmts env objorig in
      ss @ stmt env stmt1
  (* Rust: unsafe block *)
  | G.OtherStmtWithStmt (G.OSWS_Block ("Unsafe", tok), [], stmt1) ->
      let todo_stmt = fixme_stmt ToDo (G.TodoK ("unsafe_block", tok)) in
      todo_stmt @ stmt env stmt1
  | G.OtherStmt (OS_Async, [ G.S stmt1 ]) ->
      let todo_stmt = fixme_stmt ToDo (G.TodoK ("async", G.fake "async")) in
      todo_stmt @ stmt env stmt1
  | G.OtherStmt _
  | G.OtherStmtWithStmt _ ->
      todo (G.S st)
  | G.RawStmt _ -> todo (G.S st)

and for_each env tok (pat, tok2, e) st =
  let cont_label_s, break_label_s, st_env = mk_break_continue_labels env tok in
  let ss, e' = expr_with_pre_stmts env e in
  let st = stmt st_env st in

  let next_lval = fresh_lval env tok2 in
  let hasnext_lval = fresh_lval env tok2 in
  let hasnext_call =
    mk_s
      (Instr
         (mk_i
            (CallSpecial
               (Some hasnext_lval, (ForeachHasNext, tok2), [ Unnamed e' ]))
            (related_tok tok2)))
  in
  let next_call =
    mk_s
      (Instr
         (mk_i
            (CallSpecial (Some next_lval, (ForeachNext, tok2), [ Unnamed e' ]))
            (related_tok tok2)))
  in
  (* same semantic? or need to take Ref? or pass lval
     * directly in next_call instead of using intermediate next_lval?
  *)
  let assign_st =
    pattern_assign_statements env
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
             [ next_call ] @ assign_st @ st @ cont_label_s
             @ [ (* ss @ ?*) hasnext_call ] ));
    ]
  @ break_label_s

(* TODO: Maybe this and the following function could be merged *)
and switch_expr_and_cases_to_exp env tok switch_expr_orig switch_expr cases =
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
            let c_ss, c' = expr_with_pre_stmts env c in
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

and cases_to_exp env tok cases =
  (* If we have no scrutinee, the cases are boolean expressions, so we Or them together *)
  let ss, es =
    List.fold_left
      (fun (ss, es) -> function
        | G.Case (tok, G.PatLiteral l) ->
            (ss, { e = Literal l; eorig = related_tok tok } :: es)
        | G.Case (_, G.OtherPat (_, [ E c ]))
        | G.CaseEqualExpr (_, c) ->
            let c_ss, c' = expr_with_pre_stmts env c in
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

and cases_and_bodies_to_stmts env tok break_label translate_cases = function
  | [] -> ([ mk_s (Goto (tok, break_label)) ], [])
  | G.CaseEllipsis tok :: _ -> sgrep_construct (G.Tk tok)
  | [ G.CasesAndBody ([ G.Default dtok ], body) ] ->
      let label = fresh_label ~label:"__switch_default" env tok in
      ([ mk_s (Goto (dtok, label)) ], mk_s (Label label) :: stmt env body)
  | G.CasesAndBody (cases, body) :: xs ->
      let jumps, bodies =
        cases_and_bodies_to_stmts env tok break_label translate_cases xs
      in
      let label = fresh_label ~label:"__switch_case" env tok in
      let case_ss, case = translate_cases cases in
      let jump =
        mk_s (IL.If (tok, case, [ mk_s (Goto (tok, label)) ], jumps))
      in
      let body = mk_s (Label label) :: stmt env body in
      let break_if_no_fallthrough =
        if no_switch_fallthrough env.lang then
          [ mk_s (Goto (tok, break_label)) ]
        else []
      in
      (case_ss @ [ jump ], body @ break_if_no_fallthrough @ bodies)

and stmt env st =
  try stmt_aux env st with
  | Fixme (kind, any_generic) -> fixme_stmt kind any_generic

and function_body env fbody =
  let body_stmt = H.funcbody_to_stmt fbody in
  stmt env body_stmt

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
and python_with_stmt env manager opt_pat body =
  (* mgr = MANAGER *)
  let mgr = fresh_lval env G.sc in
  let ss_def_mgr =
    let ss_mk_mgr, manager' = expr_with_pre_stmts env manager in
    ss_mk_mgr @ [ mk_s (Instr (mk_i (Assign (mgr, manager')) NoOrig)) ]
  in
  (* PAT = mgr *)
  let ss_def_pat =
    match opt_pat with
    | None -> []
    | Some pat ->
        pattern_assign_statements env (mk_e (Fetch mgr) NoOrig) ~eorig:NoOrig
          pat
  in
  ss_def_mgr @ ss_def_pat @ stmt env body

(*****************************************************************************)
(* Defs *)
(*****************************************************************************)

and function_definition env fdef =
  let fparams = parameters env fdef.G.fparams in
  let fbody = function_body env fdef.G.fbody in
  { fkind = fdef.fkind; fparams; frettype = fdef.G.frettype; fbody }

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let function_definition lang ?ctx fdef =
  let env = { (empty_env lang) with ctx = ctx ||| empty_ctx } in
  function_definition env fdef

let stmt lang st =
  let env = empty_env lang in
  stmt env st

let expr lang e =
  let env = empty_env lang in
  expr env e
