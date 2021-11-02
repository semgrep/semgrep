(* Yoann Padioleau
 *
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

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
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
}

let empty_env lang =
  { stmts = ref []; break_labels = []; cont_label = None; lang }

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

exception Fixme of fixme_kind * G.any

let sgrep_construct any_generic = raise (Fixme (Sgrep_construct, any_generic))

let todo any_generic = raise (Fixme (ToDo, any_generic))

let impossible any_generic = raise (Fixme (Impossible, any_generic))

let locate opt_tok s =
  let opt_loc =
    try map_opt Parse_info.string_of_info opt_tok
    with Parse_info.NoTokenLocation _ -> None
  in
  match opt_loc with
  | Some loc -> spf "%s: %s" loc s
  | None -> s

let log_warning opt_tok msg = logger#warning "%s" (locate opt_tok msg)

let log_error opt_tok msg = logger#error "%s" (locate opt_tok msg)

let log_fixme kind gany =
  let toks = Visitor_AST.ii_of_any gany in
  let opt_tok = Common2.hd_opt toks in
  match kind with
  | ToDo ->
      log_warning opt_tok
        "Unsupported construct(s) may affect the accuracy of dataflow analyses"
  | Sgrep_construct ->
      log_error opt_tok "Cannot translate Semgrep construct(s) into IL"
  | Impossible ->
      log_error opt_tok "Impossible happened during AST-to-IL translation"

let fixme_exp kind gany eorig =
  log_fixme kind (G.E eorig);
  { e = FixmeExp (kind, gany); eorig }

let fixme_instr kind gany eorig =
  log_fixme kind (G.E eorig);
  { i = FixmeInstr (kind, gany); iorig = eorig }

let fixme_stmt kind gany =
  log_fixme kind gany;
  [ { s = FixmeStmt (kind, gany) } ]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let fresh_var ?(var = "_tmp") _env tok =
  let i = H.gensym () in
  { ident = (var, tok); sid = i; id_info = G.empty_id_info () }

let fresh_label _env tok =
  let i = H.gensym () in
  (("_label", tok), i)

let fresh_lval env tok =
  let var = fresh_var env tok in
  { base = Var var; offset = NoOffset }

let var_of_id_info id id_info =
  let sid =
    match !(id_info.G.id_resolved) with
    | Some (_resolved, sid) -> sid
    | None ->
        let id_str, id_tok = id in
        let msg = spf "the ident '%s' is not resolved" id_str in
        log_warning (Some id_tok) msg;
        -1
  in
  { ident = id; sid; id_info }

let lval_of_id_info _env id id_info =
  let var = var_of_id_info id id_info in
  { base = Var var; offset = NoOffset }

(* TODO: use also qualifiers? *)
let lval_of_id_qualified env
    { G.name_last = id, _typeargsTODO; name_info = id_info; _ } =
  lval_of_id_info env id id_info

let lval_of_base base = { base; offset = NoOffset }

(* TODO: should do first pass on body to get all labels and assign
 * a gensym to each.
 *)
let label_of_label _env lbl = (lbl, -1)

let lookup_label _env lbl = (lbl, -1)

let mk_e e eorig = { e; eorig }

let mk_i i iorig = { i; iorig }

let mk_s s = { s }

let mk_unit tok eorig =
  let unit = G.Unit tok in
  mk_e (Literal unit) eorig

let add_instr env instr = Common.push (mk_s (Instr instr)) env.stmts

(* Create an auxiliary variable for an expression---unless the expression
 * itself is already a variable! *)
let mk_aux_var env tok exp =
  match exp.e with
  | Fetch ({ base = Var var; offset = NoOffset; _ } as lval) -> (var, lval)
  | _ ->
      let var = fresh_var env tok in
      let lval = lval_of_base (Var var) in
      add_instr env (mk_i (Assign (lval, exp)) exp.eorig);
      (var, lval)

let add_call env tok eorig ~void mk_call =
  if void then (
    add_instr env (mk_i (mk_call None) eorig);
    mk_unit tok eorig)
  else
    let lval = fresh_lval env tok in
    add_instr env (mk_i (mk_call (Some lval)) eorig);
    mk_e (Fetch lval) eorig

let add_stmt env st = Common.push st env.stmts

let add_stmts env xs = xs |> List.iter (add_stmt env)

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

let composite_of_container : G.container_operator -> IL.composite_kind =
  function
  | Array -> CArray
  | List -> CList
  | Tuple -> CTuple
  | Set -> CSet
  | Dict -> CDict

(*****************************************************************************)
(* lvalue *)
(*****************************************************************************)
let rec lval env eorig =
  match eorig.G.e with
  | G.N n -> name env n
  | G.IdSpecial (G.This, tok) -> lval_of_base (VarSpecial (This, tok))
  | G.DotAccess (e1orig, tok, field) -> (
      let e1 = nested_lval env tok e1orig in
      match field with
      | G.FN (G.Id (id, idinfo)) ->
          { base = e1; offset = Dot (var_of_id_info id idinfo) }
      | G.FN name ->
          let attr = expr env (G.N name |> G.e) in
          { base = e1; offset = Index attr }
      | G.FDynamic e2orig ->
          let attr = expr env e2orig in
          { base = e1; offset = Index attr })
  | G.ArrayAccess (e1orig, (_, e2orig, _)) ->
      let tok = G.fake "[]" in
      let e1 = nested_lval env tok e1orig in
      let e2 = expr env e2orig in
      { base = e1; offset = Index e2 }
  | G.DeRef (_, e1orig) ->
      let e1 = expr env e1orig in
      lval_of_base (Mem e1)
  | _ -> todo (G.E eorig)

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

and nested_lval env tok eorig : base =
  let lval =
    match expr env eorig with
    | { e = Fetch ({ offset = NoOffset; _ } as lval); _ } -> lval
    | rhs ->
        let fresh = fresh_lval env tok in
        add_instr env (mk_i (Assign (fresh, rhs)) eorig);
        fresh
  in
  assert (lval.offset = NoOffset);
  lval.base

(*****************************************************************************)
(* Pattern *)
(*****************************************************************************)

(* TODO: This code is very similar to that of `assign`. Actually, we should not
 * be dealing with patterns in the LHS of `Assign`, those are supposed to be
 * `LetPattern`s. *)
and pattern env pat eorig =
  match pat with
  | G.PatUnderscore tok ->
      let lval = fresh_lval env tok in
      (lval, [])
  | G.PatId (id, id_info) ->
      let lval = lval_of_id_info env id id_info in
      (lval, [])
  | G.PatTuple (tok1, pats, tok2) ->
      (* P1, ..., Pn *)
      let tmp = fresh_var env tok2 in
      let tmp_lval = lval_of_base (Var tmp) in
      (* Pi = tmp[i] *)
      let ss =
        pats
        |> List.mapi (fun i pat_i ->
               let index_i = Literal (G.Int (Some i, tok1)) in
               let offset_i = Index { e = index_i; eorig } in
               let lval_i = { base = Var tmp; offset = offset_i } in
               pattern_assign_statements env
                 (mk_e (Fetch lval_i) eorig)
                 eorig pat_i)
        |> List.concat
      in
      (tmp_lval, ss)
  | _ -> todo (G.P pat)

and _catch_exn env exn eorig =
  match exn with
  | G.CatchPattern pat -> pattern env pat eorig
  | G.CatchParam { pname = Some id; pinfo = id_info; _ } ->
      let lval = lval_of_id_info env id id_info in
      (lval, [])
  | _ -> todo (G.Ce exn)

and pattern_assign_statements env exp eorig pat =
  try
    let lval, ss = pattern env pat eorig in
    [ mk_s (Instr (mk_i (Assign (lval, exp)) eorig)) ] @ ss
  with Fixme (kind, any_generic) -> fixme_stmt kind any_generic

(*****************************************************************************)
(* Assign *)
(*****************************************************************************)
and assign env lhs _tok rhs_exp eorig =
  match lhs.G.e with
  | G.N _
  | G.DotAccess _
  | G.ArrayAccess _
  | G.DeRef _ -> (
      try
        let lval = lval env lhs in
        add_instr env (mk_i (Assign (lval, rhs_exp)) eorig);
        mk_e (Fetch lval) lhs
      with Fixme (kind, any_generic) ->
        add_instr env (fixme_instr kind any_generic eorig);
        fixme_exp kind any_generic lhs)
  | G.Container (((G.Tuple | G.Array) as ckind), (tok1, lhss, tok2)) ->
      (* TODO: handle cases like [a, b, ...rest] = e *)
      (* E1, ..., En = RHS *)
      (* tmp = RHS*)
      let tmp = fresh_var env tok2 in
      let tmp_lval = lval_of_base (Var tmp) in
      add_instr env (mk_i (Assign (tmp_lval, rhs_exp)) eorig);
      (* Ei = tmp[i] *)
      let tup_elems =
        lhss
        |> List.mapi (fun i lhs_i ->
               let index_i = Literal (G.Int (Some i, tok1)) in
               let offset_i = Index { e = index_i; eorig } in
               let lval_i = { base = Var tmp; offset = offset_i } in
               assign env lhs_i tok1 { e = Fetch lval_i; eorig } eorig)
      in
      (* (E1, ..., En) *)
      mk_e
        (Composite (composite_of_container ckind, (tok1, tup_elems, tok2)))
        eorig
  | _ ->
      add_instr env (fixme_instr ToDo (G.E eorig) eorig);
      fixme_exp ToDo (G.E eorig) lhs

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)
(* less: we could pass in an optional lval that we know the caller want
 * to assign into, which would avoid creating useless fresh_var intermediates.
 *)
and expr_aux env ?(void = false) eorig =
  match eorig.G.e with
  | G.Call ({ e = G.IdSpecial (G.Op op, tok); _ }, args) -> (
      let args = arguments env args in
      if not void then mk_e (Operator ((op, tok), args)) eorig
      else
        (* The operation's result is not being used, so it may have side-effects.
         * We then assume this is just syntax sugar for a method call. E.g. in
         * Ruby `s << "hello"` is syntax sugar for `s.<<("hello")` and it mutates
         * the string `s` appending "hello" to it. *)
        match args with
        | [] -> impossible (G.E eorig)
        | obj :: args' ->
            let obj_var, _obj_lval = mk_aux_var env tok obj in
            let method_name =
              fresh_var env tok ~var:(Parse_info.str_of_info tok)
            in
            let method_lval =
              { base = Var obj_var; offset = Dot method_name }
            in
            let method_ = { e = Fetch method_lval; eorig } in
            add_call env tok eorig ~void (fun res -> Call (res, method_, args'))
      )
  | G.Call
      ( ({ e = G.IdSpecial ((G.This | G.Super | G.Self | G.Parent), tok); _ } as
        e),
        args ) ->
      call_generic env ~void tok e args
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
      match G.unbracket args with
      | [ G.Arg e ] ->
          let lval = lval env e in
          let lvalexp = mk_e (Fetch lval) e in
          let op =
            ( (match incdec with
              | G.Incr -> G.Plus
              | G.Decr -> G.Minus),
              tok )
          in
          let one = G.Int (Some 1, tok) in
          let one_exp = mk_e (Literal one) (G.L one |> G.e) in
          let opexp = mk_e (Operator (op, [ lvalexp; one_exp ])) eorig in
          add_instr env (mk_i (Assign (lval, opexp)) eorig);
          lvalexp
      | _ -> impossible (G.E eorig))
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
      let obj' = lval env obj in
      let obj_arg' = mk_e (Fetch obj') obj in
      let args' = arguments env args in
      let res =
        match env.lang with
        (* Ruby's concat method is side-effectful and updates the object. *)
        | Lang.Ruby -> obj'
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
      let args = arguments env args in
      add_instr env (mk_i (CallSpecial (Some lval, special, args)) eorig);
      mk_e (Fetch lval) eorig
  | G.Call
      ({ e = G.IdSpecial (G.InterpolatedElement, _); _ }, (_, [ G.Arg e ], _))
    ->
      (* G.InterpolatedElement is useful for matching certain patterns against
       * interpolated strings, but we do not have an use for it yet during
       * semantic analysis, so in the IL we just unwrap the expression. *)
      expr env e
  | G.Call ({ e = G.IdSpecial spec; _ }, args) ->
      let tok = snd spec in
      let special = call_special env spec in
      let args = arguments env args in
      add_call env tok eorig ~void (fun res -> CallSpecial (res, special, args))
  | G.Call (e, args) ->
      let tok = G.fake "call" in
      call_generic env ~void tok e args
  | G.L lit -> mk_e (Literal lit) eorig
  | G.N _
  | G.DotAccess (_, _, _)
  | G.ArrayAccess (_, _)
  | G.DeRef (_, _) ->
      let lval = lval env eorig in
      mk_e (Fetch lval) eorig
  | G.Assign (e1, tok, e2) ->
      let exp = expr env e2 in
      assign env e1 tok exp eorig
  | G.AssignOp (e1, (G.Eq, tok), e2) when Parse_info.str_of_info tok = ":=" ->
      (* We encode Go's `:=` as `AssignOp(Eq)`,
       * see "go_to_generic.ml" in Pfff. *)
      let exp = expr env e2 in
      assign env e1 tok exp eorig
  | G.AssignOp (e1, op, e2) ->
      let exp = expr env e2 in
      let lval = lval env e1 in
      let lvalexp = mk_e (Fetch lval) e1 in
      let opexp = mk_e (Operator (op, [ lvalexp; exp ])) eorig in
      add_instr env (mk_i (Assign (lval, opexp)) eorig);
      lvalexp
  | G.LetPattern (pat, e) ->
      let exp = expr env e in
      add_stmts env (pattern_assign_statements env exp eorig pat);
      mk_unit (G.fake "()") eorig
  | G.Seq xs -> (
      match List.rev xs with
      | [] -> impossible (G.E eorig)
      | last :: xs ->
          let xs = List.rev xs in
          xs
          |> List.iter (fun e ->
                 let _eIGNORE = expr env e in
                 ());
          expr env last)
  | G.Container (kind, xs) ->
      let xs = bracket_keep (List.map (expr env)) xs in
      let kind = composite_kind kind in
      mk_e (Composite (kind, xs)) eorig
  | G.Comprehension _ -> todo (G.E eorig)
  | G.Record fields -> record env fields
  | G.Lambda def ->
      (* TODO: we should have a use def.f_tok *)
      let tok = G.fake "lambda" in
      let lval = fresh_lval env tok in
      add_instr env (mk_i (AssignAnon (lval, Lambda def)) eorig);
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
      | None -> impossible (G.E eorig))
  | G.SliceAccess (_, _) -> todo (G.E eorig)
  (* e1 ? e2 : e3 ==>
   *  pre: lval = e1;
   *       if(lval) { lval = e2 } else { lval = e3 }
   *  exp: lval
   *)
  | G.Conditional (e1orig, e2orig, e3orig) ->
      let tok = G.fake "conditional" in
      let lval = fresh_lval env tok in
      let lvalexp = mk_e (Fetch lval) eorig in

      (* not sure this is correct *)
      let before = List.rev !(env.stmts) in
      env.stmts := [];
      let e1 = expr env e1orig in
      let ss_for_e1 = List.rev !(env.stmts) in
      env.stmts := [];
      let e2 = expr env e2orig in
      let ss_for_e2 = List.rev !(env.stmts) in
      env.stmts := [];
      let e3 = expr env e3orig in
      let ss_for_e3 = List.rev !(env.stmts) in
      env.stmts := [];

      add_stmts env before;
      add_stmts env ss_for_e1;
      add_stmt env
        (mk_s
           (If
              ( tok,
                e1,
                ss_for_e2 @ [ mk_s (Instr (mk_i (Assign (lval, e2)) e2orig)) ],
                ss_for_e3 @ [ mk_s (Instr (mk_i (Assign (lval, e3)) e3orig)) ]
              )));
      lvalexp
  | G.Xml _ -> todo (G.E eorig)
  | G.Constructor (_, _) -> todo (G.E eorig)
  | G.Yield (_, _, _)
  | G.Await (_, _) ->
      todo (G.E eorig)
  | G.Cast (typ, _, e) ->
      let e = expr env e in
      mk_e (Cast (typ, e)) eorig
  | G.Ref (_, _) -> todo (G.E eorig)
  | G.Ellipsis _
  | G.TypedMetavar (_, _, _)
  | G.DisjExpr (_, _)
  | G.DeepEllipsis _
  | G.DotAccessEllipsis _ ->
      sgrep_construct (G.E eorig)
  | G.StmtExpr _st -> todo (G.E eorig)
  | G.OtherExpr (_, _) -> todo (G.E eorig)

and expr env ?void eorig =
  try expr_aux env ?void eorig
  with Fixme (kind, any_generic) -> fixme_exp kind any_generic eorig

and expr_opt env = function
  | None ->
      let void = G.Unit (G.fake "void") in
      mk_e (Literal void) (G.L void |> G.e)
  | Some e -> expr env e

and call_generic env ?(void = false) tok e args =
  let eorig = G.Call (e, args) |> G.e in
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
  let args = arguments env args in
  add_call env tok eorig ~void (fun res -> Call (res, e, args))

and call_special _env (x, tok) =
  ( (match x with
    | G.Op _
    | G.IncrDecr _
    | G.This
    | G.Super
    | G.Self
    | G.Parent
    | G.InterpolatedElement ->
        impossible (G.E (G.IdSpecial (x, tok) |> G.e))
        (* should be intercepted before *)
    | G.Eval -> Eval
    | G.Typeof -> Typeof
    | G.Instanceof -> Instanceof
    | G.Sizeof -> Sizeof
    | G.New -> New
    | G.ConcatString _kindopt -> Concat
    | G.Spread -> Spread
    | G.EncodedString _
    | G.Defined
    | G.HashSplat
    | G.ForOf
    | G.NextArrayIndex ->
        todo (G.E (G.IdSpecial (x, tok) |> G.e))),
    tok )

and composite_kind = function
  | G.Array -> CArray
  | G.List -> CList
  | G.Dict -> CDict
  | G.Set -> CSet
  | G.Tuple -> CTuple

(* TODO: dependency of order between arguments for instr? *)
and arguments env xs = xs |> G.unbracket |> List.map (argument env)

and argument env arg =
  match arg with
  | G.Arg e -> expr env e
  | G.ArgKwd (_, e) ->
      (* TODO: Handle the keyword/label somehow (when relevant). *)
      expr env e
  | _ ->
      fixme_exp ToDo (G.Ar arg)
        (G.e (G.OtherExpr (("Arg", G.fake ""), [ G.Ar arg ])))

and record env ((_tok, origfields, _) as record_def) =
  let eorig = G.Record record_def |> G.e in
  let fields =
    origfields
    |> List.map (function
         | G.F
             {
               s =
                 G.DefStmt
                   ({ G.name = G.EN (G.Id (id, _)); tparams = []; _ }, def_kind);
               _;
             } ->
             let fdeforig =
               match def_kind with
               (* TODO: Consider what to do with vtype. *)
               | G.VarDef { G.vinit = Some fdeforig; _ }
               | G.FieldDefColon { G.vinit = Some fdeforig; _ } ->
                   fdeforig
               | ___else___ -> todo (G.E eorig)
             in
             let field_def = expr env fdeforig in
             (id, field_def)
         | G.F _ -> todo (G.E eorig))
  in
  mk_e (Record fields) eorig

(*****************************************************************************)
(* Exprs and instrs *)
(*****************************************************************************)

let lval_of_ent env ent =
  match ent.G.name with
  | G.EN (G.Id (id, idinfo)) -> lval_of_id_info env id idinfo
  | G.EN name -> lval env (G.N name |> G.e)
  | G.EDynamic eorig -> lval env eorig
  | G.EPattern _ -> (
      let any = G.En ent in
      log_fixme ToDo any;
      let toks = Visitor_AST.ii_of_any any in
      match toks with
      | [] -> raise Impossible
      | x :: _ -> fresh_lval env x)
  | G.OtherEntity _ -> (
      let any = G.En ent in
      log_fixme ToDo any;
      let toks = Visitor_AST.ii_of_any any in
      match toks with
      | [] -> raise Impossible
      | x :: _ -> fresh_lval env x)

(* just to ensure the code after does not call expr directly *)
let expr_orig = expr

let expr () = ()

let expr_with_pre_stmts env ?void e =
  ignore (expr ());
  let e = expr_orig env ?void e in
  let xs = List.rev !(env.stmts) in
  env.stmts := [];
  (xs, e)

(* alt: could use H.cond_to_expr and reuse expr_with_pre_stmts *)
let cond_with_pre_stmts env ?void cond =
  match cond with
  | G.Cond e ->
      let e = expr_orig env ?void e in
      let xs = List.rev !(env.stmts) in
      env.stmts := [];
      (xs, e)
  | G.OtherCond (categ, xs) ->
      let e = G.OtherExpr (categ, xs) |> G.e in
      log_fixme ToDo (G.E e);
      let e = expr_orig env ?void e in
      let xs = List.rev !(env.stmts) in
      env.stmts := [];
      (xs, e)

let args_with_pre_stmts env args =
  let args = arguments env args in
  let xs = List.rev !(env.stmts) in
  env.stmts := [];
  (xs, args)

let expr_with_pre_stmts_opt env eopt =
  match eopt with
  | None -> ([], expr_opt env None)
  | Some e -> expr_with_pre_stmts env e

let for_var_or_expr_list env xs =
  xs
  |> List.map (function
       | G.ForInitExpr e ->
           let ss, _eIGNORE = expr_with_pre_stmts env e in
           ss
       | G.ForInitVar (ent, vardef) -> (
           (* copy paste of VarDef case in stmt *)
           match vardef with
           | { G.vinit = Some e; vtype = _typTODO } ->
               let ss, e' = expr_with_pre_stmts env e in
               let lv = lval_of_ent env ent in
               ss @ [ mk_s (Instr (mk_i (Assign (lv, e')) e)) ]
           | _ -> []))
  |> List.flatten

(*****************************************************************************)
(* Parameters *)
(*****************************************************************************)

let parameters _env params =
  params
  |> List.filter_map (function
       | G.Param { pname = Some i; pinfo; _ } -> Some (var_of_id_info i pinfo)
       | ___else___ -> None (* TODO *))

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)

(* TODO: What other languages have no fallthrough? *)
let no_switch_fallthrough : Lang.t -> bool = function
  | Go -> true
  | _ -> false

let mk_break_continue_labels env tok =
  let cont_label = fresh_label env tok in
  let break_label = fresh_label env tok in
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

let mk_switch_break_label env tok =
  let break_label = fresh_label env tok in
  let switch_env =
    { env with break_labels = break_label :: env.break_labels }
  in
  (break_label, [ mk_s (Label break_label) ], switch_env)

let rec stmt_aux env st =
  match st.G.s with
  | G.ExprStmt (e, _) ->
      (* optimize? pass context to expr when no need for return value? *)
      let ss, _eIGNORE = expr_with_pre_stmts ~void:true env e in
      ss
  | G.DefStmt (ent, G.VarDef { G.vinit = Some e; vtype = _typTODO }) ->
      let ss, e' = expr_with_pre_stmts env e in
      let lv = lval_of_ent env ent in
      ss @ [ mk_s (Instr (mk_i (Assign (lv, e')) e)) ]
  | G.DefStmt def -> [ mk_s (MiscStmt (DefStmt def)) ]
  | G.DirectiveStmt dir -> [ mk_s (MiscStmt (DirectiveStmt dir)) ]
  | G.Block xs -> xs |> G.unbracket |> List.map (stmt env) |> List.flatten
  | G.If (tok, cond, st1, st2) ->
      let ss, e' = cond_with_pre_stmts env cond in
      let st1 = stmt env st1 in
      let st2 =
        List.map (stmt env) (st2 |> Common.opt_to_list) |> List.flatten
      in
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
      let cont_label_s, break_label_s, st_env =
        mk_break_continue_labels env tok
      in
      let ss, e' = expr_with_pre_stmts env e in
      let st = stmt st_env st in

      let next_lval = fresh_lval env tok2 in
      let hasnext_lval = fresh_lval env tok2 in
      let hasnext_call =
        mk_s
          (Instr
             (mk_i
                (CallSpecial (Some hasnext_lval, (ForeachHasNext, tok2), [ e' ]))
                e))
      in
      let next_call =
        mk_s
          (Instr
             (mk_i
                (CallSpecial (Some next_lval, (ForeachNext, tok2), [ e' ]))
                e))
      in
      (* same semantic? or need to take Ref? or pass lval
       * directly in next_call instead of using intermediate next_lval?
       *)
      let assign =
        pattern_assign_statements env (mk_e (Fetch next_lval) e) e pat
      in
      let cond = mk_e (Fetch hasnext_lval) e in

      (ss @ [ hasnext_call ])
      @ [
          mk_s
            (Loop
               ( tok,
                 cond,
                 [ next_call ] @ assign @ st @ cont_label_s
                 @ [ (* ss @ ?*) hasnext_call ] ));
        ]
      @ break_label_s
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
            ([], mk_e (Literal vtrue) (G.L vtrue |> G.e))
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
  | G.For (tok, G.ForIn (xs, e), stmts) ->
      let orig_stmt = st in
      let cont_label_s, break_label_s, st_env =
        mk_break_continue_labels env tok
      in
      let ss1 = for_var_or_expr_list env xs in
      let stmts = stmt st_env stmts in
      let ss2, cond =
        match e with
        | first :: _TODO ->
            (* TODO list *)
            expr_with_pre_stmts env first
        | [] ->
            (* TODO: empty list of elements to iterate over
               bash: for x do ... done *)
            let fake_expr =
              G.OtherExpr (("empty 'in'", tok), [ G.S st ]) |> G.e
            in
            ([], fixme_exp ToDo (G.S orig_stmt) fake_expr)
      in
      ss1 @ ss2
      @ [ mk_s (Loop (tok, cond, stmts @ cont_label_s @ ss2)) ]
      @ break_label_s
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
      let ss, e = expr_with_pre_stmts_opt env eopt in
      ss @ [ mk_s (Return (tok, e)) ]
  | G.Assert (tok, args, _) ->
      let e =
        let id = H.name_of_id ("assert", tok) in
        G.Call (G.N id |> G.e, args) |> G.e
      in
      let ss, args = args_with_pre_stmts env args in
      let special = (Assert, tok) in
      (* less: wrong e? would not be able to match on Assert, or
       * need add sorig:
       *)
      ss @ [ mk_s (Instr (mk_i (CallSpecial (None, special, args)) e)) ]
  | G.Throw (tok, e, _) ->
      let ss, e = expr_with_pre_stmts env e in
      ss @ [ mk_s (Throw (tok, e)) ]
  | G.OtherStmt (G.OS_ThrowNothing, [ G.Tk tok ]) ->
      (* Python's `raise` without arguments *)
      let fake_eorig = G.e (G.L (G.Unit tok)) in
      let todo_exp = fixme_exp ToDo (G.Tk tok) fake_eorig in
      [ mk_s (Throw (tok, todo_exp)) ]
  | G.OtherStmt
      (G.OS_ThrowFrom, [ G.E from; G.S ({ s = G.Throw _; _ } as throw_stmt) ])
    ->
      (* Python's `raise E1 from E2` *)
      let todo_stmt = fixme_stmt ToDo (G.E from) in
      todo_stmt @ stmt_aux env throw_stmt
  | G.Try (_tok, try_st, catches, opt_finally) ->
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
      let finally_stmt =
        match opt_finally with
        | None -> []
        | Some (_tok, finally_st) -> stmt env finally_st
      in
      [ mk_s (Try (try_stmt, List.rev catches_stmt_rev, finally_stmt)) ]
  | G.WithUsingResource (_, stmt1, stmt2) ->
      let stmt1 = stmt env stmt1 in
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
  | G.Match (_, _, _) -> todo (G.S st)
  | G.OtherStmt _
  | G.OtherStmtWithStmt _ ->
      todo (G.S st)

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
                        { e = Literal l; eorig = switch_expr_orig }; switch_expr;
                      ] );
                eorig = switch_expr_orig;
              }
              :: es )
        | G.Case (tok, G.OtherPat (_, [ E c ]))
        | G.CaseEqualExpr (tok, c) ->
            let c_ss, c' = expr_with_pre_stmts env c in
            ( ss @ c_ss,
              { e = Operator ((G.Eq, tok), [ c'; switch_expr ]); eorig = c }
              :: es )
        | G.Default tok ->
            (* Default should only ever be the final case, and cannot be part of a list of
               `Or`ed together cases. It's handled specially in cases_and_bodies_to_stmts
            *)
            impossible (G.Tk tok)
        | G.Case (tok, _) ->
            (ss, fixme_exp ToDo (G.Tk tok) switch_expr_orig :: es)
        | G.OtherCase ((_todo_categ, tok), _any) ->
            (ss, fixme_exp ToDo (G.Tk tok) switch_expr_orig :: es))
      ([], []) cases
  in
  (ss, { e = Operator ((Or, tok), es); eorig = switch_expr_orig })

and cases_to_exp env tok cases =
  (* If we have no scrutinee, the cases are boolean expressions, so we Or them together *)
  let ss, es =
    List.fold_left
      (fun (ss, es) -> function
        | G.Case (_, G.PatLiteral l) ->
            ( ss,
              (* TODO: seems bad to make an artificial eorig, but seems to be nothing to use  *)
              { e = Literal l; eorig = G.e (G.L l) } :: es )
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
            (* TODO: what eorig to use for the fixme_exp? *)
            ( ss,
              fixme_exp ToDo (G.Tk tok) (G.e (G.L (G.Unit (G.fake "case"))))
              :: es )
        | G.OtherCase ((_, tok), _) ->
            ( ss,
              fixme_exp ToDo (G.Tk tok) (G.e (G.L (G.Unit (G.fake "case"))))
              :: es ))
      ([], []) cases
  in
  (* TODO: even more artificial eorig, once again nothing to use *)
  ( ss,
    { e = Operator ((Or, tok), es); eorig = G.e (G.L (G.Unit (G.fake "case"))) }
  )

and cases_and_bodies_to_stmts env tok break_label translate_cases = function
  | [] -> ([ mk_s (Goto (tok, break_label)) ], [])
  | G.CaseEllipsis tok :: _ -> sgrep_construct (G.Tk tok)
  | [ G.CasesAndBody ([ G.Default dtok ], body) ] ->
      let label = fresh_label env tok in
      ([ mk_s (Goto (dtok, label)) ], mk_s (Label label) :: stmt env body)
  | G.CasesAndBody (cases, body) :: xs ->
      let jumps, bodies =
        cases_and_bodies_to_stmts env tok break_label translate_cases xs
      in
      let label = fresh_label env tok in
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
  try stmt_aux env st
  with Fixme (kind, any_generic) -> fixme_stmt kind any_generic

and function_body env fbody = stmt env (H.funcbody_to_stmt fbody)

(*
 *     with MANAGER as PAT:
 *         BODY
 *
 * ~>
 *
 *     mgr = MANAGER
 *     value = type(mgr).__enter__(mgr)
 *     try:
 *         PAT = value
 *         BODY
 *     finally:
 *         type(mgr).__exit__(mgr)
 *
 * This is NOT a 100% accurate translation but works for our purposes,
 * see https://www.python.org/dev/peps/pep-0343/.
 *)
and python_with_stmt env manager opt_pat body =
  (* mgr = MANAGER *)
  let mgr = fresh_lval env G.sc in
  let ss_def_mgr =
    let ss_mk_mgr, manager' = expr_with_pre_stmts env manager in
    ss_mk_mgr @ [ mk_s (Instr (mk_i (Assign (mgr, manager')) manager)) ]
  in
  (* type(mgr) *)
  let type_mgr_var = fresh_var env G.sc in
  let mgr_class = lval_of_base (Var type_mgr_var) in
  let ss_mgr_class =
    [
      mk_s
        (Instr
           (mk_i
              (CallSpecial
                 (Some mgr_class, (Typeof, G.sc), [ mk_e (Fetch mgr) manager ]))
              manager));
    ]
  in
  (* tmp = type(mgr).__method__(mgr) *)
  let call_mgr_method method_name =
    let tmp = fresh_lval env G.sc in
    let mgr_method =
      (* type(mgr).__method___ *)
      {
        base = Var type_mgr_var;
        offset = Dot (fresh_var env G.sc ~var:method_name);
      }
    in
    let ss =
      [
        mk_s
          (Instr
             (mk_i
                (Call
                   ( Some tmp,
                     mk_e (Fetch mgr_method) manager,
                     [ mk_e (Fetch mgr) manager ] ))
                manager));
      ]
    in
    (ss, tmp)
  in
  let ss_enter, value = call_mgr_method "__enter__" in
  let pre_try_stmts = ss_def_mgr @ ss_mgr_class @ ss_enter in
  let try_body =
    (* PAT = type(mgr).__enter__(mgr)
     * BODY *)
    let ss_def_pat =
      match opt_pat with
      | None -> []
      | Some pat ->
          pattern_assign_statements env (mk_e (Fetch value) manager) manager pat
    in
    ss_def_pat @ stmt env body
  in
  let try_catches = [] in
  let try_finally =
    let ss_exit, _ = call_mgr_method "__exit___" in
    ss_exit
  in
  pre_try_stmts @ [ mk_s (Try (try_body, try_catches, try_finally)) ]

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let function_definition lang def =
  let env = empty_env lang in
  let params = parameters env def.G.fparams in
  let body = function_body env def.G.fbody in
  (params, body)

let stmt lang st =
  let env = empty_env lang in
  stmt env st
