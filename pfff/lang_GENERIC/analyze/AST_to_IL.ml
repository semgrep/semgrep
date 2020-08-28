(*s: pfff/lang_GENERIC/analyze/AST_to_IL.ml *)
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* AST generic to IL translation.
 *
 * todo:
 *  - a lot ...
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(*s: type [[AST_to_IL.env]] *)
type env = {
  (* stmts hidden inside expressions that we want to move out of 'exp',
   * usually simple Instr, but can be also If when handling Conditional expr.
   *)
  stmts: stmt list ref;
}
(*e: type [[AST_to_IL.env]] *)

(*s: function [[AST_to_IL.empty_env]] *)
let empty_env () = {
  stmts = ref [];
}
(*e: function [[AST_to_IL.empty_env]] *)


(*****************************************************************************)
(* Error management *)
(*****************************************************************************)
(*s: function [[AST_to_IL.error]] *)
let error tok s =
  raise (Parse_info.Ast_builder_error (s, tok))
(*e: function [[AST_to_IL.error]] *)

(*s: function [[AST_to_IL.warning]] *)
let warning tok s =
  pr2 (spf "%s: %s" (Parse_info.string_of_info tok) s)
(*e: function [[AST_to_IL.warning]] *)

(*s: function [[AST_to_IL.error_any]] *)
let error_any any_generic msg =
  let toks = Lib_AST.ii_of_any any_generic in
  let s = AST_generic.show_any any_generic in
  error (List.hd toks) (spf "%s: %s" msg s)
(*e: function [[AST_to_IL.error_any]] *)

(*s: function [[AST_to_IL.sgrep_construct]] *)
let sgrep_construct any_generic =
  error_any any_generic "Sgrep Construct"
(*e: function [[AST_to_IL.sgrep_construct]] *)

(*s: function [[AST_to_IL.todo]] *)
let todo any_generic =
  error_any any_generic "TODO Construct"
(*e: function [[AST_to_IL.todo]] *)

(*s: function [[AST_to_IL.impossible]] *)
let impossible any_generic =
  error_any any_generic "Impossible Construct"
(*e: function [[AST_to_IL.impossible]] *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(*s: function [[AST_to_IL.fresh_var]] *)
let fresh_var _env tok = 
  let i = G.gensym () in
  ("_tmp", tok), i
(*e: function [[AST_to_IL.fresh_var]] *)
(*s: function [[AST_to_IL._fresh_label]] *)
let _fresh_label _env tok = 
  let i = G.gensym () in
  ("_label", tok), i
(*e: function [[AST_to_IL._fresh_label]] *)
(*s: function [[AST_to_IL.fresh_lval]] *)
let fresh_lval env tok =
  let var = fresh_var env tok in
  { base = Var var; offset = NoOffset }
(*e: function [[AST_to_IL.fresh_lval]] *)

(*s: function [[AST_to_IL.lval_of_id_info]] *)
let lval_of_id_info _env id id_info =
  let sid = 
    match !(id_info.G.id_resolved) with
    | Some (_resolved, sid) -> sid
    | None -> 
        warning (snd id) (spf "the ident '%s' is not resolved" (fst id));
        -1
  in
  let var = id, sid in
  { base = Var var; offset = NoOffset }
(*e: function [[AST_to_IL.lval_of_id_info]] *)
(*s: function [[AST_to_IL.lval_of_ent]] *)
let lval_of_ent env ent = 
  lval_of_id_info env ent.G.name ent.G.info
(*e: function [[AST_to_IL.lval_of_ent]] *)

(*s: function [[AST_to_IL.label_of_label]] *)
(* TODO: should do first pass on body to get all labels and assign
 * a gensym to each.
 *)
let label_of_label _env lbl =
  lbl, -1
(*e: function [[AST_to_IL.label_of_label]] *)
(*s: function [[AST_to_IL.lookup_label]] *)
let lookup_label _env lbl =
  lbl, -1
(*e: function [[AST_to_IL.lookup_label]] *)

(*s: function [[AST_to_IL.mk_e]] *)
let mk_e e eorig = 
  { e; eorig}
(*e: function [[AST_to_IL.mk_e]] *)
(*s: function [[AST_to_IL.mk_i]] *)
let mk_i i iorig =
  { i; iorig }
(*e: function [[AST_to_IL.mk_i]] *)
(*s: function [[AST_to_IL.mk_s]] *)
let mk_s s =
  { s }
(*e: function [[AST_to_IL.mk_s]] *)

(*s: function [[AST_to_IL.add_instr]] *)
let add_instr env instr = 
  Common.push (mk_s (Instr instr)) env.stmts
(*e: function [[AST_to_IL.add_instr]] *)
(*s: function [[AST_to_IL.add_stmt]] *)
let add_stmt env st = 
  Common.push st env.stmts
(*e: function [[AST_to_IL.add_stmt]] *)
(*s: function [[AST_to_IL.add_stmts]] *)
let add_stmts env xs = 
  xs |> List.iter (add_stmt env)
(*e: function [[AST_to_IL.add_stmts]] *)

(*s: function [[AST_to_IL.bracket_keep]] *)
let bracket_keep f (t1, x, t2) =
  t1, f x, t2
(*e: function [[AST_to_IL.bracket_keep]] *)

(*****************************************************************************)
(* lvalue *)
(*****************************************************************************)
let rec lval env eorig =
  match eorig with
  | G.Id (id, id_info) ->
      let lval = lval_of_id_info env id id_info in
      lval

  | G.DotAccess (e1orig, tok, field) ->
      let lval = lval env e1orig in
      let base =
        match lval.offset with
        | NoOffset -> lval.base
        | _ -> 
            let fresh = fresh_lval env tok in
            let lvalexp = mk_e (Lvalue lval) e1orig in
            add_instr env (mk_i (Assign (fresh, lvalexp)) e1orig);
            fresh.base
      in
      let offset = 
        match field with
        | G.FId id -> Dot id
        | G.FName _ | G.FDynamic _ -> todo (G.E eorig)
      in
      { base; offset }

  | G.ArrayAccess (e1orig, e2orig) ->
      let tok = G.fake "[]" in
      let lval = lval env e1orig in
      let e2 = expr env e2orig in
      let base =
        match lval.offset with
        | NoOffset -> lval.base
        | _ -> 
            let fresh = fresh_lval env tok in
            let lvalexp = mk_e (Lvalue lval) e1orig in
            add_instr env (mk_i (Assign (fresh, lvalexp)) e1orig);
            fresh.base
      in
      { base; offset = Index e2 }

  | _ -> todo (G.E eorig)

(*****************************************************************************)
(* Pattern *)
(*****************************************************************************)
and pattern env pat =
  match pat with
  | G.PatId (id, id_info) ->
      Left (lval_of_id_info env id id_info)
  | _ -> todo (G.P pat)

and pattern_assign_statements env exp eorig pat =
  let lval = 
     match pattern env pat with
     | Left l -> l
     | Right _ -> todo (G.P pat)
   in
   [mk_s (Instr (mk_i (Assign (lval, exp)) eorig))]

(*****************************************************************************)
(* Assign *)
(*****************************************************************************)
and assign env lhs _tok rhs_exp eorig =
  match lhs with
  | G.Id (_, _) ->
      let lval = lval env lhs in
      add_instr env (mk_i (Assign (lval, rhs_exp)) eorig);
      mk_e (Lvalue lval) lhs
  | _ -> todo (G.E lhs)

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)
(* less: we could pass in an optional lval that we know the caller want
 * to assign into, which would avoid creating useless fresh_var intermediates.
 *)
and expr env eorig =
  match eorig with
  | G.Call (G.IdSpecial (G.Op op, tok), args) ->
      let args = arguments env args in
      mk_e (Operator ((op, tok), args)) eorig
  | G.Call (G.IdSpecial (G.IncrDecr (incdec, _prepostIGNORE), tok), args) ->
      (* in theory in expr() we should return each time a list of pre-instr
       * and a list of post-instrs to execute before and after the use
       * of the expression. However this complicates the interface of 'expr()'.
       * Right now, for the pre-instr we agglomerate them instead in env 
       * and use them in 'expr_with_pre_instr()' below, but for the post
       * we dont. Anyway, for our static analysis purpose it should not matter.
       * We don't do fancy path-sensitive-evaluation-order-sensitive analysis.
       *)
      (match G.unbracket args with
      | [G.Arg e] ->
            let lval = lval env e in
            let lvalexp = mk_e (Lvalue lval) e in
            let op = 
              (match incdec with | G.Incr -> G.Plus | G.Decr -> G.Minus), tok
            in
            let one = G.Int ("1", tok) in
            let one_exp = mk_e (Literal one) (G.L one) in
            let opexp = mk_e (Operator (op, [lvalexp; one_exp])) eorig in
            add_instr env (mk_i (Assign (lval, opexp)) eorig);
            lvalexp
      | _ -> impossible (G.E eorig)
      )

  (* todo: if the xxx_to_generic forgot to generate Eval *)
  | G.Call (G.Id (("eval", tok), { G.id_resolved = {contents = None}; _}), 
      args) ->
      let lval = fresh_lval env tok in
      let special = Eval, tok in
      let args = arguments env args in
      add_instr env (mk_i (CallSpecial (Some lval, special, args)) eorig);
      mk_e (Lvalue lval) eorig
  | G.Call (G.IdSpecial spec, args) ->
      let tok = snd spec in
      let lval = fresh_lval env tok in
      let special = call_special env spec in
      let args = arguments env args in
      add_instr env (mk_i (CallSpecial (Some lval, special, args)) eorig);
      mk_e (Lvalue lval) eorig
  | G.Call (e, args) ->
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
      let tok = G.fake "call" in
      let lval = fresh_lval env tok in
      add_instr env (mk_i (Call (Some lval, e, args)) eorig);
      mk_e (Lvalue lval) eorig

  | G.L lit -> mk_e (Literal lit) eorig

  | G.Id (_, _) 
  | G.DotAccess (_, _, _) | G.ArrayAccess (_, _) 
    ->
      let lval = lval env eorig in
      mk_e (Lvalue lval) eorig

  | G.Assign (e1, tok, e2) ->
      let exp = expr env e2 in
      assign env e1 tok exp eorig
  | G.AssignOp (e1, op, e2) -> 
      let exp = expr env e2 in
      let lval = lval env e1 in
      let lvalexp = mk_e (Lvalue lval) e1 in
      let opexp = mk_e (Operator(op, [lvalexp; exp])) eorig in
      add_instr env (mk_i (Assign (lval, opexp)) eorig);
      lvalexp

  | G.Seq xs ->
      (match List.rev xs with
      | [] -> impossible (G.E eorig)
      | last::xs ->
         let xs = List.rev xs in
         xs |> List.iter (fun e -> let _eIGNORE = expr env e in ());
         expr env last
      )

  | G.Container (kind, xs) ->
      let xs = bracket_keep (List.map (expr env)) xs in
      let kind = composite_kind kind in
      mk_e (Composite (kind, xs)) eorig
  | G.Tuple xs -> 
      let xs = List.map (expr env) xs in
      mk_e (Composite (CTuple, G.fake_bracket xs)) eorig

  | G.Record _ 
  -> todo (G.E eorig)

  | G.Lambda def ->
      (* TODO: we should have a use def.f_tok *)
      let tok = G.fake "lambda" in
      let lval = fresh_lval env tok in
      add_instr env (mk_i (AssignAnon (lval, Lambda def)) eorig);
      mk_e (Lvalue lval) eorig
  | G.AnonClass def ->
      (* TODO: should use def.ckind *)
      let tok = Common2.fst3 def.G.cbody in
      let lval = fresh_lval env tok in
      add_instr env (mk_i (AssignAnon (lval, AnonClass def)) eorig);
      mk_e (Lvalue lval) eorig
      
  | G.IdSpecial (spec, tok) ->
      let var_special = 
        match spec with
        | G.This -> This | G.Super -> Super 
        | G.Self -> Self | G.Parent -> Parent
        | _ -> error_any (G.E eorig) "not expected in var_special context"
      in
      let lval = { base = VarSpecial (var_special, tok); offset = NoOffset } in
      mk_e (Lvalue lval) eorig



  | G.SliceAccess (_, _, _, _)
  -> todo (G.E eorig)

  (* e1 ? e2 : e3 ==> 
   *  pre: lval = e1;
   *       if(lval) { lval = e2 } else { lval = e3 }
   *  exp: lval
   *)
  | G.Conditional (e1orig, e2orig, e3orig) -> 
      let tok = G.fake "conditional" in
      let lval = fresh_lval env tok in
      let lvalexp = mk_e (Lvalue lval) e1orig in

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
      add_stmt env (mk_s (If (tok, e1,
          ss_for_e2 @
          [mk_s (Instr (mk_i (Assign (lval, e2)) e2orig))],
          ss_for_e3 @
          [mk_s (Instr (mk_i (Assign (lval, e3)) e3orig))])));
      lvalexp
  | G.Xml _
  -> todo (G.E eorig)

  | G.IdQualified (_, _)
  | G.Constructor (_, _)
  | G.LetPattern (_, _)
  | G.MatchPattern (_, _)
  -> todo (G.E eorig)

  | G.Yield (_, _, _)
  | G.Await (_, _)
  -> todo (G.E eorig)
  | G.Cast (typ, e) ->
      let e = expr env e in
      mk_e (Cast (typ, e)) eorig

  | G.Ref (_, _)
  | G.DeRef (_, _)
  -> todo (G.E eorig)

  | G.Ellipsis _ |G.TypedMetavar (_, _, _)|G.DisjExpr (_, _)|G.DeepEllipsis _
   -> sgrep_construct (G.E eorig)
  | G.OtherExpr (_, _) -> todo (G.E eorig)


and expr_opt env = function
  | None -> 
      let void = G.Unit (G.fake "void") in
      mk_e (Literal void) (G.L void)
  | Some e -> expr env e

and call_special _env (x, tok) = 
  (match x with
  | G.Op _ | G.IncrDecr _ -> 
        raise Impossible (* should be intercepted before *)
  | G.This | G.Super | G.Self | G.Parent ->
        raise Impossible (* should be intercepted before *)
  | G.Eval -> Eval
  | G.Typeof -> Typeof | G.Instanceof -> Instanceof | G.Sizeof -> Sizeof
  | G.New -> New 
  | G.ConcatString _kindopt -> Concat 
  | G.Spread -> Spread
  | G.EncodedString _ 
  | G.Defined
  | G.HashSplat
  | G.ForOf
    -> todo (G.E (G.IdSpecial (x, tok)))
  ), tok

and composite_kind = function
  | G.Array -> CArray | G.List -> CList | G.Dict -> CDict | G.Set -> CSet

(* TODO: dependency of order between arguments for instr? *)
and arguments env xs = 
  xs |> G.unbracket |> List.map (argument env)

and argument env arg =
  match arg with
  | G.Arg e -> expr env e
  | _ -> todo (G.Ar arg)

(*****************************************************************************)
(* Exprs and instrs *)
(*****************************************************************************)

(*s: constant [[AST_to_IL.expr_orig]] *)
(* just to ensure the code after does not call expr directly *)
let expr_orig = expr
(*e: constant [[AST_to_IL.expr_orig]] *)
(*s: function [[AST_to_IL.expr]] *)
let expr () = ()
(*e: function [[AST_to_IL.expr]] *)

(*s: function [[AST_to_IL.expr_with_pre_stmts]] *)
let expr_with_pre_stmts env e =
  ignore(expr ());
  let e = expr_orig env e in
  let xs = List.rev !(env.stmts) in
  env.stmts := [];
  xs, e
(*e: function [[AST_to_IL.expr_with_pre_stmts]] *)

(*s: function [[AST_to_IL.expr_with_pre_stmts_opt]] *)
let expr_with_pre_stmts_opt env eopt =
  match eopt with
  | None -> [], expr_opt env None
  | Some e -> expr_with_pre_stmts env e
(*e: function [[AST_to_IL.expr_with_pre_stmts_opt]] *)

(*s: function [[AST_to_IL.for_var_or_expr_list]] *)
let for_var_or_expr_list env xs =
  xs |> List.map (function
   | G.ForInitExpr e -> 
        let ss, _eIGNORE = expr_with_pre_stmts env e in
        ss
   | G.ForInitVar (ent, vardef) ->
      (* copy paste of VarDef case in stmt *)
      (match vardef with
      | { G.vinit = Some e; vtype = _typTODO} ->
         let ss, e' = expr_with_pre_stmts env e in
         let lv = lval_of_ent env ent in
         ss @ [mk_s (Instr (mk_i (Assign (lv, e')) e))]; 
      | _ -> []
      )
  ) |> List.flatten
(*e: function [[AST_to_IL.for_var_or_expr_list]] *)

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
(*s: function [[AST_to_IL.stmt]] *)
let rec stmt env st =
  match st with
  | G.ExprStmt (e, _) ->
      (* optimize? pass context to expr when no need for return value? *)
      let ss, _eIGNORE = expr_with_pre_stmts env e in
      ss

  | G.DefStmt (ent, G.VarDef { G.vinit = Some e; vtype = _typTODO}) ->
    let ss, e' = expr_with_pre_stmts env e in
    let lv = lval_of_ent env ent in
    ss @ [mk_s (Instr (mk_i (Assign (lv, e')) e))]; 
  | G.DefStmt def -> [mk_s (MiscStmt (DefStmt def))]
  | G.DirectiveStmt dir -> [mk_s (MiscStmt (DirectiveStmt dir))]

  | G.Block xs -> xs |> G.unbracket |> List.map (stmt env) |> List.flatten

  | G.If (tok, e, st1, st2) ->
    let ss, e' = expr_with_pre_stmts env e in
    let st1 = stmt env st1 in
    let st2 = List.map (stmt env) (st2 |> Common.opt_to_list) |>List.flatten in
    ss @ [mk_s (If (tok, e', st1, st2))]

  | G.Switch (_, _, _) -> todo (G.S st)

  | G.While(tok, e, st) ->
    let ss, e' = expr_with_pre_stmts env e in
    let st = stmt env st in
    ss @ [mk_s (Loop (tok, e', st @ ss))]
  | G.DoWhile(tok, st, e) ->
    let st = stmt env st in
    let ss, e' = expr_with_pre_stmts env e in
    st @ ss @ [mk_s (Loop (tok, e', st @ ss))]

  | G.For (tok, G.ForEach (pat, tok2, e), st) -> 
      let ss, e' = expr_with_pre_stmts env e in
      let st = stmt env st in

      let next_lval = fresh_lval env tok2 in
      let hasnext_lval = fresh_lval env tok2 in
      let hasnext_call = mk_s (Instr (
        mk_i (CallSpecial (Some hasnext_lval, (ForeachHasNext, tok2), [e']))e))
      in
      let next_call = mk_s (Instr(
        mk_i (CallSpecial (Some next_lval, (ForeachNext, tok2), [e'])) e))
      in
      (* same semantic? or need to take Ref? or pass lval
       * directly in next_call instead of using intermediate next_lval?
       *)
      let assign = 
        pattern_assign_statements env (mk_e (Lvalue next_lval) e) e pat in
      let cond = mk_e (Lvalue hasnext_lval) e in

      (ss @ [hasnext_call]) @
      [mk_s (Loop(tok, cond, [next_call] @ assign @
              st @ ((* ss @ ?*) [hasnext_call])))]
      
  | G.For (tok, G.ForClassic (xs, eopt1, eopt2), st) 
   -> 
      let ss1 = for_var_or_expr_list env xs in
      let st = stmt env st in
      let ss2, cond =
        match eopt1 with
        | None -> 
            let vtrue = G.Bool (true, tok) in
            [], mk_e (Literal (vtrue)) (G.L vtrue)
        | Some e -> expr_with_pre_stmts env e
      in
      let next =
        match eopt2 with
        | None -> []
        | Some e -> 
            let ss, _eIGNORE = expr_with_pre_stmts env e in
            ss
      in
      ss1 @ ss2 @ 
      [mk_s (Loop(tok, cond, st @ next @ ss2))]
  | G.For (_, G.ForEllipsis _, _) -> 
      sgrep_construct (G.S st)

  (* TODO: repeat env work of controlflow_build.ml *)
  | G.Continue (_, _) | G.Break (_, _)
   -> todo (G.S st)
    
  | G.Label (lbl, st) ->
      let lbl = label_of_label env lbl in
      let st = stmt env st in
      [mk_s (Label lbl)] @ st
  | G.Goto (tok, lbl) ->
      let lbl = lookup_label env lbl in
      [mk_s (Goto (tok, lbl))]

  | G.Return (tok, eopt) ->
      let ss, e = expr_with_pre_stmts_opt env eopt in
      ss @ [mk_s (Return (tok, e))]

  | G.Assert (tok, e, eopt) ->
      let ss1, e' = expr_with_pre_stmts env e in
      let ss2, eopt' = expr_with_pre_stmts_opt env eopt in
      let special = Assert, tok in
      (* less: wrong e? would not be able to match on Assert, or 
       * need add sorig:
       *)
      ss1 @ ss2 @
      [mk_s (Instr (mk_i (CallSpecial (None, special, [e'; eopt'])) e))]

  | G.Throw (tok, e) ->
      let ss, e = expr_with_pre_stmts env e in
      ss @ [mk_s (Throw (tok, e))]
  | G.Try (_, _, _, _) 
   -> todo (G.S st)
      
  | G.DisjStmt _ -> sgrep_construct (G.S st)
  | G.OtherStmt _ | G.OtherStmtWithStmt _ -> todo (G.S st)
(*e: function [[AST_to_IL.stmt]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*s: function [[AST_to_IL.stmt (/home/pad/pfff/lang_GENERIC/analyze/AST_to_IL.ml)]] *)
let stmt st =
  let env = empty_env () in
  stmt env st
(*e: function [[AST_to_IL.stmt (/home/pad/pfff/lang_GENERIC/analyze/AST_to_IL.ml)]] *)
(*e: pfff/lang_GENERIC/analyze/AST_to_IL.ml *)
