(* Julien Verlaguet, Yoann Padioleau
 *
 * Copyright (C) 2011, 2012 Facebook
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

open Cst_php
module A = Ast_php
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Cst_php to Ast_php *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* not used for now *)
type _env = unit

let empty_env () = ()

let error tok s =
  raise (Parse_info.Ast_builder_error (s, tok))


(* old: opti: to get www from 380MB to 190MB marshalled, but not worth it
 *  if !store_position then Some tok else None
 *)
let wrap tok = tok

let fake s = Parse_info.fake_info s
let fb = AST_generic.fake_bracket

let stmt1 xs =
  match xs with
  | [] -> A.Block (fb [])
  | [st] -> st
  | xs -> A.Block (fb xs)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let opt f env x =
  match x with
  | None -> None
  | Some x -> Some (f env x)

let rec comma_list = function
  | [] -> []
  | Common.Left x  :: rl -> x :: comma_list rl
  | Common.Right _ :: rl -> comma_list rl

let rec comma_list_dots = function
  | [] -> []
  | Common.Left3 x :: rl -> x :: comma_list_dots rl
  | (Common.Middle3 _ | Common.Right3 _) :: rl -> comma_list_dots rl

let brace (_, x, _) = x

let noop = A.Block (fb[])

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let rec program top_l =
  let env = empty_env () in
  toplevels env top_l

and toplevels env xs =
  match xs with
  | [] -> []
  | x::xs ->
    (match x with
    | NamespaceDef (t, qi, _) ->
      let (xs, rest) = xs |> Common.span (function
        (* less: actually I'm not sure you can mix NamespaceDef and BracketDef*)
        | NamespaceDef _ | NamespaceBracketDef _ -> false
        | _ -> true
      )
      in
      let body = toplevels env xs in
      let rest = toplevels env rest in
      A.NamespaceDef (t, qualified_ident env qi, G.fake_bracket body)::rest

    | _ ->
      (toplevel env x) @ toplevels env xs
    )

and toplevel env st =
  match st with
  | StmtList stmtl -> List.fold_right (stmt env) stmtl []
  | FuncDef fd -> [A.FuncDef (func_def env fd)]
  | ClassDef cd -> [A.ClassDef (class_def env cd)]
  | ConstantDef x -> [A.ConstantDef (constant_def env x)]
  | TypeDef x -> [A.TypeDef (type_def env x)]
  | FinalDef _ -> []
  (* error recovery is off by default now *)
  | NotParsedCorrectly _ -> raise Common.Impossible
  (* should be handled by toplevel above *)
  | NamespaceDef (_, _, _) -> raise Impossible
  | NamespaceBracketDef (tok, qu_opt, (t1, xs, t2)) ->
      let qi =
        match qu_opt with
        | Some qu -> qualified_ident env qu
        | None -> [A.special "ROOT", wrap tok]
      in
      [A.NamespaceDef (tok, qi, (t1, toplevels env xs, t2))]
  | NamespaceUse (tok, xs, _) ->
      xs |> uncomma |> List.map (function
      | ImportNamespace qu ->
          A.NamespaceUse (tok, qualified_ident env qu, None)
      | AliasNamespace (qu, _tok, id) ->
          A.NamespaceUse (tok, qualified_ident env qu, Some (ident env id))
      )

(* ------------------------------------------------------------------------- *)
(* Names *)
(* ------------------------------------------------------------------------- *)
and name env = function
  | XName [QI (Name ("class", tok))] -> [A.special "class", wrap tok]
  | XName qi -> qualified_ident env qi
  | Self tok -> [A.special "self", wrap tok]
  | Parent tok -> [A.special "parent", wrap tok]
  | LateStatic tok -> [A.special "static", wrap tok]

and ident _env = function
  | Name (s, tok) -> s, wrap tok
  | XhpName (tl, tok) ->
      A.string_of_xhp_tag tl, wrap tok

and qualified_ident env xs =
  let leading, rest =
    match xs with
    (* a leading '\' *)
    | (QITok tok)::rest ->
      [(A.special "ROOT", wrap tok)], rest
    | (QI (Name ("namespace", tok)))::rest ->
      [(A.special "namespace", wrap tok)], rest
    | rest -> [], rest
  in
  leading @
    (rest |> Common.map_filter (function
    | QITok _ -> None
    | QI id -> Some (ident env id)
     )
    )

and dname = function
  | DName (s, tok) ->
      if s.[0] = '$'
      then failwith "dname: the string has a dollar, weird";
      (* We abuse Id to represent both variables and functions/classes
       * identifiers in ast_php_simple, so to avoid collision
       * we prepend a $ (the $ was removed in ast_php.ml and parse_php.ml)
       *)
      ("$"^s, wrap tok)

(* ------------------------------------------------------------------------- *)
(* Statement *)
(* ------------------------------------------------------------------------- *)
and stmt env st acc =
  match st with
  | ExprStmt (e, t) ->
      let e = expr env e in
      A.Expr (e, t) :: acc
  (* Why not just acc? because we abuse noop in the abstract interpreter? *)
  | EmptyStmt _ -> noop :: acc
  | Block (_, stdl, _) -> List.fold_right (stmt_and_def env) stdl acc
  | If (tok, (_, e, _), st, il, io) ->
      let e = expr env e in
      let st = stmt1 (stmt env st []) in
      let il = List.fold_right (if_elseif env) il (if_else env io) in
      A.If (tok, e, st, il) :: acc
  | IfColon (tok, (_, e,_), _, st, il, io, _, _) -> 
      let e = expr env e in
      let st = stmt1 (List.fold_right (stmt_and_def env) st []) in
      let il = List.fold_right (new_elseif env) il (new_else env io) in
      A.If (tok, e, st, il) :: acc
  | While (tok, (_, e, _), cst) ->
      let cst = colon_stmt env cst in
      A.While (tok, expr env e, cst) :: acc
  | Do (tok, st, _, (_, e, _), _) ->
      A.Do (tok, stmt env st [], expr env e) :: acc
  | For (tok, _, e1, _, e2, _, e3, _, st) ->
      let st = colon_stmt env st in
      let e1 = for_expr env e1 in
      let e2 = for_expr env e2 in
      let e3 = for_expr env e3 in
      A.For (tok, e1, e2, e3, st) :: acc
  | Switch (tok, (_, e, _), scl) ->
      let e = expr env e in
      let scl = switch_case_list env scl in
      A.Switch (tok, e, scl) :: acc
  | Foreach (tok, _, e, _awaitTodo, tas, pat, _, cst) ->
      let e = expr env e in
      let pat = foreach_pattern env pat in
      let cst = colon_stmt env cst in
      A.Foreach (tok, e, tas, pat, cst) :: acc
  | Break (tok, e, _) -> A.Break (tok, opt expr env e) :: acc
  | Continue (tok, eopt, _) -> A.Continue (tok, opt expr env eopt) :: acc
  | Return (tok, eopt, _) -> A.Return (tok, opt expr env eopt) :: acc
  | Throw (tok, e, _) -> A.Throw (tok, expr env e) :: acc
  | Try (tok, (_, stl, _), cl, fl) ->
      let stl = List.fold_right (stmt_and_def env) stl [] in
      let cl = List.map (catch env) cl in
      let fl = List.map (finally env) fl in
      A.Try (tok, stl, cl, fl) :: acc
  | Echo (tok, el, t) ->
      A.Expr (A.Call (A.Id [A.builtin "echo", wrap tok],
                     fb (List.map (expr env) (comma_list el))), t) :: acc
  | Globals (tok, gvl, _) ->
      A.Global (tok, List.map (global_var env) (comma_list gvl)) :: acc
  | StaticVars (tok, svl, _) ->
      A.StaticVars (tok, List.map (static_var env) (comma_list svl)) :: acc
  | InlineHtml (s, tok) ->
      A.Expr (A.Call (A.Id [A.builtin "echo", wrap tok],
                     fb [A.String (s, wrap tok)]), tok) :: acc
  | Use (tok, _fn, _) ->
      error tok "TODO:Use"
  | Unset (tok, (t1, lp, t2), sc) ->
      let lp = comma_list lp in
      let lp = List.map (lvalue env) lp in
      A.Expr (A.Call (A.Id [A.builtin "unset", wrap tok], (t1, lp, t2)),sc) 
        :: acc
  (* http://php.net/manual/en/control-structures.declare.php *)
  | Declare (tok, args, colon_st) ->
      (match args, colon_st with
      (* declare(strict=1); (or 0) can be skipped,
       * See 'i wiki/index.php/Pfff/Declare_strict' *)
      | (_,[Common.Left((Name(("strict",_)),(_,Sc(C(Int((("1"|"0"),_)))))))],_),
      SingleStmt(EmptyStmt(_))
      | (_,[Common.Left((Name(("strict_types",_)),(_,Sc(C(Int((("1"|"0"),_)))))))],_),
      SingleStmt(EmptyStmt(_))
      (* declare(ticks=1); can be skipped too.
       * http://www.php.net/manual/en/control-structures.declare.php#control-structures.declare.ticks
       *)
      -> acc

      | (_,[Common.Left((Name(("ticks",_)), (_,Sc(C(Int((("1"),_)))))))],_), _
      ->
       let cst = colon_stmt env colon_st in
       cst @ acc

     |  _ -> error tok "TODO: declare"
      )

  | FuncDefNested fd -> A.FuncDef (func_def env fd) :: acc
  | ClassDefNested cd -> A.ClassDef (class_def env cd) :: acc


and if_elseif env (tok, (_, e, _), st) acc =
  let e = expr env e in
  let st = stmt1 (stmt env st []) in
  A.If (tok, e, st, acc)

and if_else env = function
  | None -> noop
  | Some (_, (If _ as st)) ->
      (match stmt env st [] with
      | [x] -> x
      | _l -> assert false
      )
  | Some (_, st) -> stmt1 (stmt env st [])

and new_elseif env (tok, (_, e, _), _, stl) acc =
  let e = expr env e in
  let st = stmt1 (List.fold_right (stmt_and_def env) stl []) in
  A.If (tok, e, st, acc)

and new_else env = function
  | None -> noop
  | Some (_, _, st) -> stmt1 (List.fold_right (stmt_and_def env) st [])

and stmt_and_def env st acc = stmt env st acc

(* ------------------------------------------------------------------------- *)
(* Expression *)
(* ------------------------------------------------------------------------- *)
and expr env = function
  | Sc sc -> scalar env sc

  | Id n -> A.Id (name env n)

  | IdVar (dn, _scope) -> A.Var (dname dn)
  | This tok -> A.IdSpecial (A.This, tok)

  (* ($o->fn)(...) ==> call_user_func($o->fn, ...) *)
  | Call (ParenExpr(tok, ObjGet (e1, arrow, Id fld), _), (lp, args, rp)) ->
      let e1 = expr env e1 in
      let fld = name env fld in
      let args = comma_list args in
      let args = List.map (argument env) args in
      A.Call (A.Id ["call_user_func", wrap tok],
              (lp, (A.Obj_get (e1, arrow, A.Id fld))::args, rp))

  | Call (e, (lp, args, rp)) ->
      let e = expr env e in
      let args = comma_list args in
      let args = List.map (argument env) args in
      A.Call (e, (lp, args, rp))
  | ObjGet (e1, tok, e2) ->
      let e1 = expr env e1 in
      let e2 = expr env e2 in
      A.Obj_get (e1, tok, e2)

  | ClassGet (e1, tok, e2) ->
      let e1 = class_name_reference env e1 in
      let e2 = expr env e2 in
      A.Class_get (e1, tok, e2)
  | HashGet (e1, (_l, e2, _r)) ->
      let e1 = expr env e1 in
      let e2 = expr env e2 in
      A.Array_get (e1, Some e2)
  | ArrayGet (e1, (_l, e2opt, _r)) ->
      let e1 = expr env e1 in
      let e2opt = opt expr env e2opt in
      A.Array_get (e1, e2opt)
  | BraceIdent (_l, e, _r) ->
      expr env e
  | Deref (tok, e) ->
      A.Call (A.Id [A.builtin "eval_var", wrap tok], fb [expr env e])

  | Binary (e1, (bop, tok), e2) ->
      let e1 = expr env e1 in
      let e2 = expr env e2 in
      let bop = binary_op bop in
      A.Binop (e1, (bop, tok), e2)
  | Unary ((uop, tok), e) ->
      let e = expr env e in
      let uop = unary_op uop in
      A.Unop ((uop, tok), e)
  | Assign (e1, tok, e2) -> A.Assign (lvalue env e1, tok, expr env e2)
  | AssignOp (lv, (op, tok), e) ->
      let op = assignOp env op in
      A.AssignOp (lvalue env lv, (op, tok), expr env e)
  | Postfix (v, (fop, tok)) -> A.Postfix ((fop, tok), lvalue env v)
  | Infix ((fop, tok), v) -> A.Infix ((fop, tok), lvalue env v)
  | CondExpr (e1, _, None, _, e3) ->
      let e = expr env e1 in
      A.CondExpr (e, e, expr env e3);
  | CondExpr (e1, _, Some e2, _, e3) ->
      A.CondExpr (expr env e1, expr env e2, expr env e3)
  | AssignList (_, (t1, la, t2), tokeq, e) ->
      let la = comma_list la in
      let la = List.fold_right (list_assign env) la [] in
      let e = expr env e in
      A.Assign (A.List (t1, la, t2), tokeq, e)
  | ArrayLong (_, (t1, apl, t2))
  | ArrayShort (t1, apl, t2) ->
      let apl = comma_list apl in
      let apl = List.map (array_pair env) apl in
      A.ConsArray (t1, apl, t2)
  | Collection (n, (t1, vel, t2)) ->
      let n = name env n in
      let vel = comma_list vel in
      let vel = List.map (array_pair env) vel in
      A.Collection (n, (t1, vel, t2))
  | New (tok, cn, args) ->
      let args =
        match args with
        | None -> []
        | Some (_, cl, _) -> List.map (argument env) (comma_list cl)
      in
      let cn = class_name_reference env cn in
      A.New (tok, cn, args)
  | Clone (tok, e) ->
      A.Call (A.Id [A.builtin "clone", wrap tok], fb [expr env e])
  | AssignRef (e1, tokeq, tokref, e2) ->
      let e1 = lvalue env e1 in
      let e2 = lvalue env e2 in
      A.Assign (e1, tokeq, A.Ref (tokref, e2))
  (* this is almost never used in our codebase, just in some third party code *)
  | AssignNew (e1, tokeq, tokref, new_tok, class_ref, args) ->
      let e1 = lvalue env e1 in
      let e2 = expr env (New (new_tok, class_ref, args)) in
      A.Assign (e1, tokeq, A.Ref (tokref, e2))
  | Cast (c, e) ->
      A.Cast (c, expr env e)
  | CastUnset (tok, _) ->
      error tok "TODO: CastUnset"
  | InstanceOf (e, tok, cn) ->
      let e = expr env e in
      let cn = class_name_reference env cn in
      A.InstanceOf (tok, e, cn)
  | Eval (tok, (lp, e, rp)) ->
      A.Call (A.Id [A.builtin "eval", wrap tok], (lp, [expr env e], rp))
  | Lambda ld ->
      A.Lambda (lambda_def env ld)
  | ShortLambda def ->
      A.Lambda (short_lambda_def env def)
  | Exit (tok, e) ->
      let arg =
        match e with
        | None
        | Some (_, None, _) -> []
        | Some (_, Some e, _) -> [expr env e]
      in
      A.Call (A.Id [A.builtin "exit", wrap tok], fb arg)
  | At (tok, e) ->
      let arg = expr env e in
      A.Call (A.Id [A.builtin "at", wrap tok], fb [arg])
  | Print (tok, e) ->
      A.Call (A.Id [A.builtin "print", wrap tok], fb [expr env e])
  | BackQuote (t1, el, t2) ->
      A.Call (A.Id [A.builtin "exec", wrap t1 (* not really an exec token *)],
             fb [A.Guil (t1, List.map (encaps env) el, t2)])
  | Include (tok, e) ->
      A.Call (A.Id [A.builtin "include", wrap tok], fb [expr env e])
  | IncludeOnce (tok, e) ->
      A.Call (A.Id [A.builtin "include_once", wrap tok], fb [expr env e])
  | Require (tok, e) ->
      A.Call (A.Id [A.builtin "require", wrap tok], fb [expr env e])
  | RequireOnce (tok, e) ->
      A.Call (A.Id [A.builtin "require_once", wrap tok], fb [expr env e])

  | Empty (tok, (lp, lv, rp)) ->
      A.Call (A.Id [A.builtin "empty", wrap tok], (lp, [lvalue env lv], rp))
  | Isset (tok, (lp, lvl, rp)) ->
      A.Call (A.Id [A.builtin "isset", wrap tok],
             (lp, List.map (lvalue env) (comma_list lvl), rp))
  | XhpHtml xhp -> A.Xhp (xhp_html env xhp)

  | Yield (tok, e) ->
      A.Call (A.Id [A.builtin "yield", wrap tok], fb[array_pair env e])
  (* todo? merge in one yield_break? *)
  | YieldBreak (tok, tok2) ->
      A.Call (A.Id [A.builtin "yield", wrap tok],
             fb[A.Id [A.builtin "yield_break", wrap tok2]])
  | Await (tok, e) ->
      A.Call (A.Id [A.builtin "await", wrap tok], fb[expr env e])
  | SgrepExprDots _ ->
      (* should never use the abstract interpreter on a sgrep pattern *)
      raise Common.Impossible
  | ParenExpr (_, e, _) -> expr env e

and arith_op = function
  | Plus -> G.Plus 
  | Minus -> G.Minus 
  | Mul -> G.Mult
  | Div  -> G.Div  
  | Mod-> G.Mod
  | DecLeft -> G.LSL
  | DecRight-> G.LSR
  | And -> G.BitAnd 
  | Or -> G.BitOr 
  | Xor-> G.BitXor

and logical_op = function
  | Inf -> G.Lt 
  | Sup -> G.Gt 
  | InfEq -> G.LtE
  | SupEq-> G.GtE
  | Eq -> G.Eq 
  | NotEq-> G.NotEq
  | Identical -> G.PhysEq
  | NotIdentical-> G.NotPhysEq
 (* less: add difference in ast_generic? 'and' as shortcut operator
  * and 'and' as boolean operator *)
  | AndLog -> G.And
  | OrLog -> G.Or 
  | XorLog-> G.Xor
  | AndBool -> G.And 
  | OrBool-> G.Or

and binary_op = function
  | Arith op -> A.ArithOp (arith_op op)
  | Logical op -> A.ArithOp (logical_op op)
  | BinaryConcat -> A.BinaryConcat
  | CombinedComparison -> A.CombinedComparison

and unary_op = function
 | UnPlus -> G.Plus
 | UnMinus-> G.Minus
 | UnBang -> G.Not
 | UnTilde-> G.BitXor



and scalar env = function
  | C cst -> constant env cst
  | Guil (t1, el, t2) -> A.Guil (t1, List.map (encaps env) el, t2)
  | HereDoc (t1, el, t2) -> A.Guil (t1, List.map (encaps env) el, t2)

and constant env = function
  | Int x -> A.Int x
  | Double x -> A.Double x
  | String x -> A.String x
  | PreProcess (cpp, tok) -> cpp_directive env tok cpp
  (* no reason to use the abstract interpreter on xdebug traces *)
  | XdebugClass _ | XdebugResource -> raise Common.Impossible

and cpp_directive _env tok = function
  | Line      -> A.Id [A.builtin "__LINE__", wrap tok]
  | File      -> A.Id [A.builtin "__FILE__", wrap tok]
  | ClassC    -> A.Id [A.builtin "__CLASS__", wrap tok]
  | MethodC   -> A.Id [A.builtin "__METHOD__", wrap tok]
  | FunctionC -> A.Id [A.builtin "__FUNCTION__", wrap tok]
  | Dir       -> A.Id [A.builtin "__DIR__", wrap tok]
  | TraitC    -> A.Id [A.builtin "__TRAIT__", wrap tok]
  | NamespaceC -> A.Id [A.builtin "__NAMESPACE__", wrap tok]

and lvalue env a = expr env a

and argument env = function
  | Arg e -> expr env e
  | ArgRef (tok, e) -> A.Ref (tok, lvalue env e)
  | ArgUnpack (_, e) -> A.Unpack (expr env e)

and class_name_reference env a = expr env a

and static_scalar_affect env (_, ss) = static_scalar env ss
and static_scalar env a = expr env a

(* ------------------------------------------------------------------------- *)
(* Type *)
(* ------------------------------------------------------------------------- *)
and hint_type env = function
  | Hint (q, _typeTODO) -> A.Hint (name env q)
  | HintArray tok -> A.HintArray tok
  | HintQuestion (tok, t) -> A.HintQuestion (tok, hint_type env t)
  | HintTuple (t1, v1, t2) -> 
      A.HintTuple (t1, List.map (hint_type env) (comma_list v1), t2)
  | HintCallback (_, (_, args, ret), _) ->
      let args = List.map (hint_type env) (comma_list_dots (brace args)) in
      let ret  = Common2.fmap (fun (_, _, t) -> hint_type env t) ret in
      A.HintCallback (args, ret)
  | HintShape (tok, (t1, xs, t2)) ->
    A.HintShape (tok, (t1,
      xs |> comma_list |> List.map (fun (e, _tok, t) ->
        expr env e, hint_type env t
      ), t2))
  | HintTypeConst (lhs, tok, rhs) ->
    A.HintTypeConst (hint_type env lhs, tok, hint_type env rhs)
  | HintVariadic (tok, hint) ->
    let hint = map_opt (hint_type env) hint in
    A.HintVariadic (tok, hint)

(* ------------------------------------------------------------------------- *)
(* Definitions *)
(* ------------------------------------------------------------------------- *)
and constant_def env {cst_name; cst_val; cst_type=_TODO; cst_toks = (tok,_,_)}=
  let name = ident env cst_name in
  let value = Some (expr env cst_val) in
  { A.cst_tok = tok; A.cst_name = name; A.cst_body = value }

and func_def env f =
  let _, params, _ = f.f_params in
  let params = comma_list_dots params in
  let _, body, _ = f.f_body in
  { A.f_ref = f.f_ref <> None;
    A.f_name = ident env f.f_name;
    A.f_attrs = attributes env f.f_attrs;
    A.f_params = List.map (parameter env) params;
    A.f_return_type =
      Common2.fmap (fun (_, _, t) -> hint_type env t) f.f_return_type;
    A.f_body = List.fold_right (stmt_and_def env) body [];
    A.f_kind = A.Function;
    A.m_modifiers = [];
    A.l_uses = [];
  }

and lambda_def env (l_use, ld) =
  let _, params, _ = ld.f_params in
  let params = comma_list_dots params in
  let _, body, _ = ld.f_body in
  { A.f_ref = ld.f_ref <> None;
    A.f_name = (A.special "_lambda", wrap ld.f_tok);
    A.f_params = List.map (parameter env) params;
    A.f_return_type = None;
    A.f_body = List.fold_right (stmt_and_def env) body [];
    A.f_kind = A.AnonLambda;
    A.m_modifiers = [];
    A.f_attrs = attributes env ld.f_attrs;
    A.l_uses =
      (match l_use with
      | None -> []
      | Some (_, (_lp, xs, _rp)) ->
          comma_list xs |> List.map (function
          | LexicalVar (is_ref, name) -> is_ref <> None, dname name
          )
      );
  }

and short_lambda_def env def =
  { A.
    f_ref = false;
    f_name = (
      A.special "_lambda",
      match def.sl_tok with
      | Some tok -> wrap tok
      | None -> wrap (fake "_lambda")
    );
    f_params =
      (match def.sl_params with
      | SLSingleParam p -> [parameter env p]
      | SLParamsOmitted -> []
      | SLParams (_, xs, _) ->
        let xs = comma_list_dots xs in
        List.map (parameter env) xs
      );
    f_return_type = None;
    f_body =
      (match def.sl_body with
      | SLExpr e -> [A.Expr (expr env e, G.sc)]
      | SLBody (_, body, _) -> List.fold_right (stmt_and_def env) body []
      );
    f_kind = A.ShortLambda;
    m_modifiers = [];
    f_attrs = [];
    l_uses = [];
  }

and type_def env def =
  { A.t_name = ident env def.t_name;
    A.t_kind = type_def_kind env def.t_kind;
  }
and type_def_kind env = function
  | Alias t -> A.Alias (hint_type env t)
  | Newtype t -> A.Newtype (hint_type env t)
  | ClassConstType v1 -> A.ClassConstType
    (match v1 with
    | Some x -> Some (hint_type env x)
    | None -> None)


and class_def env c =
  let t1, body, t2 = c.c_body in
  let (methods, implicit_fields) =
    List.fold_right (class_body env) body ([], []) in
  let kind, modifiers = class_type env c.c_type in
  {
    A.c_kind = kind;
    A.c_modifiers = modifiers;
    A.c_name = ident env c.c_name;
    A.c_attrs = attributes env c.c_attrs;
    A.c_xhp_fields = List.fold_right (xhp_fields env) body [];
    A.c_xhp_attr_inherit = List.fold_right (xhp_attr_inherit env) body [];
    A.c_extends =
      (match c.c_extends with
      | None -> None
      | Some (_, x) -> Some (hint_type env x)
      );
    A.c_uses =
      List.fold_right (class_traits env) body [];
    A.c_implements =
      (match c.c_implements with
      | None -> []
      | Some x -> interfaces env x
      );
    A.c_constants =
      List.fold_right (class_constants env) body [];
    A.c_variables =
      implicit_fields @ List.fold_right (class_variables env) body [];
    A.c_methods = methods;
    A.c_enum_type =
      (match c.c_enum_type with
        | None -> None
        | Some enum -> Some
          { A.e_base = hint_type env enum.e_base;
            A.e_constraint =
              (match enum.e_constraint with
                | None -> None
                | Some (_, cnstr_ty) -> Some (hint_type env cnstr_ty))
          });
    A.c_braces = t1, (), t2;
  }

and class_type _env = function
  | ClassRegular tok -> (A.Class, tok), []
  | ClassFinal (tokf, tok) -> (A.Class, tok), [Final, tokf]
  | ClassAbstract (toka, tok) -> (A.Class, tok), [Abstract, toka]
  | ClassAbstractFinal (toka, tokf, tok) -> 
    (A.Class, tok), [Abstract, toka; Final, tokf]
  | Interface tok -> (A.Interface, tok), []
  | Trait tok -> (A.Trait, tok), []
  | Enum tok -> (A.Enum, tok), []

and interfaces env (_, intfs) =
  let intfs = comma_list intfs in
  List.map (fun x -> (hint_type env x)) intfs

and class_traits env x acc =
  match x with
  | UseTrait (_, l, _) ->
      List.map (hint_type env) (comma_list l) @ acc
  | _ -> acc

and class_constants env st acc =
  match st with
  | ClassConstants (_, tok, _, cl, _) ->
      List.fold_right (
      fun (n, ss) acc ->
        let body = opt static_scalar_affect env ss in
        let cst = {A.cst_name = ident env n; cst_body = body; cst_tok = tok} in
        cst::acc
     ) (comma_list cl) acc
  | _ -> acc

and class_variables env st acc =
  match st with
  | ClassVariables (m, ht, cvl, _) ->
      let cvl = comma_list cvl in
      let m =
        match m with
        | NoModifiers _ -> []
        | VModifiers l -> l
      in
      let ht = opt hint_type env ht in
      List.map (fun (n, ss) ->
          let name = dname n in
          let value = opt static_scalar_affect env ss in
          {
            A.cv_name = name;
            A.cv_value = value;
            A.cv_modifiers = m;
            A.cv_type = ht;
          }
       ) cvl @ acc
  | _ -> acc

and xhp_fields env st acc =
  match st with
  | XhpDecl (XhpAttributesDecl (_, xal, _)) ->
    (comma_list xal) |> List.fold_left (fun acc xhp_attr ->
      match xhp_attr with
      | XhpAttrDecl (attr_type, attr_name, eopt, req_tok_opt) ->
        let ht =
          match attr_type with
          | XhpAttrType attr_type -> Some(hint_type env attr_type)
          | XhpAttrVar _tok -> None
          | XhpAttrEnum (_tok, _) -> None
        in
        let value =
          match eopt with
          | None -> None
          | Some (_tok,sc) -> Some(static_scalar env sc)
        in
        let required =
          match req_tok_opt with
          | None -> false
          | Some _ -> true
        in
        let attr_name =
          match attr_name with
          | (str, tok) -> (str, wrap tok)
        in
        ({
          A.cv_name = attr_name;
          A.cv_value = value;
          A.cv_modifiers = [];
          A.cv_type = ht;
        }, required)::acc
      | XhpAttrInherit _ -> acc
     ) acc
  | _ -> acc

and xhp_attr_inherit env st acc =
  match st with
  | XhpDecl (XhpAttributesDecl(_, xal, _)) ->
    (comma_list xal) |> List.fold_left (fun acc xhp_attr ->
      match xhp_attr with
      | XhpAttrInherit (source, tok) ->
        A.Hint [ident env (Cst_php.XhpName (source, tok))]::acc
      | XhpAttrDecl _ -> acc
     ) acc
  | _ -> acc



and class_body env st (mets, flds) =
  match st with
  | Method md ->
    let (met, more_flds) = method_def env md in
    met::mets, more_flds @ flds

  | ClassVariables _ | ClassConstants _ | UseTrait _
  | XhpDecl _ | TraitConstraint _ | ClassType _
    -> (mets, flds)

and method_def env m =
  let _, params, _ = m.f_params in
  let params = comma_list_dots params in
  let mds = m.f_modifiers in
  let implicits =
    params |> Common.map_filter (fun p ->
      match p.p_modifier with
      | None -> None
      | Some modifier -> Some (p.p_name, modifier, p.p_type)
    )
  in
  let implicit_flds = implicits |> List.map (fun (var, modifier, topt) ->
    { A.cv_name = dname var;
      (* less: should use default val of parameter?*)
      A.cv_value = None;
      A.cv_modifiers = [modifier];
      A.cv_type = opt hint_type env topt;
    }
  )
  in
  let implicit_assigns =
    implicits |> List.map (fun (var, _, _) ->
      let (str_with_dollar, tok) = dname var in
      let str_without_dollar = Cst_php.str_of_dname var in
      A.Expr (
        A.Assign (A.Obj_get(A.IdSpecial(A.This,tok), fake ".",
                                  A.Id [str_without_dollar, tok]),
                  fake "=",
                  A.Var (str_with_dollar, tok)), G.sc)
    )
  in

  { A.m_modifiers = mds;
    A.f_ref = (match m.f_ref with None -> false | Some _ -> true);
    A.f_name = ident env m.f_name;
    A.f_attrs = attributes env m.f_attrs;
    A.f_params = List.map (parameter env) params ;
    A.f_return_type =
      Common2.fmap (fun (_, _, t) -> hint_type env t) m.f_return_type;
    A.f_body = implicit_assigns @ method_body env m.f_body;
    A.f_kind = A.Method;
    A.l_uses = [];
  },
  implicit_flds

and method_body env (_, stl, _) =
  List.fold_right (stmt_and_def env) stl []

and parameter env
 { p_type = t; p_ref = r; p_name = name; p_default = d; p_attrs = a;
   p_modifier = _mTODO;
   (* don't care about the soft type annot, it's useful only for the runtime *)
   p_soft_type = _;
   p_variadic = variadic;
 } =
  { A.p_type = opt hint_type env t;
    A.p_ref = r;
    A.p_name = dname name;
    A.p_default = opt static_scalar_affect env d;
    A.p_attrs = attributes env a;
    A.p_variadic = variadic;
  }

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

and xhp_html env = function
  | Xhp (tag, attrl, _, body, _) ->
      let tag, tok = tag in
      let attrl = List.map (xhp_attribute env) attrl in
      { A.xml_tag = (A.string_of_xhp_tag tag, wrap tok);
        A.xml_attrs = attrl;
        A.xml_body = List.map (xhp_body env) body;
      }
  | XhpSingleton (tag, attrl, _) ->
      let tag, tok = tag in
      let attrl = List.map (xhp_attribute env) attrl in
      { A.xml_tag = (A.string_of_xhp_tag tag, wrap tok);
        A.xml_attrs = attrl;
        A.xml_body = [];
      }

and xhp_attribute env ((n, tok), _, v) =
  (n, wrap tok), xhp_attr_value env v

and xhp_attr_value env = function
  | XhpAttrString (t1, l, t2) ->
      A.Guil (t1, List.map (encaps env) l, t2)
  | XhpAttrExpr (_, e, _) ->
      (expr env e)
  | SgrepXhpAttrValueMvar _ ->
      (* should never use the abstract interpreter on a sgrep pattern *)
      raise Common.Impossible

and xhp_body env = function
  | XhpText x -> A.XhpText x
  | XhpExpr (_, e, _) -> A.XhpExpr (expr env e)
  | XhpNested xml -> A.XhpXml (xhp_html env xml)


and encaps env = function
  | EncapsString (s, tok) -> A.String (s, wrap tok)
  | EncapsVar v -> lvalue env v
  | EncapsCurly (_, lv, _) ->  lvalue env lv
  | EncapsDollarCurly (_, lv, _) -> lvalue env lv
  | EncapsExpr (_, e, _) -> expr env e

and array_pair env = function
  | ArrayExpr e -> expr env e
  | ArrayRef (tok, lv) -> A.Ref (tok, lvalue env lv)
  | ArrayArrowExpr (e1, tok, e2) -> 
      A.Arrow (expr env e1, tok, expr env e2)
  | ArrayArrowRef (e1, arrow, tokref, lv) -> 
      A.Arrow (expr env e1, arrow, A.Ref (tokref, lvalue env lv))

and for_expr env el = List.map (expr env) (comma_list el)

and colon_stmt env = function
  | SingleStmt st -> stmt env st []
  | ColonStmt (_, stl, _, _) -> List.fold_right (stmt_and_def env) stl []
(*of tok (* : *) * stmt_and_def list * tok (* endxxx *) * tok (* ; *) *)

and switch_case_list env = function
  | CaseList (_, _, cl, _) -> List.map (case env) cl
  | CaseColonList (_, _, cl, _, _) -> List.map (case env) cl

and case env = function
  | Case (tok, e, _, stl) ->
      let stl = List.fold_right (stmt_and_def env) stl [] in
      A.Case (tok, expr env e, stl)
  | Default (tok, _, stl) ->
      let stl = List.fold_right (stmt_and_def env) stl [] in
      A.Default (tok, stl)

and foreach_variable env (r, lv) =
  let e = lvalue env lv in
  let e = if r <> None then A.Ref (fake "&", e) else e in
  e

and foreach_pattern env pat =
  match pat with
  | ForeachVar v -> foreach_variable env v
  | ForeachArrow (v1, tok, v2) ->
    A.Arrow(foreach_pattern env v1, tok, foreach_pattern env v2)
  | ForeachList (_, (t1, xs, t2)) ->
    let xs = comma_list xs in
    let xs = List.fold_right (list_assign env) xs [] in
    A.List (t1, xs, t2)


and catch env (t, (_, (fq, dn), _), (_, stdl, _)) =
  let stdl = List.fold_right (stmt_and_def env) stdl [] in
  let fq = hint_type env fq in
  let dn = dname dn in
  t, fq, dn, stdl

and finally env (t, (_, stdl, _)) =
  let stdl = List.fold_right (stmt_and_def env) stdl [] in
  t, stdl

and static_var env (x, e) =
  dname x, opt static_scalar_affect env e

and list_assign env x acc =
  match x with
  | ListVar lv -> (lvalue env lv) :: acc
  | ListList (_, (t1, la, t2)) ->
      let la = comma_list la in
      let la = List.fold_right (list_assign env) la [] in
      A.List (t1, la, t2) :: acc
  | ListEmpty -> acc

and assignOp _env = function
  | AssignOpArith aop -> 
      let aop = arith_op aop in
      A.ArithOp aop
  | AssignConcat -> A.BinaryConcat

and global_var env = function
  | GlobalVar dn -> A.Var (dname dn)
  (* this is used only once in our codebase, and it should not ... *)
  | GlobalDollar (tok, lv) ->
      A.Call (A.Id [(A.builtin "eval_var", wrap tok)], fb[lvalue env lv])
  | GlobalDollarExpr (tok, (_, e, _)) ->
      A.Call (A.Id [(A.builtin "eval_var", wrap tok)], fb[expr env e])


and attributes env = function
  | None -> []
  | Some (_, xs, _) ->
    let xs = comma_list xs in
    xs |> List.map (function
    | Attribute (s, tok) -> A.Id [s, wrap tok]
    | AttributeWithArgs ((s, tok), (lp, xs, rp)) ->
      A.Call (A.Id [s, wrap tok], (lp, (List.map (static_scalar env) (comma_list xs)), rp)
    ))

