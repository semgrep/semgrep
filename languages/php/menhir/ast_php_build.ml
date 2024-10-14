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
let error tok s = raise (Parsing_error.Ast_builder_error (s, tok))

(* old: opti: to get www from 380MB to 190MB marshalled, but not worth it
 *  if !store_position then Some tok else None
 *)
let wrap tok = tok
let fake tok s = Tok.fake_tok tok s
let unsafe_fake s = Tok.unsafe_fake_tok s
let fb = Tok.fake_bracket
let unsafe_fb = Tok.unsafe_fake_bracket

let stmt1_with b xs =
  match xs with
  | [] -> A.Block (b [])
  | [ st ] -> st
  | xs -> A.Block (b xs)

let stmt1 tok xs = stmt1_with (fb tok) xs
let unsafe_stmt1 xs = stmt1_with unsafe_fb xs

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let opt f env x =
  match x with
  | None -> None
  | Some x -> Some (f env x)

let rec comma_list = function
  | [] -> []
  | Either.Left x :: rl -> x :: comma_list rl
  | Either.Right _ :: rl -> comma_list rl

let rec comma_list_dots = function
  | [] -> []
  | Either_.Left3 x :: rl -> x :: comma_list_dots rl
  | (Either_.Middle3 _ | Either_.Right3 _) :: rl -> comma_list_dots rl

let brace (_, x, _) = x
let bracket f (a, b, c) = (a, f b, c)
let noop tok = A.Block (fb tok [])

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let rec program top_l =
  let env = empty_env () in
  toplevels env top_l

and any x =
  let env = empty_env () in
  Common.save_excursion Flag_parsing.sgrep_mode true (fun () -> any_aux env x)

and partial env = function
  | PartialIf (t, e) ->
      let e = expr env e in
      A.PartialIf (t, e)

and any_aux env = function
  | Expr e ->
      let e = expr env e in
      A.Expr2 e
  | Stmt2 st ->
      let st = unsafe_stmt1 (stmt env st []) in
      A.Stmt st
  | Toplevel x -> (
      match toplevel env x with
      | [ st ] -> A.Stmt st
      | sts -> A.Program sts)
  | Toplevels x
  | Program x ->
      let x = toplevels env x in
      A.Program x
  | Partial x -> A.Partial (partial env x)
  | StmtAndDefs _
  | Argument _
  | Arguments _
  | Parameter _
  | Parameters _
  | Body _
  | ClassStmt _
  | ClassConstant2 _
  | ClassVariable _
  | ListAssign _
  | ColonStmt2 _
  | Case2 _
  | Info _
  | InfoList _
  | Ident2 _
  | Hint2 _ ->
      failwith "TODO: PHP"

and toplevels env xs =
  match xs with
  | [] -> []
  | x :: xs -> (
      match x with
      | NamespaceDef (t, qi, _) ->
          let xs, rest =
            xs
            |> List_.span (function
                 (* less: actually I'm not sure you can mix NamespaceDef and BracketDef*)
                 | NamespaceDef _
                 | NamespaceBracketDef _ ->
                     false
                 | _ -> true)
          in
          let body = toplevels env xs in
          let rest = toplevels env rest in
          A.NamespaceDef (t, qualified_ident env qi, fb t body) :: rest
      | _ -> toplevel env x @ toplevels env xs)

and toplevel env st =
  match st with
  | TopStmt x -> stmt env x []
  | FuncDef fd -> [ A.FuncDef (func_def env fd) ]
  | ClassDef cd -> [ A.ClassDef (class_def env cd) ]
  | ConstantDef x -> [ A.ConstantDef (constant_def env x) ]
  | TypeDef x -> [ A.TypeDef (type_def env x) ]
  | FinalDef _ -> []
  (* error recovery is off by default now *)
  | NotParsedCorrectly _ -> raise Common.Impossible
  (* should be handled by toplevel above *)
  | NamespaceDef (_, _, _) -> raise Impossible
  | NamespaceBracketDef (tok, qu_opt, (t1, xs, t2)) ->
      let qi =
        match qu_opt with
        | Some qu -> qualified_ident env qu
        | None -> [ (A.special "ROOT", wrap tok) ]
      in
      [ A.NamespaceDef (tok, qi, (t1, toplevels env xs, t2)) ]
  | NamespaceUse (tok, _kwdopt, xs, _) ->
      xs |> uncomma
      |> List_.map (fun (qu, alias_opt) ->
             let qu = qualified_ident env qu in
             let alias_opt =
               match alias_opt with
               | None -> None
               | Some (_t, id) -> Some (ident env id)
             in
             A.NamespaceUse (tok, qu, alias_opt))

(* ------------------------------------------------------------------------- *)
(* Names *)
(* ------------------------------------------------------------------------- *)
and name_hint_type env = function
  | XName [ QI (Name ("class", tok)) ] -> [ (A.special "class", wrap tok) ]
  | XName qi -> qualified_ident env qi
  | Self tok -> [ (A.special "self", wrap tok) ]
  | Parent tok -> [ (A.special "parent", wrap tok) ]
  | LateStatic tok -> [ (A.special "static", wrap tok) ]

and name_expr env = function
  | XName [ QI (Name ("class", tok)) ] -> A.Id [ (A.special "class", wrap tok) ]
  | XName qi -> A.Id (qualified_ident env qi)
  | Self tok -> A.IdSpecial (A.Self, tok)
  | Parent tok -> A.IdSpecial (A.Self, tok)
  | LateStatic tok -> A.Id [ (A.special "static", wrap tok) ]

and ident _env = function
  | Name (s, tok) -> (s, wrap tok)

and qualified_ident env xs =
  let leading, rest =
    match xs with
    (* a leading '\' *)
    | QITok tok :: rest -> ([ (A.special "ROOT", wrap tok) ], rest)
    | QI (Name ("namespace", tok)) :: rest ->
        ([ (A.special "namespace", wrap tok) ], rest)
    | rest -> ([], rest)
  in
  leading
  @ (rest
    |> List_.filter_map (function
         | QITok _ -> None
         | QI id -> Some (ident env id)))

and dname = function
  | DName (s, tok) ->
      if s.[0] =$= '$' then
        if !Flag_parsing.sgrep_mode then (s, wrap tok)
        else failwith "dname: the string has a dollar, weird"
      else
        (* We abuse Id to represent both variables and functions/classes
         * identifiers in ast_php_simple, so to avoid collision
         * we prepend a $ (the $ was removed in ast_php.ml and parse_php.ml)
         *)
        ("$" ^ s, wrap tok)

(* ------------------------------------------------------------------------- *)
(* Statement *)
(* ------------------------------------------------------------------------- *)
and stmt env st acc =
  match st with
  | ExprStmt (e, t) ->
      let e = expr env e in
      A.Expr (e, t) :: acc
  (* Why not just acc? because we abuse noop in the abstract interpreter? *)
  | EmptyStmt t -> noop t :: acc
  | Block (lb, stdl, rb) ->
      A.Block (lb, List_.fold_right (stmt_and_def env) stdl [], rb) :: acc
  | If (tok, (_, e, _), st, il, io) ->
      let e = expr env e in
      let st = stmt1 tok (stmt env st []) in
      let il = List_.fold_right (if_elseif env) il (if_else tok env io) in
      A.If (tok, e, st, il) :: acc
  | IfColon (tok, (_, e, _), _, st, il, io, _, _) ->
      let e = expr env e in
      let st = stmt1 tok (List_.fold_right (stmt_and_def env) st []) in
      let il = List_.fold_right (new_elseif env) il (new_else tok env io) in
      A.If (tok, e, st, il) :: acc
  | While (tok, (_, e, _), cst) ->
      let cst = colon_stmt tok env cst in
      A.While (tok, expr env e, cst) :: acc
  | Do (tok, st, _, (_, e, _), _) ->
      A.Do (tok, stmt1 tok (stmt env st []), expr env e) :: acc
  | For (tok, _, e1, _, e2, _, e3, _, st) ->
      let st = colon_stmt tok env st in
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
      let pat = foreach_pattern tok env pat in
      let cst = colon_stmt tok env cst in
      A.Foreach (tok, e, tas, pat, cst) :: acc
  | Break (tok, e, _) -> A.Break (tok, opt expr env e) :: acc
  | Continue (tok, eopt, _) -> A.Continue (tok, opt expr env eopt) :: acc
  | Label (id, tok, st) ->
      A.Label (ident env id, tok, stmt1 tok (stmt env st [])) :: acc
  | Goto (tok, id, _) -> A.Goto (tok, ident env id) :: acc
  | Return (tok, eopt, _) -> A.Return (tok, opt expr env eopt) :: acc
  | Throw (tok, e, sc) -> A.Expr (A.Throw (tok, expr env e), sc) :: acc
  | Try (tok, (lb, stl, rb), cl, fl) ->
      let stl = List_.fold_right (stmt_and_def env) stl [] in
      let cl = List_.map (catch env) cl in
      let fl = List_.map (finally env) fl in
      A.Try (tok, A.Block (lb, stl, rb), cl, fl) :: acc
  | Echo (tok, el, t) ->
      A.Expr
        ( A.Call
            ( A.Id [ (A.builtin "echo", wrap tok) ],
              fb tok (List_.map (fun e -> A.Arg (expr env e)) (comma_list el))
            ),
          t )
      :: acc
  | Globals (tok, gvl, _) ->
      A.Global (tok, List_.map (global_var env) (comma_list gvl)) :: acc
  | StaticVars (tok, svl, _) ->
      A.StaticVars (tok, List_.map (static_var env) (comma_list svl)) :: acc
  | InlineHtml (s, tok) ->
      A.Expr
        ( A.Call
            ( A.Id [ (A.builtin "echo", wrap tok) ],
              fb tok [ A.Arg (A.String (s, wrap tok)) ] ),
          tok )
      :: acc
  | Use (tok, _fn, _) -> error tok "TODO:Use"
  | Unset (tok, (t1, lp, t2), sc) ->
      let lp = comma_list lp in
      let lp = List_.map (fun e -> A.Arg (lvalue env e)) lp in
      let id = A.IdSpecial (A.FuncLike A.Unset, wrap tok) in
      A.Expr (A.Call (id, (t1, lp, t2)), sc) :: acc
  (* http://php.net/manual/en/control-structures.declare.php *)
  | Declare (tok, args, colon_st) -> (
      match (args, colon_st) with
      (* declare(strict=1); (or 0) can be skipped,
       * See 'i wiki/index.php/Pfff/Declare_strict' *)
      | ( (_, [ Either.Left (Name ("strict", _), (_, Sc (C (Int pi)))) ], _),
          SingleStmt (EmptyStmt _) )
      | ( ( _,
            [ Either.Left (Name ("strict_types", _), (_, Sc (C (Int pi)))) ],
            _ ),
          SingleStmt (EmptyStmt _) )
        when Parsed_int.eq_const pi 0 || Parsed_int.eq_const pi 1
             (* declare(ticks=1); can be skipped too.
              * http://www.php.net/manual/en/control-structures.declare.php#control-structures.declare.ticks
              *) ->
          acc
      | (_, [ Either.Left (Name ("ticks", _), (_, Sc (C (Int pi)))) ], _), _
        when Parsed_int.eq_const pi 1 ->
          let cst = colon_stmt tok env colon_st in
          cst :: acc
      | _ -> error tok "TODO: declare")
  | FuncDefNested fd -> A.FuncDef (func_def env fd) :: acc
  | ClassDefNested cd -> A.ClassDef (class_def env cd) :: acc

and if_elseif env (tok, (_, e, _), st) acc =
  let e = expr env e in
  let st = stmt1 tok (stmt env st []) in
  A.If (tok, e, st, acc)

and if_else tok env = function
  | None -> noop tok
  | Some (_, (If _ as st)) -> (
      match stmt env st [] with
      | [ x ] -> x
      | _l -> assert false)
  | Some (tok, st) -> stmt1 tok (stmt env st [])

and new_elseif env (tok, (_, e, _), _, stl) acc =
  let e = expr env e in
  let st = stmt1 tok (List_.fold_right (stmt_and_def env) stl []) in
  A.If (tok, e, st, acc)

and new_else tok env = function
  | None -> noop tok
  | Some (tok, _, st) -> stmt1 tok (List_.fold_right (stmt_and_def env) st [])

and stmt_and_def env st acc = stmt env st acc

(* ------------------------------------------------------------------------- *)
(* Expression *)
(* ------------------------------------------------------------------------- *)
and expr env = function
  | DeepEllipsis x -> A.DeepEllipsis (bracket (expr env) x)
  | Sc sc -> scalar env sc
  | Id n -> name_expr env n
  | IdVar dn -> A.Var (dname dn)
  | This tok -> A.IdSpecial (A.This, tok)
  (* ($o->fn)(...) ==> call_user_func($o->fn, ...) *)
  | Call (ParenExpr (tok, ObjGet (e1, arrow, Id fld), _), (lp, args, rp)) ->
      let e1 = expr env e1 in
      let fld_ident = name_expr env fld in
      let args = comma_list args in
      let args = List_.map (argument env) args in
      A.Call
        ( A.Id [ ("call_user_func", wrap tok) ],
          (lp, A.Arg (A.Obj_get (e1, arrow, fld_ident)) :: args, rp) )
  | Call (e, (lp, args, rp)) ->
      let e = expr env e in
      let args = comma_list args in
      let args = List_.map (argument env) args in
      A.Call (e, (lp, args, rp))
  | ObjGet (e1, tok, e2) ->
      let e1 = expr env e1 in
      let e2 = expr env e2 in
      A.Obj_get (e1, tok, e2)
  | ClassGet (e1, tok, e2) ->
      let e1 = class_name_reference env e1 in
      let e2 = expr env e2 in
      A.Class_get (e1, tok, e2)
  | HashGet (e1, e2) ->
      let e1 = expr env e1 in
      let l1, e2, l2 = bracket (expr env) e2 in
      A.Array_get (e1, (l1, Some e2, l2))
  | ArrayGet (e1, (l, e2opt, r)) ->
      let e1 = expr env e1 in
      let e2opt = opt expr env e2opt in
      A.Array_get (e1, (l, e2opt, r))
  | BraceIdent (_l, e, _r) -> expr env e
  | Deref (tok, e) ->
      A.Call
        ( A.Id [ (A.builtin "eval_var", wrap tok) ],
          fb tok [ A.Arg (expr env e) ] )
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
      (* less: actually not equivalent if 'e' is a complex expression with
       * side effect *)
      A.CondExpr (e, e, expr env e3)
  | CondExpr (e1, _, Some e2, _, e3) ->
      A.CondExpr (expr env e1, expr env e2, expr env e3)
  | AssignList (_, (t1, la, t2), tokeq, e) ->
      let la = comma_list la in
      let la = List_.fold_right (list_assign env) la [] in
      let e = expr env e in
      A.Assign (A.List (t1, la, t2), tokeq, e)
  | ArrayLong (_, (t1, apl, t2))
  | ArrayShort (t1, apl, t2) ->
      let apl = comma_list apl in
      let apl = List_.map (array_pair env) apl in
      A.ConsArray (t1, apl, t2)
  | New (tok, cn, args) ->
      let args =
        match args with
        | None -> []
        | Some (_, cl, _) -> List_.map (argument env) (comma_list cl)
      in
      let cn = class_name_reference env cn in
      A.New (tok, cn, args)
  | NewAnonClass (tok, args, cdef) ->
      let args =
        match args with
        | None -> []
        | Some (_, cl, _) -> List_.map (argument env) (comma_list cl)
      in
      let cdef = class_def env cdef in
      A.NewAnonClass (tok, args, cdef)
  | Clone (tok, e) ->
      A.Call
        (A.Id [ (A.builtin "clone", wrap tok) ], fb tok [ A.Arg (expr env e) ])
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
      let type_, tok = c in
      let type_ = cast_type env type_ in
      let c = (type_, wrap tok) in
      A.Cast (c, expr env e)
  | CastUnset (tok, _) -> error tok "TODO: CastUnset"
  | InstanceOf (e, tok, cn) ->
      let e = expr env e in
      let cn = class_name_reference env cn in
      A.InstanceOf (tok, e, cn)
  | Eval (tok, (lp, e, rp)) ->
      let id = A.IdSpecial (A.FuncLike A.Eval, wrap tok) in
      A.Call (id, (lp, [ A.Arg (expr env e) ], rp))
  | Lambda ld -> A.Lambda (lambda_def env ld)
  | ShortLambda def -> A.Lambda (short_lambda_def env def)
  | Exit (tok, e) ->
      let arg =
        match e with
        | None
        | Some (_, None, _) ->
            []
        | Some (_, Some e, _) -> [ A.Arg (expr env e) ]
      in
      let id = A.IdSpecial (A.FuncLike A.Exit, wrap tok) in
      A.Call (id, fb tok arg)
  | At (tok, e) ->
      let arg = A.Arg (expr env e) in
      A.Call (A.Id [ (A.builtin "at", wrap tok) ], fb tok [ arg ])
  | Print (tok, e) ->
      A.Call
        (A.Id [ (A.builtin "print", wrap tok) ], fb tok [ A.Arg (expr env e) ])
  | BackQuote (t1, el, t2) ->
      A.Call
        ( A.Id [ (A.builtin "exec", wrap t1 (* not really an exec token *)) ],
          fb t1 [ A.Arg (A.Guil (t1, List_.map (encaps env) el, t2)) ] )
  | Include (tok, e) ->
      A.Call
        (A.Id [ (A.builtin "include", wrap tok) ], fb tok [ A.Arg (expr env e) ])
  | IncludeOnce (tok, e) ->
      A.Call
        ( A.Id [ (A.builtin "include_once", wrap tok) ],
          fb tok [ A.Arg (expr env e) ] )
  | Require (tok, e) ->
      A.Call
        (A.Id [ (A.builtin "require", wrap tok) ], fb tok [ A.Arg (expr env e) ])
  | RequireOnce (tok, e) ->
      A.Call
        ( A.Id [ (A.builtin "require_once", wrap tok) ],
          fb tok [ A.Arg (expr env e) ] )
  | Empty (tok, (lp, lv, rp)) ->
      let id = A.IdSpecial (A.FuncLike A.Empty, wrap tok) in
      A.Call (id, (lp, [ A.Arg (lvalue env lv) ], rp))
  | Isset (tok, (lp, lvl, rp)) ->
      let id = A.IdSpecial (A.FuncLike A.Isset, wrap tok) in
      A.Call
        ( id,
          (lp, List_.map (fun e -> A.Arg (lvalue env e)) (comma_list lvl), rp)
        )
  | Yield (tok, e) ->
      A.Call
        ( A.Id [ (A.builtin "yield", wrap tok) ],
          fb tok [ A.Arg (array_pair env e) ] )
  (* todo? merge in one yield_break? *)
  | YieldBreak (tok, tok2) ->
      A.Call
        ( A.Id [ (A.builtin "yield", wrap tok) ],
          fb tok [ A.Arg (A.Id [ (A.builtin "yield_break", wrap tok2) ]) ] )
  | Await (tok, e) ->
      A.Call
        (A.Id [ (A.builtin "await", wrap tok) ], fb tok [ A.Arg (expr env e) ])
  | Ellipsis t -> A.Ellipsis t
  | ParenExpr (_, e, _) -> expr env e

and arith_op = function
  | Plus -> G.Plus
  | Minus -> G.Minus
  | Mul -> G.Mult
  | Div -> G.Div
  | Mod -> G.Mod
  | Pow -> G.Pow
  | DecLeft -> G.LSL
  | DecRight -> G.LSR
  | And -> G.BitAnd
  | Or -> G.BitOr
  | Xor -> G.BitXor

and logical_op = function
  | Inf -> G.Lt
  | Sup -> G.Gt
  | InfEq -> G.LtE
  | SupEq -> G.GtE
  | Eq -> G.Eq
  | NotEq -> G.NotEq
  | Identical -> G.PhysEq
  | NotIdentical -> G.NotPhysEq
  (* less: add difference in ast_generic? 'and' as shortcut operator
   * and 'and' as boolean operator *)
  | AndLog -> G.And
  | OrLog -> G.Or
  | XorLog -> G.Xor
  | AndBool -> G.And
  | OrBool -> G.Or

and binary_op = function
  | Arith op -> A.ArithOp (arith_op op)
  | Logical op -> A.ArithOp (logical_op op)
  | BinaryConcat -> A.BinaryConcat
  | CombinedComparison -> A.CombinedComparison

and unary_op = function
  | UnPlus -> G.Plus
  | UnMinus -> G.Minus
  | UnBang -> G.Not
  | UnTilde -> G.BitXor

and cast_type _env = function
  | BoolTy -> A.BoolTy
  | IntTy -> A.IntTy
  | DoubleTy -> A.DoubleTy
  | StringTy -> A.StringTy
  | ArrayTy -> A.ArrayTy
  | ObjectTy -> A.ObjectTy

and scalar env = function
  | C cst -> constant env cst
  | Guil (t1, el, t2) -> A.Guil (t1, List_.map (encaps env) el, t2)
  | HereDoc (t1, el, t2) -> A.Guil (t1, List_.map (encaps env) el, t2)

and constant env = function
  | Bool x -> A.Bool x
  | Int x -> A.Int x
  | Double x -> A.Double x
  | String x -> A.String x
  | PreProcess (cpp, tok) -> cpp_directive env tok cpp

and cpp_directive _env tok = function
  | Line -> A.Id [ (A.builtin "__LINE__", wrap tok) ]
  | File -> A.Id [ (A.builtin "__FILE__", wrap tok) ]
  | ClassC -> A.Id [ (A.builtin "__CLASS__", wrap tok) ]
  | MethodC -> A.Id [ (A.builtin "__METHOD__", wrap tok) ]
  | FunctionC -> A.Id [ (A.builtin "__FUNCTION__", wrap tok) ]
  | Dir -> A.Id [ (A.builtin "__DIR__", wrap tok) ]
  | TraitC -> A.Id [ (A.builtin "__TRAIT__", wrap tok) ]
  | NamespaceC -> A.Id [ (A.builtin "__NAMESPACE__", wrap tok) ]

and lvalue env a = expr env a

and argument env = function
  | Arg e -> Arg (expr env e)
  | ArgRef (tok, e) -> A.ArgRef (tok, lvalue env e)
  | ArgUnpack (tok, e) -> A.ArgUnpack (tok, expr env e)
  | ArgLabel (label, tok, e) -> A.ArgLabel (ident env label, tok, expr env e)

and class_name_reference env a = expr env a
and static_scalar_affect env (_, ss) = static_scalar env ss
and static_scalar env a = expr env a

(* ------------------------------------------------------------------------- *)
(* Type *)
(* ------------------------------------------------------------------------- *)
and hint_type env = function
  | Hint (q, _typeTODO) -> A.Hint (name_hint_type env q)
  | HintArray tok -> A.HintArray tok
  | HintQuestion (tok, t) -> A.HintQuestion (tok, hint_type env t)
  | HintTuple (t1, v1, t2) ->
      A.HintTuple (t1, List_.map (hint_type env) (comma_list v1), t2)
  | HintCallback (_, (_, args, ret), _) ->
      let args = List_.map (hint_type env) (comma_list_dots (brace args)) in
      let ret = Option.map (fun (_, t) -> hint_type env t) ret in
      A.HintCallback (args, ret)
  | HintTypeConst (lhs, tok, rhs) ->
      A.HintTypeConst (hint_type env lhs, tok, hint_type env rhs)
  | HintVariadic (tok, hint) ->
      let hint = Option.map (hint_type env) hint in
      A.HintVariadic (tok, hint)

(* ------------------------------------------------------------------------- *)
(* Definitions *)
(* ------------------------------------------------------------------------- *)
and constant_def env
    { cst_name; cst_val; cst_type = _TODO; cst_toks = tok, _, _ } =
  let name = ident env cst_name in
  let value = expr env cst_val in
  { A.cst_tok = tok; A.cst_name = name; A.cst_body = value }

and comma_list_dots_params f xs =
  match xs with
  | [] -> []
  | Either_.Left3 x :: rl -> A.ParamClassic (f x) :: comma_list_dots_params f rl
  (* less: guard by sgrep_mode? *)
  | Either_.Middle3 t :: rl -> A.ParamEllipsis t :: comma_list_dots_params f rl
  | Either_.Right3 _ :: rl -> comma_list_dots_params f rl

and func_def env f =
  let _, params, _ = f.f_params in
  let params = comma_list_dots_params (parameter env) params in
  let lb, body, rb = f.f_body in
  {
    A.f_ref = f.f_ref <> None;
    A.f_name = ident env f.f_name;
    A.f_attrs = attributes env f.f_attrs;
    A.f_params = params;
    A.f_return_type = Option.map (fun (_, t) -> hint_type env t) f.f_return_type;
    A.f_body = A.Block (lb, List_.fold_right (stmt_and_def env) body [], rb);
    A.f_kind = (A.Function, f.f_tok);
    A.m_modifiers = [];
    A.l_uses = [];
  }

and lambda_def env (l_use, ld) =
  let _, params, _ = ld.f_params in
  let params = comma_list_dots_params (parameter env) params in
  let lb, body, rb = ld.f_body in
  {
    A.f_ref = ld.f_ref <> None;
    A.f_name = (A.special "_lambda", wrap ld.f_tok);
    A.f_params = params;
    A.f_return_type =
      Option.map (fun (_, t) -> hint_type env t) ld.f_return_type;
    A.f_body = A.Block (lb, List_.fold_right (stmt_and_def env) body [], rb);
    A.f_kind = (A.AnonLambda, ld.f_tok);
    A.m_modifiers = [];
    A.f_attrs = attributes env ld.f_attrs;
    A.l_uses =
      (match l_use with
      | None -> []
      | Some (_, (_lp, xs, _rp)) ->
          comma_list xs
          |> List_.map (function LexicalVar (is_ref, name) ->
                 (is_ref <> None, dname name)));
  }

and short_lambda_def env def =
  let sl_tok =
    match def.sl_tok with
    | Some tok -> wrap tok
    | None -> wrap (unsafe_fake "_lambda")
  in
  {
    A.f_ref = false;
    f_name = (A.special "_lambda", sl_tok);
    f_params =
      (match def.sl_params with
      | SLSingleParam p -> [ ParamClassic (parameter env p) ]
      | SLParamsOmitted -> []
      | SLParams (_, xs, _) -> comma_list_dots_params (parameter env) xs);
    f_return_type = None;
    f_body =
      (match def.sl_body with
      | SLExpr e -> A.Expr (expr env e, Tok.sc sl_tok)
      | SLBody (lb, body, rb) ->
          Block (lb, List_.fold_right (stmt_and_def env) body [], rb));
    f_kind = (A.ShortLambda, sl_tok);
    m_modifiers = [];
    f_attrs = [];
    l_uses = [];
  }

and type_def env def =
  { A.t_name = ident env def.t_name; A.t_kind = type_def_kind env def.t_kind }

and type_def_kind env = function
  | Alias t -> A.Alias (hint_type env t)

and class_def env c =
  let t1, body, t2 = c.c_body in
  let methods, implicit_fields =
    List_.fold_right (class_body env) body ([], [])
  in
  let kind, modifiers = class_type env c.c_type in
  {
    A.c_kind = kind;
    A.c_modifiers = modifiers;
    A.c_name = ident env c.c_name;
    A.c_attrs = attributes env c.c_attrs;
    A.c_extends =
      (match c.c_extends with
      | None -> None
      | Some (_, x) -> Some (hint_type env x));
    A.c_uses = List_.fold_right (class_traits env) body [];
    A.c_implements =
      (match c.c_implements with
      | None -> []
      | Some x -> interfaces env x);
    A.c_constants = List_.fold_right (class_constants env) body [];
    A.c_variables =
      implicit_fields @ List_.fold_right (class_variables env) body [];
    A.c_methods = methods;
    A.c_enum_type =
      (match c.c_enum_type with
      | None -> None
      | Some enum ->
          Some
            {
              A.e_base = hint_type env enum.e_base;
              A.e_constraint =
                (match enum.e_constraint with
                | None -> None
                | Some (_, cnstr_ty) -> Some (hint_type env cnstr_ty));
            });
    A.c_braces = (t1, (), t2);
  }

and class_type _env = function
  | ClassRegular tok -> ((A.Class, tok), [])
  | ClassFinal (tokf, tok) -> ((A.Class, tok), [ (Final, tokf) ])
  | ClassAbstract (toka, tok) -> ((A.Class, tok), [ (Abstract, toka) ])
  | Interface tok -> ((A.Interface, tok), [])
  | Trait tok -> ((A.Trait, tok), [])
  | Enum tok -> ((A.Enum, tok), [])

and interfaces env (_, intfs) =
  let intfs = comma_list intfs in
  List_.map (fun x -> hint_type env x) intfs

and class_traits env x acc =
  match x with
  | UseTrait (_, l, _) -> List_.map (hint_type env) (comma_list l) @ acc
  | _ -> acc

and class_constants env st acc =
  match st with
  | ClassConstants (_, tok, _, cl, _) ->
      List_.fold_right
        (fun (n, ss) acc ->
          let body = static_scalar_affect env ss in
          let cst =
            { A.cst_name = ident env n; cst_body = body; cst_tok = tok }
          in
          cst :: acc)
        (comma_list cl) acc
  | _ -> acc

and class_variables env st acc =
  match st with
  | ClassVariables (m, ht, cvl, _) ->
      let cvl = comma_list cvl in
      let m =
        match m with
        | NoModifiers _ -> []
        | VModifiers l -> List_.map (modifier env) l
      in
      let ht = opt hint_type env ht in
      List_.map
        (fun (n, ss) ->
          let name = dname n in
          let value = opt static_scalar_affect env ss in
          {
            A.cv_name = name;
            A.cv_value = value;
            A.cv_modifiers = m;
            A.cv_type = ht;
          })
        cvl
      @ acc
  | _ -> acc

and modifier _env m =
  let m, tok = m in
  match m with
  | Public -> (A.Public, tok)
  | Private -> (A.Private, tok)
  | Protected -> (A.Protected, tok)
  | Abstract -> (A.Abstract, tok)
  | Final -> (A.Final, tok)
  | Static -> (A.Static, tok)
  | Async -> (A.Async, tok)

and class_body env st (mets, flds) =
  match st with
  | Method md ->
      let met, more_flds = method_def env md in
      (met :: mets, more_flds @ flds)
  | ClassVariables _
  | ClassConstants _
  | UseTrait _
  | ClassType _
  | DeclEllipsis _ ->
      (mets, flds)

and method_def env m =
  let _, params, _ = m.f_params in
  let mds = List_.map (modifier env) m.f_modifiers in
  let params = comma_list_dots_params (parameter env) params in
  (*
  let implicits =
    params |> List_.filter_map (fun p ->
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
                  A.Var (str_with_dollar, tok)), PI.sc)
    )
  in
*)
  let implicit_flds = [] in
  (* TODO? *)
  ( {
      A.m_modifiers = mds;
      A.f_ref =
        (match m.f_ref with
        | None -> false
        | Some _ -> true);
      A.f_name = ident env m.f_name;
      A.f_attrs = attributes env m.f_attrs;
      A.f_params = params;
      A.f_return_type =
        Option.map (fun (_, t) -> hint_type env t) m.f_return_type;
      A.f_body = (* implicit_assigns @ *) method_body env m.f_body;
      A.f_kind = (A.Method, m.f_tok);
      A.l_uses = [];
    },
    implicit_flds )

and method_body env (lb, stl, rb) =
  A.Block (lb, List_.fold_right (stmt_and_def env) stl [], rb)

and parameter env
    {
      p_type = t;
      p_ref = r;
      p_name = name;
      p_default = d;
      p_attrs = a;
      p_modifier = _mTODO;
      p_variadic = variadic;
    } =
  {
    A.p_type = opt hint_type env t;
    A.p_ref = r;
    A.p_name = dname name;
    A.p_default = opt static_scalar_affect env d;
    A.p_attrs = attributes env a;
    A.p_variadic = variadic;
  }

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

and encaps env = function
  | EncapsString (s, tok) -> A.String (s, wrap tok)
  | EncapsVar v -> lvalue env v
  | EncapsCurly (_, lv, _) -> lvalue env lv
  | EncapsDollarCurly (_, lv, _) -> lvalue env lv
  | EncapsExpr (_, e, _) -> expr env e

and array_pair env = function
  | ArrayExpr e -> expr env e
  | ArrayRef (tok, lv) -> A.Ref (tok, lvalue env lv)
  | ArrayArrowExpr (e1, tok, e2) -> A.Arrow (expr env e1, tok, expr env e2)
  | ArrayArrowRef (e1, arrow, tokref, lv) ->
      A.Arrow (expr env e1, arrow, A.Ref (tokref, lvalue env lv))

and for_expr env el = List_.map (expr env) (comma_list el)

and colon_stmt tok env = function
  | SingleStmt st -> stmt1 tok (stmt env st [])
  | ColonStmt (tok, stl, _, _) ->
      stmt1 tok (List_.fold_right (stmt_and_def env) stl [])
(*of tok (* : *) * stmt_and_def list * tok (* endxxx *) * tok (* ; *) *)

and switch_case_list env = function
  | CaseList (_, _, cl, _) -> List_.map (case env) cl
  | CaseColonList (_, _, cl, _, _) -> List_.map (case env) cl

and case env = function
  | Case (tok, e, _, stl) ->
      let stl = List_.fold_right (stmt_and_def env) stl [] in
      A.Case (tok, expr env e, stl)
  | Default (tok, _, stl) ->
      let stl = List_.fold_right (stmt_and_def env) stl [] in
      A.Default (tok, stl)

and foreach_variable tok env (r, lv) =
  let e = lvalue env lv in
  let e = if r <> None then A.Ref (fake tok "&", e) else e in
  e

and foreach_pattern tok env pat =
  match pat with
  | ForeachVar v -> foreach_variable tok env v
  | ForeachArrow (v1, tok, v2) ->
      A.Arrow (foreach_pattern tok env v1, tok, foreach_pattern tok env v2)
  | ForeachList (_, (t1, xs, t2)) ->
      let xs = comma_list xs in
      let xs = List_.fold_right (list_assign env) xs [] in
      A.List (t1, xs, t2)

and catch env (t, (_, (fq, dn), _), (lb, stdl, rb)) =
  let stdl = A.Block (lb, List_.fold_right (stmt_and_def env) stdl [], rb) in
  let fq = hint_type env fq in
  let dn = dname dn in
  (t, fq, dn, stdl)

and finally env (t, (lb, stdl, rb)) =
  let stdl = A.Block (lb, List_.fold_right (stmt_and_def env) stdl [], rb) in
  (t, stdl)

and static_var env (x, e) = (dname x, opt static_scalar_affect env e)

and list_assign env x acc =
  match x with
  | ListVar lv -> lvalue env lv :: acc
  | ListList (_, (t1, la, t2)) ->
      let la = comma_list la in
      let la = List_.fold_right (list_assign env) la [] in
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
      A.Call
        ( A.Id [ (A.builtin "eval_var", wrap tok) ],
          fb tok [ A.Arg (lvalue env lv) ] )
  | GlobalDollarExpr (tok, (_, e, _)) ->
      A.Call
        ( A.Id [ (A.builtin "eval_var", wrap tok) ],
          fb tok [ A.Arg (expr env e) ] )

and attributes env = function
  | None -> []
  | Some (_, xs, _) ->
      let xs = comma_list xs in
      xs
      |> List_.map (function
           | Attribute (s, tok) -> A.Id [ (s, wrap tok) ]
           | AttributeWithArgs ((s, tok), (lp, xs, rp)) ->
               A.Call
                 ( A.Id [ (s, wrap tok) ],
                   ( lp,
                     List_.map
                       (fun e -> A.Arg (static_scalar env e))
                       (comma_list xs),
                     rp ) ))
