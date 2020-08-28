(* Julien Verlaguet
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
open Cst_php

module A = Ast_pp
module PI = Parse_info
module T = Parser_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * The goal of this module is to take an AST (see ast_php.ml) and its
 * list of tokens (see parser_php.ml), including the newline and comment
 * tokens, and return an AST that will make it easy to pretty print
 * the code while still maintaining the comments of the original file
 * (see ast_pp.ml).
 *
 * This is mostly a copy paste of ast_php_simple_build.ml.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(*
 * An environment contains all "esthetic" newlines and all the comments
 * of the current chunk of code we are processing.
 * It also contains the line numbers where the newlines/comments appear
 * in the code.
 *
 * An 'esthetic' newline is a newline used to better separate code.
 * For instance in
 *
 *    $x = 1;
 *    $x = 2;
 *
 * there is a newline, but we don't consider it an esthetic newline.
 * We don't consider those newlines because the pretty printer already knows
 * (and arguably knows better than the developer) where to insert them
 * in the code. Here is an example of an esthetic newline:
 *
 *    $x = 1;
 *
 *    $x = 2;
 *
 * See extract_esthetic_newlines_and_comments for more information.
 *
 * The environment is a ref that gets smaller as we build the ast_pp
 * from bottom to top (hence the use of fold_right in many places below).
 *)
type env = (int * Ast_pp.esthetic) list ref

exception ObsoleteConstruct of string
exception TodoConstruct of string

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

let rec last = function
  | [] -> assert false
  | [x] -> x
  | _ :: rl -> last rl

let is_space =
  let re = Str.regexp "[\t \n]+" in
  fun x ->
  Str.string_match re x 0

let is_newline =
  let re = Str.regexp "[\t \n]*\n" in
  fun x ->
  Str.string_match re x 0

let ends_with_newline =
  let re = Str.regexp ".*\n" in
  fun x ->
  Str.string_match re x 0

(*****************************************************************************)
(* Newlines and comments helpers *)
(*****************************************************************************)

(* pad: why we need the last line of a stmt/toplevel/... ? *)

let rec last_info_of_stmt = function
  | If (_, _, _, _, Some (_, st)) -> last_info_of_stmt st
  | If (_, _, x, [], None) -> last_info_of_stmt x
  | If (_, _, _, l, None) ->
      let l = List.map (fun (_, _, x) -> x) l in
      last_info_of_stmt (last l)
  | While (_, _, (SingleStmt st)) -> last_info_of_stmt st
  | For (_, _, _, _, _, _, _, _, (SingleStmt st)) -> last_info_of_stmt st
  | Foreach (_, _, _, _, _, _, _, (SingleStmt st)) -> last_info_of_stmt st
  | Declare (_, _, (SingleStmt st)) -> last_info_of_stmt st
  | Try (_, _, [], fl) ->
      let (_, (_, _, x)) = last fl in
      x
  | Try (_, _, cl, _) ->
      let (_, _, (_, _, x)) = last cl in
      x
  | While (_, _, (ColonStmt (_, _, _, x)))
  | For (_, _, _, _, _, _, _, _, (ColonStmt (_, _, _, x)))
  | Foreach (_, _, _, _, _, _, _, (ColonStmt (_, _, _, x)))
  | Declare (_, _, ColonStmt (_, _, _, x)) -> x
  | ExprStmt (_, x)
  | EmptyStmt x
  | Block (_, _, x)
  | IfColon (_, _, _, _, _, _, _, x)
  | Do (_, _, _, _, x)
  | Switch (_, _, (CaseList (_, _, _, x) | CaseColonList (_, _, _, _, x)))
  | Break (_, _, x)
  | Continue (_, _, x)
  | Return (_, _, x)
  | Throw (_, _, x)
  | Echo (_, _, x)
  | Globals (_, _, x)
  | StaticVars (_, _, x)
  | InlineHtml (_, x)
  | Use (_, _, x)
  | Unset (_, _, x)
  | FuncDefNested { f_body = (_, _, x); _ }
  | ClassDefNested { c_body = (_, _, x); _ }
   -> x

let last_line_of_stmt x = Parse_info.line_of_info (last_info_of_stmt x)

let rec last_line_of_stmt_and_defl = function
  | [] -> assert false
  | [st] -> last_line_of_stmt st
  | _ :: rl -> last_line_of_stmt_and_defl rl


(* pad: ?? *)
let pop_env stack line =
  match !stack with
  | [] -> None
  | (x, v) :: rl when x >= line ->
      stack := rl;
      Some v
  | _ -> None

let rec get_comments env acc line =
  match pop_env env line with
  | None   -> acc
  | Some x -> get_comments env (x :: acc) line

let add_comments convert env acc line =
  let comments = get_comments env [] line in
  let comments = List.map convert comments in
  comments @ acc


let add_stmt_comments = add_comments (fun x -> A.StmtEsthet x)
let add_ce_comments = add_comments (fun x -> A.CEEsthet x)
let add_case_comments = add_comments (fun x -> A.CaseEsthet x)

let make_env l =
  let l = List.map (fun (x, y) ->
    let tag =
      match y with
      | "\n" -> A.Newline
      | x -> A.Comment x
    in
    x, tag) l in
  let l = List.sort (fun (x, _) (y, _) -> y - x) l in
  ref l

let line_and_string_from_token x =
  let info = Token_helpers_php.info_of_tok x in
  let line = Parse_info.line_of_info info in
  let str  = Parse_info.str_of_info info in
  x, line, str

let rec extract_esthetic_newlines_and_comments l =
  let k = extract_esthetic_newlines_and_comments in
  match l with
  | [] -> []

  | (_, n1, v1) :: ((_, n2, v2) :: _ as rl)
    when ends_with_newline v1 && is_newline v2 ->
      (* pad: ?? *)
      if is_space v1
      then (n2, "\n") :: k rl
      else (n1, v1) :: k rl

  | (_, _, x) :: rl when is_space x -> k rl
  | (x, n, v) :: rl when Token_helpers_php.is_comment x -> (n, v) :: k rl
  (* the <?php is treated here as a kind of comment *)
  | (Parser_php.T_OPEN_TAG _, n, v):: rl -> (n, v) :: k rl
  | _ :: rl -> k rl

let env_of_tokens tokens =
  let tokens = List.map line_and_string_from_token tokens in
  let tokens = extract_esthetic_newlines_and_comments tokens in
  let env = make_env tokens in
  env

(* We will be called on chunk of code which is a little
 * bit different that when called on the whole code. For instance
 * on a program one can detect esthetic newlines between methods
 * by looking for 2 consecutive newlines, the one after the closing }
 * and the one after that. But with a chunk of code, there will be just
 * a single newline, the one preceding the method. Indeed the chunk will be
 * "\n  public function ....}\n".
 *)
let env_of_tokens_for_spatch toks =
  let toks =
    Common.exclude (function T.TSpaces _ -> true | _ -> false) toks in
  let l info = Parse_info.line_of_info info in
  let str info = Parse_info.str_of_info info in

  let rec aux ~start xs =
    match xs with
    | [] -> []
    | (T.T_COMMENT i1 | T.T_DOC_COMMENT i1 | T.T_OPEN_TAG i1)::xs ->
        (l i1, A.Comment (str i1))::aux xs ~start:false
    (* two consecutive newlines, this is an esthetic newline *)
    | T.TNewline i1::T.TNewline i2::xs ->
        (l i1, A.Newline)::aux (T.TNewline i2::xs) ~start:false

    | T.TNewline i1::xs when start ->
        (l i1, A.Newline)::aux xs ~start:false

    | _::xs -> aux xs ~start:false
  in
  ref (List.rev (aux toks ~start:true))

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let rec toplevel env st acc =
  match st with
  | NamespaceDef _ | NamespaceBracketDef _ | NamespaceUse _
    -> failwith "no support namespace"
  | StmtList stmtl ->
      let acc = List.fold_right (stmt env) stmtl acc in
      acc
  | FinalDef _ -> acc
  | FuncDef fd ->
      let _, _, end_ = fd.f_body in
      let acc = add_stmt_comments env acc (PI.line_of_info end_) in
      A.FuncDef (func_def env fd) :: acc
  | ClassDef cd ->
      let _, _, end_ = cd.c_body in
      let acc = add_stmt_comments env acc (PI.line_of_info end_) in
      A.ClassDef (class_def env cd) :: acc
  | ConstantDef { cst_name; cst_val; cst_toks = (_, _, end_); cst_type =_TODO } ->
      let acc = add_stmt_comments env acc (PI.line_of_info end_) in
      let e = expr env cst_val in
      let s = ident env cst_name in
      A.ConstantDef { Ast_pp.cst_name = s; cst_body = Some e } :: acc
  | TypeDef _-> failwith "pretty printing not supported for typedefs"
  | NotParsedCorrectly _ -> raise Common.Impossible

and (name: env -> name -> string) = fun env -> function
   | XName [QI fqcn] -> ident env fqcn
   | XName qu -> raise (TodoNamespace (Cst_php.info_of_qualified_ident qu))
   | Self _ -> "self"
   | Parent _ -> "parent"
   | LateStatic _ -> "static"

and ident _env = function
  | Name (s, _) -> s
  | XhpName (rl, _) ->
      List.fold_left (fun x y -> x^":"^y) "" rl

and dname = function
  | DName (s, _) ->
      if s.[0] = '$' then s
      else "$"^s

and stmt env st acc =
  let line = last_line_of_stmt st in
  let acc  = add_stmt_comments env acc line in
  let acc  = stmt_ env st acc in
  acc

and stmt_ env st acc =
  match st with
  | ExprStmt (e, _) ->
      let e:(Ast_pp.expr) = expr env e in
      A.Expr e :: acc
  | EmptyStmt _ -> A.Noop :: acc
  | Block (start, stdl, _end_) ->
      let acc = List.fold_right (stmt_and_def env) stdl acc in
      let acc = add_stmt_comments env acc (PI.line_of_info start) in
      acc
  | If (_, (_, e, _), st, il, io) ->
      let e = expr env e in
      let il = List.fold_right (if_elseif env) il (if_else env io) in
      let st = A.Block (stmt env st []) in
      let acc = A.If (e, st, il) :: acc in
      acc
  | While (_, (_, e, _), cst) ->
      let cst = colon_stmt env cst in
      A.While (expr env e, cst) :: acc
  | Do (_, st, _, (_, e, _), _) ->
      A.Do (stmt env st [], expr env e) :: acc
  | For (_, _, e1, _, e2, _, e3, _, st) ->
      let st = colon_stmt env st in
      let e1 = for_expr env e1 in
      let e2 = for_expr env e2 in
      let e3 = for_expr env e3 in
      A.For (e1, e2, e3, st) :: acc
  | Switch (_, (_, e, x), scl) ->
      let e = expr env e in
      let scl = switch_case_list env scl in
      let line = PI.line_of_info x in
      let scl = add_case_comments env scl line in
      A.Switch (e, scl) :: acc
  | Foreach (_, _, e, _awaitTodo, _, fve_fao, _, cst) ->
      let e = expr env e in
      let fve, fao = foreach_pattern env fve_fao in
      let cst = colon_stmt env cst in
      A.Foreach (e, fve, fao, cst) :: acc
  | Break (_, e, _) -> A.Break (opt expr env e) :: acc
  | Continue (_, eopt, _) -> A.Continue (opt expr env eopt) :: acc
  | Return (_, eopt, _) -> A.Return (opt expr env eopt) :: acc
  | Throw (_, e, _) -> A.Throw (expr env e) :: acc
  | Try (_, (_, stl, _), cl, fl) ->
      let stl = List.fold_right (stmt_and_def env) stl [] in
      let cl = List.map (catch env) cl in
      let fl = List.map (finally env) fl in
      A.Try (stl, cl, fl) :: acc
  | Echo (_, el, _) ->
      A.Expr (A.Call (A.Id "echo", (List.map (expr env) (comma_list el))))
      :: acc
  | Globals (_, gvl, _) ->
      A.Global (List.map (global_var env) (comma_list gvl)) :: acc
  | StaticVars (_, svl, _) ->
      A.StaticVars (List.map (static_var env) (comma_list svl)) :: acc
  | InlineHtml (s, _) -> A.InlineHtml s :: acc
  | Use (_, fn, _) ->
      A.Expr (A.Call (A.Id "use", [A.String (use_filename env fn)])) :: acc
  | Unset (_, (_, lp, _), _e) ->
      let lp = comma_list lp in
      let lp = List.map (lvalue env) lp in
      A.Expr (A.Call (A.Id "unset", lp)) :: acc
  | Declare _ -> raise (TodoConstruct "Declare")
  | IfColon _ -> raise (ObsoleteConstruct "IfColo is old crazy stuff")
  | FuncDefNested fd ->
      A.FuncDef (func_def env fd) :: acc
  | ClassDefNested cd ->
      A.ClassDef (class_def env cd) :: acc


and foreach_pattern env pat =
  match pat with
  | ForeachVar (is_ref, IdVar (dv, _)) ->
      let v = A.Id (dname dv) in
      let v = if is_ref <> None then A.Ref v else v in
      v, None
  | ForeachArrow (ForeachVar (None, IdVar (dk, _)), _,
                  ForeachVar (is_ref, IdVar (dv, _))) ->
      let v = A.Id (dname dv) in
      let v = if is_ref <> None then A.Ref v else v in
      A.Id (dname dk), Some v
  | ForeachList (_, (_, vl, _)) ->
      let vl = comma_list vl in
      let vl = List.fold_right (list_assign env) vl [] in
      A.List vl, None
  | ForeachArrow (ForeachVar (None, IdVar (dk, _)), _,
                  ForeachList (_, (_, vl, _))) ->
      let vl = List.fold_right (list_assign env) (comma_list vl) [] in
      A.Id (dname dk), Some (A.List vl)
  | _ ->
      raise Common.Todo


and use_filename _env = function
  | UseDirect (s, _) -> s
  | UseParen (_, (s, _), _) -> s

and if_elseif env (_, (_, e, _), st) acc =
  let e = expr env e in
  let st = match stmt env st [] with [x] -> x | l -> A.Block l in
  A.If (e, st, acc)

and if_else env = function
  | None -> A.Noop
  | Some (_, (If _ as st)) ->
      (match stmt env st [] with
      | [x] -> x
      | _ -> assert false)
  | Some (_, st) ->
      let acc = [] in
      let line = last_line_of_stmt st in
      let acc = add_stmt_comments env acc line in
      let acc = stmt env st acc in
      A.Block acc

and stmt_and_def env st acc = stmt env st acc

and expr env = function
  | Sc sc -> scalar env sc
  | Binary (e1, (bop, _), e2) ->
      let e1 = expr env e1 in
      let e2 = expr env e2 in
      A.Binop (bop, e1, e2)
  | Unary ((uop, _), e) ->
      let e = expr env e in
      A.Unop (uop, e)
  | Assign (e1, _, e2) -> A.Assign (None, lvalue env e1, expr env e2)
  | AssignOp (lv, (op, _), e) ->
      let op = assignOp env op in
      A.Assign (Some op, lvalue env lv, expr env e)
  | Postfix (v, (fop, _)) -> A.Postfix (fop, lvalue env v)
  | Infix ((fop, _), v) -> A.Infix (fop, lvalue env v)
  | CondExpr (e1, _, None, _, e3) ->
      let e = expr env e1 in
      A.CondExpr (e, e, expr env e3);
  | CondExpr (e1, _, Some e2, _, e3) ->
      A.CondExpr (expr env e1, expr env e2, expr env e3)
  | AssignList (_, (_, la, _), _, e) ->
      let la = comma_list la in
      let la = List.fold_right (list_assign env) la [] in
      let e = expr env e in
      A.Assign (None, A.List la, e)
  | ArrayLong (_, (_, apl, _)) | ArrayShort (_, apl, _) ->
      let apl = comma_list apl in
      let apl = List.map (array_pair env) apl in
      A.ConsArray apl
  | Collection (n, (_, vel, _)) ->
      let s = name env n in
      let vel = comma_list vel in
      let vel = List.map (array_pair env) vel in
      A.Collection (s, vel)
  | New (_, cn, args) ->
      let args =
        match args with
        | None -> []
        | Some (_, cl, _) -> List.map (argument env) (comma_list cl)
      in
      let cn = class_name_reference env cn in
      A.New (cn, args)
  | Clone (_, e) ->
      A.Call (A.Id "clone", [expr env e])
  | AssignRef (e1, _, _, e2) ->
      let e1 = lvalue env e1 in
      let e2 = lvalue env e2 in
      A.Assign (None, e1, A.Ref e2)
  | AssignNew _ ->
      raise (TodoConstruct "expr AssignNew")
  | Cast ((c, _), e) -> A.Cast (c, expr env e)
  | CastUnset _ ->
      raise (TodoConstruct "expr CastUnset")
  | InstanceOf (e, _, cn) ->
      let e = expr env e in
      let cn = class_name_reference env cn in
      A.InstanceOf (e, cn)
  | Eval (_, (_, e, _)) -> A.Call (A.Id "eval", [expr env e])
  | Lambda ld ->
      A.Lambda (lambda_def env ld)
  | ShortLambda _ ->
    failwith "no support short lambda"

  | Exit (_, e) ->
      let arg =
        match e with
        | None
        | Some (_, None, _) -> []
        | Some (_, Some e, _) -> [expr env e]
      in
      A.Call (A.Id "exit", arg)
  | At _ -> A.Id "@" (* TODO look at this *)
  | Print (_, e) ->
      A.Call (A.Id "print", [expr env e])
  | BackQuote (_, _el, _) ->
      raise (TodoConstruct "BackQuote")
      (* A.Call (A.Id "exec", [A.Guil (List.map (encaps env) el)]) *)
  | Include (_, e) ->
      A.Call (A.Id "include", [expr env e])
  | IncludeOnce (_, e) ->
      A.Call (A.Id "include_once", [expr env e])
  | Require (_, e) ->
      A.Call (A.Id "require", [expr env e])
  | RequireOnce (_, e) ->
      A.Call (A.Id "require_once", [expr env e])
  | Empty (_, (_, lv, _)) ->
      A.Call (A.Id "empty", [lvalue env lv])
  | Isset (_, (_, lvl, _)) ->
      A.Call (A.Id "isset", List.map (lvalue env) (comma_list lvl))
  | XhpHtml xhp -> A.Xhp (xhp_html env xhp)
  | Yield (_, ArrayExpr e) -> A.Call (A.Id "yield", [expr env e])
  | Yield (_, _) -> failwith "yield $k => $v not supported"
  | YieldBreak _ -> A.Call (A.Id "yield", [A.Id "break"])
  | Await (_, e) -> A.Call (A.Id "await", [expr env e])
  (* only appear in sgrep pattern *)
  | SgrepExprDots _ -> raise Common.Impossible
  | ParenExpr (_, e, _) -> expr env e

  | Id n -> A.Id (name env n)

  | IdVar (dn, _scope) -> A.Id (dname dn)
  | This _tok -> A.This


  | Call (e, (_lp, args, _rp)) ->
      let e = expr env e in
      let args = comma_list args in
      let args = List.map (argument env) args in
      A.Call (e, args)
  | ObjGet (e1, _tok, e2) ->
      let e1 = expr env e1 in
      let e2 = expr env e2 in
      A.Obj_get (e1, e2)

  | ClassGet (e1, _tok, e2) ->
      let e1 = class_name_reference env e1 in
      let e2 = expr env e2 in
      A.Class_get (e1, e2)
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
  | Deref (_tok, e) ->
      A.Call (A.Id ("eval_var"), [expr env e])



and lambda_def env (l_use, ld) =
  let _, params, _ = ld.f_params in
  let params = comma_list_dots params in
  let _, body, _ = ld.f_body in
  { A.l_ref = ld.f_ref <> None;
    A.l_params = List.map (parameter env) params;
    A.l_use =
      (match l_use with
      | None -> []
      | Some (_tokuse, (_, lexical_vars, _)) ->
          let lexical_vars = comma_list lexical_vars in
          List.map (lexical_var env) lexical_vars
      );
    A.l_body = List.fold_right (stmt_and_def env) body [];
  }

and lexical_var _env = function
  | LexicalVar (is_ref, name) ->
      { A.p_type = None;
        A.p_ref = is_ref <> None;
        A.p_name = dname name;
        A.p_default = None;
        A.p_variadic = false;
      }

and scalar env = function
  | C cst -> constant env cst
  | Guil (_, el, _) -> A.Guil (List.map (encaps env) el)
  | HereDoc ({ PI.token = PI.OriginTok x; _ },
             el,
             { PI.token = PI.OriginTok y; _ }) ->
      A.HereDoc (x.PI.str, List.map (encaps env) el, y.PI.str)
  | HereDoc _ -> assert false

and constant env = function
  | Int (n, _) -> A.Int n
  | Double (n, _) -> A.Double n
  | String (s, _) -> A.String s
  | PreProcess (cpp, _) -> cpp_directive env cpp
  (* only appear when process xdebug coverage file *)
  | XdebugClass _ -> raise Common.Impossible
  | XdebugResource -> raise Common.Impossible

and cpp_directive _env = function
  | Line      -> A.Id "__LINE__"
  | File      -> A.Id "__FILE__"
  | ClassC    -> A.Id "__CLASS__"
  | MethodC   -> A.Id "__METHOD__"
  | FunctionC -> A.Id "__FUNCTION__"
  | Dir       -> A.Id "__DIRECTORY__"
  | TraitC    -> A.Id "__TRAIT__"
  | NamespaceC -> A.Id "__NAMESPACE__"


and hint_type env = function
  | Hint (q, _targsTODO) -> A.Hint (name env q)
  | HintArray _ -> A.HintArray
  | HintQuestion (_i, t) -> A.HintQuestion (hint_type env t)
  | HintTuple (v1)      -> A.HintTuple (List.map (hint_type env) (comma_list (unbrace v1)))
  | HintCallback v1 ->
    let args, ret =
      (fun (_, args, ret) ->
        (List.map (hint_type env) (comma_list_dots (unbrace args)),
         Common2.fmap (fun (_, _, t) -> hint_type env t) ret))
        (unbrace v1)
    in
    A.HintCallback (args, ret)
  | HintShape _ ->
    failwith "no support for shape"
  | HintTypeConst _ ->
    failwith "no support for type consts"
  | HintVariadic (_, hint) -> A.HintVariadic (Common.map_opt (hint_type env) hint)

and class_name_reference env a = expr env a

and lvalue env x = expr env x

and argument env = function
  | Arg e -> expr env e
  | ArgRef (_, e) -> A.Ref (lvalue env e)
  | ArgUnpack (_, e) -> A.Unpack (expr env e)

and class_def env c =
  let _, body, _ = c.c_body in
  let acc = List.fold_right (class_body env) body [] in
  let line = PI.line_of_info (info_of_ident c.c_name) in
  let acc = add_ce_comments env acc line in
  {
    A.c_type = class_type env c.c_type ;
    A.c_name = ident env c.c_name;
    A.c_extends =
    (match c.c_extends with
    | None -> []
    | Some (_, x) -> [hint_type env x]);
    A.c_implements =
    (match c.c_implements with None -> []
    | Some x -> interfaces env x);
    A.c_body = acc;
  }

and class_type _env = function
  | ClassRegular _  -> A.ClassRegular
  | ClassFinal _    -> A.ClassFinal
  | ClassAbstract _ -> A.ClassAbstract
  | ClassAbstractFinal _ -> A.ClassAbstractFinal
  | Interface _ -> A.Interface
  | Trait _ -> A.Trait
  | Enum _ ->
    failwith "enum not supported"

and interfaces env (_, intfs) =
  let intfs = comma_list intfs in
  List.map (hint_type env) intfs

and static_scalar_affect env (_, ss) = static_scalar env ss
and static_scalar env a = expr env a

and visibility env = function
  | [] -> (* TODO CHECK *) A.Novis
  | Public :: _ -> A.Public
  | Private :: _ -> A.Private
  | Protected :: _ -> A.Protected
  | (Static | Abstract | Final | Async) :: rl -> visibility env rl

and static env = function
  | [] -> false
  | Static :: _ -> true
  | _ :: rl -> static env rl

and abstract env = function
  | [] -> false
  | Abstract :: _ -> true
  | _ :: rl -> abstract env rl

and final env = function
  | [] -> false
  | Final :: _ -> true
  | _ :: rl -> final env rl

and class_body env st acc =
  match st with
  | Method md ->
      let x = match md.f_body with (_, _, x) -> x in
      let line = PI.line_of_info x in
      let acc  = add_ce_comments env acc line in
      let acc  = A.CEmethod (method_def env md) :: acc in
      acc
  | ClassVariables (m, ht, cvl, x) ->
      let line = PI.line_of_info x in
      let acc  = add_ce_comments env acc line in
      let cvl = comma_list cvl in
      let m =
        match m with
        | NoModifiers _ -> []
        | VModifiers l -> List.map (fun (x, _) -> x) l
      in
      let vis = visibility env m in
      let static = static env m in
      let abstract = abstract env m in
      let final = final env m in
      let ht = opt hint_type env ht in
      let vars = List.map (
        fun (n, ss) ->
          dname n, opt static_scalar_affect env ss
       ) cvl in
      let cv = {
        A.cv_final = final;
        A.cv_static = static;
        A.cv_abstract = abstract;
        A.cv_visibility = vis;
        A.cv_type = ht;
        A.cv_vars = vars;
        } in
      let acc = A.CEdef cv :: acc in
      let line = PI.line_of_info (info_of_dname (fst (List.hd cvl))) in
      let acc = add_ce_comments env acc line in
      acc
  | ClassConstants (abs_tok, _, _, cl, _) ->
      let consts = List.map (
        fun (n, ss) -> begin
          let name = ident env n in
          let value = opt static_scalar_affect env ss in
          {A.cst_name = name; cst_body = value}
        end
      ) (comma_list cl) in
      A.CEconst (abs_tok <> None, consts) :: acc
  | XhpDecl _ -> acc (* TODO xhp decl *)
  | UseTrait _ -> raise (TodoConstruct "UseTrait")
  | TraitConstraint _ -> raise (TodoConstruct "TraitConstraint")
  | ClassType _ -> raise (TodoConstruct "ConstType")

and method_def env m =
  let acc = [] in
  let body = m.f_body in
  let acc = method_body env m.f_type body acc in
  let line = PI.line_of_info (info_of_ident m.f_name) in
  let acc = add_stmt_comments env acc line in
  let _, params, _ = m.f_params in
  let params = comma_list_dots params in
  let mds = List.map (fun (x, _) -> x) m.f_modifiers in
  { A.m_visibility = visibility env mds;
    A.m_static = static env mds;
    A.m_final = final env mds;
    A.m_abstract = abstract env mds;
    A.m_ref = (match m.f_ref with None -> false | Some _ -> true);
    A.m_name = ident env m.f_name;
    A.m_params = List.map (parameter env) params ;
    A.m_return_type = Common2.fmap (fun (_, _, t) -> hint_type env t)
      m.f_return_type;
    A.m_body = acc;
  }

and method_body env _ftype x acc =
  let (_, stl, _) = x in
  List.fold_right (stmt_and_def env) stl acc

and parameter env p =
  { A.p_type = opt hint_type env p.p_type;
    A.p_ref = p.p_ref <> None;
    A.p_name = dname p.p_name;
    A.p_default = opt static_scalar_affect env p.p_default;
    A.p_variadic = p.p_variadic <> None;
  }

and func_def env f =
  let acc = [] in
  let _, body, end_ = f.f_body in
  let acc = add_stmt_comments env acc (PI.line_of_info end_) in
  let acc = List.fold_right (stmt_and_def env) body acc in
  let line = PI.line_of_info (info_of_ident f.f_name) in
  let acc = add_stmt_comments env acc line in
  let _, params, _ = f.f_params in
  let params = comma_list_dots params in
  { A.f_ref = f.f_ref <> None;
    A.f_name = ident env f.f_name;
    A.f_params = List.map (parameter env) params;
    A.f_return_type = Common2.fmap (fun (_, _, t) -> hint_type env t)
      f.f_return_type;
    A.f_body = acc;
  }

and xhp_html env = function
  | Xhp (tag, attrl, _, body, _) ->
      let tag, _ = tag in
      let attrl = List.map (xhp_attribute env) attrl in
      { A.xml_tag = tag;
        A.xml_attrs = attrl;
        A.xml_body = List.map (xhp_body env) body;
      }
  | XhpSingleton (tag, attrl, _) ->
      let tag, _ = tag in
      let attrl = List.map (xhp_attribute env) attrl in
      { A.xml_tag = tag;
        A.xml_attrs = attrl;
        A.xml_body = [];
      }

and xhp_attribute env ((n, _), _, v) =
  n, xhp_attr_value env v

and xhp_attr_value env = function
  | XhpAttrString (_, l, _) ->
      A.AttrString (List.map (encaps env) l)
  | XhpAttrExpr (_, e, _) ->
      A.AttrExpr (expr env e)
  | SgrepXhpAttrValueMvar _ -> raise Common.Impossible

and xhp_body env = function
  | XhpText (s, _) -> A.XhpText s
  | XhpExpr (_, e, _) -> A.XhpExpr (expr env e)
  | XhpNested xml -> A.XhpXml (xhp_html env xml)

and encaps env = function
  | EncapsString (s, _) -> A.EncapsString s
  | EncapsVar v -> A.EncapsVar (lvalue env v)
  | EncapsCurly (_, lv, _) -> A.EncapsCurly (lvalue env lv)
  | EncapsDollarCurly (_, lv, _) -> A.EncapsDollarCurly (lvalue env lv)
  | EncapsExpr (_, e, _) -> A.EncapsExpr (expr env e)

and array_pair env = function
  | ArrayExpr e -> A.Aval (expr env e)
  | ArrayRef (_, lv) -> A.Aval (A.Ref (lvalue env lv))
  | ArrayArrowExpr (e1, _, e2) -> A.Akval (expr env e1, expr env e2)
  | ArrayArrowRef (e1, _, _, lv) -> A.Akval (expr env e1, A.Ref (lvalue env lv))

and for_expr env el = List.map (expr env) (comma_list el)

and colon_stmt env = function
  | SingleStmt st -> stmt env st []
  | ColonStmt (_, stl, _, _) -> List.fold_right (stmt_and_def env) stl []
(*of tok (* : *) * stmt_and_def list * tok (* endxxx *) * tok (* ; *) *)

and switch_case_list env = function
  | CaseList (_, _, cl, _) ->
      List.fold_right (case env) cl []
  | CaseColonList _ -> raise (ObsoleteConstruct "CaseColonList")

and case env x acc =
  match x with
  | Case (_, e, _, []) -> A.Case (expr env e, []) :: acc
  | Case (_, e, _, stl) ->
      let acc =
        match stl with
        | [] -> acc
        | _ ->
          let line = last_line_of_stmt_and_defl stl in
          add_case_comments env acc line
      in
      let stl = List.fold_right (stmt_and_def env) stl [] in
      A.Case (expr env e, stl) :: acc
  | Default (_, _, stl) ->
      let acc =
        match stl with
        | [] -> acc
        | _ ->
          let line = last_line_of_stmt_and_defl stl in
          add_case_comments env acc line
      in
      let stl = List.fold_right (stmt_and_def env) stl [] in
      A.Default stl :: acc

and catch env (_, (_, (fq, dn), _), (_, stdl, _)) =
  let stdl = List.fold_right (stmt_and_def env) stdl [] in
  let fq = hint_type env fq in
  let dn = dname dn in
  fq, dn, stdl

and finally env (_, (_, stdl, _)) =
  let stdl = List.fold_right (stmt_and_def env) stdl [] in
  stdl

and static_var env (x, e) =
  dname x, opt static_scalar_affect env e

and list_assign env x acc =
  match x with
  | ListVar lv -> (lvalue env lv) :: acc
  | ListList (_, (_, la, _)) ->
      let la = comma_list la in
      let la = List.fold_right (list_assign env) la [] in
      A.List la :: acc
  | ListEmpty -> acc

and assignOp _env = function
  | AssignOpArith aop -> Arith aop
  | AssignConcat -> BinaryConcat

and global_var _env = function
  | GlobalVar dn -> A.Id (dname dn)
  | GlobalDollar _ -> raise (TodoConstruct "GlobalDollar")
  | GlobalDollarExpr _ -> raise (TodoConstruct "GlobalDollarExpr")

(*****************************************************************************)
(* Entry points used by prettyphp *)
(*****************************************************************************)

let program_with_comments tokens top_l =
  let env = env_of_tokens tokens in
  let acc = [] in
  let acc = List.fold_right (toplevel env) top_l acc in
  let acc = add_stmt_comments env acc 0 in
  acc

(*****************************************************************************)
(* Entry points used by spatch *)
(*****************************************************************************)

let toplevels tokens top_l =
  let env = env_of_tokens_for_spatch tokens in
  let acc = [] in
  let acc = List.fold_right (toplevel env) top_l acc in
  let acc = add_stmt_comments env acc 0 in
  acc

let class_stmts tokens xs =
  let env = env_of_tokens_for_spatch tokens in
  let acc = [] in
  let acc = List.fold_right (class_body env) xs acc in
  let acc = add_ce_comments env acc 0 in
  acc
