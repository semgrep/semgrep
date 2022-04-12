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
open Ast_ruby
module G = AST_generic
module H = AST_generic_helpers
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Ast_ruby to AST_generic.
 *
 * See AST_generic.ml for more information.
 *
 * alternatives:
 *  - starting from il_ruby.ml, which is good to get real stmts instead
 *    of stmt_as_expr, but expr may be too far from original expr
 *  - start from an ast_ruby_stmt.ml which is half between ast_ruby.ml and
 *    il_ruby.ml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let id x = x
let option = Option.map
let list = List.map
let bool = id
let string = id
let fake tok s = Parse_info.fake_info tok s
let unsafe_fake s = Parse_info.unsafe_fake_info s
let fb = G.fake_bracket
let nonbasic_entity id_or_e = { G.name = id_or_e; attrs = []; tparams = [] }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let info x = x
let tok v = info v

let wrap _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = info v2 in
  (v1, v2)

let bracket of_a (t1, x, t2) = (info t1, of_a x, info t2)
let ident x = wrap string x

let rec expr e =
  (match e with
  | Literal x -> literal x
  | Atom (tk, x) -> atom tk x
  | Id (id, kind) -> (
      match kind with
      | ID_Self -> G.IdSpecial (G.Self, snd id)
      | ID_Super -> G.IdSpecial (G.Super, snd id)
      | _ -> G.N (G.Id (ident id, G.empty_id_info ())))
  | ScopedId x ->
      let name = scope_resolution x in
      G.N name
  | Hash (_bool, xs) -> G.Container (G.Dict, bracket (list expr) xs)
  | Array (l, xs, r) ->
      let xs = args_to_exprs xs in
      G.Container (G.Array, (l, list expr xs, r))
  | Tuple xs -> G.Container (G.Tuple, G.fake_bracket (list expr xs))
  | Unary (op, e) ->
      let e = expr e in
      unary op e
  | Binop (e1, op, e2) ->
      let e1 = expr e1 in
      let e2 = expr e2 in
      binary op e1 e2
  | Ternary (e1, _t1, e2, _t2, e3) ->
      let e1 = expr e1 in
      let e2 = expr e2 in
      let e3 = expr e3 in
      G.Conditional (e1, e2, e3)
  | Call (e, xs, bopt) -> (
      let e = expr e in
      let lb, xs, rb = bracket (list argument) xs in
      let e_call = G.Call (e, (lb, xs, rb)) in
      match bopt with
      | None -> e_call
      | Some b ->
          (* There is a block to pass to `e`. We add an extra `Call` so that
           * `f(x) { |n| puts n }` is translated as `f(x)({ |n| puts n })`
           * rather than as `f(x, { |n| puts n })`. This way the pattern
           * `f(...)` will only match `f(x)` and not the entire block,
           * and `f($X)` will match `f(x)`. *)
          let barg = b |> expr |> G.arg in
          G.Call (G.e e_call, (lb, [ barg ], rb)))
  | DotAccess (e, t, m) -> (
      let e = expr e in
      match m with
      | MethodEllipsis t -> G.DotAccessEllipsis (e, t)
      | _ ->
          let fld =
            match method_name m with
            | Left id -> G.FN (G.Id (id, G.empty_id_info ()))
            | Right e -> G.FDynamic e
          in
          G.DotAccess (e, t, fld))
  | Splat (t, eopt) ->
      let xs = option expr eopt |> Option.to_list |> List.map G.arg in
      let special = G.IdSpecial (G.Spread, t) |> G.e in
      G.Call (special, fb xs)
  | CodeBlock ((t1, _, t2), params_opt, xs) ->
      let params =
        match params_opt with
        | None -> []
        | Some xs -> xs
      in
      let params = list formal_param params in
      let st = G.Block (t1, list_stmts xs, t2) |> G.s in
      let def =
        {
          G.fparams = params;
          frettype = None;
          fbody = G.FBStmt st;
          fkind = (G.LambdaKind, t1);
        }
      in
      G.Lambda def
  | Lambda (tok, params_opt, xs) ->
      let params =
        match params_opt with
        | None -> []
        | Some xs -> xs
      in
      let params = list formal_param params in
      let st = G.Block (tok, list_stmts xs, tok) |> G.s in
      let def =
        {
          G.fparams = params;
          frettype = None;
          fbody = G.FBStmt st;
          fkind = (G.LambdaKind, tok);
        }
      in
      G.Lambda def
  | S x ->
      let st = stmt x in
      let x = G.stmt_to_expr st in
      x.G.e
  | D x ->
      let st = definition x in
      let x = G.stmt_to_expr st in
      x.G.e
  | Ellipsis x ->
      let x = info x in
      G.Ellipsis x
  | DeepEllipsis x ->
      let x = bracket expr x in
      G.DeepEllipsis x
  | TypedMetavar (v1, v2, v3) ->
      let v1 = ident v1 in
      let v3 = type_ v3 in
      G.TypedMetavar (v1, v2, v3))
  |> G.e

and argument arg : G.argument =
  match arg with
  (* sgrep: equivalence between different keyword argument syntax *)
  | Arg (Binop (Atom (_tk, AtomSimple id), (Op_ASSOC, _v2), v3)) ->
      let e3 = expr v3 in
      G.ArgKwd (id, e3)
  | Arg e -> G.Arg (expr e)
  | ArgKwd (id, _tk, arg) ->
      let id = ident id in
      let arg = expr arg in
      G.ArgKwd (id, arg)

and formal_param = function
  | Formal_id id -> G.Param (G.param_of_id id)
  | Formal_amp (t, id) ->
      let param = G.Param (G.param_of_id id) in
      G.OtherParam (("Ref", t), [ G.Pa param ])
  | Formal_star (t, id) -> G.ParamRest (t, G.param_of_id id)
  | Formal_rest t ->
      let p =
        {
          G.pattrs = [];
          pinfo = G.empty_id_info ();
          ptype = None;
          pname = None;
          pdefault = None;
        }
      in
      G.ParamRest (t, p)
  | Formal_hash_splat (t, idopt) ->
      let p =
        match idopt with
        | Some id -> G.param_of_id id
        | None ->
            {
              G.pattrs = [];
              pinfo = G.empty_id_info ();
              ptype = None;
              pname = None;
              pdefault = None;
            }
      in
      G.ParamHashSplat (t, p)
  | Formal_default (id, _t, e) ->
      let e = expr e in
      let p = { (G.param_of_id id) with G.pdefault = Some e } in
      G.Param p
  (* TODO? diff with Formal_default? *)
  | Formal_kwd (id, _t, eopt) ->
      let eopt = option expr eopt in
      let p =
        match eopt with
        | None -> G.param_of_id id
        | Some e -> { (G.param_of_id id) with G.pdefault = Some e }
      in
      G.Param p
  | Formal_tuple (_t1, xs, _t2) ->
      let xs = list formal_param_pattern xs in
      let pat = G.PatTuple (G.fake_bracket xs) in
      G.ParamPattern pat
  | ParamEllipsis tok -> G.ParamEllipsis tok

and formal_param_pattern = function
  | Formal_id id -> G.PatId (id, G.empty_id_info ())
  | Formal_tuple (_t1, xs, _t2) ->
      let xs = list formal_param_pattern xs in
      G.PatTuple (G.fake_bracket xs)
  | ( Formal_amp _ | Formal_star _ | Formal_rest _ | Formal_default _
    | Formal_hash_splat _ | Formal_kwd _ | ParamEllipsis _ ) as x ->
      let x = formal_param x in
      G.OtherPat (("ParamPattern", PI.unsafe_fake_info ""), [ G.Pa x ])

and scope_resolution x : G.name =
  match x with
  | TopScope (t, v) ->
      let id = variable v in
      IdQualified
        {
          G.name_last = (id, None);
          name_middle = None;
          name_top = Some t;
          name_info = G.empty_id_info ();
        }
  | Scope (e, t, v_or_m) ->
      let id = variable_or_method_name v_or_m in
      (* TODO: use an 'expr_for_scope' instead of 'expr' below, because
       * the expression itself could be another Scope ...
       * in which case we could generate a QDots instead of a QEXpr
       *)
      let e = expr e in
      let qualif = G.QExpr (e, t) in
      IdQualified
        {
          G.name_last = (id, None);
          name_middle = Some qualif;
          name_top = None;
          name_info = G.empty_id_info ();
        }

and variable (id, _kind) = ident id

and variable_or_method_name = function
  | SV v -> variable v
  | SM m -> (
      match method_name m with
      | Left id -> id
      | Right _ -> failwith "TODO: variable_or_method_name")

and method_name (mn : method_name) : (G.ident, G.expr) Common.either =
  match mn with
  | MethodId v -> Left (variable v)
  | MethodIdAssign (id, teq, id_kind) ->
      let s, t = variable (id, id_kind) in
      Left (s ^ "=", PI.combine_infos t [ teq ])
  | MethodUOperator (_, t)
  | MethodOperator (_, t) ->
      Left (PI.str_of_info t, t)
  | MethodSpecialCall (l, (), _r) ->
      let special = ident ("call", l) in
      Left special
  | MethodAtom (_tcolon, x) -> (
      (* todo? add ":" in name? *)
      match x with
      | AtomSimple x -> Left x
      | AtomFromString (l, xs, r) -> (
          match xs with
          | [ StrChars (s, t2) ] ->
              let t = PI.combine_infos l [ t2; r ] in
              Left (s, t)
          | _ -> Right (string_contents_list (l, xs, r) |> G.e)))
  (* sgrep-ext: this should be covered in the caller *)
  | MethodEllipsis t -> raise (Parse_info.Parsing_error t)

and string_contents_list (t1, xs, t2) : G.expr_kind =
  let xs = list (string_contents t1) xs in
  G.Call
    ( G.IdSpecial (G.ConcatString G.InterpolatedConcat, t1) |> G.e,
      (t1, xs |> List.map (fun e -> G.Arg e), t2) )

and string_contents tok = function
  | StrChars s -> G.L (G.String s) |> G.e
  | StrExpr (l, e, r) ->
      G.Call
        ( G.IdSpecial (G.InterpolatedElement, tok) |> G.e,
          (l, [ G.Arg (expr e) ], r) )
      |> G.e

and method_name_to_any mn =
  match method_name mn with
  | Left id -> G.I id
  | Right e -> G.E e

and binary_msg = function
  | Op_PLUS -> G.Plus
  | Op_MINUS -> G.Minus
  | Op_TIMES -> G.Mult
  | Op_REM -> G.Mod
  | Op_DIV -> G.Div
  | Op_LSHIFT -> G.LSL
  | Op_RSHIFT -> G.LSR
  | Op_BAND -> G.BitAnd
  | Op_BOR -> G.BitOr
  | Op_XOR -> G.BitXor
  | Op_POW -> G.Pow
  | Op_CMP -> G.Cmp
  | Op_EQ -> G.Eq
  | Op_EQQ -> G.PhysEq (* abuse PhysEq here, maybe not semantic*)
  | Op_NEQ -> G.NotEq
  | Op_GEQ -> G.GtE
  | Op_LEQ -> G.LtE
  | Op_LT -> G.Lt
  | Op_GT -> G.Gt
  | Op_MATCH -> G.RegexpMatch
  | Op_NMATCH -> G.NotMatch
  | Op_DOT2 -> G.Range
  (* never in Binop, only in DotAccess or MethodDef *)
  | Op_AREF
  | Op_ASET ->
      raise Impossible

and binary (op, t) e1 e2 =
  match op with
  | B msg ->
      let op = binary_msg msg in
      G.Call (G.IdSpecial (G.Op op, t) |> G.e, fb [ G.Arg e1; G.Arg e2 ])
  | Op_kAND
  | Op_AND ->
      G.Call (G.IdSpecial (G.Op G.And, t) |> G.e, fb [ G.Arg e1; G.Arg e2 ])
  | Op_kOR
  | Op_OR ->
      G.Call (G.IdSpecial (G.Op G.Or, t) |> G.e, fb [ G.Arg e1; G.Arg e2 ])
  | Op_ASSIGN -> G.Assign (e1, t, e2)
  | Op_OP_ASGN op ->
      let op =
        match op with
        | B msg -> binary_msg msg
        | Op_AND -> G.And
        | Op_OR -> G.Or
        (* see lexer_ruby.mll code for T_OP_ASGN *)
        | _ -> raise Impossible
      in
      G.AssignOp (e1, (op, t), e2)
  | Op_ASSOC -> (G.keyval e1 t e2).e
  | Op_DOT3 ->
      (* coupling: make sure to check for the string in generic_vs_generic *)
      G.Call (G.IdSpecial (G.Op G.Range, t) |> G.e, fb [ G.Arg e1; G.Arg e2 ])

and unary (op, t) e =
  match op with
  | U msg ->
      let op =
        match msg with
        | Op_UMinus -> G.Minus
        | Op_UPlus -> G.Plus
        | Op_UBang -> G.Not
        | Op_UTilde -> G.BitNot
      in
      G.Call (G.IdSpecial (G.Op op, t) |> G.e, fb [ G.Arg e ])
  | Op_UNot -> G.Call (G.IdSpecial (G.Op G.Not, t) |> G.e, fb [ G.Arg e ])
  | Op_DefinedQuestion ->
      G.Call (G.IdSpecial (G.Defined, t) |> G.e, fb [ G.Arg e ])
  | Op_UStarStar -> G.Call (G.IdSpecial (G.HashSplat, t) |> G.e, fb [ G.Arg e ])
  (* should be only in arguments, to pass procs. I abuse Ref for now *)
  | Op_UAmper -> G.Ref (t, e)

and atom tcolon x =
  match x with
  | AtomSimple x -> G.L (G.Atom (tcolon, x))
  | AtomFromString (l, xs, r) -> (
      match xs with
      | [ StrChars (s, t2) ] ->
          let t = PI.combine_infos l [ t2; r ] in
          G.L (G.Atom (tcolon, (s, t)))
      | _ -> string_contents_list (l, xs, r))

and literal x =
  match x with
  | Bool x -> G.L (G.Bool (wrap bool x))
  (* TODO: put real numbers here *)
  | Num (s, x) ->
      let i = int_of_string_opt s in
      G.L (G.Int (i, tok x))
  | Float (s, x) ->
      let f = float_of_string_opt s in
      G.L (G.Float (f, tok x))
  | Complex x -> G.L (G.Imag (wrap string x))
  | Rational ((s, t1), t2) ->
      let t = PI.combine_infos t1 [ t2 ] in
      G.L (G.Ratio (s, t))
  | Char x -> G.L (G.Char (wrap string x))
  | Nil t -> G.L (G.Null (tok t))
  | String skind -> (
      match skind with
      | Single x -> G.L (G.String x)
      | Double (l, [], r) ->
          let t = PI.combine_infos l [ r ] in
          G.L (G.String ("", t))
      | Double (l, [ StrChars (s, t2) ], r) ->
          let t = PI.combine_infos l [ t2; r ] in
          G.L (G.String (s, t))
      (* TODO: generate interpolation Special *)
      | Double xs -> string_contents_list xs
      | Tick (l, xs, r) ->
          G.OtherExpr
            (("Subshell", l), [ G.E (string_contents_list (l, xs, r) |> G.e) ]))
  | Regexp ((l, xs, r), opt) ->
      let rec f strs toks = function
        | [ StrChars (s, t) ] ->
            let str = String.concat "" (s :: strs) in
            let tok = PI.combine_infos t toks in
            G.L (G.Regexp ((l, (str, tok), r), opt))
        | StrChars (s, t) :: tl -> f (s :: strs) (t :: toks) tl
        | StrExpr _ :: _
        | [] ->
            (* TODO *)
            string_contents_list (l, xs, r)
      in
      f [] [] (List.rev xs)

and expr_as_stmt = function
  | S x -> stmt x
  | D x -> definition x
  | e -> (
      let e = expr e in
      match e.G.e with
      (* a single name on its own line is probably an hidden fun call,
       * unless it's a metavariable
       *)
      | G.N (G.Id ((s, _), _)) ->
          if AST_generic_.is_metavar_name s then G.exprstmt e
          else
            let call = G.Call (e, fb []) |> G.e in
            G.exprstmt call
      | _ -> G.exprstmt e)

and stmt st =
  match st with
  | Block (t1, xs, t2) ->
      let xs = list_stmts xs in
      G.Block (t1, xs, t2) |> G.s
  | If (t, e, st, elseopt) ->
      let e = expr e in
      let st = list_stmt1 st in
      let elseopt = option_tok_stmts elseopt in
      G.If (t, G.Cond e, st, elseopt) |> G.s
  | While (t, _bool, e, st) ->
      let e = expr e in
      let st = list_stmt1 st in
      G.While (t, G.Cond e, st) |> G.s
  | Until (t, _bool, e, st) ->
      let e = expr e in
      let special = G.IdSpecial (G.Op G.Not, t) |> G.e in
      let e = G.Call (special, fb [ G.Arg e ]) |> G.e in
      let st = list_stmt1 st in
      G.While (t, G.Cond e, st) |> G.s
  | Unless (t, e, st, elseopt) ->
      let e = expr e in
      let st = list_stmt1 st in
      let elseopt = option_tok_stmts elseopt in
      let special = G.IdSpecial (G.Op G.Not, t) |> G.e in
      let e = G.Call (special, fb [ G.Arg e ]) |> G.e in
      let st1 =
        match elseopt with
        | None -> G.Block (fb []) |> G.s
        | Some st -> st
      in
      G.If (t, G.Cond e, st1, Some st) |> G.s
  | For (t1, pat, t2, e, st) ->
      let pat = pattern pat in
      let e = expr e in
      let st = list_stmt1 st in
      let header = G.ForEach (pat, t2, e) in
      G.For (t1, header, st) |> G.s
  | Return (t, es) ->
      let eopt = args_to_eopt es in
      G.Return (t, eopt, G.sc) |> G.s
  | Yield (t, es) ->
      let eopt = args_to_eopt es in
      G.exprstmt (G.Yield (t, eopt, false) |> G.e)
  | Break (t, es) ->
      let lbl = args_to_label_ident es in
      G.Break (t, lbl, G.sc) |> G.s
  | Next (t, es) ->
      let lbl = args_to_label_ident es in
      G.Continue (t, lbl, G.sc) |> G.s
  | Redo (t, es) ->
      let lbl = args_to_label_ident es in
      G.OtherStmt (G.OS_Redo, [ G.Tk t; G.Lbli lbl ]) |> G.s
  | Retry (t, es) ->
      let lbl = args_to_label_ident es in
      G.OtherStmt (G.OS_Retry, [ G.Tk t; G.Lbli lbl ]) |> G.s
  | Case (t, { case_guard = eopt; case_whens = whens; case_else = stopt }) ->
      let eopt = option expr eopt in
      let whens = list when_clause whens in
      let default =
        match stopt with
        | None -> []
        | Some (t, sts) ->
            let st = list_stmt1 sts in
            [ ([ G.Default t ], st) ]
      in
      let condopt =
        match eopt with
        | None -> None
        | Some e -> Some (G.Cond e)
      in
      G.Switch
        (t, condopt, whens @ default |> List.map (fun x -> G.CasesAndBody x))
      |> G.s
  | ExnBlock b -> body_exn b

and when_clause (t, pats, sts) =
  let pats = list pattern pats in
  let st = list_stmt1 sts in
  (pats |> List.map (fun pat -> G.Case (t, pat)), st)

and args_to_label_ident xs = xs |> args_to_exprs |> exprs_to_label_ident

and exprs_to_label_ident = function
  | [] -> G.LNone
  (* TODO: check if x is an Int or label? *)
  | [ x ] ->
      let x = expr x in
      G.LDynamic x
  | xs ->
      let xs = list expr xs in
      G.LDynamic (G.Container (G.Tuple, G.fake_bracket xs) |> G.e)

and args_to_eopt xs = xs |> args_to_exprs |> exprs_to_eopt

and exprs_to_eopt = function
  | [] -> None
  | [ x ] -> Some (expr x)
  | xs ->
      let xs = list expr xs in
      Some (G.Container (G.Tuple, G.fake_bracket xs) |> G.e)

and pattern pat =
  let e = expr pat in
  H.expr_to_pattern e

and type_ e =
  let e = expr e in
  H.expr_to_type e

and option_tok_stmts x =
  match x with
  | None -> None
  | Some (_t, xs) -> Some (list_stmt1 xs)

and definition def =
  match def with
  | MethodDef (t, kind, params, body) -> (
      let params = list formal_param params in
      let body = body_exn body in
      let funcdef =
        {
          G.fparams = params;
          frettype = None;
          fbody = G.FBStmt body;
          fkind = (G.Method, t);
        }
      in
      match kind with
      | M mn -> (
          match method_name mn with
          | Left id ->
              let ent = G.basic_entity id in
              G.DefStmt (ent, G.FuncDef funcdef) |> G.s
          | Right e ->
              let ent = G.basic_entity ("", fake t "") in
              G.OtherStmt (G.OS_Todo, [ G.E e; G.Def (ent, G.FuncDef funcdef) ])
              |> G.s)
      | SingletonM e ->
          let e = expr e in
          let ent = G.basic_entity ("", fake t "") in
          G.OtherStmt (G.OS_Todo, [ G.E e; G.Def (ent, G.FuncDef funcdef) ])
          |> G.s)
  | ClassDef (t, kind, body) -> (
      let body = body_exn body in
      match kind with
      | C (name, inheritance_opt) ->
          let extends =
            match inheritance_opt with
            | None -> []
            | Some (_t2, e) ->
                let e = expr e in
                [ H.expr_to_class_parent e ]
          in
          let ent =
            match name with
            | NameConstant id -> G.basic_entity id
            | NameScope x ->
                let name = scope_resolution x in
                nonbasic_entity (G.EN name)
          in
          let def =
            {
              G.ckind = (G.Class, t);
              cextends = extends;
              (* TODO: this is done by special include/require builtins *)
              cimplements = [];
              cmixins = [];
              cparams = [];
              cbody = fb [ G.F body ];
            }
          in
          G.DefStmt (ent, G.ClassDef def) |> G.s
      | SingletonC (t, e) ->
          let e = expr e in
          G.OtherStmt (G.OS_Todo, [ G.Tk t; G.E e; G.S body ]) |> G.s)
  | ModuleDef (_t, name, body) ->
      let body = body_exn body in
      let ent =
        match name with
        | NameConstant id -> G.basic_entity id
        | NameScope x ->
            let name = scope_resolution x in
            nonbasic_entity (G.EN name)
      in
      let mkind = G.ModuleStruct (None, [ body ]) in
      let def = { G.mbody = mkind } in
      G.DefStmt (ent, G.ModuleDef def) |> G.s
  | BeginBlock (_t, (t1, st, t2)) ->
      let st = list_stmts st in
      let st = G.Block (t1, st, t2) |> G.s in
      G.OtherStmtWithStmt (G.OSWS_BEGIN, [], st) |> G.s
  | EndBlock (_t, (t1, st, t2)) ->
      let st = list_stmts st in
      let st = G.Block (t1, st, t2) |> G.s in
      G.OtherStmtWithStmt (G.OSWS_END, [], st) |> G.s
  | Alias (t, mn1, mn2) ->
      let mn1 = method_name mn1 in
      let name_or_dyn =
        match mn1 with
        | Left id -> G.EN (G.Id (id, G.empty_id_info ()))
        | Right e -> G.EDynamic e
      in
      let ent = { G.name = name_or_dyn; attrs = []; tparams = [] } in
      let def = G.OtherDef (("Alias", t), [ method_name_to_any mn2 ]) in
      G.DefStmt (ent, def) |> G.s
  | Undef (t, mns) ->
      let mns = list method_name_to_any mns in
      G.DirectiveStmt (G.OtherDirective (("Undef", t), mns) |> G.d) |> G.s

and body_exn x =
  match x with
  | { body_exprs = xs; rescue_exprs = []; ensure_expr = None; else_expr = None }
    ->
      list_stmt1 xs
  | {
   body_exprs = xs;
   rescue_exprs = catches;
   ensure_expr = finally_opt;
   else_expr = elseopt;
  } -> (
      let body = list_stmt1 xs in
      let catches = list rescue_clause catches in
      let finally_opt =
        match finally_opt with
        | None -> None
        | Some (t, sts) ->
            let st = list_stmt1 sts in
            Some (t, st)
      in
      match elseopt with
      | None ->
          let try_ =
            G.Try (unsafe_fake "try", body, catches, finally_opt) |> G.s
          in
          G.Block (fb [ try_ ]) |> G.s
      | Some (t, sts) ->
          let st = list_stmt1 sts in
          let try_ = G.Try (fake t "try", body, catches, finally_opt) |> G.s in
          let st = G.Block (fb [ try_; st ]) |> G.s in
          G.OtherStmtWithStmt (G.OSWS_Else_in_try, [], st) |> G.s)

and rescue_clause (t, exns, exnvaropt, sts) : G.catch =
  let st = list_stmt1 sts in
  let exns = list exception_ exns in
  match (exns, exnvaropt) with
  | [], None -> (t, G.CatchPattern (G.PatUnderscore t), st)
  | [], Some (t, lhs) ->
      let e = expr lhs in
      (t, G.CatchPattern (G.OtherPat (("Rescue", t), [ G.E e ])), st)
  | x :: xs, None ->
      let disjs = List.fold_left (fun e acc -> G.PatDisj (e, acc)) x xs in
      (t, G.CatchPattern disjs, st)
  | x :: xs, Some (t, lhs) ->
      let disjs = List.fold_left (fun e acc -> G.PatDisj (e, acc)) x xs in
      let e = expr lhs in
      ( t,
        G.CatchPattern (G.OtherPat (("RescueDisj", t), [ G.E e; G.P disjs ])),
        st )

and exception_ e : G.pattern =
  let t = type_ e in
  G.OtherPat (("Exn", unsafe_fake ""), [ G.T t ])

(* similar to Python_to_generic.list_stmt1 *)
and list_stmt1 xs =
  match list expr_as_stmt xs with
  (* bugfix: We do not want actually to optimize and remove the
   * intermediate Block because otherwise sgrep will not work
   * correctly with a list of stmt.
   *
   * old: | [e] -> e
   *
   * For example
   * if $E:
   *   ...
   *   foo()
   *
   * will not match code like
   *
   * if True:
   *   foo()
   *
   * because above we have a Block ([Ellipsis; foo()] and down we would
   * have just (foo()). We do want Block ([foo()]].
   *
   * Unless the body is actually just a metavar, in which case we probably
   * want to match a list of stmts, as in
   *
   *  if $E:
   *    $S
   *
   * in which case we remove the G.Block around it.
   * hacky ...
   *)
  | [ ({ G.s = G.ExprStmt ({ e = G.N (G.Id ((s, _), _)); _ }, _); _ } as x) ]
    when AST_generic_.is_metavar_name s ->
      x
  | xs -> G.Block (fb xs) |> G.s

(* was called stmts, but you should either use list_stmt1 or list_stmts *)
and list_stmts xs = list expr_as_stmt xs

let program xs = list_stmts xs

let any x =
  match x with
  | E x -> (
      match x with
      | S x -> G.S (stmt x)
      | D x -> G.S (definition x)
      | _ -> G.E (expr x))
  | S2 x -> G.S (stmt x)
  | Ss xs -> G.Ss (list_stmts xs)
  | Pr xs -> G.Ss (list_stmts xs)
  (* sgrep_spatch_pattern just generate E/S2/Ss *)
  | _ -> raise Impossible
