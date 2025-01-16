(* Yoann Padioleau
 *
 * Copyright (C) 2019-2022 r2c
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
open AST_python
module G = AST_generic
module H = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* AST_python to AST_generic.
 *
 * See AST_generic.ml for more information.
 *
 * TODO:
 *  - intercept Call to eval and transform in special Eval?
 *  - call to (list stmt) should be converted to list_stmt
 *    to avoid intermediates Block
 *    (should use embedded-Semgrep-rule idea of rcoh!)
 *  - transform more Assign in VarDef, e.g., also local variables
 *    (but take care if same var defined first in different branch,
 *     which one should be a VarDef?)
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* We use the environment below for transforming certain Assign in VarDef.
 * Indeed, Python does not have a special construct (e.g., 'let' in OCaml)
 * to declare variables, and instead abuse Assign to do so (which then requires
 * to have constructs like 'nonlocal' and 'global' to explicitely say you
 * want to reuse an enclosing variable and not declare a new one, argh).
 *
 * To avoid transforming wrongly some actual assignments, we need to detect
 * all introduced entities (e.g, parameters, imported entities, patterns,
 * exn vars, iterators, 'nonlocal', 'global'), so when we see 'x = v',
 * we check whether this x was previously declared. If not, then
 * it's probably a VarDef.
 *
 * Note that it does not really matter for semgrep, which anyway has some
 * vardef_to_assign magic. However, this is useful for codegraph to
 * correctly detect all global entities.
 *)
type env = {
  context : context;
  assign_to_vardef : bool;
  (* the different Python scopes *)
  mutable imported : string list;
  (* for now we just transform the toplevels Assign *)
  mutable current_scope : string list;
  (* TODO: put parameters here? or use separate fld? also add
   * iterators, exn handler, and just push into it for each
   * new "block" scope *)
  from_parent_scope : string list;
      (* TODO? parameters: string list *)
      (* TODO? handle also iterator, exn? *)
}

and context = InSourceToplevel | InPattern | InClass | InFunctionOrMethod

let empty_env ~assign_to_vardef context =
  {
    context;
    imported = [];
    current_scope = [];
    from_parent_scope = [];
    assign_to_vardef;
  }

(* lookup *)
let is_in_scope env s =
  List.mem s env.imported
  || List.mem s env.current_scope
  || List.mem s env.from_parent_scope

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let id x = x
let option = Option.map
let list = List_.map
let string = id
let bool = id
let fake tok s = Tok.fake_tok tok s
let unsafe_fake s = Tok.unsafe_fake_tok s
let fb = Tok.unsafe_fake_bracket

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let info x = x

let wrap _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = info v2 in
  (v1, v2)

let bracket of_a (t1, x, t2) = (info t1, of_a x, info t2)
let name _env v = wrap string v
let dotted_name env v = list (name env) v

let module_name env (v1, dots) =
  let v1 = dotted_name env v1 in
  match dots with
  | None -> G.DottedName v1
  (* transforming '.foo.bar' into ["."; "foo"; "bar"] *)
  | Some toks -> (
      (* This code relies on the parser enforcing that (..+) isn't
         allowed after the first module name. IE multiple dots will
         always be the first n elements. Should we assert this is true
         when debugging so this will explode otherwise? *)
      let count =
        toks
        |> List_.map Tok.content_of_tok
        |> String.concat "" |> String.length
      in
      let tok = List_.hd_exn "unexpected empty list" toks in
      let prefixes =
        match count with
        | 1 -> [ (".", tok) ]
        | 2 -> [ ("..", tok) ]
        | n -> Common2.repeat ("..", tok) (n - 1)
      in
      (* TODO: The parser should really just always handback the dotted
         name list with these relative names ("." and ".."). *)
      (* This is gross, but there are empty strings in some dotted names
         that where getting removed via string concatination earlier. *)
      match v1 with
      | [ ("", _) ] -> G.DottedName prefixes
      | _ -> G.DottedName (prefixes @ v1))

let rec expr env (x : expr) =
  match x with
  | DotAccessEllipsis (v1, v2) ->
      let v1 = expr env v1 in
      G.DotAccessEllipsis (v1, v2) |> G.e
  | Ellipsis x ->
      let x = info x in
      G.Ellipsis x |> G.e
  | DeepEllipsis x ->
      let x = bracket (expr env) x in
      G.DeepEllipsis x |> G.e
  | Literal l -> G.L (literal env l) |> G.e
  | EncodedStr (v1, pre) ->
      let v1 = wrap string v1 in
      (* bugfix: do not reuse the same tok! otherwise in semgrep
       * if a metavar is bound to an encoded string (e.g., r'foo'), and
       * the metavar is used in the message, r'foo' will be displayed
       * three times.
       * todo: the right fix is to have EncodedStr of string wrap * string wrap
       *)
      G.Call
        ( G.IdSpecial (G.EncodedString pre, fake (snd v1) "") |> G.e,
          fb [ G.Arg (G.L (G.String (fb v1)) |> G.e) ] )
      |> G.e
  | InterpolatedString (v1, xs, v3) ->
      G.Call
        (* Python interpolated strings are always of the form f"...", we need to support arbitary strings in the generic AST because Scala has custom interpolators *)
        ( G.IdSpecial (G.ConcatString (G.FString "f"), unsafe_fake "concat")
          |> G.e,
          ( v1,
            xs
            |> List_.map (fun x ->
                   let x = expr env x in
                   G.Arg x),
            v3 ) )
      |> G.e
  | ConcatenatedString xs ->
      G.Call
        ( G.IdSpecial (G.ConcatString G.SequenceConcat, unsafe_fake "concat")
          |> G.e,
          fb
            (xs
            |> List_.map (fun x ->
                   let x = expr env x in
                   G.Arg x)) )
      |> G.e
  | TypedMetavar (v1, v2, v3) ->
      let v1 = name env v1 in
      let v3 = type_ env v3 in
      G.TypedMetavar (v1, v2, v3) |> G.e
  | ExprStar v1 ->
      let v1 = expr env v1 in
      G.Call
        (G.IdSpecial (G.Spread, unsafe_fake "spread") |> G.e, fb [ G.arg v1 ])
      |> G.e
  (* In theory one can use any name for "self" in Python; it just has
   * to be the first parameter of a method. In practice, it's
   * always "self" and it's useful to convert it to 'This' here
   * to simplify further analysis (e.g., naming).
   * TODO? we could use 'env' to store that we are in a method
   * and what is the first parameter of a method?
   *)
  | Name (("self", t), _expr_ctx) -> G.IdSpecial (G.Self, t) |> G.e
  (* new: We have now included `cls`, as another canonical name
     for class self-reference
     it behaves slightly differently than `self`, as it indicates the
     type of the class, not the instance of the class, so let's inject
     it into a separate variant
  *)
  | Name (("cls", t), _expr_ctx) -> G.IdSpecial (G.Cls, t) |> G.e
  | Name (v1, _expr_ctx) ->
      let v1 = name env v1 in
      G.N (G.Id (v1, G.empty_id_info ())) |> G.e
  | Tuple (CompList v1, _expr_ctx) ->
      let v1 = bracket (list (expr env)) v1 in
      G.Container (G.Tuple, v1) |> G.e
  | Tuple (CompForIf (l, (v1, v2), r), _expr_ctx) ->
      let e1 = comprehension env expr v1 v2 in
      G.Comprehension (G.Tuple, (l, e1, r)) |> G.e
  | List (CompList v1, _expr_ctx) ->
      let v1 = bracket (list (expr env)) v1 in
      G.Container (G.List, v1) |> G.e
  | List (CompForIf (l, (v1, v2), r), _expr_ctx) ->
      let e1 = comprehension env expr v1 v2 in
      G.Comprehension (G.List, (l, e1, r)) |> G.e
  | Subscript (v1, v2, _expr_ctx) ->
      (let e = expr env v1 in
       match v2 with
       | l1, [ x ], l2 -> slice1 env e (l1, x, l2)
       | l1, xs, _ ->
           let xs = list (slice env e) xs in
           G.OtherExpr (("Slices", l1), xs |> List_.map (fun x -> G.E x)))
      |> G.e
  | Attribute (v1, t, v2, _expr_ctx) ->
      let v1 = expr env v1 and t = info t and v2 = name env v2 in
      G.DotAccess (v1, t, G.FN (G.Id (v2, G.empty_id_info ()))) |> G.e
  (* this shouldn't occur in expr position but let's handle it gracefully *)
  | ConstrainedType (v1, v2, v3) ->
      let v1 = type_ env v1 and v3 = expr env v3 in
      G.OtherExpr (("ConstrainedType", v2), [ G.T v1; G.E v3 ]) |> G.e
  | DictOrSet (CompList (t1, v, t2)) ->
      let v' = list (dictorset_elt env) v in
      let kind =
        if
          v
          |> List.for_all (function
               | KeyVal _
               (* semgrep-ext: ... should not count *)
               | Key (Ellipsis _) ->
                   true
               | _ -> false)
          || v =*= []
        then G.Dict
        else G.Set
      in
      G.Container (kind, (t1, v', t2)) |> G.e
  | DictOrSet (CompForIf (l, (v1, v2), r)) ->
      let e1 = comprehension2 env dictorset_elt v1 v2 in
      G.Comprehension (G.Dict, (l, e1, r)) |> G.e
  | BoolOp ((v1, tok), v2) ->
      let v1 = boolop v1 and v2 = list (expr env) v2 in
      G.Call (G.IdSpecial (G.Op v1, tok) |> G.e, fb (v2 |> List_.map G.arg))
      |> G.e
  | BinOp (v1, (v2, tok), v3) ->
      let v1 = expr env v1 and v2 = operator v2 and v3 = expr env v3 in
      G.Call
        (G.IdSpecial (G.Op v2, tok) |> G.e, fb ([ v1; v3 ] |> List_.map G.arg))
      |> G.e
  | UnaryOp ((v1, tok), v2) ->
      let op = unaryop v1 and v2 = expr env v2 in
      G.opcall (op, tok) [ v2 ]
  | Compare (v1, v2, v3) -> (
      let v1 = expr env v1 and v2 = list cmpop v2 and v3 = list (expr env) v3 in
      match (v2, v3) with
      | [ (op, tok) ], [ e ] ->
          G.Call
            ( G.IdSpecial (G.Op op, tok) |> G.e,
              fb ([ v1; e ] |> List_.map G.arg) )
          |> G.e
      | _ ->
          let anyops =
            v2
            |> List_.map (function arith, tok ->
                   G.E (G.IdSpecial (G.Op arith, tok) |> G.e))
          in
          let anys = anyops @ (v3 |> List_.map (fun e -> G.E e)) in
          G.OtherExpr (("CmpOps", unsafe_fake ""), anys) |> G.e)
  | Call (v1, v2) ->
      let v1 = expr env v1 in
      let v2 = bracket (list (argument env)) v2 in
      G.Call (v1, v2) |> G.e
  | Lambda (t0, v1, _t2, v2) ->
      let v1 = parameters env v1 and v2 = expr env v2 in
      G.Lambda
        {
          G.fparams = fb v1;
          fbody = G.FBExpr v2;
          frettype = None;
          fkind = (G.LambdaKind, t0);
        }
      |> G.e
  | IfExp (v1, v2, v3) ->
      let v1 = expr env v1 and v2 = expr env v2 and v3 = expr env v3 in
      G.Conditional (v1, v2, v3) |> G.e
  | Yield (t, v1, v2) ->
      let v1 = option (expr env) v1 and v2 = v2 in
      G.Yield (t, v1, v2) |> G.e
  | Await (t, v1) ->
      let v1 = expr env v1 in
      G.Await (t, v1) |> G.e
  | Repr v1 ->
      let l, v1, _ = bracket (expr env) v1 in
      G.OtherExpr (("Repr", l), [ G.E v1 ]) |> G.e
  | NamedExpr (v, t, e) -> G.Assign (expr env v, t, expr env e) |> G.e
  | ParenExpr (l, e, r) ->
      let e = expr env e in
      H.set_e_range l r e;
      e

and literal _env = function
  | Bool v1 ->
      let v1 = wrap bool v1 in
      G.Bool v1
  | None_ x ->
      let x = info x in
      G.Null x
  | Num v1 ->
      let v1 = number v1 in
      v1
  | Str v1 ->
      let v1 = wrap string v1 in
      G.String (fb v1)

and argument env = function
  | Arg e ->
      let e = expr env e in
      G.Arg e
  | ArgStar (t, e) ->
      let e = expr env e in
      G.Arg (G.Call (G.IdSpecial (G.Spread, t) |> G.e, fb [ G.arg e ]) |> G.e)
  | ArgPow (t, e) ->
      let e = expr env e in
      G.Arg (G.Call (G.IdSpecial (G.HashSplat, t) |> G.e, fb [ G.arg e ]) |> G.e)
  | ArgKwd (n, e) ->
      let n = name env n in
      let e = expr env e in
      G.ArgKwd (n, e)
  | ArgComp (e, xs) ->
      let e = expr env e in
      G.Arg
        (G.Comprehension
           (G.List, Tok.unsafe_fake_bracket (e, list (for_if env) xs))
        |> G.e)

and for_if env = function
  | CompFor (e1, e2) ->
      let e1 = expr env e1 in
      let e2 = expr env e2 in
      let p = H.expr_to_pattern e1 in
      G.CompFor (unsafe_fake "for", p, unsafe_fake "in", e2)
  | CompIf e1 ->
      let e1 = expr env e1 in
      G.CompIf (unsafe_fake "if", e1)

and dictorset_elt env = function
  | KeyVal (v1, v2) ->
      let v1 = expr env v1 in
      let v2 = expr env v2 in
      G.keyval v1 (unsafe_fake "=>") v2
  | Key v1 ->
      let v1 = expr env v1 in
      v1
  (* TODO: Spread? this is a DictSpread? *)
  | PowInline v1 ->
      let v1 = expr env v1 in
      G.Call
        (G.IdSpecial (G.Spread, unsafe_fake "spread") |> G.e, fb [ G.arg v1 ])
      |> G.e

and number = function
  | Int v1 -> G.Int v1
  | LongInt v1 -> G.Int v1
  | Float v1 ->
      let v1 = wrap id v1 in
      G.Float v1
  | Imag v1 ->
      let v1 = wrap string v1 in
      G.Imag v1

and boolop = function
  | And -> G.And
  | Or -> G.Or

and operator = function
  | Add -> G.Plus
  | Sub -> G.Minus
  | Mult -> G.Mult
  | Div -> G.Div
  | Mod -> G.Mod
  | Pow -> G.Pow
  | FloorDiv -> G.FloorDiv
  | LShift -> G.LSL
  | RShift -> G.LSR
  | BitOr -> G.BitOr
  | BitXor -> G.BitXor
  | BitAnd -> G.BitAnd
  | MatMult -> G.MatMult

and unaryop = function
  | Invert -> G.BitNot
  | Not -> G.Not
  | UAdd -> G.Plus
  | USub -> G.Minus

and cmpop (a, b) =
  match a with
  | Eq -> (G.Eq, b)
  | NotEq -> (G.NotEq, b)
  | Lt -> (G.Lt, b)
  | LtE -> (G.LtE, b)
  | Gt -> (G.Gt, b)
  | GtE -> (G.GtE, b)
  | Is -> (G.PhysEq, b)
  | IsNot -> (G.NotPhysEq, b)
  | In -> (G.In, b)
  | NotIn -> (G.NotIn, b)

and comprehension env f v1 v2 : G.comprehension =
  let v1 = f env v1 in
  let v2 = list (for_if env) v2 in
  (v1, v2)

and comprehension2 env f v1 v2 : G.comprehension =
  let v1 = f env v1 in
  let v2 = list (for_if env) v2 in
  (v1, v2)

and slice1 env e1 (t1, e2, t2) : G.expr_kind =
  match e2 with
  | Index v1 ->
      let v1 = expr env v1 in
      G.ArrayAccess (e1, (t1, v1, t2))
  | Slice (v1, v2, v3) ->
      let v1 = option (expr env) v1
      and v2 = option (expr env) v2
      and v3 = option (expr env) v3 in
      G.SliceAccess (e1, (t1, (v1, v2, v3), t2))

and slice env e = function
  | Index v1 ->
      let v1 = expr env v1 in
      G.ArrayAccess (e, fb v1) |> G.e
  | Slice (v1, v2, v3) ->
      let v1 = option (expr env) v1
      and v2 = option (expr env) v2
      and v3 = option (expr env) v3 in
      G.SliceAccess (e, fb (v1, v2, v3)) |> G.e

and param_pattern_pat env = function
  | PatternName n -> G.PatId (name env n, G.empty_id_info ())
  | PatternTuple t ->
      let t = list (param_pattern_pat env) t in
      PatTuple (Tok.unsafe_fake_bracket t)

and parameters env xs : G.parameter list =
  let param_of_param_pattern ~topt ~eopt = function
    | PatternName n ->
        let n = name env n in
        G.Param { (G.param_of_id n) with G.ptype = topt; pdefault = eopt }
    | PatternTuple ps ->
        let ps = list (param_pattern_pat env) ps in
        G.ParamPattern (PatTuple (fb ps))
  in
  xs
  |> List_.map (function
       | ParamDefault ((param_pat, topt), e) ->
           let topt = option (type_ env) topt in
           let e = expr env e in
           param_of_param_pattern ~topt ~eopt:(Some e) param_pat
       | ParamPattern (param_pat, topt) ->
           let topt = option (type_ env) topt in
           param_of_param_pattern ~topt ~eopt:None param_pat
       | ParamStar (t, (n, topt)) ->
           let n = name env n in
           let topt = option (type_ env) topt in
           G.ParamRest (t, { (G.param_of_id n) with G.ptype = topt })
       | ParamPow (t, (n, topt)) ->
           let n = name env n in
           let topt = option (type_ env) topt in
           G.ParamHashSplat (t, { (G.param_of_id n) with G.ptype = topt })
       | ParamEllipsis tok -> G.ParamEllipsis tok
       | ParamSingleStar tok -> G.OtherParam (("SingleStar", tok), [])
       | ParamSlash tok -> G.OtherParam (("SlashParam", tok), []))

and type_ env v =
  match v with
  (* coupling: This variant was added only for `type_` nodes, which are represented
     using `AST_python.expr`.
     As such, we must explicitly handle it before calling `expr`.
  *)
  | ConstrainedType (v1, v2, v3) ->
      let v1 = type_ env v1 and v3 = type_ env v3 in
      G.OtherType (("ConstrainedType", v2), [ G.T v1; G.T v3 ]) |> G.t
  | _ ->
      let v = expr env v in
      H.expr_to_type v

(* TODO: recognize idioms? *)
and type_parent env v : G.class_parent =
  let v = argument env v in
  match v with
  | G.Arg e -> H.expr_to_class_parent e
  (* less: could raise an error *)
  | G.ArgKwd (id, e) ->
      (G.OtherType (("ArgKwdParent", snd id), [ G.I id; G.E e ]) |> G.t, None)
  (* see argument code *)
  | _ -> raise Impossible

and list_stmt1 env xs =
  match list (stmt env) xs with
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
    when AST_generic.is_metavar_name s ->
      x
  | xs -> G.Block (fb xs) |> G.s

(* This will avoid intermediate Block. You should prefer this function
 * to calls to (list stmt)
 *)
and list_stmt env xs = list (stmt_aux env) xs |> List_.flatten

(* In Python, many Assign are actually VarDef. We should transform those,
 * because this would simplify Naming_AST.ml later, but this requires
 * some semantic analysis to detect which of those Assign are the first
 * and can be safely transform in a VarDef.
 * However, for class fields, an Assign in a field position is surely
 * a VarDef (actually a FieldDef, but VarDef works too), so let's
 * transform those.
 *
 * This tranformation is useful for Generic_vs_generic in m_list__m_field
 * where we do some special magic to allow a definition using a metavariable
 * to be matched at any position. If this definition was actually
 * an Assign, we don't do the magic.
 *)
and fieldstmt x =
  match x with
  | {
   G.s = G.ExprStmt ({ e = G.Assign ({ e = G.N name; _ }, _teq, e); _ }, _sc);
   _;
  } ->
      let vdef = { G.vinit = Some e; vtype = None; vtok = G.no_sc } in
      let ent = { G.name = G.EN name; attrs = []; tparams = None } in
      G.fld (ent, G.VarDef vdef)
  | _ -> G.F x

(* All the types which are allowed to appear in parameter position, e.g.
   a, b, and c in `type T[a, b, c] = int`
*)
and type_parameter env = function
  | Name (id, _) -> G.tparam_of_id id
  | ConstrainedType (Name (id, _), _v2, v3) ->
      let v3 = type_ env v3 in
      G.tparam_of_id ~tp_bounds:[ v3 ] (name env id)
  | ExprStar t ->
      let t = type_ env t in
      G.OtherTypeParam (("ExprStar", unsafe_fake "OtherTypeParam"), [ G.T t ])
  | t ->
      let t = type_ env t in
      G.OtherTypeParam
        (("OtherTypeParam", unsafe_fake "OtherTypeParam"), [ G.T t ])

and type_alias_def env (_tok, v1, v2) =
  let v2 = type_ env v2 in
  let def = G.TypeDef { tbody = G.AliasType v2 } in
  match v1 with
  (* Case 1: The type has generic parameters. Like:
      type IntFunc[T: Hashable, **P] = int
  *)
  | Subscript (lhs, (l, args, r), _ctx) -> (
      let tparams =
        List_.filter_map
          (fun slice ->
            match slice with
            (* Don't know how to interpret these, they shouldn't be allowed, so skip
               This is like `type t[1:2] = int`
            *)
            | Slice _ -> None
            | Index e -> Some (type_parameter env e))
          args
      in
      match H.expr_to_entity_name_opt (expr env lhs) with
      | Some en ->
          let ent =
            { G.name = en; tparams = Some (l, tparams, r); attrs = [] }
          in
          [ G.DefStmt (ent, def) |> G.s ]
      | _ ->
          let tparam_anys = List_.map (fun x -> G.Tp x) tparams in
          [ G.OtherStmt (G.OS_Todo, [ G.Anys tparam_anys; G.Dk def ]) |> G.s ])
  (* Case 2: The type does not, in which case it is likely just a `Name`.
      But, let's call `expr_to_entity_name_opt` for future-proofing.
  *)
  | _ -> (
      match H.expr_to_entity_name_opt (expr env v1) with
      | None -> [ G.OtherStmt (G.OS_Todo, [ G.Dk def ]) |> G.s ]
      | Some en ->
          let ent = { G.name = en; attrs = []; tparams = None } in
          [ G.DefStmt (ent, def) |> G.s ])

and stmt_aux env x =
  match x with
  | FunctionDef (t, v1, v2, v3, v4, v5) ->
      let fkind =
        match env.context with
        | InClass -> G.Method
        | _ -> G.Function
      in
      let env = { env with context = InFunctionOrMethod } in
      let v1 = name env v1
      and v2 = parameters env v2
      and v3 = option (type_ env) v3
      and v4 = list_stmt1 env v4
      and v5 = list (decorator env) v5 in
      let ent = G.basic_entity v1 ~attrs:v5 in
      let def =
        {
          G.fparams = fb v2;
          frettype = v3;
          fbody = G.FBStmt v4;
          fkind = (fkind, t);
        }
      in
      [ G.DefStmt (ent, G.FuncDef def) |> G.s ]
  | ClassDef (v0, v1, v2, v3, v4) ->
      let env = { env with context = InClass } in
      let v1 = name env v1
      and v2 = list (type_parent env) v2
      and v3 = list_stmt env v3
      and v4 = list (decorator env) v4 in
      let ent = G.basic_entity v1 ~attrs:v4 in
      let def =
        {
          G.ckind = (G.Class, v0);
          cextends = v2;
          cimplements = [];
          cmixins = [];
          cparams = fb [];
          cbody = fb (v3 |> List_.map (fun x -> fieldstmt x));
        }
      in
      [ G.DefStmt (ent, G.ClassDef def) |> G.s ]
  | TypeAliasDef (tok, v1, v2) -> type_alias_def env (tok, v1, v2)
  | Return (t, v1) ->
      let v1 = option (expr env) v1 in
      [ G.Return (t, v1, G.sc) |> G.s ]
  | Delete (_t, v1) ->
      let v1 = list (expr env) v1 in
      [ G.OtherStmt (G.OS_Delete, v1 |> List_.map (fun x -> G.E x)) |> G.s ]
  | If (t, v1, v2, v3) ->
      let v1 = expr env v1
      and v2 = list_stmt1 env v2
      and v3 = option (list_stmt1 env) v3 in
      [ G.If (t, Cond v1, v2, v3) |> G.s ]
  | While (t, v1, v2, v3) -> (
      (* TODO? missing list_stmt1 for v3? *)
      let v1 = expr env v1
      and v2 = list_stmt1 env v2
      and v3 = list_stmt env v3 in
      match v3 with
      | [] -> [ G.While (t, G.Cond v1, v2) |> G.s ]
      | _ ->
          [
            G.Block
              (fb
                 [
                   G.While (t, G.Cond v1, v2) |> G.s;
                   G.OtherStmt
                     (G.OS_WhileOrElse, v3 |> List_.map (fun x -> G.S x))
                   |> G.s;
                 ])
            |> G.s;
          ])
  | For (t, v1, t2, v2, v3, v4) -> (
      let foreach = expr env v1
      and ins = expr env v2
      and body = list_stmt1 env v3
      and orelse = list_stmt env v4 in
      let header = G.ForEach (H.expr_to_pattern foreach, t2, ins) in
      match orelse with
      | [] -> [ G.For (t, header, body) |> G.s ]
      | _ ->
          [
            G.Block
              (fb
                 [
                   G.For (t, header, body) |> G.s;
                   G.OtherStmt
                     (G.OS_ForOrElse, orelse |> List_.map (fun x -> G.S x))
                   |> G.s;
                 ])
            |> G.s;
          ])
  (* TODO: unsugar in sequence? *)
  | With (_t, (v1, v2), v3) ->
      let v1 = expr env v1
      and v2 = option (expr env) v2
      and v3 = list_stmt1 env v3 in
      let anys =
        match v2 with
        | None -> [ G.E v1 ]
        | Some e2 -> [ G.E (G.LetPattern (H.expr_to_pattern e2, v1) |> G.e) ]
      in
      [ G.OtherStmtWithStmt (G.OSWS_With, anys, v3) |> G.s ]
  | Switch (t, v1, v2) ->
      let expr = expr env v1 in
      let cases_and_body = list (cases_and_body env) v2 in
      [ G.Switch (t, Some (G.Cond expr), cases_and_body) |> G.s ]
  (* TODO: v1 contains actually a list of lhs, because a = b = c is
   * translated in Assign ([a;b], c)
   *)
  | Assign (v1, v2, v3) -> (
      let v1 =
        list
          (fun (x, y) ->
            (expr env x, option (fun (tk, ty) -> (info tk, type_ env ty)) y))
          v1
      and v2 = info v2
      and v3 = expr env v3 in
      let v1 =
        List_.map
          (fun (e, tyopt) ->
            match tyopt with
            | None -> e
            | Some (tok, ty) -> G.Cast (ty, tok, e) |> G.e)
          v1
      in
      match v1 with
      | [] -> raise Impossible
      | [ a ] -> (
          (* Trying to convert certain Assign in VarDef, which can benefit
           * Codegraph.
           *)
          let to_vardef id idinfo topt =
            let ent =
              { G.name = G.EN (G.Id (id, idinfo)); attrs = []; tparams = None }
            in
            let var =
              G.VarDef { G.vinit = Some v3; vtype = topt; vtok = G.no_sc }
            in
            [ G.DefStmt (ent, var) |> G.s ]
          in
          let default_res = [ G.exprstmt (G.Assign (a, v2, v3) |> G.e) ] in
          match a.G.e with
          (* x: t = ... is definitely a VarDef. This one does not need
           * to be guarded by env.assign_to_vardef because it does not
           * cause any regressions.
           *)
          | G.Cast (t, _, { e = G.N (G.Id (id, idinfo)); _ }) ->
              to_vardef id idinfo (Some t)
          (* x = ... at toplevel. This one is guarded by assign_to_vardef
           * because it causes regressions in semgrep, probably because
           * we do the translation for the target but not for the pattern.
           * So, this is disabled in semgrep (and enabled in codegraph).
           *)
          | G.N (G.Id (id, idinfo))
            when env.context =*= InSourceToplevel && env.assign_to_vardef ->
              let s = fst id in
              if not (is_in_scope env s) then (
                env.current_scope <- s :: env.current_scope;
                to_vardef id idinfo None)
              else default_res
          (* TODO: We should turn more Assign in G.VarDef!
           * Is it bad for semgrep to turn all those Assign in VarDef?
           * In theory no because we have some magic equivalences to
           * convert some Vardef back in Assign in Generic_vs_generic anyway.
           * In practice this leads to regressions in semgrep-rules, probably
           * because we transform them in the target but not in the pattern
           *)
          | _ -> default_res)
      | xs ->
          [
            G.exprstmt
              (G.Assign
                 ( G.Container (G.Tuple, Tok.unsafe_fake_bracket xs) |> G.e,
                   v2,
                   v3 )
              |> G.e);
          ])
  | AugAssign (v1, (v2, tok), v3) ->
      let v1 = expr env v1 and v2 = operator v2 and v3 = expr env v3 in
      [ G.exprstmt (G.AssignOp (v1, (v2, tok), v3) |> G.e) ]
  | Cast (Name (id, _kind), _tok, ty) ->
      (* In the following example, `x : int` is not a type cast but a variable
       * declaration:
       *
       *      class Test:
       *        x : int
       *        ...
       *)
      (* no need to guard with assign_to_vardef here *)
      let id = name env id in
      let ty = type_ env ty in
      let ent = G.basic_entity id in
      let def = G.VarDef { G.vtype = Some ty; vinit = None; vtok = G.no_sc } in
      [ G.DefStmt (ent, def) |> G.s ]
  | Cast (e, tok, ty) ->
      [ G.exprstmt (G.Cast (type_ env ty, info tok, expr env e) |> G.e) ]
  | Raise (t, v1) -> (
      match v1 with
      | Some (e, None) ->
          let e = expr env e in
          [ G.Throw (t, e, G.sc) |> G.s ]
      | Some (e, Some from) ->
          let e = expr env e in
          let from = expr env from in
          let st = G.Throw (t, e, G.sc) |> G.s in
          [ G.OtherStmt (G.OS_ThrowFrom, [ G.E from; G.S st ]) |> G.s ]
      | None -> [ G.OtherStmt (G.OS_ThrowNothing, [ G.Tk t ]) |> G.s ])
  | RaisePython2 (t, e, v2, v3) -> (
      let e = expr env e in
      let st = G.Throw (t, e, G.sc) |> G.s in
      match (v2, v3) with
      | Some args, Some loc ->
          let args = expr env args and loc = expr env loc in
          [
            G.OtherStmt (G.OS_ThrowArgsLocation, [ G.E loc; G.E args; G.S st ])
            |> G.s;
          ]
      | Some args, None ->
          let args = expr env args in
          [ G.OtherStmt (G.OS_ThrowArgsLocation, [ G.E args; G.S st ]) |> G.s ]
      | None, _ -> [ st ])
  | TryExcept (t, v1, v2, v3, v4) ->
      let v1 = list_stmt1 env v1
      and v2 = list (excepthandler env) v2
      and orelse = option (try_else env) v3
      and finally = option (try_finally env) v4 in
      [ G.Try (t, v1, v2, orelse, finally) |> G.s ]
  | Assert (t, v1, v2) ->
      let v1 = expr env v1 and v2 = option (expr env) v2 in
      let es = v1 :: Option.to_list v2 in
      let args = es |> List_.map G.arg in
      [ G.Assert (t, fb args, G.sc) |> G.s ]
  | ImportAs (t, v1, v2) ->
      let mname = module_name env v1
      and nopt = option (ident_and_id_info env) v2 in
      [ G.DirectiveStmt (G.ImportAs (t, mname, nopt) |> G.d) |> G.s ]
  | ImportAll (t, v1, v2) ->
      let mname = module_name env v1 and v2 = info v2 in
      [ G.DirectiveStmt (G.ImportAll (t, mname, v2) |> G.d) |> G.s ]
  | ImportFrom (t, v1, v2) ->
      let v1 = module_name env v1 and v2 = list (import_from_kind env) v2 in
      [ G.DirectiveStmt (G.ImportFrom (t, v1, v2) |> G.d) |> G.s ]
  | Global (t, v1)
  | NonLocal (t, v1) ->
      let v1 = list (name env) v1 in
      v1
      |> List_.map (fun x ->
             let ent = G.basic_entity x in
             G.DefStmt (ent, G.UseOuterDecl t) |> G.s)
  | ExprStmt v1 ->
      let v1 = expr env v1 in
      [ G.exprstmt v1 ]
  | Async (t, x) -> (
      let x = stmt env x in
      match x.G.s with
      | G.DefStmt (ent, func) ->
          [
            G.DefStmt
              ({ ent with G.attrs = G.attr G.Async t :: ent.G.attrs }, func)
            |> G.s;
          ]
      | _ -> [ G.OtherStmt (G.OS_Async, [ G.S x ]) |> G.s ])
  | Pass t -> [ G.OtherStmt (G.OS_Pass, [ G.Tk t ]) |> G.s ]
  | Break t -> [ G.Break (t, G.LNone, G.sc) |> G.s ]
  | Continue t -> [ G.Continue (t, G.LNone, G.sc) |> G.s ]
  (* python2: *)
  | Print (tok, _dest, vals, _nl) ->
      let id = Name (("print", tok), Load) in
      stmt_aux env
        (ExprStmt (Call (id, fb (vals |> List_.map (fun e -> Arg e)))))
  | Exec (tok, e, _eopt, _eopt2) ->
      let id = Name (("exec", tok), Load) in
      stmt_aux env (ExprStmt (Call (id, fb [ Arg e ])))

and cases_and_body env = function
  | CasesAndBody (cases, stmts) ->
      G.CasesAndBody (list (case env) cases, list_stmt1 env stmts)
  | CaseEllipsis tok ->
      let x = info tok in
      G.CaseEllipsis x

and case env = function
  | Case (tok, pat) ->
      let x = info tok in
      let pat = pattern env pat in
      G.Case (x, pat)

and ident_and_id_info env x =
  let x = name env x in
  (x, G.empty_id_info ())

and import_from_kind _env (ident, asopt) = H.mk_import_from_kind ident asopt

(* try avoid using that function as it may introduce
 * intermediate Block that could prevent some semgrep matching
 *)
and stmt env x = G.stmt1 (stmt_aux env x)

and pattern_attribute env = function
  | PatAttribute (v1, _v2, v3) ->
      let v1 = pattern_attribute env v1 and v2 = name env v3 in
      v1 @ [ v2 ]
  | PatName n -> [ name env n ]
  | _ -> []

and pattern env (p : AST_python.pattern) =
  match p with
  | PatName n -> G.PatId (name env n, G.empty_id_info ())
  | PatInterpolatedString (t1, xs, t2) ->
      let xs =
        list (fun x -> G.E (expr env (InterpolatedString (t1, [ x ], t2)))) xs
      in
      OtherPat
        ( ("PatInterpolatedString", Tok.unsafe_fake_tok "PatInterpolatedString"),
          xs )
  | PatConcatenatedString xs ->
      let xs = list (fun x -> G.E (expr env (ConcatenatedString [ x ]))) xs in
      OtherPat
        ( ("PatConcatenatedString", Tok.unsafe_fake_tok "PatConcatenatedString"),
          xs )
  | PatAttribute _ ->
      let ns = pattern_attribute env p in
      OtherPat
        ( ("PatAttribute", Tok.unsafe_fake_tok "PatAttribute"),
          [ G.Name (H.name_of_ids ns) ] )
  | PatConstructor (dn, (_l, ps, _r)) ->
      let dn = dotted_name env dn in
      let ps = list (pattern env) ps in
      G.PatConstructor (H.name_of_ids dn, ps)
  | PatSplat (t, p) ->
      let p = pattern env p in
      G.OtherPat (("PatSplat", t), [ G.P p ])
  | PatDisj ps -> (
      match ps with
      | [] -> raise Impossible
      | [ p ] -> pattern env p
      | p :: ps -> G.PatDisj (pattern env p, pattern env (PatDisj ps)))
  | PatList (l, ps, r) ->
      let ps = list (pattern env) ps in
      G.PatList (l, ps, r)
  | PatTuple (l, ps, r) ->
      let ps = list (pattern env) ps in
      G.PatTuple (l, ps, r)
  | PatDict (l, ps, r) ->
      let ps =
        list
          (fun p ->
            match pattern env p with
            | PatKeyVal (PatId (id, _), p2) -> ([ id ], p2)
            | _ -> raise Impossible)
          ps
      in
      G.PatRecord (l, ps, r)
  | PatLiteral l -> G.PatLiteral (literal env l)
  | PatAs (p, _t, n) ->
      let p = pattern env p in
      let n = name env n in
      G.PatAs (p, (n, G.empty_id_info ()))
  | PatUnderscore t -> G.PatWildcard t
  | PatComplex (t1, n1, t2, n2) ->
      let t =
        match t1 with
        | None -> Tok.unsafe_fake_tok "PatComplex"
        | Some t -> t
      in
      let n1 = G.PatLiteral (number n1) in
      let n2 = G.PatLiteral (number n2) in
      G.OtherPat (("PatComplex", t), [ G.P n1; G.Tk t2; G.P n2 ])
  | PatKeyVal (p1, _t, p2) ->
      let p1 = pattern env p1 in
      let p2 = pattern env p2 in
      G.PatKeyVal (p1, p2)

and excepthandler env = function
  | ExceptHandler (t, v1, v2, v3) ->
      let v1 = option (expr env) v1 (* a type actually, even tuple of types *)
      and v2 = option (name env) v2
      and v3 = list_stmt1 env v3 in
      let exn : G.catch_exn =
        match (v1, v2) with
        | Some e, None -> (
            match e.G.e with
            | G.Ellipsis tok -> G.CatchPattern (G.PatEllipsis tok)
            | G.Container (G.Tuple, _) ->
                G.CatchParam (G.param_of_type (H.expr_to_type e))
            | _ ->
                G.CatchParam
                  (G.param_of_type
                     (H.expr_to_type
                        (G.Container (G.Tuple, Tok.unsafe_fake_bracket [ e ])
                        |> G.e))))
        | None, None -> G.CatchPattern (G.PatWildcard (fake t "_"))
        | None, Some _ -> raise Impossible (* see the grammar *)
        | Some e, Some n ->
            G.CatchParam (G.param_of_type (H.expr_to_type e) ~pname:n)
      in
      (t, exn, v3)

and try_else env (t, v1) = (t, list_stmt1 env v1)
and try_finally env (t, v1) = (t, list_stmt1 env v1)

and decorator env (t, v1) =
  (* We'll try to first translate it as a G.NamedAttr; if we could not
   * and the decorator is a pip0614 namedexpr decorator, we'll translate
   * it as an otherAttr.
   *)
  let pip0614_expr_attr tok anys =
    G.OtherAttribute (("pip0614: expr attr", tok), anys)
  in
  let rec get_dotted_name e acc =
    match e with
    | Name (id, _) -> Some (id :: acc)
    | Attribute (e, _, id, _) -> get_dotted_name e (id :: acc)
    | _ -> None
  in
  let dotted_name, args =
    match v1 with
    | Call (e, (p1, args, p2)) ->
        (get_dotted_name e [], Some (p1, list (argument env) args, p2))
    | _ -> (get_dotted_name v1 [], None)
  in
  match dotted_name with
  | Some d_name -> (
      match (d_name, args) with
      (* Use standard static keyword for staticmethod attribute. This
         is used by other parts of the pro-engine to check if a method
         is static. *)
      | [ ("staticmethod", tok) ], (None | Some (_, [], _)) ->
          G.attr G.Static tok
      | [ x ], args ->
          G.NamedAttr
            ( t,
              Id (x, G.empty_id_info ()),
              Option.value ~default:(Tok.unsafe_fake_bracket []) args )
      | x :: xs, args ->
          let base = G.N (G.Id (x, G.empty_id_info ())) |> G.e in
          let dot_access =
            xs
            |> List.fold_left
                 (fun e x ->
                   let tok = Tok.fake_tok (snd x) "." in
                   G.DotAccess (e, tok, G.FN (G.Id (x, G.empty_id_info ())))
                   |> G.e)
                 base
          in
          pip0614_expr_attr t
            [
              G.E
                (G.Call
                   ( dot_access,
                     args |> Option.value ~default:(Tok.unsafe_fake_bracket [])
                   )
                |> G.e);
            ]
      | [], _ -> raise Impossible)
  | None ->
      let v1 = expr env v1 in
      pip0614_expr_attr t [ G.E v1 ]

let program ?(assign_to_vardef = false) v =
  let env : env = empty_env ~assign_to_vardef InSourceToplevel in
  let v = list_stmt env v in
  v

let any x =
  let env : env = empty_env ~assign_to_vardef:false InPattern in
  match x with
  | Expr v1 ->
      let v1 = expr env v1 in
      G.E v1
  | Stmt v1 ->
      let v1 = stmt env v1 in
      G.S v1
  | Stmts v1 ->
      let v1 = list_stmt env v1 in
      G.Ss v1
  | Decorator v1 ->
      let v1 = decorator env v1 in
      G.At v1
  | Program v1 ->
      let v1 = list_stmt env v1 in
      G.Pr v1
  | DictElem v1 ->
      let v1 = dictorset_elt env v1 in
      G.E v1

let type_for_lsif ty =
  let env : env = empty_env ~assign_to_vardef:false InPattern in
  type_ env ty

let parameters_for_lsif params =
  let env : env = empty_env ~assign_to_vardef:false InPattern in
  parameters env params
