(* Yoann Padioleau
 *
 * Copyright (C) 2019-2022 r2c
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
let list = List.map
let vref f x = ref (f !x)
let string = id
let bool = id
let fake tok s = Parse_info.fake_info tok s
let unsafe_fake s = Parse_info.unsafe_fake_info s
let fb = AST_generic.fake_bracket

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
  (* transforming '. foo.bar' in G.Filename "./foo/bar" *)
  | Some toks ->
      let count =
        toks
        |> List.map Parse_info.str_of_info
        |> String.concat "" |> String.length
      in
      let tok = List.hd toks in
      let elems = v1 |> List.map fst in
      let prefixes =
        match count with
        | 1 -> [ "." ]
        | 2 -> [ ".." ]
        | n -> Common2.repeat ".." (n - 1)
      in
      let s = String.concat "/" (prefixes @ elems) in
      G.FileName (s, tok)

let resolved_name = function
  | LocalVar -> Some (G.LocalVar, G.sid_TODO)
  | Parameter -> Some (G.Parameter, G.sid_TODO)
  | GlobalVar -> Some (G.Global, G.sid_TODO)
  | ClassField -> None
  | ImportedModule xs -> Some (G.ImportedModule (G.DottedName xs), G.sid_TODO)
  | ImportedEntity xs -> Some (G.ImportedEntity xs, G.sid_TODO)
  | NotResolved -> None

let expr_context = function
  | Load -> ()
  | Store -> ()
  | Del -> ()
  | AugLoad -> ()
  | AugStore -> ()
  | Param -> ()

let rec expr env (x : expr) =
  match x with
  | DotAccessEllipsis (v1, v2) ->
      let v1 = expr env v1 in
      G.DotAccessEllipsis (v1, v2) |> G.e
  | Bool v1 ->
      let v1 = wrap bool v1 in
      G.L (G.Bool v1) |> G.e
  | None_ x ->
      let x = info x in
      G.L (G.Null x) |> G.e
  | Ellipsis x ->
      let x = info x in
      G.Ellipsis x |> G.e
  | DeepEllipsis x ->
      let x = bracket (expr env) x in
      G.DeepEllipsis x |> G.e
  | Num v1 ->
      let v1 = number v1 in
      G.L v1 |> G.e
  | Str v1 ->
      let v1 = wrap string v1 in
      G.L (G.String v1) |> G.e
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
          fb [ G.Arg (G.L (G.String v1) |> G.e) ] )
      |> G.e
  | InterpolatedString (v1, xs, v3) ->
      G.Call
        (* Python interpolated strings are always of the form f"...", we need to support arbitary strings in the generic AST because Scala has custom interpolators *)
        ( G.IdSpecial (G.ConcatString (G.FString "f"), unsafe_fake "concat")
          |> G.e,
          ( v1,
            xs
            |> List.map (fun x ->
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
            |> List.map (fun x ->
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
  | Name (v1, v2, v3) ->
      let v1 = name env v1
      and _v2TODO = expr_context v2
      and v3 = vref resolved_name v3 in
      G.N (G.Id (v1, { (G.empty_id_info ()) with G.id_resolved = v3 })) |> G.e
  | Tuple (CompList v1, v2) ->
      let v1 = bracket (list (expr env)) v1 and _v2TODO = expr_context v2 in
      G.Container (G.Tuple, v1) |> G.e
  | Tuple (CompForIf (l, (v1, v2), r), v3) ->
      let e1 = comprehension env expr v1 v2 in
      let _v4TODO = expr_context v3 in
      G.Comprehension (G.Tuple, (l, e1, r)) |> G.e
  | List (CompList v1, v2) ->
      let v1 = bracket (list (expr env)) v1 and _v2TODO = expr_context v2 in
      G.Container (G.List, v1) |> G.e
  | List (CompForIf (l, (v1, v2), r), v3) ->
      let e1 = comprehension env expr v1 v2 in
      let _v3TODO = expr_context v3 in
      G.Comprehension (G.List, (l, e1, r)) |> G.e
  | Subscript (v1, v2, v3) ->
      (let e = expr env v1 and _v3TODO = expr_context v3 in
       match v2 with
       | l1, [ x ], l2 -> slice1 env e (l1, x, l2)
       | l1, xs, _ ->
           let xs = list (slice env e) xs in
           G.OtherExpr (("Slices", l1), xs |> List.map (fun x -> G.E x)))
      |> G.e
  | Attribute (v1, t, v2, v3) ->
      let v1 = expr env v1
      and t = info t
      and v2 = name env v2
      and _v3TODO = expr_context v3 in
      G.DotAccess (v1, t, G.FN (G.Id (v2, G.empty_id_info ()))) |> G.e
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
          || v = []
        then G.Dict
        else G.Set
      in
      G.Container (kind, (t1, v', t2)) |> G.e
  | DictOrSet (CompForIf (l, (v1, v2), r)) ->
      let e1 = comprehension2 env dictorset_elt v1 v2 in
      G.Comprehension (G.Dict, (l, e1, r)) |> G.e
  | BoolOp ((v1, tok), v2) ->
      let v1 = boolop v1 and v2 = list (expr env) v2 in
      G.Call (G.IdSpecial (G.Op v1, tok) |> G.e, fb (v2 |> List.map G.arg))
      |> G.e
  | BinOp (v1, (v2, tok), v3) ->
      let v1 = expr env v1 and v2 = operator v2 and v3 = expr env v3 in
      G.Call
        (G.IdSpecial (G.Op v2, tok) |> G.e, fb ([ v1; v3 ] |> List.map G.arg))
      |> G.e
  | UnaryOp ((v1, tok), v2) ->
      let op = unaryop v1 and v2 = expr env v2 in
      G.opcall (op, tok) [ v2 ]
  | Compare (v1, v2, v3) -> (
      let v1 = expr env v1 and v2 = list cmpop v2 and v3 = list (expr env) v3 in
      match (v2, v3) with
      | [ (op, tok) ], [ e ] ->
          G.Call
            (G.IdSpecial (G.Op op, tok) |> G.e, fb ([ v1; e ] |> List.map G.arg))
          |> G.e
      | _ ->
          let anyops =
            v2
            |> List.map (function arith, tok ->
                   G.E (G.IdSpecial (G.Op arith, tok) |> G.e))
          in
          let anys = anyops @ (v3 |> List.map (fun e -> G.E e)) in
          G.OtherExpr (("CmpOps", unsafe_fake ""), anys) |> G.e)
  | Call (v1, v2) ->
      let v1 = expr env v1 in
      let v2 = bracket (list (argument env)) v2 in
      G.Call (v1, v2) |> G.e
  | Lambda (t0, v1, _t2, v2) ->
      let v1 = parameters env v1 and v2 = expr env v2 in
      G.Lambda
        {
          G.fparams = v1;
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
        (G.Comprehension (G.List, G.fake_bracket (e, list (for_if env) xs))
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
  | Int v1 ->
      let v1 = wrap id v1 in
      G.Int v1
  | LongInt v1 ->
      let v1 = wrap id v1 in
      G.Int v1
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

and param_pattern env = function
  | PatternName n -> G.PatId (name env n, G.empty_id_info ())
  | PatternTuple t ->
      let t = list (param_pattern env) t in
      G.PatTuple (G.fake_bracket t)

and parameters env xs =
  xs
  |> List.map (function
       | ParamDefault ((n, topt), e) ->
           let n = name env n in
           let topt = option (type_ env) topt in
           let e = expr env e in
           G.Param { (G.param_of_id n) with G.ptype = topt; pdefault = Some e }
       | ParamPattern (PatternName n, topt) ->
           let n = name env n and topt = option (type_ env) topt in
           G.Param { (G.param_of_id n) with G.ptype = topt }
       | ParamPattern (PatternTuple pat, _) ->
           let pat = list (param_pattern env) pat in
           G.ParamPattern (G.PatTuple (G.fake_bracket pat))
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
    when AST_generic_.is_metavar_name s ->
      x
  | xs -> G.Block (fb xs) |> G.s

(* This will avoid intermediate Block. You should prefer this function
 * to calls to (list stmt)
 *)
and list_stmt env xs = list (stmt_aux env) xs |> List.flatten

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
      let vdef = { G.vinit = Some e; vtype = None } in
      let ent = { G.name = G.EN name; attrs = []; tparams = [] } in
      G.fld (ent, G.VarDef vdef)
  | _ -> G.F x

and stmt_aux env x =
  match x with
  | FunctionDef (t, v1, v2, v3, v4, v5) ->
      let env = { env with context = InFunctionOrMethod } in
      let v1 = name env v1
      and v2 = parameters env v2
      and v3 = option (type_ env) v3
      and v4 = list_stmt1 env v4
      and v5 = list (decorator env) v5 in
      let ent = G.basic_entity v1 ~attrs:v5 in
      let def =
        {
          G.fparams = v2;
          frettype = v3;
          fbody = G.FBStmt v4;
          fkind = (G.Function, t);
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
          cparams = [];
          cbody = fb (v3 |> List.map (fun x -> fieldstmt x));
        }
      in
      [ G.DefStmt (ent, G.ClassDef def) |> G.s ]
  | Return (t, v1) ->
      let v1 = option (expr env) v1 in
      [ G.Return (t, v1, G.sc) |> G.s ]
  | Delete (_t, v1) ->
      let v1 = list (expr env) v1 in
      [ G.OtherStmt (G.OS_Delete, v1 |> List.map (fun x -> G.E x)) |> G.s ]
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
                     (G.OS_WhileOrElse, v3 |> List.map (fun x -> G.S x))
                   |> G.s;
                 ])
            |> G.s;
          ])
  | For (t, v1, t2, v2, v3, v4) -> (
      let foreach = pattern env v1
      and ins = expr env v2
      and body = list_stmt1 env v3
      and orelse = list_stmt env v4 in
      let header = G.ForEach (foreach, t2, ins) in
      match orelse with
      | [] -> [ G.For (t, header, body) |> G.s ]
      | _ ->
          [
            G.Block
              (fb
                 [
                   G.For (t, header, body) |> G.s;
                   G.OtherStmt
                     (G.OS_ForOrElse, orelse |> List.map (fun x -> G.S x))
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
        Common.map
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
              { G.name = G.EN (G.Id (id, idinfo)); attrs = []; tparams = [] }
            in
            let var = G.VarDef { G.vinit = Some v3; vtype = topt } in
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
            when env.context = InSourceToplevel && env.assign_to_vardef ->
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
              (G.Assign (G.Container (G.Tuple, G.fake_bracket xs) |> G.e, v2, v3)
              |> G.e);
          ])
  | AugAssign (v1, (v2, tok), v3) ->
      let v1 = expr env v1 and v2 = operator v2 and v3 = expr env v3 in
      [ G.exprstmt (G.AssignOp (v1, (v2, tok), v3) |> G.e) ]
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
  | TryExcept (t, v1, v2, v3) -> (
      let v1 = list_stmt1 env v1
      and v2 = list (excepthandler env) v2
      and orelse = list_stmt env v3 in
      match orelse with
      | [] -> [ G.Try (t, v1, v2, None) |> G.s ]
      | _ ->
          [
            G.Block
              (fb
                 [
                   G.Try (t, v1, v2, None) |> G.s;
                   G.OtherStmt
                     (G.OS_TryOrElse, orelse |> List.map (fun x -> G.S x))
                   |> G.s;
                 ])
            |> G.s;
          ])
  | TryFinally (t, v1, t2, v2) ->
      let v1 = list_stmt1 env v1 and v2 = list_stmt1 env v2 in
      (* could lift down the Try in v1 *)
      [ G.Try (t, v1, [], Some (t2, v2)) |> G.s ]
  | Assert (t, v1, v2) ->
      let v1 = expr env v1 and v2 = option (expr env) v2 in
      let es = v1 :: Option.to_list v2 in
      let args = es |> List.map G.arg in
      [ G.Assert (t, fb args, G.sc) |> G.s ]
  | ImportAs (t, v1, v2) ->
      let mname = module_name env v1
      and nopt = option (ident_and_id_info env) v2 in
      [ G.DirectiveStmt (G.ImportAs (t, mname, nopt) |> G.d) |> G.s ]
  | ImportAll (t, v1, v2) ->
      let mname = module_name env v1 and v2 = info v2 in
      [ G.DirectiveStmt (G.ImportAll (t, mname, v2) |> G.d) |> G.s ]
  | ImportFrom (t, v1, v2) ->
      let v1 = module_name env v1 and v2 = list (alias env) v2 in
      List.map
        (fun (a, b) ->
          G.DirectiveStmt (G.ImportFrom (t, v1, a, b) |> G.d) |> G.s)
        v2
  | Global (t, v1)
  | NonLocal (t, v1) ->
      let v1 = list (name env) v1 in
      v1
      |> List.map (fun x ->
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
      let id = Name (("print", tok), Load, ref NotResolved) in
      stmt_aux env
        (ExprStmt (Call (id, fb (vals |> List.map (fun e -> Arg e)))))
  | Exec (tok, e, _eopt, _eopt2) ->
      let id = Name (("exec", tok), Load, ref NotResolved) in
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
      let pat = expr env pat in
      G.Case (x, H.expr_to_pattern pat)

and ident_and_id_info env x =
  let x = name env x in
  (x, G.empty_id_info ())

(* try avoid using that function as it may introduce
 * intermediate Block that could prevent some semgrep matching
 *)
and stmt env x = G.stmt1 (stmt_aux env x)

and pattern env e =
  let e = expr env e in
  H.expr_to_pattern e

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
                        (G.Container (G.Tuple, G.fake_bracket [ e ]) |> G.e))))
        | None, None -> G.CatchPattern (G.PatUnderscore (fake t "_"))
        | None, Some _ -> raise Impossible (* see the grammar *)
        | Some e, Some n ->
            G.CatchParam (G.param_of_type (H.expr_to_type e) ~pname:(Some n))
      in
      (t, exn, v3)

and decorator env (t, v1, v2) =
  let v1 = dotted_name env v1 in
  let v2 = option (bracket (list (argument env))) v2 in
  let args =
    match v2 with
    | Some (t1, x, t2) -> (t1, x, t2)
    | None -> G.fake_bracket []
  in
  let name = H.name_of_ids v1 in
  G.NamedAttr (t, name, args)

and alias env (v1, v2) =
  let v1 = name env v1 and v2 = option (ident_and_id_info env) v2 in
  let imported_ident =
    match v2 with
    | None -> v1
    | Some (id, _) -> id
  in
  env.imported <- fst imported_ident :: env.imported;
  (v1, v2)

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
  | Stmt v1 -> (
      let v1 = stmt env v1 in
      (* in Python Assign is a stmt but in the generic AST it's an expression*)
      match v1.G.s with
      | G.ExprStmt (x, _t) -> G.E x
      | _ -> G.S v1)
  | Stmts v1 ->
      let v1 = list_stmt env v1 in
      G.Ss v1
  | Program v1 ->
      let v1 = list_stmt env v1 in
      G.Pr v1
  | DictElem v1 ->
      let v1 = dictorset_elt env v1 in
      G.E v1
