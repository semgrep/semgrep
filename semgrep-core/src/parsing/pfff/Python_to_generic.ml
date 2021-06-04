(*s: pfff/lang_python/analyze/Python_to_generic.ml *)
(*s: pad/r2c copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
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
(*e: pad/r2c copyright *)
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
 * TODO: intercept Call to eval and transform in special Eval?
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(*s: constant [[Python_to_generic.id]] *)
let id x = x

(*e: constant [[Python_to_generic.id]] *)
(*s: constant [[Python_to_generic.option]] *)
let option = Common.map_opt

(*e: constant [[Python_to_generic.option]] *)
(*s: constant [[Python_to_generic.list]] *)
let list = List.map

(*e: constant [[Python_to_generic.list]] *)
(*s: function [[Python_to_generic.vref]] *)
let vref f x = ref (f !x)

(*e: function [[Python_to_generic.vref]] *)

(*s: constant [[Python_to_generic.string]] *)
let string = id

(*e: constant [[Python_to_generic.string]] *)
(*s: constant [[Python_to_generic.bool]] *)
let bool = id

(*e: constant [[Python_to_generic.bool]] *)

(*s: function [[Python_to_generic.fake]] *)
let fake s = Parse_info.fake_info s

(*e: function [[Python_to_generic.fake]] *)
(*s: function [[Python_to_generic.fake_bracket]] *)
let fb = AST_generic.fake_bracket

(*e: function [[Python_to_generic.fake_bracket]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*s: function [[Python_to_generic.info]] *)
let info x = x

(*e: function [[Python_to_generic.info]] *)

(*s: constant [[Python_to_generic.wrap]] *)
let wrap _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = info v2 in
  (v1, v2)

(*e: constant [[Python_to_generic.wrap]] *)

(*s: function [[Python_to_generic.bracket]] *)
let bracket of_a (t1, x, t2) = (info t1, of_a x, info t2)

(*e: function [[Python_to_generic.bracket]] *)

(*s: function [[Python_to_generic.name]] *)
let name v = wrap string v

(*e: function [[Python_to_generic.name]] *)

(*s: function [[Python_to_generic.dotted_name]] *)
let dotted_name v = list name v

(*e: function [[Python_to_generic.dotted_name]] *)

(*s: function [[Python_to_generic.module_name]] *)
let module_name (v1, dots) =
  let v1 = dotted_name v1 in
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

(*e: function [[Python_to_generic.module_name]] *)

(*s: function [[Python_to_generic.resolved_name]] *)
let resolved_name = function
  | LocalVar -> Some (G.Local, G.sid_TODO)
  | Parameter -> Some (G.Param, G.sid_TODO)
  | GlobalVar -> Some (G.Global, G.sid_TODO)
  | ClassField -> None
  | ImportedModule xs -> Some (G.ImportedModule (G.DottedName xs), G.sid_TODO)
  | ImportedEntity xs -> Some (G.ImportedEntity xs, G.sid_TODO)
  | NotResolved -> None

(*e: function [[Python_to_generic.resolved_name]] *)

(*s: function [[Python_to_generic.expr_context]] *)
let expr_context = function
  | Load -> ()
  | Store -> ()
  | Del -> ()
  | AugLoad -> ()
  | AugStore -> ()
  | Param -> ()

(*e: function [[Python_to_generic.expr_context]] *)

(*s: function [[Python_to_generic.expr]] *)
let rec expr (x : expr) =
  match x with
  | Bool v1 ->
      let v1 = wrap bool v1 in
      G.L (G.Bool v1)
  | None_ x ->
      let x = info x in
      G.L (G.Null x)
  | Ellipsis x ->
      let x = info x in
      G.Ellipsis x
  | DeepEllipsis x ->
      let x = bracket expr x in
      G.DeepEllipsis x
  | Num v1 ->
      let v1 = number v1 in
      G.L v1
  | Str v1 ->
      let v1 = wrap string v1 in
      G.L (G.String v1)
  | EncodedStr (v1, pre) ->
      let v1 = wrap string v1 in
      (* bugfix: do not reuse the same tok! otherwise in semgrep
       * if a metavar is bound to an encoded string (e.g., r'foo'), and
       * the metavar is used in the message, r'foo' will be displayed
       * three times.
       * todo: the right fix is to have EncodedStr of string wrap * string wrap
       *)
      G.Call
        ( G.IdSpecial (G.EncodedString pre, fake ""),
          fb [ G.Arg (G.L (G.String v1)) ] )
  | InterpolatedString xs ->
      G.Call
        ( G.IdSpecial (G.ConcatString G.FString, fake "concat"),
          fb
            ( xs
            |> List.map (fun x ->
                   let x = expr x in
                   G.Arg x) ) )
  | ConcatenatedString xs ->
      G.Call
        ( G.IdSpecial (G.ConcatString G.SequenceConcat, fake "concat"),
          fb
            ( xs
            |> List.map (fun x ->
                   let x = expr x in
                   G.Arg x) ) )
  | TypedExpr (v1, v2) ->
      let v1 = expr v1 in
      let v2 = type_ v2 in
      G.Cast (v2, v1)
  | TypedMetavar (v1, v2, v3) ->
      let v1 = name v1 in
      let v3 = type_ v3 in
      G.TypedMetavar (v1, v2, v3)
  | ExprStar v1 ->
      let v1 = expr v1 in
      G.Call (G.IdSpecial (G.Spread, fake "spread"), fb [ G.arg v1 ])
  | Name (v1, v2, v3) ->
      let v1 = name v1
      and _v2TODO = expr_context v2
      and v3 = vref resolved_name v3 in
      G.N (G.Id (v1, { (G.empty_id_info ()) with G.id_resolved = v3 }))
  | Tuple (CompList v1, v2) ->
      let v1 = bracket (list expr) v1 and _v2TODO = expr_context v2 in
      G.Tuple v1
  | Tuple (CompForIf (v1, v2), v3) ->
      let e1 = comprehension expr v1 v2 in
      let _v4TODO = expr_context v3 in
      G.Tuple (G.fake_bracket e1)
  | List (CompList v1, v2) ->
      let v1 = bracket (list expr) v1 and _v2TODO = expr_context v2 in
      G.Container (G.List, v1)
  | List (CompForIf (v1, v2), v3) ->
      let e1 = comprehension expr v1 v2 in
      let _v3TODO = expr_context v3 in
      G.Container (G.List, fb e1)
  | Subscript (v1, v2, v3) -> (
      let e = expr v1 and _v3TODO = expr_context v3 in
      match v2 with
      | l1, [ x ], l2 -> slice1 e (l1, x, l2)
      | _, xs, _ ->
          let xs = list (slice e) xs in
          G.OtherExpr (G.OE_Slices, xs |> List.map (fun x -> G.E x)) )
  | Attribute (v1, t, v2, v3) ->
      let v1 = expr v1
      and t = info t
      and v2 = name v2
      and _v3TODO = expr_context v3 in
      G.DotAccess (v1, t, G.EN (G.Id (v2, G.empty_id_info ())))
  | DictOrSet (CompList (t1, v, t2)) ->
      let v' = list dictorset_elt v in
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
      G.Container (kind, (t1, v', t2))
  | DictOrSet (CompForIf (v1, v2)) ->
      let e1 = comprehension2 dictorset_elt v1 v2 in
      G.Container (G.Dict, fb e1)
  | BoolOp ((v1, tok), v2) ->
      let v1 = boolop v1 and v2 = list expr v2 in
      G.Call (G.IdSpecial (G.Op v1, tok), fb (v2 |> List.map G.arg))
  | BinOp (v1, (v2, tok), v3) ->
      let v1 = expr v1 and v2 = operator v2 and v3 = expr v3 in
      G.Call (G.IdSpecial (G.Op v2, tok), fb ([ v1; v3 ] |> List.map G.arg))
  | UnaryOp ((v1, tok), v2) -> (
      let v1 = unaryop v1 and v2 = expr v2 in
      match v1 with
      | Left op ->
          G.Call (G.IdSpecial (G.Op op, tok), fb ([ v2 ] |> List.map G.arg))
      | Right oe -> G.OtherExpr (oe, [ G.E v2 ]) )
  | Compare (v1, v2, v3) -> (
      let v1 = expr v1 and v2 = list cmpop v2 and v3 = list expr v3 in
      match (v2, v3) with
      | [ (op, tok) ], [ e ] ->
          G.Call (G.IdSpecial (G.Op op, tok), fb ([ v1; e ] |> List.map G.arg))
      | _ ->
          let anyops =
            v2
            |> List.map (function arith, tok ->
                   G.E (G.IdSpecial (G.Op arith, tok)))
          in
          let any = anyops @ (v3 |> List.map (fun e -> G.E e)) in
          G.OtherExpr (G.OE_CmpOps, any) )
  | Call (v1, v2) ->
      let v1 = expr v1 in
      let v2 = bracket (list argument) v2 in
      G.Call (v1, v2)
  | Lambda (t0, v1, _t2, v2) ->
      let v1 = parameters v1 and v2 = expr v2 in
      G.Lambda
        {
          G.fparams = v1;
          fbody = G.exprstmt v2;
          frettype = None;
          fkind = (G.LambdaKind, t0);
        }
  | IfExp (v1, v2, v3) ->
      let v1 = expr v1 and v2 = expr v2 and v3 = expr v3 in
      G.Conditional (v1, v2, v3)
  | Yield (t, v1, v2) ->
      let v1 = option expr v1 and v2 = v2 in
      G.Yield (t, v1, v2)
  | Await (t, v1) ->
      let v1 = expr v1 in
      G.Await (t, v1)
  | Repr v1 ->
      let _, v1, _ = bracket expr v1 in
      G.OtherExpr (G.OE_Repr, [ G.E v1 ])
  | NamedExpr (v, t, e) -> G.Assign (expr v, t, expr e)

(*e: function [[Python_to_generic.expr]] *)

(*s: function [[Python_to_generic.argument]] *)
and argument = function
  | Arg e ->
      let e = expr e in
      G.Arg e
  | ArgStar (t, e) ->
      let e = expr e in
      G.Arg (G.Call (G.IdSpecial (G.Spread, t), fb [ G.arg e ]))
  | ArgPow (t, e) ->
      let e = expr e in
      G.Arg (G.Call (G.IdSpecial (G.HashSplat, t), fb [ G.arg e ]))
  | ArgKwd (n, e) ->
      let n = name n in
      let e = expr e in
      G.ArgKwd (n, e)
  | ArgComp (e, xs) ->
      let e = expr e in
      G.ArgOther (G.OA_ArgComp, G.E e :: list for_if xs)

(*e: function [[Python_to_generic.argument]] *)

(*s: function [[Python_to_generic.for_if]] *)
and for_if = function
  | CompFor (e1, e2) ->
      let e1 = expr e1 in
      let e2 = expr e2 in
      G.E (G.OtherExpr (G.OE_CompFor, [ G.E e1; G.E e2 ]))
  | CompIf e1 ->
      let e1 = expr e1 in
      G.E (G.OtherExpr (G.OE_CompIf, [ G.E e1 ]))

(*e: function [[Python_to_generic.for_if]] *)

(*s: function [[Python_to_generic.dictorset_elt]] *)
and dictorset_elt = function
  | KeyVal (v1, v2) ->
      let v1 = expr v1 in
      let v2 = expr v2 in
      G.Tuple (G.fake_bracket [ v1; v2 ])
  | Key v1 ->
      let v1 = expr v1 in
      v1
  | PowInline v1 ->
      let v1 = expr v1 in
      G.Call (G.IdSpecial (G.Spread, fake "spread"), fb [ G.arg v1 ])

(*e: function [[Python_to_generic.dictorset_elt]] *)

(*s: function [[Python_to_generic.number]] *)
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

(*e: function [[Python_to_generic.number]] *)

(*s: function [[Python_to_generic.boolop]] *)
and boolop = function And -> G.And | Or -> G.Or

(*e: function [[Python_to_generic.boolop]] *)

(*s: function [[Python_to_generic.operator]] *)
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

(*e: function [[Python_to_generic.operator]] *)

(*s: function [[Python_to_generic.unarop]] *)
and unaryop = function
  | Invert -> Right G.OE_Invert
  | Not -> Left G.Not
  | UAdd -> Left G.Plus
  | USub -> Left G.Minus

(*e: function [[Python_to_generic.unarop]] *)

(*s: function [[Python_to_generic.cmpop]] *)
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

(*e: function [[Python_to_generic.cmpop]] *)

(*s: function [[Python_to_generic.comprehension]] *)
and comprehension f v1 v2 =
  let v1 = f v1 in
  let v2 = list for_if v2 in
  [ G.OtherExpr (G.OE_CompForIf, G.E v1 :: v2) ]

(*e: function [[Python_to_generic.comprehension]] *)

(*s: function [[Python_to_generic.comprehension2]] *)
and comprehension2 f v1 v2 =
  let v1 = f v1 in
  let v2 = list for_if v2 in
  [ G.OtherExpr (G.OE_CompForIf, G.E v1 :: v2) ]

(*e: function [[Python_to_generic.comprehension2]] *)

(*s: function [[Python_to_generic.slice]] *)
and slice1 e1 (t1, e2, t2) =
  match e2 with
  | Index v1 ->
      let v1 = expr v1 in
      G.ArrayAccess (e1, (t1, v1, t2))
  | Slice (v1, v2, v3) ->
      let v1 = option expr v1 and v2 = option expr v2 and v3 = option expr v3 in
      G.SliceAccess (e1, (t1, (v1, v2, v3), t2))

and slice e = function
  | Index v1 ->
      let v1 = expr v1 in
      G.ArrayAccess (e, fb v1)
  | Slice (v1, v2, v3) ->
      let v1 = option expr v1 and v2 = option expr v2 and v3 = option expr v3 in
      G.SliceAccess (e, fb (v1, v2, v3))

(*e: function [[Python_to_generic.slice]] *)
and param_pattern = function
  | PatternName n -> G.PatId (name n, G.empty_id_info ())
  | PatternTuple t ->
      let t = list param_pattern t in
      G.PatTuple (G.fake_bracket t)

(*s: function [[Python_to_generic.parameters]] *)
and parameters xs =
  xs
  |> List.map (function
       | ParamDefault ((n, topt), e) ->
           let n = name n in
           let topt = option type_ topt in
           let e = expr e in
           G.ParamClassic
             { (G.param_of_id n) with G.ptype = topt; pdefault = Some e }
       | ParamPattern (PatternName n, topt) ->
           let n = name n and topt = option type_ topt in
           G.ParamClassic { (G.param_of_id n) with G.ptype = topt }
       | ParamPattern (PatternTuple pat, _) ->
           let pat = list param_pattern pat in
           G.ParamPattern (G.PatTuple (G.fake_bracket pat))
       | ParamStar (t, (n, topt)) ->
           let n = name n in
           let topt = option type_ topt in
           G.ParamRest (t, { (G.param_of_id n) with G.ptype = topt })
       | ParamPow (t, (n, topt)) ->
           let n = name n in
           let topt = option type_ topt in
           G.ParamHashSplat (t, { (G.param_of_id n) with G.ptype = topt })
       | ParamEllipsis tok -> G.ParamEllipsis tok
       | ParamSingleStar tok ->
           G.OtherParam (G.OPO_SingleStarParam, [ G.Tk tok ])
       | ParamSlash tok -> G.OtherParam (G.OPO_SlashParam, [ G.Tk tok ]))

(*e: function [[Python_to_generic.parameters]] *)

(*s: function [[Python_to_generic.type_]] *)
and type_ v =
  let v = expr v in
  H.expr_to_type v

(*e: function [[Python_to_generic.type_]] *)

(*s: function [[Python_to_generic.type_parent]] *)
and type_parent v =
  let v = argument v in
  G.OtherType (G.OT_Arg, [ G.Ar v ])

(*e: function [[Python_to_generic.type_parent]] *)

(*s: function [[Python_to_generic.list_stmt1]] *)
and list_stmt1 xs =
  match list stmt xs with
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
  | [ ({ G.s = G.ExprStmt (G.N (G.Id ((s, _), _)), _); _ } as x) ]
    when AST_generic_.is_metavar_name s ->
      x
  | xs -> G.Block (fb xs) |> G.s

(*e: function [[Python_to_generic.list_stmt1]] *)

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
  | { G.s = G.ExprStmt (G.Assign (G.N name, _teq, e), _sc); _ } ->
      let vdef = { G.vinit = Some e; vtype = None } in
      let ent = { G.name = G.EN name; attrs = []; tparams = [] } in
      G.FieldStmt (G.DefStmt (ent, G.VarDef vdef) |> G.s)
  | _ -> G.FieldStmt x

(*s: function [[Python_to_generic.stmt_aux]] *)
and stmt_aux x =
  match x with
  | FunctionDef (t, v1, v2, v3, v4, v5) ->
      let v1 = name v1
      and v2 = parameters v2
      and v3 = option type_ v3
      and v4 = list_stmt1 v4
      and v5 = list decorator v5 in
      let ent = G.basic_entity v1 v5 in
      let def =
        { G.fparams = v2; frettype = v3; fbody = v4; fkind = (G.Function, t) }
      in
      [ G.DefStmt (ent, G.FuncDef def) |> G.s ]
  | ClassDef (v0, v1, v2, v3, v4) ->
      let v1 = name v1
      and v2 = list type_parent v2
      and v3 = list stmt v3
      and v4 = list decorator v4 in
      let ent = G.basic_entity v1 v4 in
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
  | Assign (v1, v2, v3) -> (
      let v1 = list expr v1 and v2 = info v2 and v3 = expr v3 in
      match v1 with
      | [] -> raise Impossible
      | [ a ] -> (
          match a with
          (* x: t = ... is definitely a VarDef *)
          | G.Cast (t, G.N (G.Id (id, idinfo))) ->
              let ent =
                { G.name = G.EN (G.Id (id, idinfo)); attrs = []; tparams = [] }
              in
              let var = G.VarDef { G.vinit = Some v3; vtype = Some t } in
              [ G.DefStmt (ent, var) |> G.s ]
          (* TODO: We should turn more Assign in G.VarDef!
           * Is it bad for semgrep to turn only the typed assign in VarDef?
           * No because we have some magic equivalences to convert some
           * Vardef back in Assign in Generic_vs_generic.
           *)
          | _ -> [ G.exprstmt (G.Assign (a, v2, v3)) ] )
      | xs -> [ G.exprstmt (G.Assign (G.Tuple (G.fake_bracket xs), v2, v3)) ] )
  | AugAssign (v1, (v2, tok), v3) ->
      let v1 = expr v1 and v2 = operator v2 and v3 = expr v3 in
      [ G.exprstmt (G.AssignOp (v1, (v2, tok), v3)) ]
  | Return (t, v1) ->
      let v1 = option expr v1 in
      [ G.Return (t, v1, G.sc) |> G.s ]
  | Delete (_t, v1) ->
      let v1 = list expr v1 in
      [ G.OtherStmt (G.OS_Delete, v1 |> List.map (fun x -> G.E x)) |> G.s ]
  | If (t, v1, v2, v3) ->
      let v1 = expr v1 and v2 = list_stmt1 v2 and v3 = option list_stmt1 v3 in
      [ G.If (t, v1, v2, v3) |> G.s ]
  | While (t, v1, v2, v3) -> (
      let v1 = expr v1 and v2 = list_stmt1 v2 and v3 = list stmt v3 in
      match v3 with
      | [] -> [ G.While (t, v1, v2) |> G.s ]
      | _ ->
          [
            G.Block
              (fb
                 [
                   G.While (t, v1, v2) |> G.s;
                   G.OtherStmt
                     (G.OS_WhileOrElse, v3 |> List.map (fun x -> G.S x))
                   |> G.s;
                 ])
            |> G.s;
          ] )
  | For (t, v1, t2, v2, v3, v4) -> (
      let foreach = pattern v1
      and ins = expr v2
      and body = list_stmt1 v3
      and orelse = list stmt v4 in
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
          ] )
  (* TODO: unsugar in sequence? *)
  | With (_t, v1, v2, v3) ->
      let v1 = expr v1 and v2 = option expr v2 and v3 = list_stmt1 v3 in
      let e =
        match v2 with
        | None -> v1
        | Some e2 -> G.LetPattern (H.expr_to_pattern e2, v1)
      in
      [ G.OtherStmtWithStmt (G.OSWS_With, Some e, v3) |> G.s ]
  | Raise (t, v1) -> (
      match v1 with
      | Some (e, None) ->
          let e = expr e in
          [ G.Throw (t, e, G.sc) |> G.s ]
      | Some (e, Some from) ->
          let e = expr e in
          let from = expr from in
          let st = G.Throw (t, e, G.sc) |> G.s in
          [ G.OtherStmt (G.OS_ThrowFrom, [ G.E from; G.S st ]) |> G.s ]
      | None -> [ G.OtherStmt (G.OS_ThrowNothing, [ G.Tk t ]) |> G.s ] )
  | RaisePython2 (t, e, v2, v3) -> (
      let e = expr e in
      let st = G.Throw (t, e, G.sc) |> G.s in
      match (v2, v3) with
      | Some args, Some loc ->
          let args = expr args and loc = expr loc in
          [
            G.OtherStmt (G.OS_ThrowArgsLocation, [ G.E loc; G.E args; G.S st ])
            |> G.s;
          ]
      | Some args, None ->
          let args = expr args in
          [ G.OtherStmt (G.OS_ThrowArgsLocation, [ G.E args; G.S st ]) |> G.s ]
      | None, _ -> [ st ] )
  | TryExcept (t, v1, v2, v3) -> (
      let v1 = list_stmt1 v1
      and v2 = list excepthandler v2
      and orelse = list stmt v3 in
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
          ] )
  | TryFinally (t, v1, t2, v2) ->
      let v1 = list_stmt1 v1 and v2 = list_stmt1 v2 in
      (* could lift down the Try in v1 *)
      [ G.Try (t, v1, [], Some (t2, v2)) |> G.s ]
  | Assert (t, v1, v2) ->
      let v1 = expr v1 and v2 = option expr v2 in
      [ G.Assert (t, v1, v2, G.sc) |> G.s ]
  | ImportAs (t, v1, v2) ->
      let mname = module_name v1 and nopt = option ident_and_id_info v2 in
      [ G.DirectiveStmt (G.ImportAs (t, mname, nopt)) |> G.s ]
  | ImportAll (t, v1, v2) ->
      let mname = module_name v1 and v2 = info v2 in
      [ G.DirectiveStmt (G.ImportAll (t, mname, v2)) |> G.s ]
  | ImportFrom (t, v1, v2) ->
      let v1 = module_name v1 and v2 = list alias v2 in
      List.map
        (fun (a, b) -> G.DirectiveStmt (G.ImportFrom (t, v1, a, b)) |> G.s)
        v2
  | Global (t, v1) | NonLocal (t, v1) ->
      let v1 = list name v1 in
      v1
      |> List.map (fun x ->
             let ent = G.basic_entity x [] in
             G.DefStmt (ent, G.UseOuterDecl t) |> G.s)
  | ExprStmt v1 ->
      let v1 = expr v1 in
      [ G.exprstmt v1 ]
  | Async (t, x) -> (
      let x = stmt x in
      match x.G.s with
      | G.DefStmt (ent, func) ->
          [
            G.DefStmt
              ({ ent with G.attrs = G.attr G.Async t :: ent.G.attrs }, func)
            |> G.s;
          ]
      | _ -> [ G.OtherStmt (G.OS_Async, [ G.S x ]) |> G.s ] )
  | Pass t -> [ G.OtherStmt (G.OS_Pass, [ G.Tk t ]) |> G.s ]
  | Break t -> [ G.Break (t, G.LNone, G.sc) |> G.s ]
  | Continue t -> [ G.Continue (t, G.LNone, G.sc) |> G.s ]
  (* python2: *)
  | Print (tok, _dest, vals, _nl) ->
      let id = Name (("print", tok), Load, ref NotResolved) in
      stmt_aux (ExprStmt (Call (id, fb (vals |> List.map (fun e -> Arg e)))))
  | Exec (tok, e, _eopt, _eopt2) ->
      let id = Name (("exec", tok), Load, ref NotResolved) in
      stmt_aux (ExprStmt (Call (id, fb [ Arg e ])))

(*e: function [[Python_to_generic.stmt_aux]] *)
and ident_and_id_info x =
  let x = name x in
  (x, G.empty_id_info ())

(*s: function [[Python_to_generic.stmt]] *)
and stmt x = G.stmt1 (stmt_aux x)

(*e: function [[Python_to_generic.stmt]] *)

(*s: function [[Python_to_generic.pattern]] *)
and pattern e =
  let e = expr e in
  H.expr_to_pattern e

(*e: function [[Python_to_generic.pattern]] *)

(*s: function [[Python_to_generic.excepthandler]] *)
and excepthandler = function
  | ExceptHandler (t, v1, v2, v3) ->
      let v1 = option expr v1 (* a type actually, even tuple of types *)
      and v2 = option name v2
      and v3 = list_stmt1 v3 in
      ( t,
        ( match (v1, v2) with
        | Some e, None -> (
            match e with
            | G.Ellipsis tok -> G.PatEllipsis tok
            | G.Tuple _ -> G.PatVar (H.expr_to_type e, None)
            | _ ->
                G.PatVar (H.expr_to_type (G.Tuple (G.fake_bracket [ e ])), None)
            )
        | None, None -> G.PatUnderscore (fake "_")
        | None, Some _ -> raise Impossible (* see the grammar *)
        | Some e, Some n ->
            G.PatVar (H.expr_to_type e, Some (n, G.empty_id_info ())) ),
        v3 )

(*e: function [[Python_to_generic.excepthandler]] *)

(*s: function [[Python_to_generic.expr_to_attribute]] *)
(*e: function [[Python_to_generic.expr_to_attribute]] *)

(*s: function [[Python_to_generic.decorator]] *)
and decorator (t, v1, v2) =
  let v1 = dotted_name v1 in
  let v2 = option (bracket (list argument)) v2 in
  let args =
    match v2 with Some (t1, x, t2) -> (t1, x, t2) | None -> G.fake_bracket []
  in
  let name = H.name_of_ids v1 in
  G.NamedAttr (t, name, args)

(*e: function [[Python_to_generic.decorator]] *)

(*s: function [[Python_to_generic.alias]] *)
and alias (v1, v2) =
  let v1 = name v1 and v2 = option ident_and_id_info v2 in
  (v1, v2)

(*e: function [[Python_to_generic.alias]] *)

(*s: function [[Python_to_generic.program]] *)
let program v =
  let v = list stmt v in
  v

(*e: function [[Python_to_generic.program]] *)

(*s: function [[Python_to_generic.any]] *)
let any = function
  | Expr v1 ->
      let v1 = expr v1 in
      G.E v1
  | Stmt v1 -> (
      let v1 = stmt v1 in
      (* in Python Assign is a stmt but in the generic AST it's an expression*)
      match v1.G.s with G.ExprStmt (x, _t) -> G.E x | _ -> G.S v1 )
  (* TODO? should use list stmt_aux here? Some intermediate Block
   * could be inserted preventing some sgrep matching?
   *)
  | Stmts v1 ->
      let v1 = list stmt v1 in
      G.Ss v1
  | Program v1 ->
      let v1 = program v1 in
      G.Pr v1
  | DictElem v1 ->
      let v1 = dictorset_elt v1 in
      G.E v1

(*e: function [[Python_to_generic.any]] *)

(*e: pfff/lang_python/analyze/Python_to_generic.ml *)
