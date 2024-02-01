(* Yoann Padioleau
 *
 * Copyright (C) 2019 r2c
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
open Either_
module G = AST_generic
module H = AST_generic_helpers
open Ast_cpp
open Ast_c

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Ast_c to AST_generic.
 *
 * See ast_generic.ml for more information.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let id x = x
let option = Option.map
let list = List_.map
let string = id
let fake tok s = Tok.fake_tok tok s
let unsafe_fake s = Tok.unsafe_fake_tok s
let fb = Tok.unsafe_fake_bracket

let opt_to_ident opt =
  match opt with
  | None -> ("FakeNAME", Tok.unsafe_fake_tok "FakeNAME")
  | Some n -> n

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let info x = x

let wrap _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = info v2 in
  (v1, v2)

let bracket of_a (t1, x, t2) = (info t1, of_a x, info t2)
let name v = wrap string v

let rec unaryOp (a, tok) =
  match a with
  | GetRef -> fun e -> G.Ref (tok, e) |> G.e
  | DeRef -> fun e -> G.DeRef (tok, e) |> G.e
  | UnPlus -> fun e -> G.opcall (G.Plus, tok) [ e ]
  | UnMinus -> fun e -> G.opcall (G.Minus, tok) [ e ]
  | Tilde -> fun e -> G.opcall (G.BitNot, tok) [ e ]
  | Not -> fun e -> G.opcall (G.Not, tok) [ e ]
  | GetRefLabel ->
      fun e -> G.OtherExpr (("GetRefLabel", unsafe_fake ""), [ G.E e ]) |> G.e

and assignOp = function
  | SimpleAssign tok -> Left tok
  | OpAssign (v1, tok) ->
      let v1 = arithOp v1 in
      Right (v1, tok)

and fixOp = function
  | Dec -> G.Decr
  | Inc -> G.Incr

and binaryOp = function
  | Arith v1 ->
      let v1 = arithOp v1 in
      v1
  | Logical v1 ->
      let v1 = logicalOp v1 in
      v1

and arithOp = function
  | Plus -> G.Plus
  | Minus -> G.Minus
  | Mul -> G.Mult
  | Div -> G.Div
  | Mod -> G.Mod
  | DecLeft -> G.LSL
  | DecRight -> G.LSR
  | And -> G.BitAnd
  | Or -> G.BitOr
  | Xor -> G.BitXor

and logicalOp = function
  | Inf -> G.Lt
  | Sup -> G.Gt
  | InfEq -> G.LtE
  | SupEq -> G.GtE
  | Eq -> G.Eq
  | NotEq -> G.NotEq
  | AndLog -> G.And
  | OrLog -> G.Or
  | Spaceship -> G.Cmp

and type_ = function
  | TBase v1 ->
      let v1 = name v1 in
      G.ty_builtin v1
  | TPointer (t, v1) ->
      let v1 = type_ v1 in
      G.TyPointer (t, v1) |> G.t
  | TArray (v1, v2) ->
      let v1 = option const_expr v1 and v2 = type_ v2 in
      G.TyArray (fb v1, v2) |> G.t
  | TFunction v1 ->
      let ret, params = function_type v1 in
      G.TyFun (params, ret) |> G.t
  | TStructName (v1, v2) ->
      let v1 = struct_kind v1 and v2 = name v2 in
      G.OtherType (v1, [ G.I v2 ]) |> G.t
  | TEnumName v1 ->
      let v1 = name v1 in
      G.OtherType (("EnumName", unsafe_fake ""), [ G.I v1 ]) |> G.t
  | TTypeName v1 ->
      let v1 = name v1 in
      G.TyN (H.name_of_id v1) |> G.t
  | TMacroApply (v1, (lp, v2, rp)) ->
      let v1 = H.name_of_id v1 in
      let v2 = type_ v2 in
      G.TyApply (G.TyN v1 |> G.t, (lp, [ G.TA v2 ], rp)) |> G.t

and function_type (v1, v2) =
  let v1 = type_ v1 and v2 = list (fun x -> parameter x) v2 in
  (v1, v2)

and parameter x =
  match x with
  | ParamClassic x -> G.Param (parameter_classic x)
  | ParamDots t -> G.ParamEllipsis t

and parameter_classic { p_type; p_name } =
  let arg1 = type_ p_type in
  let arg2 = option name p_name in
  {
    G.ptype = Some arg1;
    pname = arg2;
    pattrs = [];
    pdefault = None;
    pinfo = G.empty_id_info ();
  }

and struct_kind = function
  | Struct -> ("StructName", unsafe_fake "")
  | Union -> ("UnionName", unsafe_fake "")

and expr e =
  match e with
  | Int pi -> G.L (G.Int pi) |> G.e
  | Float v1 ->
      let v1 = wrap id v1 in
      G.L (G.Float v1) |> G.e
  | Bool v1 ->
      let v1 = wrap id v1 in
      G.L (G.Bool v1) |> G.e
  | String v1 ->
      let v1 = wrap string v1 in
      G.L (G.String (fb v1)) |> G.e
  | Char v1 ->
      let v1 = wrap string v1 in
      G.L (G.Char v1) |> G.e
  | Null v1 -> G.L (G.Null v1) |> G.e
  | ConcatString xs ->
      let xs = list string_component xs in
      G.interpolated (fb xs)
  | Defined (t, id) ->
      let e = G.N (G.Id (id, G.empty_id_info ())) |> G.e in
      G.Call (G.IdSpecial (G.Defined, t) |> G.e, fb [ G.Arg e ]) |> G.e
  | Id v1 ->
      let v1 = name v1 in
      G.N (G.Id (v1, G.empty_id_info ())) |> G.e
  | IdSpecial x ->
      let v1 = special_wrap x in
      v1
  | Ellipses v1 ->
      let v1 = info v1 in
      G.Ellipsis v1 |> G.e
  | DeepEllipsis v1 ->
      let v1 = bracket expr v1 in
      G.DeepEllipsis v1 |> G.e
  | Call (v1, v2) ->
      let v1 = expr v1 and v2 = bracket (list argument) v2 in
      G.Call (v1, v2) |> G.e
  | Assign (v1, v2, v3) -> (
      let v1 = assignOp v1 and v2 = expr v2 and v3 = expr v3 in
      match v1 with
      | Left tok -> G.Assign (v2, tok, v3) |> G.e
      | Right (op, tok) -> G.AssignOp (v2, (op, tok), v3) |> G.e)
  | ArrayAccess (v1, v2) ->
      let v1 = expr v1 and v2 = bracket expr v2 in
      G.ArrayAccess (v1, v2) |> G.e
  | RecordPtAccess (v1, t, v2) ->
      let v1 = expr v1 and t = info t and v2 = name v2 in
      G.DotAccess (G.DeRef (t, v1) |> G.e, t, G.FN (Id (v2, G.empty_id_info ())))
      |> G.e
  | Cast (v1, v2) ->
      let v1 = type_ v1 and v2 = expr v2 in
      G.Cast (v1, unsafe_fake "(", v2) |> G.e
  | Postfix (v1, (v2, v3)) ->
      let v1 = expr v1 and v2 = fixOp v2 in
      G.Call
        (G.IdSpecial (G.IncrDecr (v2, G.Postfix), v3) |> G.e, fb [ G.Arg v1 ])
      |> G.e
  | Infix (v1, (v2, v3)) ->
      let v1 = expr v1 and v2 = fixOp v2 in
      G.Call
        (G.IdSpecial (G.IncrDecr (v2, G.Prefix), v3) |> G.e, fb [ G.Arg v1 ])
      |> G.e
  | Unary (v1, v2) ->
      let v1 = expr v1 and v2 = unaryOp v2 in
      v2 v1
  | Binary (v1, (v2, tok), v3) ->
      let v1 = expr v1 and v2 = binaryOp v2 and v3 = expr v3 in
      G.Call (G.IdSpecial (G.Op v2, tok) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
      |> G.e
  | CondExpr (v1, v2, v3) -> (
      (* This is a GCC extension that allows x ? : y to be equivalent to
         x ? x : y.
         This is actually kind of problematic because if we naively copy
         `x`, we could reach exponentially sized trees for certain pathological
         inputs.
         So we'll just do an OtherExpr for now.
      *)
      let v1 = expr v1
      and v2 = option expr v2
      and v3 = expr v3 in
      match v2 with
      | Some v2 -> G.Conditional (v1, v2, v3) |> G.e
      | None ->
          G.OtherExpr
            ( ("ConditionalNoMiddle", G.fake "ConditionalNoMiddle"),
              [ G.E v1; G.E v3 ] )
          |> G.e)
  | Sequence (v1, v2) ->
      let v1 = expr v1 and v2 = expr v2 in
      G.Seq [ v1; v2 ] |> G.e
  | ArrayInit v1 ->
      let v1 =
        bracket
          (list (fun (v1, v2) ->
               let v1 = option expr v1 and v2 = expr v2 in
               match v1 with
               | None -> v2
               | Some e ->
                   G.OtherExpr
                     (("ArrayInitDesignator", unsafe_fake ""), [ G.E e; G.E v2 ])
                   |> G.e))
          v1
      in
      G.Container (G.Array, v1) |> G.e
  | RecordInit v1 ->
      let v1 =
        bracket
          (list (fun (v1, v2) ->
               let v1 = name v1 and v2 = expr v2 in
               G.basic_field v1 (Some v2) None))
          v1
      in
      G.Record v1 |> G.e
  | GccConstructor (v1, v2) ->
      let v1 = type_ v1 and v2 = expr v2 in
      G.New
        ( unsafe_fake "new",
          v1,
          G.empty_id_info (),
          fb ([ v2 ] |> List_.map G.arg) )
      |> G.e
  | Generic (tk, (_l, (e, assocs), _r)) ->
      let e = expr e in
      let cond =
        G.Cond (Call (IdSpecial (Typeof, tk) |> G.e, fb [ G.Arg e ]) |> G.e)
      in
      let cases =
        List_.map
          (fun (ty, e) ->
            G.CasesAndBody
              ( [ G.Case (G.fake "", G.PatType (type_ ty)) ],
                G.ExprStmt (expr e, G.sc) |> G.s ))
          assocs
      in
      StmtExpr (G.Switch (tk, Some cond, cases) |> G.s) |> G.e
  | TypedMetavar (v1, v2) ->
      let v1 = name v1 in
      let v2 = type_ v2 in
      G.TypedMetavar (v1, fake (snd v1) " ", v2) |> G.e
  | DotAccessEllipsis (e, tk) -> G.DotAccessEllipsis (expr e, tk) |> G.e

and special_wrap (spec, tk) =
  match spec with
  | SizeOf -> G.IdSpecial (G.Sizeof, tk) |> G.e
  | OffsetOf -> G.N (Id (("offsetof", tk), G.empty_id_info ())) |> G.e
  | AlignOf -> N (Id (("alignof", tk), G.empty_id_info ())) |> G.e

and string_component = function
  | StrIdent id -> Either_.Middle3 (G.N (Id (id, G.empty_id_info ())) |> G.e)
  | StrLit s -> Either_.Left3 s

and argument v =
  match v with
  | Arg v ->
      let v = expr v in
      G.Arg v
  | ArgType v ->
      let v = type_ v in
      G.ArgType v
  | ArgBlock (l, stmts, r) ->
      Arg (StmtExpr (Block (l, list stmt stmts, r) |> G.s) |> G.e)

and const_expr v = expr v

and stmt st =
  (match st with
  | DefStmt x -> definition x
  | DirStmt x -> directive x
  | CaseStmt x ->
      (* should not happen, should only appear in Switch *)
      let case, st = case_and_body x in
      let anys = [ case ] |> List_.map (fun cs -> G.Cs cs) in
      G.OtherStmtWithStmt (OSWS_Todo, anys, st)
  | ExprSt (v1, t) ->
      let v1 = expr v1 in
      G.ExprStmt (v1, t)
  | Block v1 ->
      let v1 = bracket (list stmt) v1 in
      G.Block v1
  | If (t, v1, v2, v3) ->
      let v1 = expr v1 and v2 = stmt v2 and v3 = option stmt v3 in
      G.If (t, G.Cond v1, v2, v3)
  | Switch (v0, v1, v2) ->
      let v0 = info v0 in
      let v1 = expr v1 and v2 = list case_and_body v2 in
      let cases =
        v2 |> List_.map (fun (case, body) -> G.CasesAndBody ([ case ], body))
      in
      G.Switch (v0, Some (G.Cond v1), cases)
  | While (t, v1, v2) ->
      let v1 = expr v1 and v2 = stmt v2 in
      G.While (t, G.Cond v1, v2)
  | DoWhile (t, v1, v2) ->
      let v1 = stmt v1 and v2 = expr v2 in
      G.DoWhile (t, v1, v2)
  | For (t, header, st) ->
      let header =
        match header with
        | ForClassic (v1, v2, v3) ->
            let init = expr_or_vars v1 in
            let v2 = option expr v2 in
            let v3 = option expr v3 in
            G.ForClassic (init, v2, v3)
        | ForEllipsis v1 -> G.ForEllipsis v1
      in
      let st = stmt st in
      G.For (t, header, st)
  | Return (t, v1) ->
      let v1 = option expr v1 in
      G.Return (t, v1, G.sc)
  | Continue t -> G.Continue (t, G.LNone, G.sc)
  | Break t -> G.Break (t, G.LNone, G.sc)
  | Label (v1, v2) ->
      let v1 = name v1 and v2 = stmt v2 in
      G.Label (v1, v2)
  | Goto (t, v1) ->
      let v1 = name v1 in
      G.Goto (t, v1, G.sc)
  | Vars v1 ->
      let v1 = list var_decl v1 in
      (G.stmt1 (v1 |> List_.map (fun v -> G.s (G.DefStmt v)))).G.s
  | AsmStmt
      ( asm_tk,
        (_l, { a_template; a_outputs; a_inputs; a_clobbers; a_gotos }, _r),
        sc ) ->
      let a_template = [ G.E (expr a_template) ] in
      let a_outputs = List.concat_map name_asm_operand a_outputs in
      let a_inputs = List.concat_map expr_asm_operand a_inputs in
      let a_clobbers = List_.map (fun x -> G.I (name x)) a_clobbers in
      let a_gotos = List_.map (fun x -> G.I (name x)) a_gotos in
      G.OtherStmt
        ( G.OS_Asm,
          [ G.Tk asm_tk ] @ a_template @ a_outputs @ a_inputs @ a_clobbers
          @ a_gotos @ [ G.Tk sc ] )
  | PreprocStmt { p_condition; p_stmts; p_elifs; p_else; p_endif = _ } -> (
      (* We take every
         #ifdef <x>
            <stuff>
         #elifdef <y>
            <stuff2>
         #endif
         and translate it to a series of conditionals, like

         if (<x>) {
           <stuff>
         } else {
           if (<y>) {
             <stuff2>
           }
         }

         where each conditional is an OtherCond.

         We may desire to treat this differently in the future, as this is
         not the same as how C++ represents macros.
      *)
      let top_tk, cond =
        match p_condition with
        | PreprocIfdef (tk, n) ->
            (tk, G.OtherCond (("ifdef", G.fake "ifdef"), [ G.I (name n) ]))
        | PreprocIf (tk, e) ->
            (tk, OtherCond (("if", G.fake "if"), [ G.E (expr e) ]))
      in
      let elifs =
        List_.map
          (fun (tk, e, stmts) -> (tk, G.Cond (expr e), list stmt stmts))
          p_elifs
      in
      let p_else =
        match p_else with
        | None -> None
        | Some stmts -> Some (G.Block (fb (list stmt stmts)))
      in
      match
        List.fold_right
          (fun (tk, cond, stmts) acc ->
            match acc with
            | None -> Some (G.If (tk, cond, Block (fb stmts) |> G.s, None))
            | Some old_s ->
                Some
                  (G.If (tk, cond, Block (fb stmts) |> G.s, Some (old_s |> G.s))))
          ((top_tk, cond, List_.map stmt p_stmts) :: elifs)
          p_else
      with
      | None -> failwith "impossible"
      | Some stmt -> stmt))
  |> G.s

and expr_asm_operand (v1, v2, v3) =
  let v1 =
    match option (bracket name) v1 with
    | None -> []
    | Some (_, n, _) -> [ G.I n ]
  in
  let v2 = name v2 in
  let _, v3, _ = bracket expr v3 in
  v1 @ [ G.I v2 ] @ [ G.E v3 ]

and name_asm_operand (v1, v2, v3) =
  let v1 =
    match option (bracket name) v1 with
    | None -> []
    | Some (_, n, _) -> [ G.I n ]
  in
  let v2 = name v2 in
  let _, v3, _ = bracket name v3 in
  v1 @ [ G.I v2 ] @ [ G.I v3 ]

and expr_or_vars v1 =
  match v1 with
  | Right e ->
      let e = expr e in
      [ G.ForInitExpr e ]
  | Left _varsTODO -> []

and case_and_body = function
  | Case (t, v1, v2) ->
      let v1 = expr v1 and v2 = list stmt v2 in
      (G.Case (t, H.expr_to_pattern v1), G.stmt1 v2)
  | Default (t, v1) ->
      let v1 = list stmt v1 in
      (G.Default t, G.stmt1 v1)

and var_decl
    { v_name = xname; v_type = xtype; v_storage = xstorage; v_init = init } =
  let v1 = name xname in
  let v2 = type_ xtype in
  let v3 = storage (snd v1) xstorage in
  let v4 = option initialiser init in
  let entity = G.basic_entity v1 ~attrs:v3 in
  (entity, G.VarDef { G.vinit = v4; vtype = Some v2 })

and initialiser v = expr v

and storage tok = function
  | Extern -> [ G.attr G.Extern (fake tok "extern") ]
  | Static -> [ G.attr G.Static (fake tok "static") ]
  | DefaultStorage -> []

and func_def { f_name; f_type; f_body; f_static } =
  let v1 = name f_name in
  let ret, params = function_type f_type in
  let v3 = bracket (list stmt) f_body in
  let v4 =
    if f_static then [ G.attr G.Static (fake (snd v1) "static") ] else []
  in
  let entity = G.basic_entity v1 ~attrs:v4 in
  let body =
    match v3 with
    (* explicitly mark that this funcdef is a prototype. *)
    | _, [], _ -> G.FBNothing
    | _ -> G.FBStmt (G.s (G.Block v3))
  in
  ( entity,
    G.FuncDef
      {
        G.fparams = fb params;
        frettype = Some ret;
        fbody = body;
        fkind = (G.Function, G.fake "");
      } )

and struct_def { s_name; s_kind; s_flds } =
  let v1 = name s_name in
  let v3 = bracket (list field_def) s_flds in
  let entity = G.basic_entity v1 in
  match s_kind with
  | Struct ->
      let fields =
        bracket (List_.map (fun (n, t) -> G.basic_field n None (Some t))) v3
      in
      (entity, G.TypeDef { G.tbody = G.AndType fields })
  | Union ->
      let ctors =
        v3 |> Tok.unbracket |> List_.map (fun (n, t) -> G.OrUnion (n, t))
      in
      (entity, G.TypeDef { G.tbody = G.OrType ctors })

and field_def { fld_name; fld_type } =
  let v1 = option name fld_name in
  let v2 = type_ fld_type in
  (opt_to_ident v1, v2)

and enum_def { e_name = v1; e_type = _v2_TODO; e_consts = v3 } =
  let v1 = name v1
  and v3 =
    list
      (fun (v1, v2) ->
        let v1 = name v1 and v2 = option const_expr v2 in
        (v1, v2))
      v3
  in
  let entity = G.basic_entity v1 in
  let ors = v3 |> List_.map (fun (n, eopt) -> G.OrEnum (n, eopt)) in
  (entity, G.TypeDef { G.tbody = G.OrType ors })

and type_def { t_name = v1; t_type = v2 } =
  let v1 = name v1 and v2 = type_ v2 in
  let entity = G.basic_entity v1 in
  (entity, G.TypeDef { G.tbody = G.AliasType v2 })

and define_body = function
  | None -> []
  | Some (CppExpr v1) ->
      let v1 = expr v1 in
      [ G.E v1 ]
  | Some (CppStmt v1) ->
      let v1 = stmt v1 in
      [ G.S v1 ]

and directive = function
  | Include (t, IncludePath v1) ->
      let v1 = wrap string v1 in
      G.DirectiveStmt (G.ImportAll (t, G.FileName v1, fake t "") |> G.d)
  | Include (t, IncludeCall v1) ->
      let _v1 = expr v1 in
      G.DirectiveStmt
        (G.ImportAll
           (t, G.FileName ("PREPROC_EXPR", fake t "PREPROC_EXPR"), fake t "")
        |> G.d)
  | Define (_t, v1, v2) ->
      let v1 = name v1 and v2 = define_body v2 in
      let ent = G.basic_entity v1 in
      G.DefStmt (ent, G.MacroDef { G.macroparams = []; G.macrobody = v2 })
  | Macro (_t, v1, v2, v3) ->
      let v1 = name v1 and v2 = list name v2 and v3 = define_body v3 in
      let ent = G.basic_entity v1 in
      G.DefStmt (ent, G.MacroDef { G.macroparams = v2; G.macrobody = v3 })
  | OtherDirective (v1, v2) ->
      let v1 = name v1 in
      let v2 =
        match v2 with
        | None -> []
        | Some s -> [ G.E (G.L (G.String (fb s)) |> G.e) ]
      in
      G.DirectiveStmt (G.Pragma (v1, v2) |> G.d)

and definition = function
  | StructDef v1 ->
      let v1 = struct_def v1 in
      G.DefStmt v1
  | TypeDef v1 ->
      let v1 = type_def v1 in
      G.DefStmt v1
  | EnumDef v1 ->
      let v1 = enum_def v1 in
      G.DefStmt v1
  | FuncDef v1 ->
      let v1 = func_def v1 in
      G.DefStmt v1
  | VarDef v1 ->
      let v1 = var_decl v1 in
      G.DefStmt v1
  | Prototype v1 ->
      let v1 = func_def v1 in
      G.DefStmt v1

let toplevel x = stmt x
let program v = list toplevel v

let any = function
  | Expr v1 ->
      let v1 = expr v1 in
      G.E v1
  | Stmt v1 ->
      let v1 = stmt v1 in
      G.S v1
  | Stmts v1 ->
      let v1 = list stmt v1 in
      G.Ss v1
  | Type v1 ->
      let v1 = type_ v1 in
      G.T v1
  | Program v1 ->
      let v1 = program v1 in
      G.Pr v1
