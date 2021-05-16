(* Yoann Padioleau
 *
 * Copyright (C) 2019 r2c
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
module G = AST_generic
module H = AST_generic_helpers
open Cst_cpp
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

let option = Common.map_opt

let list = List.map

let either f g x = match x with Left x -> Left (f x) | Right x -> Right (g x)

let string = id

let fake s = Parse_info.fake_info s

let fb = G.fake_bracket

let opt_to_ident opt =
  match opt with
  | None -> ("FakeNAME", Parse_info.fake_info "FakeNAME")
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
  | GetRef -> fun e -> G.Ref (tok, e)
  | DeRef -> fun e -> G.DeRef (tok, e)
  | UnPlus -> fun e -> G.Call (G.IdSpecial (G.Op G.Plus, tok), fb [ G.Arg e ])
  | UnMinus -> fun e -> G.Call (G.IdSpecial (G.Op G.Minus, tok), fb [ G.Arg e ])
  | Tilde -> fun e -> G.Call (G.IdSpecial (G.Op G.BitNot, tok), fb [ G.Arg e ])
  | Not -> fun e -> G.Call (G.IdSpecial (G.Op G.Not, tok), fb [ G.Arg e ])
  | GetRefLabel -> fun e -> G.OtherExpr (G.OE_GetRefLabel, [ G.E e ])

and assignOp = function
  | SimpleAssign tok -> Left tok
  | OpAssign (v1, tok) ->
      let v1 = arithOp v1 in
      Right (v1, tok)

and fixOp = function Dec -> G.Decr | Inc -> G.Incr

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

let rec type_ = function
  | TBase v1 ->
      let v1 = name v1 in
      G.TyBuiltin v1
  | TPointer (t, v1) ->
      let v1 = type_ v1 in
      G.TyPointer (t, v1)
  | TArray (v1, v2) ->
      let v1 = option const_expr v1 and v2 = type_ v2 in
      G.TyArray (fb v1, v2)
  | TFunction v1 ->
      let ret, params = function_type v1 in
      G.TyFun (params, ret)
  | TStructName (v1, v2) ->
      let v1 = struct_kind v1 and v2 = name v2 in
      G.OtherType (v1, [ G.I v2 ])
  | TEnumName v1 ->
      let v1 = name v1 in
      G.OtherType (G.OT_EnumName, [ G.I v1 ])
  | TTypeName v1 ->
      let v1 = name v1 in
      G.TyN (G.Id (v1, G.empty_id_info ()))
  | TMacroApply (v1, (_lp, v2, _rp)) ->
      let v1 = dotted_ident_of_id v1 in
      let v2 = type_ v2 in
      G.TyNameApply (v1, [ G.TypeArg v2 ])

and dotted_ident_of_id id = [ id ]

and function_type (v1, v2) =
  let v1 = type_ v1 and v2 = list (fun x -> G.ParamClassic (parameter x)) v2 in
  (v1, v2)

and parameter x =
  match x with
  | ParamClassic x -> parameter_classic x
  | ParamDots _ -> raise Todo

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

and struct_kind = function Struct -> G.OT_StructName | Union -> G.OT_UnionName

and expr = function
  | Int v1 ->
      let v1 = wrap id v1 in
      G.L (G.Int v1)
  | Float v1 ->
      let v1 = wrap id v1 in
      G.L (G.Float v1)
  | Bool v1 ->
      let v1 = wrap id v1 in
      G.L (G.Bool v1)
  | String v1 ->
      let v1 = wrap string v1 in
      G.L (G.String v1)
  | Char v1 ->
      let v1 = wrap string v1 in
      G.L (G.Char v1)
  | Null v1 -> G.L (G.Null v1)
  | ConcatString xs ->
      G.Call
        ( G.IdSpecial (G.ConcatString G.SequenceConcat, fake " "),
          fb (xs |> List.map (fun x -> G.Arg (G.L (G.String x)))) )
  | Defined (t, id) ->
      let e = G.N (G.Id (id, G.empty_id_info ())) in
      G.Call (G.IdSpecial (G.Defined, t), fb [ G.Arg e ])
  | Id v1 ->
      let v1 = name v1 in
      G.N (G.Id (v1, G.empty_id_info ()))
  | Ellipses v1 ->
      let v1 = info v1 in
      G.Ellipsis v1
  | DeepEllipsis v1 ->
      let v1 = bracket expr v1 in
      G.DeepEllipsis v1
  | Call (v1, v2) ->
      let v1 = expr v1 and v2 = bracket (list argument) v2 in
      G.Call (v1, v2)
  | Assign (v1, v2, v3) -> (
      let v1 = assignOp v1 and v2 = expr v2 and v3 = expr v3 in
      match v1 with
      | Left tok -> G.Assign (v2, tok, v3)
      | Right (op, tok) -> G.AssignOp (v2, (op, tok), v3) )
  | ArrayAccess (v1, v2) ->
      let v1 = expr v1 and v2 = bracket expr v2 in
      G.ArrayAccess (v1, v2)
  | RecordPtAccess (v1, t, v2) ->
      let v1 = expr v1 and t = info t and v2 = name v2 in
      G.DotAccess (G.DeRef (t, v1), t, G.EN (Id (v2, G.empty_id_info ())))
  | Cast (v1, v2) ->
      let v1 = type_ v1 and v2 = expr v2 in
      G.Cast (v1, v2)
  | Postfix (v1, (v2, v3)) ->
      let v1 = expr v1 and v2 = fixOp v2 in
      G.Call (G.IdSpecial (G.IncrDecr (v2, G.Postfix), v3), fb [ G.Arg v1 ])
  | Infix (v1, (v2, v3)) ->
      let v1 = expr v1 and v2 = fixOp v2 in
      G.Call (G.IdSpecial (G.IncrDecr (v2, G.Prefix), v3), fb [ G.Arg v1 ])
  | Unary (v1, v2) ->
      let v1 = expr v1 and v2 = unaryOp v2 in
      v2 v1
  | Binary (v1, (v2, tok), v3) ->
      let v1 = expr v1 and v2 = binaryOp v2 and v3 = expr v3 in
      G.Call (G.IdSpecial (G.Op v2, tok), fb [ G.Arg v1; G.Arg v3 ])
  | CondExpr (v1, v2, v3) ->
      let v1 = expr v1 and v2 = expr v2 and v3 = expr v3 in
      G.Conditional (v1, v2, v3)
  | Sequence (v1, v2) ->
      let v1 = expr v1 and v2 = expr v2 in
      G.Seq [ v1; v2 ]
  | SizeOf (t, v1) ->
      let v1 = either expr type_ v1 in
      G.Call
        ( G.IdSpecial (G.Sizeof, t),
          match v1 with
          | Left e -> fb [ G.Arg e ]
          | Right t -> fb [ G.ArgType t ] )
  | ArrayInit v1 ->
      let v1 =
        bracket
          (list (fun (v1, v2) ->
               let v1 = option expr v1 and v2 = expr v2 in
               match v1 with
               | None -> v2
               | Some e ->
                   G.OtherExpr (G.OE_ArrayInitDesignator, [ G.E e; G.E v2 ])))
          v1
      in
      G.Container (G.Array, v1)
  | RecordInit v1 ->
      let v1 =
        bracket
          (list (fun (v1, v2) ->
               let v1 = name v1 and v2 = expr v2 in
               G.basic_field v1 (Some v2) None))
          v1
      in
      G.Record v1
  | GccConstructor (v1, v2) ->
      let v1 = type_ v1 and v2 = expr v2 in
      G.Call
        ( G.IdSpecial (G.New, fake "new"),
          fb (G.ArgType v1 :: ([ v2 ] |> List.map G.arg)) )
  | TypedMetavar (v1, v2) ->
      let v1 = name v1 in
      let v2 = type_ v2 in
      G.TypedMetavar (v1, Parse_info.fake_info " ", v2)

and argument v =
  match v with
  | Arg v ->
      let v = expr v in
      G.Arg v

and const_expr v = expr v

let rec stmt st =
  ( match st with
  | DefStmt x -> definition x
  | DirStmt x -> directive x
  | CaseStmt x -> case_stmt x
  | ExprSt (v1, t) ->
      let v1 = expr v1 in
      G.ExprStmt (v1, t)
  | Block v1 ->
      let v1 = bracket (list stmt) v1 in
      G.Block v1
  | If (t, v1, v2, v3) ->
      let v1 = expr v1 and v2 = stmt v2 and v3 = option stmt v3 in
      G.If (t, v1, v2, v3)
  | Switch (v0, v1, v2) ->
      let v0 = info v0 in
      let v1 = expr v1
      and v2 = list case v2 |> List.map (fun x -> G.CasesAndBody x) in
      G.Switch (v0, Some v1, v2)
  | While (t, v1, v2) ->
      let v1 = expr v1 and v2 = stmt v2 in
      G.While (t, v1, v2)
  | DoWhile (t, v1, v2) ->
      let v1 = stmt v1 and v2 = expr v2 in
      G.DoWhile (t, v1, v2)
  | For (t, v1, v2, v3, v4) ->
      let init = expr_or_vars v1
      and v2 = option expr v2
      and v3 = option expr v3
      and v4 = stmt v4 in
      let header = G.ForClassic (init, v2, v3) in
      G.For (t, header, v4)
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
      G.Goto (t, v1)
  | Vars v1 ->
      let v1 = list var_decl v1 in
      (G.stmt1 (v1 |> List.map (fun v -> G.s (G.DefStmt v)))).G.s
  | Asm v1 ->
      let v1 = list expr v1 in
      G.OtherStmt (G.OS_Asm, v1 |> List.map (fun e -> G.E e)) )
  |> G.s

and expr_or_vars v1 =
  match v1 with
  | Right e ->
      let e = expr e in
      [ G.ForInitExpr e ]
  | Left _varsTODO -> []

(* todo: should use OtherStmtWithStmt really *)
and case_stmt = function
  | Case (t, e, st) ->
      let e = expr e in
      let st = list stmt st in
      G.OtherStmt (G.OS_Todo, [ G.TodoK ("case", t); G.E e; G.Ss st ])
  | Default (t, st) ->
      let st = list stmt st in
      G.OtherStmt (G.OS_Todo, [ G.TodoK ("default", t); G.Ss st ])

and case = function
  | Case (t, v1, v2) ->
      let v1 = expr v1 and v2 = list stmt v2 in
      ([ G.Case (t, H.expr_to_pattern v1) ], G.stmt1 v2)
  | Default (t, v1) ->
      let v1 = list stmt v1 in
      ([ G.Default t ], G.stmt1 v1)

and var_decl
    { v_name = xname; v_type = xtype; v_storage = xstorage; v_init = init } =
  let v1 = name xname in
  let v2 = type_ xtype in
  let v3 = storage xstorage in
  let v4 = option initialiser init in
  let entity = G.basic_entity v1 v3 in
  (entity, G.VarDef { G.vinit = v4; vtype = Some v2 })

and initialiser v = expr v

and storage = function
  | Extern -> [ G.attr G.Extern (fake "extern") ]
  | Static -> [ G.attr G.Static (fake "static") ]
  | DefaultStorage -> []

and func_def { f_name; f_type; f_body; f_static } =
  let v1 = name f_name in
  let ret, params = function_type f_type in
  let v3 = bracket (list stmt) f_body in
  let v4 = if f_static then [ G.attr G.Static (fake "static") ] else [] in
  let entity = G.basic_entity v1 v4 in
  ( entity,
    G.FuncDef
      {
        G.fparams = params;
        frettype = Some ret;
        fbody = G.s (G.Block v3);
        fkind = (G.Function, G.fake "");
      } )

and struct_def { s_name; s_kind; s_flds } =
  let v1 = name s_name in
  let v3 = bracket (list field_def) s_flds in
  let entity = G.basic_entity v1 [] in
  match s_kind with
  | Struct ->
      let fields =
        bracket (List.map (fun (n, t) -> G.basic_field n None (Some t))) v3
      in
      (entity, G.TypeDef { G.tbody = G.AndType fields })
  | Union ->
      let ctors =
        v3 |> G.unbracket |> List.map (fun (n, t) -> G.OrUnion (n, t))
      in
      (entity, G.TypeDef { G.tbody = G.OrType ctors })

and field_def { fld_name; fld_type } =
  let v1 = option name fld_name in
  let v2 = type_ fld_type in
  (opt_to_ident v1, v2)

and enum_def { e_name = v1; e_consts = v2 } =
  let v1 = name v1
  and v2 =
    list
      (fun (v1, v2) ->
        let v1 = name v1 and v2 = option const_expr v2 in
        (v1, v2))
      v2
  in
  let entity = G.basic_entity v1 [] in
  let ors = v2 |> List.map (fun (n, eopt) -> G.OrEnum (n, eopt)) in
  (entity, G.TypeDef { G.tbody = G.OrType ors })

and type_def { t_name = v1; t_type = v2 } =
  let v1 = name v1 and v2 = type_ v2 in
  let entity = G.basic_entity v1 [] in
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
  | Include (t, v1) ->
      let v1 = wrap string v1 in
      G.DirectiveStmt (G.ImportAs (t, G.FileName v1, None))
  | Define (_t, v1, v2) ->
      let v1 = name v1 and v2 = define_body v2 in
      let ent = G.basic_entity v1 [] in
      G.DefStmt (ent, G.MacroDef { G.macroparams = []; G.macrobody = v2 })
  | Macro (_t, v1, v2, v3) ->
      let v1 = name v1 and v2 = list name v2 and v3 = define_body v3 in
      let ent = G.basic_entity v1 [] in
      G.DefStmt (ent, G.MacroDef { G.macroparams = v2; G.macrobody = v3 })
  | OtherDirective (v1, v2) ->
      let v1 = name v1 in
      let v2 =
        match v2 with None -> [] | Some s -> [ G.E (G.L (G.String s)) ]
      in
      G.DirectiveStmt (G.Pragma (v1, v2))

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
