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

open Ast_go
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Ast_go to AST_generic.
 *
 * See ast_generic.ml for more information.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let id = fun x -> x
let string = id
let list = List.map
let option = Common.map_opt
let either = OCaml.map_of_either

let arithmetic_operator = id
let incr_decr = id
let prefix_postfix = id

let error = AST_generic.error

let name_of_qualified_ident = function
  | Left id -> id, G.empty_name_info
  | Right (xs, id) -> id, 
      { G.name_qualifier = Some (G.QDots xs); name_typeargs = None }

let fake s = Parse_info.fake_info s
let fake_id s = (s, fake s)
let fb = G.fake_bracket
let fake_name s = (s, fake s), G.empty_name_info
let mk_name s tok = (s, tok), G.empty_name_info

let ii_of_any = Lib_parsing_go.ii_of_any

(* TODO? do results "parameters" can have names? *)
let return_type_of_results results = 
  match results with
  | [] | [G.ParamClassic {G. ptype = None; _}] -> None
  | [G.ParamClassic {G. ptype = Some t; _}] -> Some t
  | xs -> Some (G.TyTuple (xs |> List.map (function
            | G.ParamClassic { G.ptype = Some t;_ } -> t
            | G.ParamClassic { G.ptype = None; _ } -> raise Impossible
            | _ -> raise Impossible
            ) |> fb))
  

let list_to_tuple_or_expr xs =
  match xs with
  | [] -> raise Impossible
  | [x] -> x
  | xs -> G.Tuple xs

let mk_func_def params ret st =
 { G.
    fparams = params;
    frettype = ret;
    fbody = st;
  }

let wrap_init_in_block_maybe x v =
  match x with
  | None -> [v]
  | Some e -> [G.Block (fb [G.exprstmt e;v])]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tok v = v

let wrap _of_a (v1, v2) = 
  let v1 = _of_a v1 and v2 = tok v2 in 
  (v1, v2)

let bracket of_a (t1, x, t2) = (tok t1, of_a x, tok t2)

let ident v = wrap string v

let qualified_ident v = 
  match list ident v with
  | [x] -> Left x
  | [x;y] -> Right ([x], y)
  | _ -> raise Impossible

let top_func () = 
let anon_types = ref [] in
let cnt = ref 0 in
let gensym () = 
  incr cnt;
  spf "_anon%d" !cnt
in

let rec type_ =
  function
  | TName v1 -> let v1 = qualified_ident v1 in
      G.TyName (name_of_qualified_ident v1)
  | TPtr (t, v1) -> let v1 = type_ v1 in 
      G.TyPointer (t, v1)
  | TArray ((v1, v2)) -> let v1 = expr v1 and v2 = type_ v2 in 
      G.TyArray (Some v1, v2)
  | TSlice v1 -> let v1 = type_ v1 in 
      G.TyArray (None, v1) (* not sure worth introducing an OT_Slice *)
  | TArrayEllipsis ((v1, v2)) -> let _v1 = tok v1 and v2 = type_ v2 in
      G.TyArray (None, v2)
  | TFunc v1 -> let (params, ret) = func_type v1 in 
      let ret = 
        match ret with
        | None -> G.TyBuiltin (fake_id "void")
        | Some t -> t
      in
      let params = params |> Common.map_filter (function
           | G.ParamClassic x -> Some x
           | _ -> None
       ) in
      G.TyFun (params, ret)
  | TMap ((t, v1, v2)) -> let v1 = type_ v1 and v2 = type_ v2 in 
      G.TyNameApply (mk_name "map" t, [G.TypeArg v1; G.TypeArg v2])
  | TChan ((t, v1, v2)) -> let v1 = chan_dir v1 and v2 = type_ v2 in 
      G.TyNameApply (mk_name "chan" t, [G.TypeArg v1; G.TypeArg v2])

  | TStruct (_t, v1) -> let v1 = bracket (list struct_field) v1 in 
      G.TyRecordAnon v1
  | TInterface (t, v1) -> let v1 = bracket (list interface_field) v1 in 
      let s = gensym () in
      let ent = G.basic_entity (s, t) [] in
      let def = G.ClassDef { G.ckind = (G.Interface, t); 
          cextends = []; cimplements = []; cmixins = [];
          cbody = v1; } in
      Common.push (ent, def) anon_types;
      G.TyName (mk_name s t)

and chan_dir = function 
  | TSend -> G.TyName (fake_name "send")
  | TRecv -> G.TyName (fake_name "recv") 
  | TBidirectional -> G.TyName (fake_name "bidirectional")

and func_type { fparams = fparams; fresults = fresults } =
  let fparams = list parameter_binding fparams in
  let fresults = list parameter_binding fresults in
  fparams, return_type_of_results fresults

and parameter_binding x = 
 match x with
 | ParamClassic x -> G.ParamClassic (parameter x)
 | ParamEllipsis t -> G.ParamEllipsis t

and parameter x = 
  match x with
  { pname = pname; ptype = ptype; pdots = pdots } ->
  let arg1 = option ident pname in
  let arg2 = type_ ptype in 
  let arg3 = option tok pdots in 
  { G.pname = arg1; ptype = Some arg2; 
    pdefault = None; 
    pattrs = (match arg3 with None -> [] | Some tok -> [G.attr G.Variadic tok]);
    pinfo = G.empty_id_info ()
    }


and struct_field (v1, v2) =
  let v1 = struct_field_kind v1 and _v2TODO = option tag v2 in
  v1

and struct_field_kind =
  function
  | Field ((v1, v2)) -> let v1 = ident v1 and v2 = type_ v2 in 
      G.basic_field v1 None (Some v2)
  | EmbeddedField ((v1, v2)) ->
      let _v1TODO = option tok v1 and v2 = qualified_ident v2 in
      let name = name_of_qualified_ident v2 in
      G.FieldSpread (fake "...", G.IdQualified (name, G.empty_id_info()))
  | FieldEllipsis t ->
      G.fieldEllipsis t

and tag v = wrap string v

and interface_field =
  function
  | Method ((v1, v2)) -> 
      let v1 = ident v1 in
      let (params, ret) = func_type v2 in
      let ent = G.basic_entity v1 [] in
      G.FieldStmt (G.DefStmt 
          (ent, G.FuncDef (mk_func_def params ret (G.Block (fb [])))))
  | EmbeddedInterface v1 -> let v1 = qualified_ident v1 in 
      let name = name_of_qualified_ident v1 in
      G.FieldSpread (fake "...", G.IdQualified (name, G.empty_id_info()))
  | FieldEllipsis2 t ->
      G.fieldEllipsis t

and expr_or_type v = either expr type_ v



and expr =
  function
  | BasicLit v1 -> let v1 = literal v1 in 
      G.L v1
  | Id (v1, vref) -> let v1 = ident v1 in 
      G.Id (v1,
           (* we share the resolved_name ref! so any naming done on the
           * generic AST will have a (wanted) side effect on the Go AST *)
           { (G.empty_id_info()) with G.id_resolved = vref; })
  | Selector ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = tok v2 and v3 = ident v3 in
      G.DotAccess (v1, v2, G.FId v3)
  | Index ((v1, v2)) -> let v1 = expr v1 and v2 = index v2 in
      G.ArrayAccess (v1, v2)
  | Call v1 -> let (e, args) = call_expr v1 in 
      G.Call (e, args)
  | Cast ((v1, v2)) -> let v1 = type_ v1 and v2 = expr v2 in
      G.Cast (v1, v2)
  | Deref ((v1, v2)) -> let v1 = tok v1 and v2 = expr v2 in
      G.DeRef (v1, v2)
  | Ref ((v1, v2)) -> let v1 = tok v1 and v2 = expr v2 in
      G.Ref (v1, v2)
  | Unary ((v1, v2)) ->
      let (v1, tok) = wrap arithmetic_operator v1
      and v2 = expr v2
      in
      G.Call (G.IdSpecial (G.Op v1, tok), fb[G.expr_to_arg v2])
  | Binary ((v1, v2, v3)) ->
      let v1 = expr v1
      and (v2, tok) = wrap arithmetic_operator v2
      and v3 = expr v3
      in
      G.Call (G.IdSpecial (G.Op v2, tok), fb([v1;v3] |> List.map G.expr_to_arg))
  | CompositeLit ((v1, v2)) ->
      let v1 = type_ v1 and (_t1, v2, _t2) = bracket (list init_for_composite_lit) v2 in
      G.Call (G.IdSpecial (G.New, fake "new"), 
        fb((G.ArgType v1)::(v2)))
  | Slice ((v1, v2)) ->
      let e = expr v1 in
      let (v1, v2, v3) = v2 in
      let v1 = option expr v1
      and v2 = option expr v2
      and v3 = option expr v3
      in 
      G.SliceAccess (e, v1, v2, v3)
  | TypeAssert ((v1, v2)) -> let v1 = expr v1 and v2 = type_ v2 in
      G.Call (G.IdSpecial (G.Instanceof, fake "instanceof"),
        fb[G.Arg v1; G.ArgType v2])
  | Ellipsis v1 -> let v1 = tok v1 in 
      G.Ellipsis v1
  | DeepEllipsis v1 -> let v1 = bracket expr v1 in 
      G.DeepEllipsis v1
  | TypedMetavar (v1, v2, v3) ->
      let v1 = ident v1 in
      let v3 = type_ v3 in
      G.TypedMetavar (v1, v2, v3)
  | FuncLit ((v1, v2)) -> 
      let (params, ret) = func_type v1 
      and v2 = stmt v2 
      in
      G.Lambda (mk_func_def params ret v2)
  | Receive ((v1, v2)) -> let _v1 = tok v1 and v2 = expr v2 in 
      G.OtherExpr (G.OE_Recv, [G.E v2])
  | Send ((v1, v2, v3)) ->
      let v1 = expr v1 and _v2 = tok v2 and v3 = expr v3 in
      G.OtherExpr (G.OE_Send, [G.E v1; G.E v3])
  | TypeSwitchExpr ((v1, v2)) -> let _v1 = expr v1 and v2 = tok v2 in
      error v2 "TypeSwitchExpr should be handled in Switch statement"
  | ParenType v1 -> let _v1 = type_ v1 in
      error (ii_of_any (T v1) |> List.hd) 
        ("ParenType should disappear" ^ Common.dump v1)

and literal =
  function
  | Int v1 -> let v1 = wrap string v1 in 
      G.Int v1
  | Float v1 -> let v1 = wrap string v1 in 
      G.Float v1
  | Imag v1 -> let v1 = wrap string v1 in 
      G.Imag v1
  | Rune v1 -> let v1 = wrap string v1 in 
      G.Char v1
  | String v1 -> let v1 = wrap string v1 in 
      G.String v1

and index v = expr v

and arguments v = list argument v
and argument =
  function
  | Arg v1 -> let v1 = expr v1 in 
      G.Arg v1
  | ArgType v1 -> let v1 = type_ v1 in 
      G.ArgType v1
  | ArgDots (v1, v2) -> let v1 = expr v1 in let v2 = tok v2 in
      let special = G.Call (G.IdSpecial (G.Spread, v2), fb[G.expr_to_arg v1]) in
      G.Arg special

and init =
  function
  | InitExpr v1 -> let v1 = expr v1 in v1
  | InitKeyValue ((v1, v2, v3)) ->
      let v1 = init v1 and _v2 = tok v2 and v3 = init v3 in
      G.Tuple [v1; v3]
  | InitBraces v1 -> let v1 = bracket (list init) v1 in
      G.Container (G.List, v1)

and init_for_composite_lit =
   function
   | InitExpr v1 -> let v1 = expr v1 in G.Arg v1
   | InitKeyValue ((v1, v2, v3)) ->
       let _v2 = tok v2 and v3 = init v3 in
       (match v1 with 
        | InitExpr (Id (id, _id_info)) -> G.ArgKwd (id, v3)
        | _ -> G.Arg (G.Tuple [init v1; v3]))
   | InitBraces v1 -> let v1 = bracket (list init) v1 in
       G.Arg (G.Container (G.List, v1))

and constant_expr v = expr v

and simple = function
  | ExprStmt v1 -> let v1 = expr v1 in 
      v1
  (* nice language! Assigns are at statement level! *)
  | Assign ((v1, v2, v3)) ->
      let v1 = list expr v1
      and v2 = tok v2
      and v3 = list expr v3
      in
      (G.Assign (list_to_tuple_or_expr v1, v2, list_to_tuple_or_expr v3))
  | DShortVars ((v1, v2, v3)) ->
      let v1 = list expr v1
      and v2 = tok v2
      and v3 = list expr v3
      in
      (* people don't want '=' assign pattern to match ':=' short var decls,
       * so better to not generate an Assign there too.
       * less: could define a ColonEq operator in AST_generic.ml
       *)
      (G.AssignOp (list_to_tuple_or_expr v1, (G.Eq, v2), 
                   list_to_tuple_or_expr v3))
  | AssignOp ((v1, v2, v3)) ->
      let v1 = expr v1
      and v2 = wrap arithmetic_operator v2
      and v3 = expr v3
      in
      (G.AssignOp (v1, v2, v3))
  | IncDec ((v1, v2, v3)) ->
      let v1 = expr v1
      and (v2, tok) = wrap incr_decr v2
      and v3 = prefix_postfix v3
      in
      (G.Call (G.IdSpecial (G.IncrDecr (v2, v3), tok), 
          fb[G.Arg v1]))


(* invariant: you should not use 'list stmt', but instead always
 * use list stmt_aux ... |> List.flatten
 *)
and stmt x = 
  G.stmt1 (stmt_aux x)

and stmt_aux =
  function
  | DeclStmts v1 -> 
   (* bugfix: 
    *  old: let v1 = list decl v1 in 
    *       G.Block v1
    * but in sgrep with
    *   var $X = "...";
    *   ...
    *   var $Y = $X
    * we do no want the DeclStmts to be put under an extra Block
    * otherwise they will not match. This is why stmt_aux need
    * to return a list of stmts that may or may not be put inside
    * a Block depending on the context (Ss and Items will not).
    *)
      let v1 = list decl v1 in 
      v1
  | Block (t1, v1, t2) -> let v1 = list stmt_aux v1 |> List.flatten in 
      [G.Block (t1, v1, t2)]
  | Empty -> 
      [G.Block (fb[])]
  | SimpleStmt v1 ->
      let v1 = simple v1 in
      [G.exprstmt v1]
  | If ((t, v1, v2, v3, v4)) ->
      let v1 = option simple v1
      and v2 = expr v2
      and v3 = stmt v3
      and v4 = option stmt v4
      in 
      wrap_init_in_block_maybe v1 
       (G.If (t, v2, v3, v4))
  | Switch ((v0, v1, v2, v3)) ->
      let v0 = tok v0 in
      let v1 = option simple v1
      and v2 = 
        match v2 with
        | None -> None
        | Some s ->
            Some (match s with
            | ExprStmt (TypeSwitchExpr (e, tok1)) ->
                let e = expr e in
                G.Call (G.IdSpecial (G.Typeof, tok1), fb[G.Arg e])
            | DShortVars (xs, tok1, [TypeSwitchExpr (e, tok2)]) ->
                let xs = list expr xs in
                let e = expr e in
                G.Assign (list_to_tuple_or_expr xs, tok1,
                G.Call (G.IdSpecial (G.Typeof, tok2), fb[G.Arg e])
                  )
            | s -> simple s
            )   
      and v3 = list case_clause v3
      in
      wrap_init_in_block_maybe v1 
        (G.Switch (v0, v2, v3))
  | Select ((v1, v2)) ->
      let v1 = tok v1 and v2 = list comm_clause v2 in 
      [G.Switch (v1, None, v2)]
  | For (t, (v1, v2, v3), v4) ->
      let v1 = option simple v1
      and v2 = option expr v2
      and v3 = option simple v3
      and v4 = stmt v4
      in
      (* TODO: some of v1 are really ForInitVar *)
      let init = match v1 with None -> [] | Some e -> [G.ForInitExpr e] in
      [G.For (t, G.ForClassic (init, v2, v3), v4)]
        
  | Range ((t, v1, v2, v3, v4)) ->
      let opt =  option 
          (fun (v1, v2) -> let v1 = list expr v1 and v2 = tok v2 in 
            v1, v2) v1
      and v2 = tok v2
      and v3 = expr v3
      and v4 = stmt v4
      in 
      (match opt with
      | None -> 
         let pattern = G.PatUnderscore (fake "_") in
         [G.For (t, G.ForEach (pattern, v2, v3), v4)]
      | Some (xs, _tokEqOrColonEqTODO) -> 
          let pattern = G.PatTuple (xs |> List.map G.expr_to_pattern) in
          [G.For (t, G.ForEach (pattern, v2, v3), v4)]
      )
  | Return ((v1, v2)) ->
      let v1 = tok v1 and v2 = option (list expr) v2 in
      [G.Return (v1, v2 |> Common.map_opt (list_to_tuple_or_expr))]
  | Break ((v1, v2)) -> 
      let v1 = tok v1 and v2 = option ident v2 in 
      [G.Break (v1, G.opt_to_label_ident v2)]
  | Continue ((v1, v2)) ->
      let v1 = tok v1 and v2 = option ident v2 in
      [G.Continue (v1, G.opt_to_label_ident v2)]
  | Goto ((v1, v2)) -> 
      let v1 = tok v1 and v2 = ident v2 in 
      [G.Goto (v1, v2)]
  | Fallthrough v1 -> 
      let v1 = tok v1 in 
      [G.OtherStmt (G.OS_Fallthrough, [G.Tk v1])]
  | Label ((v1, v2)) -> 
      let v1 = ident v1 and v2 = stmt v2 in 
      [G.Label (v1, v2)]
  | Go ((v1, v2)) -> 
      let _v1 = tok v1 and (e, args) = call_expr v2 in 
      [G.OtherStmt (G.OS_Go, [G.E (G.Call (e, args))])]
  | Defer ((v1, v2)) -> 
      let _v1 = tok v1 and (e, args) = call_expr v2 in 
      [G.OtherStmt (G.OS_Defer, [G.E (G.Call (e, args))])]

and case_clause (v1, v2) = let v1 = case_kind v1 and v2 = stmt v2 in 
  v1, v2
and expr_or_type_to_pattern = function
  (* can't call expr_or_type because we want to intercept this one *)
  | Left (ParenType t) -> 
      let t = type_ t in
     G.PatType t
  | x ->
      (match expr_or_type x with
      | Left e -> G.expr_to_pattern e
      | Right t ->G.PatType t
      )

and case_kind =
  function
  | CaseExprs (tok, v1) -> 
      v1 |> List.map (fun x -> (G.Case (tok, expr_or_type_to_pattern x)))
  | CaseAssign ((tok, v1, v2, v3)) ->
      let v1 = list expr_or_type v1
      and v3 = expr v3
      in
      let v1 = v1 |> List.map (function
            | Left e -> e
            | Right _ -> error tok "TODO: Case Assign with Type?"
       ) in
      [G.CaseEqualExpr (tok, G.Assign (list_to_tuple_or_expr v1, v2, v3))]
  | CaseDefault v1 -> let v1 = tok v1 in
      [G.Default v1]

and comm_clause v = case_clause v

and call_expr (v1, v2) = 
  let v1 = expr v1 and v2 = bracket arguments v2 in 
  v1, v2


and decl =
  function
  | DConst ((v1, v2, v3)) ->
      let v1 = ident v1
      and v2 = option type_ v2
      and v3 = option constant_expr v3
      in 
      let ent = G.basic_entity v1 [G.attr G.Const (fake "const")] in
      G.DefStmt (ent, G.VarDef { G.vinit = v3; vtype = v2 })
  | DVar ((v1, v2, v3)) ->
      let v1 = ident v1
      and v2 = option type_ v2
      and v3 = option expr v3
      in
      let ent = G.basic_entity v1 [G.attr G.Var (fake "var")] in
      G.DefStmt (ent, G.VarDef { G.vinit = v3; vtype = v2 })
  | DTypeAlias ((v1, v2, v3)) ->
      let v1 = ident v1 and _v2 = tok v2 and v3 = type_ v3 in 
      let ent = G.basic_entity v1 [] in
      G.DefStmt (ent, G.TypeDef { G.tbody = G.AliasType v3 })
  | DTypeDef ((v1, v2)) -> let v1 = ident v1 and v2 = type_ v2 in 
      let ent = G.basic_entity v1 [] in
      G.DefStmt (ent, G.TypeDef { G.tbody = G.NewType v2 })

and top_decl =
  function
  | DFunc ((v1, (v2, v3))) -> pr2 "func";
      let v1 = ident v1 and (params, ret) = func_type v2 and v3 = stmt v3 in 
      let ent = G.basic_entity v1 [] in
      G.DefStmt (ent, G.FuncDef (mk_func_def params ret v3))
  | DMethod ((v1, v2, (v3, v4))) -> pr2 "method";
      let v1 = ident v1 
      and v2 = parameter v2
      and (params, ret) = func_type v3 and v4 = stmt v4 in
      let ent = G.basic_entity v1 [] in
      let def = mk_func_def params ret v4 in
      let receiver = G.OtherParam (G.OPO_Receiver, [G.Pa (G.ParamClassic v2)])
      in
      G.DefStmt (ent, G.FuncDef { def with G.fparams=receiver::def.G.fparams})
  | DTop v1 -> let v1 = decl v1 in v1
  | STop v1 -> stmt v1
  | Package (t1, id) -> 
      let id = ident id in
      G.DirectiveStmt (G.Package (t1, [id]))
  | Import x ->
      let x = import x in
      G.DirectiveStmt x

and import { i_path = i_path; i_kind = i_kind; i_tok } =
  let module_name = G.FileName (wrap string i_path) in
  let (s,tok) = i_path in
  import_kind i_tok i_kind module_name (Filename.basename s, tok)
  
and import_kind itok kind module_name _id_no_more_used =
  match kind with
  | ImportOrig -> 
     (* old: in Go, import "a/b/c" is really equivalent to import c "a/b/c",
      * but we don't do return anymore G.ImportAs (itok, module_name, Some id)
      * otherwise sgrep can not know if an alias was explicitely given or not
      *)
      G.ImportAs (itok, module_name, None)
  | ImportNamed v1 -> let v1 = ident v1 in 
      G.ImportAs (itok, module_name, Some v1)
  | ImportDot v1 -> let v1 = tok v1 in 
      G.ImportAll (itok, module_name, v1)

and program xs =
  anon_types := [];
  let xs = list top_decl xs in
  let arg_types = !anon_types |> List.map (fun x -> G.DefStmt x) in
  (* TODO? put after program and imports? *)
  arg_types @ xs

and item_aux = function
  | ITop x -> pr2 "gotogen itop"; [top_decl x]
 | IImport x -> let x = import x in [G.DirectiveStmt x]
 | IStmt x -> stmt_aux x

and item x  = 
  G.stmt1 (item_aux x)
  
and any x =
  anon_types := [];
  let res = 
  match x with
  | E v1 -> let v1 = expr v1 in G.E v1
  | S v1 -> let v1 = stmt v1 in G.S v1
  | T v1 -> let v1 = type_ v1 in G.T v1
  | Decl v1 -> let v1 = decl v1 in G.S v1
  | I v1 -> let v1 = import v1 in G.S (G.DirectiveStmt v1)
  | P v1 -> let v1 = program v1 in G.Pr v1
  | Ident v1 -> let v1 = ident v1 in G.I v1
  (* not used anymore, Items is now used for sgrep *)
  | Ss v1 -> let v1 = list stmt_aux v1 in G.Ss (List.flatten v1)
  | Item v1 -> let v1 = item v1 in G.S v1
  | Items v1 -> let v1 = list item_aux v1 in G.Ss (List.flatten v1)
  in
  if !anon_types <> []
  then failwith "TODO: anon_types not empty";
  res

in
program, any


let program x = 
  let p, _ = top_func () in
  p x

let any x = 
  let _, a = top_func () in
  a x
