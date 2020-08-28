(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

open OCaml

open Cst_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* hooks *)
type visitor_in = {
  kexpr: (expr  -> expr) * visitor_out -> expr  -> expr;
  kstmt_and_def:
    (stmt_and_def -> stmt_and_def) * visitor_out -> stmt_and_def ->stmt_and_def;
  kstmt: (stmt -> stmt) * visitor_out -> stmt -> stmt;
  kname: (name -> name) * visitor_out -> name -> name;
  kclass_def:  (class_def -> class_def) * visitor_out -> class_def -> class_def;

  kinfo: (tok -> tok) * visitor_out -> tok -> tok;

}
and visitor_out = {
  vtop: toplevel -> toplevel;
  vstmt_and_def: stmt_and_def -> stmt_and_def;
  vprogram: program -> program;
  vexpr: expr -> expr;
  vxhpattrvalue: xhp_attr_value -> xhp_attr_value;
  vany: any -> any;
}

let map_option = Common2.map_option
let map_scope x = x

let default_visitor =
  { kexpr   = (fun (k,_) x -> k x);
    kstmt_and_def = (fun (k,_) x -> k x);
    kstmt = (fun (k,_) x -> k x);
    kname = (fun (k,_) x -> k x);
    kclass_def = (fun (k,_) x -> k x);
    kinfo = (fun (k,_) x -> k x);
  }

let (mk_visitor: visitor_in -> visitor_out) = fun vin ->

(* start of auto generation *)

let rec map_info x =
  let k x =
    match x with
    { Parse_info.token = v_pinfo;
      transfo = v_transfo;
    } ->
    let v_pinfo =
      (* todo? map_pinfo v_pinfo *)
    v_pinfo
    in
    (* not recurse in transfo ? *)
    { Parse_info.token = v_pinfo;   (* generete a fresh field *)
      transfo = v_transfo;
    }
  in
  vin.kinfo (k, all_functions) x

and map_tok v =
  map_info v
and map_wrap:'a. ('a -> 'a) -> 'a wrap -> 'a wrap = fun _of_a (v1, v2) ->
  let v1 = _of_a v1 and v2 = map_info v2 in (v1, v2)
and map_paren:'a. ('a -> 'a) -> 'a paren -> 'a paren = fun _of_a (v1, v2, v3)->
  let v1 = map_tok v1 and v2 = _of_a v2 and v3 = map_tok v3 in (v1, v2, v3)
and map_brace: 'a. ('a -> 'a) -> 'a brace -> 'a brace = fun _of_a (v1, v2, v3)->
  let v1 = map_tok v1 and v2 = _of_a v2 and v3 = map_tok v3 in (v1, v2, v3)
and map_bracket: 'a. ('a -> 'a) -> 'a bracket -> 'a bracket =
 fun _of_a (v1, v2, v3)->
  let v1 = map_tok v1 and v2 = _of_a v2 and v3 = map_tok v3 in (v1, v2, v3)
and map_single_angle: 'a. ('a -> 'a) -> 'a single_angle -> 'a single_angle =
  fun _of_a (v1, v2, v3) ->
  let v1 = map_tok v1 and v2 = _of_a v2 and v3 = map_tok v3 in (v1, v2, v3)
and map_angle: 'a. ('a -> 'a) -> 'a angle -> 'a angle =
 fun _of_a (v1, v2, v3)->
  let v1 = map_tok v1 and v2 = _of_a v2 and v3 = map_tok v3 in (v1, v2, v3)
and map_comma_list_dots : 'a. ('a -> 'a) -> 'a comma_list_dots -> 'a comma_list_dots =
  fun _of_a xs ->
  map_of_list (fun x -> OCaml.map_of_either3 _of_a map_info map_info x) xs
and map_comma_list:'a. ('a -> 'a) -> 'a comma_list -> 'a comma_list =
  fun _of_a xs ->
  map_of_list (fun x -> OCaml.map_of_either _of_a map_info x) xs


and map_ident =
  function
  | Name v1 -> let v1 = map_wrap map_of_string v1 in Name ((v1))
  | XhpName v1 -> let v1 = map_wrap (map_of_list map_of_string) v1 in
                  XhpName ((v1))
and map_xhp_tag v = map_of_list map_of_string v
and map_dname =
  function | DName v1 -> let v1 = map_wrap map_of_string v1 in DName ((v1))

and map_qualified_ident v = map_of_list map_qualified_ident_element v
and map_qualified_ident_element =
  function
  | QI v1 -> let v1 = map_ident v1 in QI ((v1))
  | QITok v1 -> let v1 = map_tok v1 in QITok ((v1))

and map_name x = map_class_name_or_selfparent x
and map_class_name_or_selfparent v =
  let k v =
    match v with
    | XName (v1) ->
        let v1 = map_qualified_ident v1 in
        XName (v1)
    | Self v1 -> let v1 = map_tok v1 in Self ((v1))
    | Parent v1 -> let v1 = map_tok v1 in Parent ((v1))
    | LateStatic v1 -> let v1 = map_tok v1 in LateStatic ((v1))
  in
  vin.kname (k, all_functions) v
and map_type_args v = map_single_angle (map_comma_list map_hint_type) v
and map_fully_qualified_class_name v = map_hint_type v

and map_type_params v = map_single_angle (map_comma_list map_type_param) v
and map_type_param =
  function
  | TParam v1 -> let v1 = map_ident v1 in TParam ((v1))
  | TParamConstraint ((v1, v2, v3)) ->
      let v1 = map_ident v1
      and v2 = map_tok v2
      and v3 = map_class_name v3
      in TParamConstraint ((v1, v2, v3))
and map_class_name v = map_hint_type v

and map_ptype =
  function
  | BoolTy -> BoolTy
  | IntTy -> IntTy
  | DoubleTy -> DoubleTy
  | StringTy -> StringTy
  | ArrayTy -> ArrayTy
  | ObjectTy -> ObjectTy

and map_expr (x) =
  let k x =  match x with
  | Id v1 ->
    let v1 = map_name v1 in
    Id v1

  | IdVar ((v1, v2)) ->
      let v1 = map_dname v1
      and v2 = map_of_ref map_scope v2
      in IdVar ((v1, v2))
  | This v1 -> let v1 = map_tok v1 in This ((v1))


  | Call ((v1, v2)) ->
      let v1 = map_expr v1
      and v2 = map_paren (map_comma_list map_argument) v2
      in Call ((v1, v2))
  | ObjGet ((v1, v2, v3)) ->
      let v1 = map_expr v1
      and v2 = map_tok v2
      and v3 = map_expr v3
      in ObjGet ((v1, v2, v3))
  | ClassGet ((v1, v2, v3)) ->
      let v1 = map_class_name_reference v1
      and v2 = map_tok v2
      and v3 = map_expr v3
      in ClassGet ((v1, v2, v3))
  | ArrayGet ((v1, v2)) ->
      let v1 = map_expr v1
      and v2 = map_bracket (map_of_option map_expr) v2
      in ArrayGet ((v1, v2))
  | HashGet ((v1, v2)) ->
      let v1 = map_expr v1
      and v2 = map_brace map_expr v2
      in HashGet ((v1, v2))
  | BraceIdent v1 -> let v1 = map_brace map_expr v1 in BraceIdent ((v1))
  | Deref ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_expr v2 in Deref ((v1, v2))
  | Sc v1 -> let v1 = map_scalar v1 in Sc ((v1))
  | Binary ((v1, v2, v3)) ->
      let v1 = map_expr v1
      and v2 = map_wrap map_binaryOp v2
      and v3 = map_expr v3
      in Binary ((v1, v2, v3))
  | Unary ((v1, v2)) ->
      let v1 = map_wrap map_unaryOp v1
      and v2 = map_expr v2
      in Unary ((v1, v2))
  | Assign ((v1, v2, v3)) ->
      let v1 = map_lvalue v1
      and v2 = map_tok v2
      and v3 = map_expr v3
      in Assign ((v1, v2, v3))
  | AssignOp ((v1, v2, v3)) ->
      let v1 = map_lvalue v1
      and v2 = map_wrap map_assignOp v2
      and v3 = map_expr v3
      in AssignOp ((v1, v2, v3))
  | Postfix ((v1, v2)) ->
      let v1 = map_rw_variable v1
      and v2 = map_wrap map_fixOp v2
      in Postfix ((v1, v2))
  | Infix ((v1, v2)) ->
      let v1 = map_wrap map_fixOp v1
      and v2 = map_rw_variable v2
      in Infix ((v1, v2))
  | CondExpr ((v1, v2, v3, v4, v5)) ->
      let v1 = map_expr v1
      and v2 = map_tok v2
      and v3 = map_option map_expr v3
      and v4 = map_tok v4
      and v5 = map_expr v5
      in CondExpr ((v1, v2, v3, v4, v5))
  | AssignList ((v1, v2, v3, v4)) ->
      let v1 = map_tok v1
      and v2 = map_paren (map_comma_list map_list_assign) v2
      and v3 = map_tok v3
      and v4 = map_expr v4
      in AssignList ((v1, v2, v3, v4))
  | ArrayLong ((v1, v2)) ->
      let v1 = map_tok v1
      and v2 = map_paren (map_comma_list map_array_pair) v2
      in ArrayLong ((v1, v2))
  | ArrayShort ((v1)) ->
      let v1 = map_bracket (map_comma_list map_array_pair) v1
      in ArrayShort ((v1))
  | Collection ((v1, v2)) ->
      let v1 = map_name v1 in
      let v2 = map_brace (map_comma_list map_array_pair) v2 in
      Collection ((v1,v2))
  | New ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_class_name_reference v2
      and v3 = map_of_option (map_paren (map_comma_list map_argument)) v3
      in New ((v1, v2, v3))
  | Clone ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_expr v2 in Clone ((v1, v2))
  | AssignRef ((v1, v2, v3, v4)) ->
      let v1 = map_lvalue v1
      and v2 = map_tok v2
      and v3 = map_tok v3
      and v4 = map_lvalue v4
      in AssignRef ((v1, v2, v3, v4))
  | AssignNew ((v1, v2, v3, v4, v5, v6)) ->
      let v1 = map_lvalue v1
      and v2 = map_tok v2
      and v3 = map_tok v3
      and v4 = map_tok v4
      and v5 = map_class_name_reference v5
      and v6 = map_of_option (map_paren (map_comma_list map_argument)) v6
      in AssignNew ((v1, v2, v3, v4, v5, v6))
  | Cast ((v1, v2)) ->
      let v1 = map_wrap map_castOp v1 and v2 = map_expr v2 in Cast ((v1, v2))
  | CastUnset ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_expr v2 in CastUnset ((v1, v2))
  | InstanceOf ((v1, v2, v3)) ->
      let v1 = map_expr v1
      and v2 = map_tok v2
      and v3 = map_class_name_reference v3
      in InstanceOf ((v1, v2, v3))
  | Eval ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_paren map_expr v2 in Eval ((v1, v2))
  | Lambda v1 -> let v1 = map_lambda_def v1 in Lambda ((v1))
  | ShortLambda v1 -> let v1 = map_short_lambda_def v1 in ShortLambda ((v1))
  | Exit ((v1, v2)) ->
      let v1 = map_tok v1
      and v2 = map_of_option (map_paren (map_of_option map_expr)) v2
      in Exit ((v1, v2))
  | At ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_expr v2 in At ((v1, v2))
  | Print ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_expr v2 in Print ((v1, v2))
  | BackQuote ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_of_list map_encaps v2
      and v3 = map_tok v3
      in BackQuote ((v1, v2, v3))
  | Include ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_expr v2 in Include ((v1, v2))
  | IncludeOnce ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_expr v2 in IncludeOnce ((v1, v2))
  | Require ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_expr v2 in Require ((v1, v2))
  | RequireOnce ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_expr v2 in RequireOnce ((v1, v2))
  | Yield ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_array_pair v2 in Yield ((v1, v2))
  | YieldBreak ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_tok v2 in YieldBreak ((v1, v2))
  | Await ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_expr v2 in Await ((v1, v2))
  | Empty ((v1, v2)) ->
      let v1 = map_tok v1
      and v2 = map_paren map_lvalue v2
      in Empty ((v1, v2))
  | Isset ((v1, v2)) ->
      let v1 = map_tok v1
      and v2 = map_paren (map_comma_list map_lvalue) v2
      in Isset ((v1, v2))
  | SgrepExprDots v1 -> let v1 = map_info v1 in SgrepExprDots ((v1))
  | ParenExpr v1 -> let v1 = map_paren map_expr v1 in ParenExpr ((v1))
  | XhpHtml v1 -> let v1 = map_xhp_html v1 in XhpHtml ((v1))
 in
 vin.kexpr (k, all_functions) x

and map_scalar =
  function
  | C v1 -> let v1 = map_constant v1 in C ((v1))
  | Guil ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_of_list map_encaps v2
      and v3 = map_tok v3
      in Guil ((v1, v2, v3))
  | HereDoc ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_of_list map_encaps v2
      and v3 = map_tok v3
      in HereDoc ((v1, v2, v3))
and map_constant =
  function
  | Int v1 -> let v1 = map_wrap map_of_string v1 in Int ((v1))
  | Double v1 -> let v1 = map_wrap map_of_string v1 in Double ((v1))
  | String v1 -> let v1 = map_wrap map_of_string v1 in String ((v1))
  | PreProcess v1 ->
      let v1 = map_wrap map_cpp_directive v1 in PreProcess ((v1))
  | XdebugClass ((v1, v2)) ->
      let v1 = map_name v1
      and v2 = map_of_list map_class_stmt v2
      in XdebugClass ((v1, v2))
  | XdebugResource -> XdebugResource

and map_cpp_directive =
  function
  | Line -> Line
  | File -> File
  | Dir -> Dir
  | ClassC -> ClassC
  | MethodC -> MethodC
  | FunctionC -> FunctionC
  | TraitC -> TraitC
  | NamespaceC -> NamespaceC
and map_encaps =
  function
  | EncapsString v1 ->
      let v1 = map_wrap map_of_string v1 in EncapsString ((v1))
  | EncapsVar v1 -> let v1 = map_lvalue v1 in EncapsVar ((v1))
  | EncapsCurly ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_lvalue v2
      and v3 = map_tok v3
      in EncapsCurly ((v1, v2, v3))
  | EncapsDollarCurly ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_lvalue v2
      and v3 = map_tok v3
      in EncapsDollarCurly ((v1, v2, v3))
  | EncapsExpr ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_expr v2
      and v3 = map_tok v3
      in EncapsExpr ((v1, v2, v3))
and map_fixOp x = x
and map_binaryOp =
  function
  | Arith v1 -> let v1 = map_arithOp v1 in Arith ((v1))
  | Logical v1 -> let v1 = map_logicalOp v1 in Logical ((v1))
  | BinaryConcat -> BinaryConcat
  | CombinedComparison -> CombinedComparison
and map_arithOp =
  function
  | Plus -> Plus
  | Minus -> Minus
  | Mul -> Mul
  | Div -> Div
  | Mod -> Mod
  | DecLeft -> DecLeft
  | DecRight -> DecRight
  | And -> And
  | Or -> Or
  | Xor -> Xor
and map_logicalOp =
  function
  | Inf -> Inf
  | Sup -> Sup
  | InfEq -> InfEq
  | SupEq -> SupEq
  | Eq -> Eq
  | NotEq -> NotEq
  | Identical -> Identical
  | NotIdentical -> NotIdentical
  | AndLog -> AndLog
  | OrLog -> OrLog
  | XorLog -> XorLog
  | AndBool -> AndBool
  | OrBool -> OrBool
and map_assignOp =
  function
  | AssignOpArith v1 -> let v1 = map_arithOp v1 in AssignOpArith ((v1))
  | AssignConcat -> AssignConcat
and map_unaryOp =
  function
  | UnPlus -> UnPlus
  | UnMinus -> UnMinus
  | UnBang -> UnBang
  | UnTilde -> UnTilde
and map_castOp v = map_ptype v
and map_list_assign =
  function
  | ListVar v1 -> let v1 = map_lvalue v1 in ListVar ((v1))
  | ListList ((v1, v2)) ->
      let v1 = map_tok v1
      and v2 = map_paren (map_comma_list map_list_assign) v2
      in ListList ((v1, v2))
  | ListEmpty -> ListEmpty
and map_array_pair =
  function
  | ArrayExpr v1 -> let v1 = map_expr v1 in ArrayExpr ((v1))
  | ArrayRef ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_lvalue v2 in ArrayRef ((v1, v2))
  | ArrayArrowExpr ((v1, v2, v3)) ->
      let v1 = map_expr v1
      and v2 = map_tok v2
      and v3 = map_expr v3
      in ArrayArrowExpr ((v1, v2, v3))
  | ArrayArrowRef ((v1, v2, v3, v4)) ->
      let v1 = map_expr v1
      and v2 = map_tok v2
      and v3 = map_tok v3
      and v4 = map_lvalue v4
      in ArrayArrowRef ((v1, v2, v3, v4))
and map_class_name_reference a = map_expr a

and map_xhp_html =
  function
  | Xhp ((v1, v2, v3, v4, v5)) ->
      let v1 = map_wrap map_xhp_tag v1
      and v2 = map_of_list map_xhp_attribute v2
      and v3 = map_tok v3
      and v4 = map_of_list map_xhp_body v4
      and v5 = map_wrap (map_of_option map_xhp_tag) v5
      in Xhp ((v1, v2, v3, v4, v5))
  | XhpSingleton ((v1, v2, v3)) ->
      let v1 = map_wrap map_xhp_tag v1
      and v2 = map_of_list map_xhp_attribute v2
      and v3 = map_tok v3
      in XhpSingleton ((v1, v2, v3))
and map_xhp_attribute (v1, v2, v3) =
  let v1 = map_xhp_attr_name v1
  and v2 = map_tok v2
  and v3 = map_xhp_attr_value v3
  in (v1, v2, v3)
and map_xhp_attr_name v = map_wrap map_of_string v
and map_xhp_attr_value =
  function
  | XhpAttrString ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_of_list map_encaps v2
      and v3 = map_tok v3
      in XhpAttrString ((v1, v2, v3))
  | XhpAttrExpr v1 -> let v1 = map_brace map_expr v1 in XhpAttrExpr ((v1))
  | SgrepXhpAttrValueMvar v1 ->
      let v1 = map_wrap map_of_string v1 in SgrepXhpAttrValueMvar ((v1))
and map_xhp_body =
  function
  | XhpText v1 -> let v1 = map_wrap map_of_string v1 in XhpText ((v1))
  | XhpExpr v1 -> let v1 = map_brace map_expr v1 in XhpExpr ((v1))
  | XhpNested v1 -> let v1 = map_xhp_html v1 in XhpNested ((v1))



and map_lvalue a = map_expr a

and map_argument =
  function
  | Arg v1 -> let v1 = map_expr v1 in Arg ((v1))
  | ArgRef ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_w_variable v2 in ArgRef ((v1, v2))
  | ArgUnpack ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_expr v2 in ArgUnpack ((v1, v2))
and map_rw_variable v = map_lvalue v
and map_r_variable v = map_lvalue v
and map_w_variable v = map_lvalue v
and map_stmt x =
  let k x =
    match x with
  | ExprStmt ((v1, v2)) ->
      let v1 = map_expr v1 and v2 = map_tok v2 in ExprStmt ((v1, v2))
  | EmptyStmt v1 -> let v1 = map_tok v1 in EmptyStmt ((v1))
  | Block v1 ->
      let v1 = map_brace (map_of_list map_stmt_and_def) v1 in Block ((v1))
  | If ((v1, v2, v3, v4, v5)) ->
      let v1 = map_tok v1
      and v2 = map_paren map_expr v2
      and v3 = map_stmt v3
      and v4 =
        map_of_list
          (fun (v1, v2, v3) ->
             let v1 = map_tok v1
             and v2 = map_paren map_expr v2
             and v3 = map_stmt v3
             in (v1, v2, v3))
          v4
      and v5 =
        map_of_option
          (fun (v1, v2) ->
             let v1 = map_tok v1 and v2 = map_stmt v2 in (v1, v2))
          v5
      in If ((v1, v2, v3, v4, v5))
  | IfColon ((v1, v2, v3, v4, v5, v6, v7, v8)) ->
      let v1 = map_tok v1
      and v2 = map_paren map_expr v2
      and v3 = map_tok v3
      and v4 = map_of_list map_stmt_and_def v4
      and v5 = map_of_list map_new_elseif v5
      and v6 = map_of_option map_new_else v6
      and v7 = map_tok v7
      and v8 = map_tok v8
      in IfColon ((v1, v2, v3, v4, v5, v6, v7, v8))
  | While ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_paren map_expr v2
      and v3 = map_colon_stmt v3
      in While ((v1, v2, v3))
  | Do ((v1, v2, v3, v4, v5)) ->
      let v1 = map_tok v1
      and v2 = map_stmt v2
      and v3 = map_tok v3
      and v4 = map_paren map_expr v4
      and v5 = map_tok v5
      in Do ((v1, v2, v3, v4, v5))
  | For ((v1, v2, v3, v4, v5, v6, v7, v8, v9)) ->
      let v1 = map_tok v1
      and v2 = map_tok v2
      and v3 = map_for_expr v3
      and v4 = map_tok v4
      and v5 = map_for_expr v5
      and v6 = map_tok v6
      and v7 = map_for_expr v7
      and v8 = map_tok v8
      and v9 = map_colon_stmt v9
      in For ((v1, v2, v3, v4, v5, v6, v7, v8, v9))
  | Foreach ((v1, v2, v3, v4, v5, v6, v7, v8)) ->
      let v1 = map_tok v1
      and v2 = map_tok v2
      and v3 = map_expr v3
      and v4 = map_option map_tok v4
      and v5 = map_tok v5
      and v6 = map_foreach_pattern v6
      and v7 = map_tok v7
      and v8 = map_colon_stmt v8
      in Foreach ((v1, v2, v3, v4, v5, v6, v7, v8))
  | Switch ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_paren map_expr v2
      and v3 = map_switch_case_list v3
      in Switch ((v1, v2, v3))
  | Break ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_of_option map_expr v2
      and v3 = map_tok v3
      in Break ((v1, v2, v3))
  | Continue ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_of_option map_expr v2
      and v3 = map_tok v3
      in Continue ((v1, v2, v3))
  | Return ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_of_option map_expr v2
      and v3 = map_tok v3
      in Return ((v1, v2, v3))
  | Throw ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_expr v2
      and v3 = map_tok v3
      in Throw ((v1, v2, v3))
  | Try ((v1, v2, v3, v4)) ->
      let v1 = map_tok v1
      and v2 = map_brace (map_of_list map_stmt_and_def) v2
      and v3 = map_of_list map_catch v3
      and v4 = map_of_list map_finally v4
      in Try ((v1, v2, v3, v4))
  | Echo ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_comma_list map_expr v2
      and v3 = map_tok v3
      in Echo ((v1, v2, v3))
  | Globals ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_comma_list map_global_var v2
      and v3 = map_tok v3
      in Globals ((v1, v2, v3))
  | StaticVars ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_comma_list map_static_var v2
      and v3 = map_tok v3
      in StaticVars ((v1, v2, v3))
  | InlineHtml v1 -> let v1 = map_wrap map_of_string v1 in InlineHtml ((v1))
  | Use ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_use_filename v2
      and v3 = map_tok v3
      in Use ((v1, v2, v3))
  | Unset ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_paren (map_comma_list map_lvalue) v2
      and v3 = map_tok v3
      in Unset ((v1, v2, v3))
  | Declare ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_paren (map_comma_list map_declare) v2
      and v3 = map_colon_stmt v3
      in Declare ((v1, v2, v3))
  | FuncDefNested v1 -> let v1 = map_func_def v1 in FuncDefNested ((v1))
  | ClassDefNested v1 -> let v1 = map_class_def v1 in ClassDefNested ((v1))

  in
  vin.kstmt (k, all_functions) x

and map_switch_case_list =
  function
  | CaseList ((v1, v2, v3, v4)) ->
      let v1 = map_tok v1
      and v2 = map_of_option map_tok v2
      and v3 = map_of_list map_case v3
      and v4 = map_tok v4
      in CaseList ((v1, v2, v3, v4))
  | CaseColonList ((v1, v2, v3, v4, v5)) ->
      let v1 = map_tok v1
      and v2 = map_of_option map_tok v2
      and v3 = map_of_list map_case v3
      and v4 = map_tok v4
      and v5 = map_tok v5
      in CaseColonList ((v1, v2, v3, v4, v5))
and map_case =
  function
  | Case ((v1, v2, v3, v4)) ->
      let v1 = map_tok v1
      and v2 = map_expr v2
      and v3 = map_tok v3
      and v4 = map_of_list map_stmt_and_def v4
      in Case ((v1, v2, v3, v4))
  | Default ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_tok v2
      and v3 = map_of_list map_stmt_and_def v3
      in Default ((v1, v2, v3))
and map_for_expr v = map_comma_list map_expr v
and map_foreach_pattern =
  function
  | ForeachVar v1 -> let v1 = map_foreach_variable v1 in ForeachVar ((v1))
  | ForeachArrow ((v1, v2, v3)) ->
      let v1 = map_foreach_pattern v1
      and v2 = map_tok v2
      and v3 = map_foreach_pattern v3
      in ForeachArrow ((v1, v2, v3))
  | ForeachList ((v1, v2)) ->
      let v1 = map_tok v1
      and v2 = map_paren (map_comma_list map_list_assign) v2
      in ForeachList ((v1, v2))
and map_foreach_variable (v1, v2) =
  let v1 = map_is_ref v1 and v2 = map_lvalue v2 in (v1, v2)
and map_catch (v1, v2, v3) =
  let v1 = map_tok v1
  and v2 =
    map_paren
      (fun (v1, v2) ->
         let v1 = map_fully_qualified_class_name v1
         and v2 = map_dname v2
         in (v1, v2))
      v2
  and v3 = map_brace (map_of_list map_stmt_and_def) v3
  in (v1, v2, v3)
and map_finally (v1, v2) =
  let v1 = map_tok v1
  and v2 = map_brace (map_of_list map_stmt_and_def) v2
  in (v1, v2)
and map_use_filename =
  function
  | UseDirect v1 -> let v1 = map_wrap map_of_string v1 in UseDirect ((v1))
  | UseParen v1 ->
      let v1 = map_paren (map_wrap map_of_string) v1 in UseParen ((v1))
and map_declare (v1, v2) =
  let v1 = map_ident v1 and v2 = map_static_scalar_affect v2 in (v1, v2)
and map_colon_stmt =
  function
  | SingleStmt v1 -> let v1 = map_stmt v1 in SingleStmt ((v1))
  | ColonStmt ((v1, v2, v3, v4)) ->
      let v1 = map_tok v1
      and v2 = map_of_list map_stmt_and_def v2
      and v3 = map_tok v3
      and v4 = map_tok v4
      in ColonStmt ((v1, v2, v3, v4))
and map_new_elseif (v1, v2, v3, v4) =
  let v1 = map_tok v1
  and v2 = map_paren map_expr v2
  and v3 = map_tok v3
  and v4 = map_of_list map_stmt_and_def v4
  in (v1, v2, v3, v4)
and map_new_else (v1, v2, v3) =
  let v1 = map_tok v1
  and v2 = map_tok v2
  and v3 = map_of_list map_stmt_and_def v3
  in (v1, v2, v3)
and
  map_func_def {
                 f_tok = v_f_tok;
                 f_type = v_f_type;
                 f_attrs = v_f_attrs;
                 f_modifiers = v_f_modifiers;
                 f_ref = v_f_ref;
                 f_name = v_f_name;
                 f_tparams = v_f_tparams;
                 f_params = v_f_params;
                 f_body = v_f_body;
                 f_return_type = v_f_return_type;
               } =
  let v_f_body = map_brace (map_of_list map_stmt_and_def) v_f_body in
  let v_f_params = map_paren (map_comma_list_dots map_parameter) v_f_params in
  let v_f_name = map_ident v_f_name in
  let v_f_tparams = map_of_option map_type_params v_f_tparams in
  let v_f_ref = map_is_ref v_f_ref in
  let v_f_modifiers = map_of_list (map_wrap map_modifier) v_f_modifiers in
  let v_f_attrs = map_of_option map_attributes v_f_attrs in
  let v_f_type = map_function_type v_f_type in
  let v_f_tok = map_tok v_f_tok in
  let v_f_return_type =
    map_of_option
      (fun (v1, v2, v3) ->
         let v1 = map_tok v1
         and v2 = map_of_option map_tok v2
         and v3 = map_hint_type v3 in
         (v1, v2, v3))
      v_f_return_type in
  {
    f_tok = v_f_tok;
    f_type = v_f_type;
    f_attrs = v_f_attrs;
    f_modifiers = v_f_modifiers;
    f_ref = v_f_ref;
    f_name = v_f_name;
    f_params = v_f_params;
    f_tparams = v_f_tparams;
    f_return_type = v_f_return_type;
    f_body = v_f_body;
  }
and map_function_type =
  function
  | FunctionRegular -> FunctionRegular
  | FunctionLambda -> FunctionLambda
  | MethodRegular -> MethodRegular
  | MethodAbstract -> MethodAbstract
and
  map_parameter {
                  p_attrs = v_p_attrs;
                  p_modifier = v_p_modifier;
                  p_soft_type = v_p_soft_type;
                  p_type = v_p_type;
                  p_ref = v_p_ref;
                  p_name = v_p_name;
                  p_default = v_p_default;
                  p_variadic = v_p_variadic
                } =
  let v_p_default = map_of_option map_static_scalar_affect v_p_default in
  let v_p_name = map_dname v_p_name in
  let v_p_ref = map_is_ref v_p_ref in
  let v_p_modifier = map_of_option (map_wrap map_modifier) v_p_modifier in
  let v_p_type = map_of_option map_hint_type v_p_type in
  let v_p_soft_type = map_of_option map_tok v_p_soft_type in
  let v_p_attrs = map_of_option map_attributes v_p_attrs in
  let v_p_variadic = map_of_option map_tok v_p_variadic in
  {
    p_attrs = v_p_attrs;
    p_modifier = v_p_modifier;
    p_soft_type = v_p_soft_type;
    p_type = v_p_type;
    p_ref = v_p_ref;
    p_name = v_p_name;
    p_default = v_p_default;
    p_variadic = v_p_variadic
  }

and map_hint_type =
  function
  | Hint (v1, v2) ->
    let v1 = map_class_name_or_selfparent v1 in
    let v2 = map_option map_type_args v2 in
    Hint ((v1, v2))

  | HintArray v1 -> let v1 = map_tok v1 in HintArray ((v1))
  | HintQuestion (v1, v2) -> let v1 = map_tok v1 in
                             let v2 = map_hint_type v2 in
                             HintQuestion (v1, v2)
  | HintTuple v1 -> let v1 = map_paren (map_comma_list map_hint_type) v1 in
                    HintTuple v1
  | HintCallback v1 ->
      let v1 =
        map_paren
          (fun (v1, v2, v3) ->
             let v1 = map_tok v1
             and v2 = map_paren (map_comma_list_dots map_hint_type) v2
             and v3 =
               map_of_option
                 (fun (v1, v2, v3) ->
                    let v1 = map_tok v1
                    and v2 = map_of_option map_tok v2
                    and v3 = map_hint_type v3 in (v1, v2, v3))
                 v3
             in (v1, v2, v3))
          v1
      in HintCallback ((v1))
  | HintShape ((v1, v2)) ->
      let v1 = map_tok v1
      and v2 =
        map_paren
          (map_comma_list
             (fun (v1, v2, v3) ->
                let v1 = map_expr v1
                and v2 = map_tok v2
                and v3 = map_hint_type v3
                in (v1, v2, v3)))
          v2
      in HintShape ((v1, v2))
  | HintTypeConst ((v1, v2, v3)) ->
      let v1 = map_hint_type v1
      and v2 = map_tok v2
      and v3 = map_hint_type v3
      in HintTypeConst ((v1, v2, v3))
  | HintVariadic (v1, v2) ->
      let v1 = map_tok v1
      and v2 = map_option map_hint_type v2
      in HintVariadic (v1, v2)

and map_is_ref v = map_of_option map_tok v
and map_lambda_def (v1, v2) =
  let v1 = map_of_option map_lexical_vars v1
  and v2 = map_func_def v2
  in (v1, v2)

and map_lexical_vars (v1, v2) =
  let v1 = map_tok v1
  and v2 = map_paren (map_comma_list map_lexical_var) v2
  in (v1, v2)
and map_lexical_var =
  function
  | LexicalVar ((v1, v2)) ->
      let v1 = map_is_ref v1 and v2 = map_dname v2 in LexicalVar ((v1, v2))

and map_constraint (v1, v2) =
  let v1 = map_tok v1 in
  let v2 = map_hint_type v2 in
  (v1, v2)
and map_enum_type {
                    e_tok = v_e_tok;
                    e_base = v_e_base;
                    e_constraint = v_e_constraint;
                   } =
  let v_e_tok = map_tok v_e_tok in
  let v_e_base = map_hint_type v_e_base in
  let v_e_constraint = map_option map_constraint v_e_constraint in
  {
    e_tok = v_e_tok;
    e_base = v_e_base;
    e_constraint = v_e_constraint;
  }

and
  map_class_def x =
  let k {
                  c_type = v_c_type;
                  c_name = v_c_name;
                  c_tparams = v_c_tparams;
                  c_extends = v_c_extends;
                  c_implements = v_c_implements;
                  c_body = v_c_body;
                  c_attrs = v_c_attrs;
                  c_enum_type = v_c_enum_type;
                } =
  let v_c_body = map_brace (map_of_list map_class_stmt) v_c_body in
  let v_c_implements = map_of_option map_interface v_c_implements in
  let v_c_extends = map_of_option map_extend v_c_extends in
  let v_c_tparams = map_of_option map_type_params v_c_tparams in
  let v_c_attrs = map_of_option map_attributes v_c_attrs in
  let v_c_name = map_ident v_c_name in
  let v_c_type = map_class_type v_c_type in
  let v_c_enum_type = map_of_option map_enum_type v_c_enum_type in
  {
    c_type = v_c_type;
    c_name = v_c_name;
    c_tparams = v_c_tparams;
    c_extends = v_c_extends;
    c_implements = v_c_implements;
    c_body = v_c_body;
    c_attrs = v_c_attrs;
    c_enum_type = v_c_enum_type;
  }
 in
  vin.kclass_def (k, all_functions) x


and map_class_type =
  function
  | ClassRegular v1 -> let v1 = map_tok v1 in ClassRegular ((v1))
  | ClassFinal ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_tok v2 in ClassFinal ((v1, v2))
  | ClassAbstract ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_tok v2 in ClassAbstract ((v1, v2))
  | ClassAbstractFinal ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_tok v2
      and v3 = map_tok v3
      in ClassAbstractFinal ((v1, v2, v3))
  | Interface v1 -> let v1 = map_tok v1 in Interface ((v1))
  | Trait v1 -> let v1 = map_tok v1 in Trait ((v1))
  | Enum v1 -> let v1 = map_tok v1 in Enum ((v1))
and map_extend (v1, v2) =
  let v1 = map_tok v1 and v2 = map_fully_qualified_class_name v2 in (v1, v2)
and map_interface (v1, v2) =
  let v1 = map_tok v1
  and v2 = map_comma_list map_fully_qualified_class_name v2
  in (v1, v2)

and map_class_stmt =
  function
  | TraitConstraint ((v1, v2, v3, v4)) ->
      let v1 = map_tok v1
      and v2 = map_wrap map_trait_constraint_kind v2
      and v3 = map_hint_type v3
      and v4 = map_tok v4
      in TraitConstraint ((v1, v2, v3, v4))
  | ClassType ((v1)) ->
      let v1 = map_type_def v1
      in ClassType ((v1))
  | ClassConstants ((v1, v2, opt_ty, v3, v4)) ->
      let v1 = map_option map_tok v1
      and v2 = map_tok v2
      and opt_ty = map_option map_hint_type opt_ty
      and v3 = map_comma_list map_class_constant v3
      and v4 = map_tok v4
      in ClassConstants ((v1, v2, opt_ty, v3, v4))
  | ClassVariables ((v1, opt_ty, v2, v3)) ->
      let v1 = map_class_var_modifier v1
      and opt_ty = map_option map_hint_type opt_ty
      and v2 = map_comma_list map_class_variable v2
      and v3 = map_tok v3
      in ClassVariables ((v1, opt_ty, v2, v3))
  | Method v1 -> let v1 = map_method_def v1 in Method ((v1))
  | XhpDecl v1 -> let v1 = map_xhp_decl v1 in XhpDecl ((v1))
  | UseTrait (v1, v2, v3) ->
      let v1 = map_tok v1 in
      let v2 = map_comma_list map_fully_qualified_class_name v2 in
      let v3 = OCaml.map_of_either map_tok (map_brace (List.map map_trait_rule))
        v3 in
      UseTrait (v1, v2, v3)

and map_trait_rule =
  function
  | InsteadOf ((v1, v2, v3, v4, v5, v6)) ->
      let v1 = map_name v1
      and v2 = map_tok v2
      and v3 = map_ident v3
      and v4 = map_tok v4
      and v5 = map_comma_list map_class_name v5
      and v6 = map_tok v6
      in InsteadOf ((v1, v2, v3, v4, v5, v6))
  | As ((v1, v2, v3, v4, v5)) ->
      let v1 =
        OCaml.map_of_either map_ident
          (fun (v1, v2, v3) ->
             let v1 = map_name v1
             and v2 = map_tok v2
             and v3 = map_ident v3
             in (v1, v2, v3))
          v1
      and v2 = map_tok v2
      and v3 = map_of_list (map_wrap map_modifier) v3
      and v4 = map_of_option map_ident v4
      and v5 = map_tok v5
      in As ((v1, v2, v3, v4, v5))

and map_trait_constraint_kind =
  function | MustExtend -> MustExtend | MustImplement -> MustImplement

and map_xhp_decl =
  function
  | XhpAttributesDecl ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_comma_list map_xhp_attribute_decl v2
      and v3 = map_tok v3
      in XhpAttributesDecl ((v1, v2, v3))
  | XhpChildrenDecl ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_xhp_children_decl v2
      and v3 = map_tok v3
      in XhpChildrenDecl ((v1, v2, v3))
  | XhpCategoriesDecl ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_comma_list map_xhp_category_decl v2
      and v3 = map_tok v3
      in XhpCategoriesDecl ((v1, v2, v3))

and map_xhp_attribute_decl =
  function
  | XhpAttrInherit v1 ->
      let v1 = map_wrap map_xhp_tag v1 in XhpAttrInherit ((v1))
  | XhpAttrDecl ((v1, v2, v3, v4)) ->
      let v1 = map_xhp_attribute_type v1
      and v2 = map_xhp_attr_name v2
      and v3 = map_of_option map_xhp_value_affect v3
      and v4 = map_of_option map_tok v4
      in XhpAttrDecl ((v1, v2, v3, v4))

and map_xhp_attribute_type =
  function
  | XhpAttrType v1 -> let v1 = map_hint_type v1 in XhpAttrType ((v1))
  | XhpAttrVar v1 -> let v1 = map_tok v1 in XhpAttrVar ((v1))
  | XhpAttrEnum ((v1, v2)) ->
      let v1 = map_tok v1
      and v2 = map_brace (map_comma_list map_constant) v2
      in XhpAttrEnum ((v1, v2))
and map_xhp_value_affect (v1, v2) =
  let v1 = map_tok v1 and v2 = map_static_scalar v2 in (v1, v2)

and map_xhp_children_decl =
  function
  | XhpChild v1 -> let v1 = map_wrap map_xhp_tag v1 in XhpChild ((v1))
  | XhpChildCategory v1 ->
      let v1 = map_wrap map_xhp_tag v1 in XhpChildCategory ((v1))
  | XhpChildAny v1 -> let v1 = map_tok v1 in XhpChildAny ((v1))
  | XhpChildEmpty v1 -> let v1 = map_tok v1 in XhpChildEmpty ((v1))
  | XhpChildPcdata v1 -> let v1 = map_tok v1 in XhpChildPcdata ((v1))
  | XhpChildSequence ((v1, v2, v3)) ->
      let v1 = map_xhp_children_decl v1
      and v2 = map_tok v2
      and v3 = map_xhp_children_decl v3
      in XhpChildSequence ((v1, v2, v3))
  | XhpChildAlternative ((v1, v2, v3)) ->
      let v1 = map_xhp_children_decl v1
      and v2 = map_tok v2
      and v3 = map_xhp_children_decl v3
      in XhpChildAlternative ((v1, v2, v3))
  | XhpChildMul ((v1, v2)) ->
      let v1 = map_xhp_children_decl v1
      and v2 = map_tok v2
      in XhpChildMul ((v1, v2))
  | XhpChildOption ((v1, v2)) ->
      let v1 = map_xhp_children_decl v1
      and v2 = map_tok v2
      in XhpChildOption ((v1, v2))
  | XhpChildPlus ((v1, v2)) ->
      let v1 = map_xhp_children_decl v1
      and v2 = map_tok v2
      in XhpChildPlus ((v1, v2))
  | XhpChildParen v1 ->
      let v1 = map_paren map_xhp_children_decl v1 in XhpChildParen ((v1))
and map_xhp_category_decl v = map_wrap map_xhp_tag v


and map_class_constant (v1, v2) =
  let v1 = map_ident v1
  and v2 = map_of_option map_static_scalar_affect v2
  in (v1, v2)
and map_class_variable (v1, v2) =
  let v1 = map_dname v1
  and v2 = map_of_option map_static_scalar_affect v2
  in (v1, v2)
and map_class_var_modifier =
  function
  | NoModifiers v1 -> let v1 = map_tok v1 in NoModifiers ((v1))
  | VModifiers v1 ->
      let v1 = map_of_list (map_wrap map_modifier) v1 in VModifiers ((v1))
and map_method_def x = map_func_def x

and map_modifier =
  function
  | Public -> Public
  | Private -> Private
  | Protected -> Protected
  | Static -> Static
  | Abstract -> Abstract
  | Final -> Final
  | Async -> Async

and map_global_var =
  function
  | GlobalVar v1 -> let v1 = map_dname v1 in GlobalVar ((v1))
  | GlobalDollar ((v1, v2)) ->
      let v1 = map_tok v1
      and v2 = map_r_variable v2
      in GlobalDollar ((v1, v2))
  | GlobalDollarExpr ((v1, v2)) ->
      let v1 = map_tok v1
      and v2 = map_brace map_expr v2
      in GlobalDollarExpr ((v1, v2))
and map_static_var (v1, v2) =
  let v1 = map_dname v1
  and v2 = map_of_option map_static_scalar_affect v2
  in (v1, v2)
and map_static_scalar x = map_expr x
and map_static_scalar_affect (v1, v2) =
  let v1 = map_tok v1 and v2 = map_static_scalar v2 in (v1, v2)
and map_stmt_and_def def =
  let k x = map_stmt x in
  vin.kstmt_and_def (k, all_functions) def

and map_short_lambda_def { sl_modifiers = v_sl_modifiers;
                           sl_params = v_sl_params;
                           sl_tok = v_sl_tok;
                           sl_body = v_sl_body
                         } =
  let v_sl_modifiers = map_of_list (map_wrap map_modifier) v_sl_modifiers in
  let v_sl_body = map_short_lambda_body v_sl_body in
  let v_sl_tok = map_of_option map_tok v_sl_tok in
  let v_sl_params = map_short_lambda_params v_sl_params in
  {
    sl_modifiers = v_sl_modifiers;
    sl_body = v_sl_body;
    sl_tok = v_sl_tok;
    sl_params = v_sl_params;
  }
and map_short_lambda_params =
  function
  | SLSingleParam v1 -> let v1 = map_parameter v1 in SLSingleParam ((v1))
  | SLParams v1 ->
      let v1 = map_paren (map_comma_list_dots map_parameter) v1
      in SLParams ((v1))
  | SLParamsOmitted -> SLParamsOmitted
and map_short_lambda_body =
  function
  | SLExpr v1 -> let v1 = map_expr v1 in SLExpr ((v1))
  | SLBody v1 ->
      let v1 = map_brace (map_of_list map_stmt_and_def) v1 in SLBody ((v1))

and  map_constant_def {
                     cst_toks = v_cst_toks;
                     cst_name = v_cst_name;
                     cst_type = v_cst_type;
                     cst_val = v_cst_val
                   } =
  let v_cst_val = map_static_scalar v_cst_val in
  let v_cst_type = map_of_option map_hint_type v_cst_type in
  let v_cst_name = map_ident v_cst_name in
  let v_cst_toks =
    match v_cst_toks with
    | (v1, v2, v3) ->
        let v1 = map_tok v1
        and v2 = map_tok v2
        and v3 = map_tok v3
        in (v1, v2, v3)
  in
  {
    cst_toks = v_cst_toks;
    cst_name = v_cst_name;
    cst_type = v_cst_type;
    cst_val = v_cst_val
  }



and map_attribute =
  function
  | Attribute v1 -> let v1 = map_wrap map_of_string v1 in Attribute ((v1))
  | AttributeWithArgs ((v1, v2)) ->
      let v1 = map_wrap map_of_string v1
      and v2 = map_paren (map_comma_list map_static_scalar) v2
      in AttributeWithArgs ((v1, v2))
and map_attributes v = map_angle (map_comma_list map_attribute) v
and
  map_type_def {
                 t_tok = v_t_tok;
                 t_name = v_t_name;
                 t_tparams = v_t_tparams;
                 t_tconstraint = v_t_tconstraint;
                 t_tokeq = v_t_tokeq;
                 t_kind = v_t_kind;
                 t_sc = v_t_sc
               } =
  let v_t_sc = map_tok v_t_sc in
  let v_t_kind = map_type_def_kind v_t_kind in
  let v_t_tokeq = map_tok v_t_tokeq in
  let v_t_tconstraint = map_of_option map_constraint v_t_tconstraint in
  let v_t_tparams = map_of_option map_type_params v_t_tparams in
  let v_t_name = map_ident v_t_name in
  let v_t_tok = map_tok v_t_tok in
  {
                 t_tok = v_t_tok;
                 t_name = v_t_name;
                 t_tparams = v_t_tparams;
                 t_tconstraint = v_t_tconstraint;
                 t_tokeq = v_t_tokeq;
                 t_kind = v_t_kind;
                 t_sc = v_t_sc
               }


and map_type_def_kind =
  function
  | Alias v1 -> let v1 = map_hint_type v1 in Alias ((v1))
  | Newtype v1 -> let v1 = map_hint_type v1 in Newtype ((v1))
  | ClassConstType v1 ->
    let v1 = map_option map_hint_type v1 in ClassConstType ((v1))

and map_namespace_use_rule =
  function
  | ImportNamespace v1 ->
      let v1 = map_qualified_ident v1 in ImportNamespace ((v1))
  | AliasNamespace ((v1, v2, v3)) ->
      let v1 = map_qualified_ident v1
      and v2 = map_tok v2
      and v3 = map_ident v3
      in AliasNamespace ((v1, v2, v3))

and map_toplevel =
  function
  | StmtList v1 -> let v1 = map_of_list map_stmt v1 in StmtList ((v1))
  | FuncDef v1 -> let v1 = map_func_def v1 in FuncDef ((v1))
  | ClassDef v1 -> let v1 = map_class_def v1 in ClassDef ((v1))
  | ConstantDef v1 -> let v1 = map_constant_def v1 in ConstantDef v1
  | TypeDef v1 -> let v1 = map_type_def v1 in TypeDef ((v1))
  | NotParsedCorrectly v1 ->
      let v1 = map_of_list map_info v1 in NotParsedCorrectly ((v1))
  | FinalDef v1 -> let v1 = map_info v1 in FinalDef ((v1))
  | NamespaceDef ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_qualified_ident v2
      and v3 = map_tok v3
      in NamespaceDef ((v1, v2, v3))
  | NamespaceBracketDef ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_of_option map_qualified_ident v2
      and v3 = map_brace (map_of_list map_toplevel) v3
      in NamespaceBracketDef ((v1, v2, v3))
  | NamespaceUse ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_comma_list map_namespace_use_rule v2
      and v3 = map_tok v3
      in NamespaceUse ((v1, v2, v3))
and map_program v = map_of_list map_toplevel v

and map_entity =
  function
  | FunctionE v1 -> let v1 = map_func_def v1 in FunctionE ((v1))
  | ClassE v1 -> let v1 = map_class_def v1 in ClassE ((v1))
  | TypedefE v1 -> let v1 = map_type_def v1 in TypedefE ((v1))
  | StmtListE v1 -> let v1 = map_of_list map_stmt v1 in StmtListE ((v1))
  | MethodE v1 -> let v1 = map_method_def v1 in MethodE ((v1))
  | ConstantE v1 -> let v1 = map_constant_def v1 in ConstantE v1
  | ClassConstantE v1 ->
      let v1 = map_class_constant v1 in ClassConstantE ((v1))
  | ClassVariableE ((v1, v2)) ->
      let v1 = map_class_variable v1
      and v2 = map_of_list map_modifier v2
      in ClassVariableE ((v1, v2))
  | XhpAttrE v1 -> let v1 = map_xhp_attribute_decl v1 in XhpAttrE ((v1))
  | MiscE v1 -> let v1 = map_of_list map_info v1 in MiscE ((v1))

and map_any =
  function
  | Expr v1 -> let v1 = map_expr v1 in Expr ((v1))
  | Stmt2 v1 -> let v1 = map_stmt v1 in Stmt2 ((v1))
  | Toplevel v1 -> let v1 = map_toplevel v1 in Toplevel ((v1))
  | Program v1 -> let v1 = map_program v1 in Program ((v1))
  | Entity v1 -> let v1 = map_entity v1 in Entity ((v1))
  | Argument v1 -> let v1 = map_argument v1 in Argument ((v1))
  | Arguments v1 ->
      let v1 = (map_comma_list map_argument) v1
      in Arguments ((v1))
  | Parameter v1 -> let v1 = map_parameter v1 in Parameter ((v1))
  | Parameters v1 ->
      let v1 = map_paren (map_comma_list_dots map_parameter) v1
      in Parameters ((v1))
  | Body v1 ->
      let v1 = map_brace (map_of_list map_stmt_and_def) v1 in Body ((v1))
  | StmtAndDefs v1 ->
      let v1 = map_of_list map_stmt_and_def v1 in StmtAndDefs ((v1))
  | ClassStmt v1 -> let v1 = map_class_stmt v1 in ClassStmt ((v1))
  | ClassConstant2 v1 ->
      let v1 = map_class_constant v1 in ClassConstant2 ((v1))
  | ClassVariable v1 ->
      let v1 = map_class_variable v1 in ClassVariable ((v1))
  | ListAssign v1 -> let v1 = map_list_assign v1 in ListAssign ((v1))
  | ColonStmt2 v1 -> let v1 = map_colon_stmt v1 in ColonStmt2 ((v1))
  | XhpAttribute v1 -> let v1 = map_xhp_attribute v1 in XhpAttribute ((v1))
  | XhpAttrValue v1 -> let v1 = map_xhp_attr_value v1 in XhpAttrValue ((v1))
  | XhpHtml2 v1 -> let v1 = map_xhp_html v1 in XhpHtml2 ((v1))
  | XhpChildrenDecl2 v1 -> let v1 = map_xhp_children_decl v1 in
                           XhpChildrenDecl2 ((v1))
  | Info v1 -> let v1 = map_info v1 in Info ((v1))
  | InfoList v1 -> let v1 = map_of_list map_info v1 in InfoList ((v1))
  | Case2 v1 -> let v1 = map_case v1 in Case2 ((v1))
  | Ident2 v1 -> let v1 = map_ident v1 in Ident2 v1
  | Hint2 v1 -> let v1 = map_hint_type v1 in Hint2 ((v1))

 and all_functions =
    {
      vtop = map_toplevel;
      vstmt_and_def = map_stmt_and_def;
      vprogram = map_program;
      vexpr = map_expr;
      vxhpattrvalue = map_xhp_attr_value;
      vany = map_any;
    }
  in
  all_functions
