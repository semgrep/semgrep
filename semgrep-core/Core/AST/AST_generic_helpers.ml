(*s: pfff/lang_GENERIC_base/AST_generic_helpers.ml *)
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

open AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: constant [[AST_generic.str_of_ident]] *)
let str_of_ident = fst
(*e: constant [[AST_generic.str_of_ident]] *)

(*s: constant [[AST_generic.gensym_counter]] *)
(* You can use 0 for globals, even though this will work only on a single
 * file. Any global analysis will need to set a unique ID for globals too. *)
let gensym_counter = ref 0
(* see sid type in resolved_name *)
(*e: constant [[AST_generic.gensym_counter]] *)
(*s: function [[AST_generic.gensym]] *)
(* see sid type in resolved_name *)
let gensym () =
  incr gensym_counter;
  !gensym_counter
(* before Naming_AST.resolve can do its job *)
(*e: function [[AST_generic.gensym]] *)

(* todo: should also mae sure nameinfo.name_typeargs is empty? *)
let id_of_name (id, nameinfo) =
  match nameinfo.name_qualifier with
  | None | Some (QDots []) -> Id (id, empty_id_info ())
  | _ -> IdQualified ((id, nameinfo), empty_id_info())

let name_of_id id =
  (id, empty_name_info)

let name_of_ids ?(name_typeargs=None) xs =
  match List.rev xs with
  | [] -> failwith "name_of_ids: empty ids"
  | x::xs ->
      let qualif =
        if xs = []
        then None
        else Some (QDots (List.rev xs))
      in
      (x, { name_qualifier = qualif; name_typeargs })

let tyid_of_name (id, nameinfo) =
  match nameinfo.name_qualifier with
  | None | Some (QDots []) -> TyId (id, empty_id_info ())
  | _ -> TyIdQualified ((id, nameinfo), empty_id_info())

(*s: function [[AST_generic.expr_to_pattern]] *)
(* In Go a pattern can be a complex expressions. It is just
 * matched for equality with the thing it's matched against, so in that
 * case it should be a pattern like | _ when expr = x.
 * For Python you can actually have a PatDisj of exception classes.
 * coupling: see pattern_to_expr below
*)
let rec expr_to_pattern e =
  (* TODO: diconstruct e and generate the right pattern (PatLiteral, ...) *)
  match e with
  | Id (id, info) -> PatId (id, info)
  | Tuple (t1, xs, t2) -> PatTuple (t1, xs  |> List.map expr_to_pattern, t2)
  | L l -> PatLiteral l
  | Container(List, (t1, xs, t2)) ->
      PatList(t1, xs |> List.map expr_to_pattern, t2)
  (* Todo:  PatKeyVal *)
  | _ -> OtherPat (OP_Expr, [E e])
(*e: function [[AST_generic.expr_to_pattern]] *)

(*s: exception [[AST_generic.NotAnExpr]] *)
exception NotAnExpr
(*e: exception [[AST_generic.NotAnExpr]] *)
(*s: function [[AST_generic.pattern_to_expr]] *)
(* sgrep: this is to treat pattern metavars as expr metavars *)
let rec pattern_to_expr p =
  match p with
  | PatId (id, info) -> Id (id, info)
  | PatTuple (t1, xs, t2) -> Tuple (t1, xs |> List.map pattern_to_expr, t2)
  | PatLiteral l -> L l
  | PatList (t1, xs, t2) ->
      Container(List, (t1, xs |> List.map pattern_to_expr, t2))
  | OtherPat (OP_Expr, [E e]) -> e
  | PatAs _ | PatVar _ -> raise NotAnExpr
  | _ -> raise NotAnExpr
(*e: function [[AST_generic.pattern_to_expr]] *)

(*s: function [[AST_generic.expr_to_type]] *)
let expr_to_type e =
  (* TODO: diconstruct e and generate the right type (TyBuiltin, ...) *)
  OtherType (OT_Expr, [E e])
(*e: function [[AST_generic.expr_to_type]] *)

(* old: there was a stmt_to_item before *)
(* old: there was a stmt_to_field before *)

(* see also Java_to_generic.entity_to_param *)
(* see also Python_to_generic.expr_to_attribute *)
(* see also Php_generic.list_expr_to_opt *)
(* see also Php_generic.name_of_qualified_ident (also in Java) *)

(*s: function [[AST_generic.opt_to_empty]] *)
(*e: function [[AST_generic.opt_to_empty]] *)

(*s: function [[AST_generic.opt_to_label_ident]] *)
let opt_to_label_ident = function
  | None -> LNone
  | Some id -> LId id
(*e: function [[AST_generic.opt_to_label_ident]] *)

(*s: function [[AST_generic.is_boolean_operator]] *)
(* used in abstract interpreter and type for PHP where we now reuse
 * 'AST_generic.arithmetic_operator' above *)
let is_boolean_operator = function
  | Plus (* unary too *) | Minus (* unary too *)
  | Mult | Div | Mod
  | Pow | FloorDiv | MatMult (* Python *)
  | LSL | LSR | ASR (* L = logic, A = Arithmetic, SL = shift left *)
  | BitOr | BitXor | BitAnd | BitNot | BitClear (* unary *)
  | Range | Nullish | NotNullPostfix | Elvis | Length
  | RangeInclusive
    -> false
  | And | Or | Xor | Not
  | Eq     | NotEq
  | PhysEq | NotPhysEq
  | Lt | LtE | Gt | GtE
  | Cmp | Concat | Append
  | RegexpMatch | NotMatch
  | In | NotIn | Is | NotIs
    -> true
(*e: function [[AST_generic.is_boolean_operator]] *)

let ident_or_dynamic_to_expr name idinfo_opt =
  match name, idinfo_opt with
  (* assert idinfo = _idinfo below? *)
  | EId (id, idinfo), None -> Id (id, idinfo)
  | EId (id, _idinfo), Some idinfo -> Id (id, idinfo)
  | EName n, None -> IdQualified (n, empty_id_info())
  | EName n, Some idinfo -> IdQualified (n, idinfo)
  | EDynamic e, _ -> e


(*s: function [[AST_generic.vardef_to_assign]] *)
(* used in controlflow_build and semgrep *)
let vardef_to_assign (ent, def) =
  let name = ident_or_dynamic_to_expr ent.name None in
  let v =
    match def.vinit with
    | Some v -> v
    | None -> L (Null (Parse_info.fake_info "null"))
  in
  Assign (name, Parse_info.fake_info "=", v)
(*e: function [[AST_generic.vardef_to_assign]] *)

(*s: function [[AST_generic.funcdef_to_lambda]] *)
(* used in controlflow_build *)
let funcdef_to_lambda (ent, def) resolved =
  let idinfo = { (empty_id_info()) with id_resolved = ref resolved } in
  let name = ident_or_dynamic_to_expr ent.name (Some idinfo) in
  let v = Lambda def in
  Assign (name, Parse_info.fake_info "=", v)
(*e: function [[AST_generic.funcdef_to_lambda]] *)

(*s: function [[AST_generic.has_keyword_attr]] *)
let has_keyword_attr kwd attrs =
  attrs |> List.exists (function
    | KeywordAttr (kwd2, _) -> kwd =*= kwd2
    | _ -> false
  )
(*e: function [[AST_generic.has_keyword_attr]] *)

(*e: pfff/lang_GENERIC_base/AST_generic_helpers.ml *)
