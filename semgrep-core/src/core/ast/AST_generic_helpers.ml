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
open Common
open AST_generic
module G = AST_generic
module M = Map_AST

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Helpers to build or convert AST_generic elements.
 *
 * The very often used helper functions are actually in AST_generic.ml at
 * the end (e.g., AST_generic.basic_entity).
 * This module is for the more rarely used helpers.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let str_of_ident = fst

let name_of_entity ent =
  match ent.name with
  | EN (Id (i, pinfo))
  | EN (IdQualified ((i, _), pinfo)) ->
      Some (i, pinfo)
  | EDynamic _ -> None

(* You can use 0 for globals, even though this will work only on a single
 * file. Any global analysis will need to set a unique ID for globals too. *)
let gensym_counter = ref 0

(* see sid type in resolved_name *)
(* see sid type in resolved_name *)
let gensym () =
  incr gensym_counter;
  !gensym_counter

(* before Naming_AST.resolve can do its job *)

let name_of_ids ?(name_typeargs = None) xs =
  match List.rev xs with
  | [] -> failwith "name_of_ids: empty ids"
  | [ x ] -> Id (x, empty_id_info ())
  | x :: xs ->
      let qualif = if xs = [] then None else Some (QDots (List.rev xs)) in
      IdQualified
        ((x, { name_qualifier = qualif; name_typeargs }), empty_id_info ())

(* TODO: refactor name_qualifier and correctly handle this,
 * and factorize code in name_of_ids by calling this function.
 *)
let name_of_ids_with_opt_typeargs xs =
  match List.rev xs with
  | [] -> failwith "name_of_ids_with_opt_typeargs: empty ids"
  | [ (x, None) ] -> Id (x, empty_id_info ())
  | (x, _toptTODO) :: xs ->
      let qualif =
        if xs = [] then None
        else
          Some
            (QDots
               (xs |> List.rev |> List.map fst (* TODO use typeargs in xs!! *)))
      in
      IdQualified
        ( (x, { name_qualifier = qualif; name_typeargs = None (*TODO*) }),
          empty_id_info () )

let name_of_id id = Id (id, empty_id_info ())

let name_of_dot_access e =
  let ( let* ) = Option.bind in
  let rec fetch_ids = function
    | G.N (G.Id (x, _)) -> Some [ x ]
    | G.DotAccess (e1, _, G.EN (G.Id (x, _))) ->
        let* xs = fetch_ids e1.e in
        Some (xs @ [ x ])
    | ___else___ -> None
  in
  let* xs = fetch_ids e.e in
  Some (name_of_ids xs)

(* TODO: you should not need to use that. This is mostly because
 * Constructor and PatConstructor currently takes a dotted_ident instead
 * of a name.
 *)
let dotted_ident_of_name (n : name) : dotted_ident =
  match n with
  | Id (id, _) -> [ id ]
  | IdQualified ((id, _nameinfoTODO), _) ->
      (* TODO, look QDots, ... *)
      [ id ]

(* In Go a pattern can be a complex expressions. It is just
 * matched for equality with the thing it's matched against, so in that
 * case it should be a pattern like | _ when expr = x.
 * For Python you can actually have a PatDisj of exception classes.
 * coupling: see pattern_to_expr below
 *)
let rec expr_to_pattern e =
  (* TODO: diconstruct e and generate the right pattern (PatLiteral, ...) *)
  match e.e with
  | N (Id (id, info)) -> PatId (id, info)
  | Container (Tuple, (t1, xs, t2)) ->
      PatTuple (t1, xs |> List.map expr_to_pattern, t2)
  | L l -> PatLiteral l
  | Container (List, (t1, xs, t2)) ->
      PatList (t1, xs |> List.map expr_to_pattern, t2)
  | Ellipsis t -> PatEllipsis t
  (* Todo:  PatKeyVal *)
  | _ -> OtherPat (("ExprToPattern", fake ""), [ E e ])

exception NotAnExpr

(* sgrep: this is to treat pattern metavars as expr metavars *)
let rec pattern_to_expr p =
  (match p with
  | PatId (id, info) -> N (Id (id, info))
  | PatTuple (t1, xs, t2) ->
      Container (Tuple, (t1, xs |> List.map pattern_to_expr, t2))
  | PatLiteral l -> L l
  | PatList (t1, xs, t2) ->
      Container (List, (t1, xs |> List.map pattern_to_expr, t2))
  | OtherPat (("ExprToPattern", _), [ E e ]) -> e.e
  | PatAs _
  | PatVar _ ->
      raise NotAnExpr
  | _ -> raise NotAnExpr)
  |> G.e

let expr_to_type e =
  (* TODO: diconstruct e and generate the right type (TyBuiltin, ...) *)
  OtherType (OT_Expr, [ E e ]) |> G.t

(* See also exprstmt, and stmt_to_expr in AST_generic.ml *)

(* old: there was a stmt_to_item before *)
(* old: there was a stmt_to_field before *)

(* see also Java_to_generic.entity_to_param *)
(* see also Python_to_generic.expr_to_attribute *)
(* see also Php_generic.list_expr_to_opt *)
(* see also Php_generic.name_of_qualified_ident (also in Java) *)

let opt_to_label_ident = function
  | None -> LNone
  | Some id -> LId id

(* used in abstract interpreter and type for PHP where we now reuse
 * 'AST_generic.arithmetic_operator' above *)
(*
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
*)

let name_or_dynamic_to_expr name idinfo_opt =
  (match (name, idinfo_opt) with
  (* assert idinfo = _idinfo below? *)
  | EN (Id (id, idinfo)), None -> N (Id (id, idinfo))
  | EN (Id (id, _idinfo)), Some idinfo -> N (Id (id, idinfo))
  | EN (IdQualified (n, idinfo)), None -> N (IdQualified (n, idinfo))
  | EN (IdQualified (n, _idinfo)), Some idinfo -> N (IdQualified (n, idinfo))
  | EDynamic e, _ -> e.e)
  |> G.e

(* used in controlflow_build and semgrep *)
let vardef_to_assign (ent, def) =
  let name = name_or_dynamic_to_expr ent.name None in
  let v =
    match def.vinit with
    | Some v -> v
    | None -> L (Null (Parse_info.unsafe_fake_info "null")) |> G.e
  in
  Assign (name, Parse_info.unsafe_fake_info "=", v) |> G.e

(* used in controlflow_build *)
let funcdef_to_lambda (ent, def) resolved =
  let idinfo = { (empty_id_info ()) with id_resolved = ref resolved } in
  let name = name_or_dynamic_to_expr ent.name (Some idinfo) in
  let v = Lambda def |> G.e in
  Assign (name, Parse_info.unsafe_fake_info "=", v) |> G.e

let funcbody_to_stmt = function
  | FBStmt st -> st
  | FBExpr e -> G.exprstmt e
  | FBDecl sc -> Block (sc, [], sc) |> G.s
  | FBNothing -> Block (G.fake_bracket []) |> G.s

let has_keyword_attr kwd attrs =
  attrs
  |> List.exists (function
       | KeywordAttr (kwd2, _) -> kwd =*= kwd2
       | _ -> false)

(*****************************************************************************)
(* Abstract position and constness for comparison *)
(*****************************************************************************)

(* update: you should now use AST_generic.equal_any which internally
 * does not care about position information.
 * TODO: can we remove this function now then?
 *)
let abstract_for_comparison_visitor recursor =
  let hooks =
    {
      M.default_visitor with
      M.kinfo = (fun (_k, _) i -> { i with Parse_info.token = Parse_info.Ab });
      M.kidinfo =
        (fun (k, _) ii -> k { ii with AST_generic.id_constness = ref None });
    }
  in
  let vout = M.mk_visitor hooks in
  recursor vout

let abstract_for_comparison_any x =
  abstract_for_comparison_visitor (fun visitor -> visitor.M.vany x)

(*****************************************************************************)
(* Associative-Commutative (AC) matching *)
(*****************************************************************************)

let is_associative_operator op =
  match op with
  | Or
  | And
  | BitOr
  | BitAnd
  | BitXor
  | Concat ->
      true
  (* TODO: Plus, Mult, ... *)
  | __else__ -> false

let ac_matching_nf op args =
  (* yes... here we use exceptions like a "goto" to avoid the option monad *)
  let rec nf args1 =
    args1
    |> List.map (function
         | Arg e -> e
         | ArgKwd _
         | ArgType _
         | ArgOther _ ->
             raise_notrace Exit)
    |> List.map nf_one |> List.flatten
  and nf_one e =
    match e.e with
    | Call ({ e = IdSpecial (Op op1, _tok1); _ }, (_, args1, _)) when op = op1
      ->
        nf args1
    | _ -> [ e ]
  in
  if is_associative_operator op then (
    try Some (nf args)
    with Exit ->
      logger#error
        "ac_matching_nf: %s(%s): unexpected ArgKwd | ArgType | ArgOther"
        (show_operator op) (show_arguments args);
      None)
  else None

let undo_ac_matching_nf tok op : expr list -> expr option = function
  | [] -> None
  | [ arg ] -> Some arg
  | a1 :: a2 :: args ->
      let mk_op x y =
        Call (IdSpecial (Op op, tok) |> G.e, fake_bracket [ Arg x; Arg y ])
        |> G.e
      in
      Some (List.fold_left mk_op (mk_op a1 a2) args)

(*****************************************************************************)
(* AST_generic_ conversions *)
(*****************************************************************************)

module G_ = AST_generic_

(* This AST_generic_ module is ugly, but it was written to allow to move
 * AST_generic.ml out of pfff/ and inside semgrep/. However there are many
 * language-specific ASTs that were using AST_generic.ml to factorize
 * the definitions of operators. To break the dependency we had
 * to duplicate that part of AST_generic in pfff/h_program-lang/AST_generic_.ml
 * (note the underscore at the end), but then we need those boilerplate
 * functions below to convert them back to AST_generic.
 *
 * alt: use polymorphic variants (e.g., `Plus)
 *)

let (conv_op : AST_generic_.operator -> AST_generic.operator) = function
  | G_.Plus -> G.Plus
  | G_.Minus -> G.Minus
  | G_.Mult -> G.Mult
  | G_.Div -> G.Div
  | G_.Mod -> G.Mod
  | G_.Pow -> G.Pow
  | G_.FloorDiv -> G.FloorDiv
  | G_.MatMult -> G.MatMult
  | G_.LSL -> G.LSL
  | G_.LSR -> G.LSR
  | G_.ASR -> G.ASR
  | G_.BitOr -> G.BitOr
  | G_.BitXor -> G.BitXor
  | G_.BitAnd -> G.BitAnd
  | G_.BitNot -> G.BitNot
  | G_.BitClear -> G.BitClear
  | G_.And -> G.And
  | G_.Or -> G.Or
  | G_.Xor -> G.Xor
  | G_.Not -> G.Not
  | G_.Eq -> G.Eq
  | G_.NotEq -> G.NotEq
  | G_.PhysEq -> G.PhysEq
  | G_.NotPhysEq -> G.NotPhysEq
  | G_.Lt -> G.Lt
  | G_.LtE -> G.LtE
  | G_.Gt -> G.Gt
  | G_.GtE -> G.GtE
  | G_.Cmp -> G.Cmp
  | G_.Concat -> G.Concat
  | G_.Append -> G.Append
  | G_.RegexpMatch -> G.RegexpMatch
  | G_.NotMatch -> G.NotMatch
  | G_.Range -> G.Range
  | G_.RangeInclusive -> G.RangeInclusive
  | G_.NotNullPostfix -> G.NotNullPostfix
  | G_.Length -> G.Length
  | G_.Elvis -> G.Elvis
  | G_.Nullish -> G.Nullish
  | G_.In -> G.In
  | G_.NotIn -> G.NotIn
  | G_.Is -> G.Is
  | G_.NotIs -> G.NotIs

let (conv_incr : AST_generic_.incr_decr -> AST_generic.incr_decr) = function
  | G_.Incr -> G.Incr
  | G_.Decr -> G.Decr

let (conv_prepost : AST_generic_.prefix_postfix -> AST_generic.prefix_postfix) =
  function
  | G_.Prefix -> G.Prefix
  | G_.Postfix -> G.Postfix

let (conv_incdec :
      AST_generic_.incr_decr * AST_generic_.prefix_postfix ->
      AST_generic.incr_decr * AST_generic.prefix_postfix) =
 fun (x, y) -> (conv_incr x, conv_prepost y)

let (conv_class_kind :
      AST_generic_.class_kind * Parse_info.t ->
      AST_generic.class_kind * Parse_info.t) =
 fun (c, t) ->
  ( (match c with
    | G_.Class -> G.Class
    | G_.Interface -> G.Interface
    | G_.Trait -> G.Trait),
    t )

let (conv_function_kind :
      AST_generic_.function_kind * Parse_info.t ->
      AST_generic.function_kind * Parse_info.t) =
 fun (c, t) ->
  ( (match c with
    | G_.Function -> G.Function
    | G_.Method -> G.Method
    | G_.LambdaKind -> G.LambdaKind
    | G_.Arrow -> G.Arrow),
    t )
