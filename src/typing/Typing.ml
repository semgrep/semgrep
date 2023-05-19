(* Nat Mote
 *
 * Copyright (C) 2019-2023 Semgrep, Inc.
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
module G = AST_generic

(* returns possibly the inferred type of the expression,
 * as well as an ident option that can then be used to query LSP to get the
 * type of the ident.
 *
 * Old type inference over `AST_generic.type_`. TODO Let's migrate all of this
 * to the new type inference over `Type.t`.
 *)
let rec type_of_expr_old lang e : G.type_ option * G.ident option =
  match e.G.e with
  (* TODO? or generate a fake "new" id for LSP to query on tk? *)
  | G.New (_tk, t, _ii, _) -> (Some t, None)
  (* this is covered by the basic type propagation done in Naming_AST.ml *)
  | G.N
      (G.IdQualified
        { name_last = idb, None; name_info = { G.id_type = tb; _ }; _ })
  | G.DotAccess
      ({ e = IdSpecial (This, _); _ }, _, FN (Id (idb, { G.id_type = tb; _ })))
    ->
      (!tb, Some idb)
  (* deep: those are usually resolved only in deep mode *)
  | G.DotAccess (_, _, FN (Id (idb, { G.id_type = tb; _ }))) -> (!tb, Some idb)
  (* deep: same *)
  | G.Call
      ( { e = G.DotAccess (_, _, FN (Id (idb, { G.id_type = tb; _ }))); _ },
        _args ) -> (
      match !tb with
      (* less: in OCaml functions can be curried, so we need to match
       * _params and _args to calculate the resulting type.
       *)
      | Some { t = TyFun (_params, tret); _ } -> (Some tret, Some idb)
      | Some _
      | None ->
          (None, Some idb))
  (* deep: in Java, there can be an implicit `this.`
     so calculate the type in the same way as above
     THINK: should we do this for all languages? Why not? *)
  | G.Call ({ e = N (Id (idb, { G.id_type = tb; _ })); _ }, _args)
    when lang =*= Lang.Java -> (
      match !tb with
      | Some { t = TyFun (_params, tret); _ } -> (Some tret, Some idb)
      | Some _
      | None ->
          (None, Some idb))
  | G.Conditional (_, e1, e2) ->
      let ( let* ) = Option.bind in
      let t1opt, id1opt = type_of_expr lang e1 in
      let t2opt, id2opt = type_of_expr lang e2 in
      (* LATER: we could also not enforce to have a type for both branches,
       * but let's go simple for now and enforce both branches have
       * a type and that the types are equal.
       *)
      let topt =
        let* t1 = t1opt in
        let* t2 = t2opt in
        (* LATER: in theory we should look if the types are compatible,
         * and take the lowest upper bound of the two types *)
        if AST_utils.with_structural_equal G.equal_type_ t1 t2 then Some t1
        else None
      in
      let idopt =
        (* TODO? is there an Option.xxx or Common.xxx function for that? *)
        match (id1opt, id2opt) with
        | Some id1, _ -> Some id1
        | _, Some id2 -> Some id2
        | None, None -> None
      in
      (topt, idopt)
  | _else_ -> (None, None)

(* returns possibly the inferred type of the expression,
 * as well as an ident option that can then be used to query LSP to get the
 * type of the ident.
 *
 * New Type inference over `Type.t`. Prefer this to the old type inference over
 * `AST_generic.type_`. Eventually we'll do all the type inference here and
 * delete the old. *)
and type_of_expr_new lang e : G.name Type.t * G.ident option =
  match e.G.e with
  | G.L lit ->
      let t =
        match lit with
        | G.Int _ -> Type.Builtin Type.Int
        | G.Bool _ -> Type.Builtin Type.Bool
        | _else_ -> Type.NoType
      in
      (t, None)
  | G.N (Id (ident, id_info)) ->
      let t = resolved_type_of_id_info lang id_info in
      (t, Some ident)
  (* Binary operator *)
  | G.Call ({ e = IdSpecial (Op op, _); _ }, (_l, [ Arg e1; Arg e2 ], _r)) ->
      let t1, _id = type_of_expr_new lang e1 in
      let t2, _id = type_of_expr_new lang e2 in
      let t =
        match (t1, op, t2) with
        | ( Type.Builtin Type.Int,
            (G.Plus | G.Minus (* TODO more *)),
            Type.Builtin Type.Int ) ->
            Type.Builtin Type.Int
        | ( _,
            ( G.Eq | G.PhysEq | G.NotEq | G.NotPhysEq | G.Lt | G.LtE | G.Gt
            | G.GtE | G.And | G.Or ),
            _ ) ->
            Type.Builtin Type.Bool
        | Type.Builtin Type.Bool, (G.BitOr | G.BitAnd | G.BitXor), _
        | _, (G.BitOr | G.BitAnd | G.BitXor), Type.Builtin Type.Bool
          when lang =*= Lang.Java ->
            (* If the operands to |, &, or ^ are boolean, in Java these are
             * boolean operators. If we can resolve one operand to a boolean, we
             * know that in a well-formed program, the other is also a boolean.
             * *)
            Type.Builtin Type.Bool
        | _else_ -> Type.NoType
      in
      (t, None)
  (* Unary operator *)
  | G.Call ({ e = IdSpecial (Op op, _); _ }, (_l, [ Arg e ], _r)) ->
      let t, _id = type_of_expr_new lang e in
      let t =
        match (op, t) with
        | G.Not, _ -> Type.Builtin Type.Bool
        | _else_ -> Type.NoType
      in
      (t, None)
  | _else_ -> (Type.NoType, None)

and type_of_expr lang e : G.type_ option * G.ident option =
  let t, id = type_of_expr_new lang e in
  match
    Type.to_ast_generic_type_ lang
      (fun name _alts ->
        (* TODO Do something with alts? Or are they already there? *) name)
      t
  with
  | None -> type_of_expr_old lang e
  | Some t -> (Some t, id)

and resolved_type_of_id_info lang info : G.name Type.t =
  match !(info.G.id_type) with
  | Some t -> type_of_ast_generic_type lang t
  | None -> Type.NoType

and type_of_ast_generic_type lang t : G.name Type.t =
  match t.G.t with
  | G.TyN (Id ((str, _), _) as name) -> (
      match Type.builtin_type_of_string lang str with
      | Some t -> Type.Builtin t
      | None -> Type.N ((name, []), []))
  (* TODO *)
  | _else_ -> Type.NoType
