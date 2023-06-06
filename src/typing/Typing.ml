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
 * type of the ident. *)
let rec type_of_expr lang e : G.name Type.t * G.ident option =
  let ty, ident =
    match e.G.e with
    | G.L lit ->
        let t =
          match lit with
          (* NB: We could infer Type.Number for JS int/float literals, but we can
           * handle that relationship in matching and we can be more precise for
           * now. One actual rule uses `float` for a typed metavariable in JS so
           * let's avoid breaking that for now at least. *)
          | G.Int _ -> Type.Builtin Type.Int
          | G.Float _ -> Type.Builtin Type.Float
          | G.Bool _ -> Type.Builtin Type.Bool
          | G.String _ -> Type.Builtin Type.String
          | _else_ -> Type.NoType
        in
        (t, None)
    | G.N name
    | G.DotAccess (_, _, FN name) ->
        type_of_name lang name
    (* TODO? or generate a fake "new" id for LSP to query on tk? *)
    (* We conflate the type of a class with the type of its instance. Maybe at
     * some point we should introduce a `Class` type and unwrap it here upon
     * instantiation. *)
    | G.New (_tk, t, _ii, _) -> (type_of_ast_generic_type lang t, None)
    (* Binary operator *)
    | G.Call ({ e = IdSpecial (Op op, _); _ }, (_l, [ Arg e1; Arg e2 ], _r)) ->
        let t1, _id = type_of_expr lang e1 in
        let t2, _id = type_of_expr lang e2 in
        let t =
          match (t1, op, t2) with
          | Type.(Builtin (Int | Float)), (G.Plus | G.Minus (* TODO more *)), _
          | _, (G.Plus | G.Minus (* TODO more *)), Type.(Builtin (Int | Float))
          (* Note that `+` is overloaded in many languages and may also be
           * string concatenation, and unfortunately some languages such
           * as Java and JS/TS have implicit coercions to string. *)
            when lang =*= Lang.Python (* TODO more *) ->
              Type.Builtin Type.Number
          | ( Type.Builtin Type.Int,
              (G.Plus | G.Minus (* TODO more *)),
              Type.Builtin Type.Int ) ->
              Type.Builtin Type.Int
          | ( _,
              ( G.Eq | G.PhysEq | G.NotEq | G.NotPhysEq | G.Lt | G.LtE | G.Gt
              | G.GtE | G.In | G.NotIn | G.Is | G.NotIs | G.And ),
              _ ) ->
              Type.Builtin Type.Bool
          | Type.Builtin Type.Bool, G.Or, _
          | _, G.Or, Type.Builtin Type.Bool ->
              Type.Builtin Type.Bool
          | _, G.Or, _ when lang =*= Lang.Java ->
              (* E.g. in Python you can write `x or ""` to mean `""` in case `x` is `None`.
               * THINK: Is there a similar idiom involving `and`/`&&` ? *)
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
        let t, _id = type_of_expr lang e in
        let t =
          match (op, t) with
          | G.Not, _ -> Type.Builtin Type.Bool
          | _else_ -> Type.NoType
        in
        (t, None)
    | G.Call (e, _args) ->
        let t, id = type_of_expr lang e in
        let t =
          match t with
          (* less: in OCaml functions can be curried, so we need to match _params
           * and _args to calculate the resulting type. *)
          | Function (_params, ret) -> ret
          | _else_ -> Type.NoType
        in
        (t, id)
    | G.Conditional (_, e1, e2) ->
        let t1, id1opt = type_of_expr lang e1 in
        let t2, id2opt = type_of_expr lang e2 in
        (* LATER: we could also not enforce to have a type for both branches,
         * but let's go simple for now and enforce both branches have
         * a type and that the types are equal.
         *)
        let t =
          (* LATER: in theory we should look if the types are compatible,
           * and take the lowest upper bound of the two types *)
          let eq = Type.equal (AST_utils.with_structural_equal G.equal_name) in
          if eq t1 t2 then t1 else Type.NoType
        in
        let idopt =
          (* TODO? is there an Option.xxx or Common.xxx function for that? *)
          match (id1opt, id2opt) with
          | Some id1, _ -> Some id1
          | _, Some id2 -> Some id2
          | None, None -> None
        in
        (t, idopt)
    | _else_ -> (Type.NoType, None)
  in
  match ty with
  | Type.NoType
  | Type.Todo _
  | Type.UnresolvedName _ ->
      (* Dependency injection to avoid a circular dependency between this module
       * and Typing_heuristics. To resolve a DotAccess, for example, we need to
       * get the type of the LHS in order to apply heuristics for the RHS. *)
      let f e = type_of_expr lang e |> fst in
      let guessed_type = Typing_heuristics.guess_type lang f e in
      if Type.is_real_type guessed_type then (guessed_type, ident)
      else (ty, ident)
  | _else_ -> (ty, ident)

and type_of_name lang = function
  | Id (ident, id_info) ->
      let t = resolved_type_of_id_info lang id_info in
      let t =
        match t with
        (* Even if we can't resolve the type, the name of the ident can still be
         * useful for matching. If we had a Typeof variant of Type.t, it might
         * be more accurate to say this is `Type.Typeof (Type.UnresolvedName
         * ...)`. See also how we conflate `Class<T>` with `T` itself, evident
         * in the way we infer the type for a `new` expression. *)
        | Type.NoType -> Type.UnresolvedName (fst ident, [])
        | _else_ -> t
      in
      (t, Some ident)
  | IdQualified { name_last = ident, None; name_info; _ } ->
      let t = resolved_type_of_id_info lang name_info in
      (* TODO Use UnresolvedName like above when we can't resolve the name? What
       * part of the qualified name should be used? The whole thing? How should
       * it be converted to a string representation? *)
      (t, Some ident)
  | IdQualified { name_last = _, Some _; _ } ->
      (* TODO What to do with type arguments? *)
      (Type.NoType, None)

and resolved_type_of_id_info lang info : G.name Type.t =
  match !(info.G.id_type) with
  | Some t -> type_of_ast_generic_type lang t
  | None -> Type.NoType

and type_of_ast_generic_type lang t : G.name Type.t =
  match t.G.t with
  (* TODO Check language? Someone could make a user type named `nil` in Java,
   * for example. *)
  | G.TyN (Id ((("null" | "nil" | "NULL"), _), _)) -> Type.Null
  | G.TyN (Id ((str, _), _) as name) -> (
      match Type.builtin_type_of_string lang str with
      | Some t -> Type.Builtin t
      | None -> Type.N ((name, []), []))
  (* Pick up IdQualified as well *)
  | G.TyN name -> Type.N ((name, []), [])
  | G.TyApply ({ G.t = G.TyN name; _ }, (_l, args, _r)) ->
      let args =
        args
        |> Common.map (function
             | G.TA t -> Type.TA (type_of_ast_generic_type lang t)
             | _else_ -> Type.OtherTypeArg None)
      in
      Type.N ((name, args), [])
  | G.TyApply _ ->
      (* Should always be a TyN according to the comments in AST_generic.ml *)
      Type.NoType
  | G.TyArray ((_l, size_expr, _r), elem_type) ->
      let size =
        match size_expr with
        | Some { G.e = G.L (G.Int (Some n, _)); _ } -> Some n
        | _else_ -> None
      in
      let elem_type = type_of_ast_generic_type lang elem_type in
      Type.Array (size, elem_type)
  | G.TyFun (params, tret) ->
      let params =
        params
        |> Common.map (function
             | G.Param { G.pname; ptype; _ } ->
                 let pident = Option.map fst pname in
                 let ptype =
                   Option.map (type_of_ast_generic_type lang) ptype
                   |> Type.of_opt
                 in
                 Type.Param { Type.pident; ptype }
             | _else_ -> OtherParam None)
      in
      let tret = type_of_ast_generic_type lang tret in
      Type.Function (params, tret)
  | G.TyPointer (_, t) ->
      let t = type_of_ast_generic_type lang t in
      Type.Pointer t
  | G.TyExpr e when Lang.is_js lang ->
      (* TyExpr is a bit of a suspicious construct with a few uses. But in JS/TS
       * it's most often used for `new` where you could write `new (foo())()` to
       * instantiate the class returned by the function `foo()` (or any other
       * arbitrary expression). So, we find the type of that expression and pass
       * it up. *)
      let t, _id = type_of_expr lang e in
      t
  (* TODO: Need to expand Type.ml if we want to represent more *)
  | _else_ -> Type.NoType
