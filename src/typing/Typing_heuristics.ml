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

module G = AST_generic

(* This module is for guessing the type of an expression, when we can't figure
 * it out using ordinary type inference. This typically happens when some name
 * cannot be resolved because it either came from an external file (in OSS
 * Semgrep) or from the standard library or a third party library (in Pro
 * Engine).
 *
 * For example, in Java we guess that `x.equals(y)` returns a `boolean`, even if
 * we don't know the type of `x`. *)

(******************************************************************************)
(* Helpers *)
(******************************************************************************)

(* Currently, for types created during naming in Semgrep (OSS and Pro Engine),
 * we can't tell the difference between a resolved name and an unresolved name,
 * so we turn them all into `Type.N`s. Some of the names will be fully-qualified
 * resolved names, and some will just be names as written by the user.
 *
 * We should use Type.t for `id_type` to address this ambiguity. In the
 * meantime, we will use this helper function to abstract it away for type
 * guessing purposes. *)
let name_and_targs_of_named_type lang = function
  | Type.N ((G.Id ((str, _), _), targs), _)
  | Type.UnresolvedName (str, targs) ->
      Some (str, targs)
  | Type.N
      ( ( G.IdQualified { G.name_last; name_middle = Some (QDots middle); _ },
          targs ),
        _ ) ->
      let (str_last, _), _ = name_last in
      let middle_strs =
        middle |> Common.map (fun ((str, _info), _targs) -> str)
      in
      let str = String.concat "." (middle_strs @ [ str_last ]) in
      Some (str, targs)
  | Type.Builtin b -> Some (Type.name_of_builtin_type lang b, [])
  | _else_ -> None

let guess_type_of_dotaccess lang lhs_ty str =
  match (lang, name_and_targs_of_named_type lang lhs_ty, str) with
  | ( Lang.Java,
      _,
      ("equals" | "isEmpty" | "contains" | "containsKey" | "containsValue") ) ->
      (* Really the return type is all that matters. We could add the parameters
       * later if we need to. *)
      Type.Function ([], Type.Builtin Type.Bool)
  | Lang.Java, _, ("size" | "length") ->
      Type.Function ([], Type.Builtin Type.Int)
  (* For unresolved types with one type parameter, assume that the `get`
   * method's return type is the type parameter (e.g. List<T>). For unresolved
   * types with two type parameters, assume that the `get` method's return type
   * is the second (e.g. Map<K, V>) *)
  | ( Lang.Java,
      Some (_str, ([ _; Type.TA elt_type ] | [ Type.TA elt_type ])),
      "get" ) ->
      (* Param type could be Top if we add that as a type *)
      let param = Type.Param { pident = None; ptype = Type.NoType } in
      Type.Function ([ param ], elt_type)
  | Lang.Java, Some (("String" | "java.lang.String"), _), "matches" ->
      let param =
        Type.Param { pident = None; ptype = Type.Builtin Type.String }
      in
      Type.Function ([ param ], Type.Builtin Type.Bool)
  | _else_ -> Type.NoType

(******************************************************************************)
(* Entry Point *)
(******************************************************************************)

(* Guess the type of an expression based on heuristics. This is a fallback used
 * only if we cannot resolve the type of an expression using traditional type
 * inference.
 *
 * We take `type_of_expr` as a parameter so that we can determine the type of
 * subexpressions as part of guessing the type of `e`. *)
let guess_type lang type_of_expr e =
  match e.G.e with
  | G.DotAccess (lhs, _, FN name) -> (
      let lhs_ty = type_of_expr lhs in
      match name with
      | G.Id ((str, _), _) -> guess_type_of_dotaccess lang lhs_ty str
      | G.IdQualified _ -> Type.NoType)
  | _else_ -> Type.NoType
