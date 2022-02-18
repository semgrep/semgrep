(* Yoann Padioleau
 *
 * Copyright (C) 2022 r2c
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
open AST_generic
module G = AST_generic
module H = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Types for the typing of AST_generic.expr.
 *
 * Note that right now this module is only used by Graph_code_AST.ml
 * during name resolving as we need to remember the type of entities
 * and expressions to be able to resolve field or method access.
 *
 * Why not simply reuse AST_generic.type_ ? Because here we want to make
 * sure type names are fully resolved.
 * In the futur, we may also want to apply more complex normalizations to
 * simplify type checking or typing/name resolving.
 *
 * alt: reuse, which would remove the need for the converters?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* TODO: polymorphic types *)
type t =
  (* fully qualified name, as in graph_code, for L.lookup_dotted_ident_opt
   * less: use Graph_code.node instead?
   *)
  | TN of G.dotted_ident
  (* a few builtins *)
  | TBuiltin of string G.wrap
  | TList of t
  | TFunction of G.parameters (* TODO? normalize also params? *) * t
  (* todos *)
  | TTodo of G.todo_kind
[@@deriving show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let ( let* ) = Option.bind

(* todo: generate an Id with id_resolved or directly an IdQualified?
 * What will work in typed_metavar m_compatible_type?
 *)
let name_of_dotted_ident (xs : G.dotted_ident) : G.name = H.name_of_ids xs

(*****************************************************************************)
(* TyExpr conversion (mostly for Python) *)
(*****************************************************************************)

(* reverse of Generic_vs_generic.make_dotted
 * transform a.b.c.d, which is parsed as (((a.b).c).d), in Some [d;c;b;a]
 * precondition: Naming_AST must have been called.
 *)
let undot_expr_opt e =
  let rec aux e =
    match e.e with
    (* TODO: Id itself can have been resolved!! so we need to
     * concatenate. See tests/python/misc_regression[12].py
     *)
    | N (Id (id, _)) -> Some [ id ]
    | DotAccess (e, _, FN (Id (id, _))) ->
        let* ids = aux e in
        Some (id :: ids)
    | _ -> None
  in
  let* ids = aux e in
  Some (List.rev ids)

let expr_to_type_after_naming_opt e =
  match e.e with
  | N n -> Some (TyN n |> G.t)
  (* For Python we need to transform a.b.c DotAccess expr in a qualified name*)
  | DotAccess (_, _, _) ->
      let* ids = undot_expr_opt e in
      Some (TyN (H.name_of_ids ids) |> G.t)
  | _ -> None

(*****************************************************************************)
(* Converters *)
(*****************************************************************************)

(* reverse of type_of_type_generic_opt *)
let type_generic_of_type_opt t =
  let rec aux t =
    match t with
    | TN xs ->
        let n = name_of_dotted_ident xs in
        Some (TyN n |> G.t)
    (* right now we don't use the params in
     * Generic_vs_generic.m_compatible_type so create empty params for now
     * TODO: generate right TyFun params from fparams
     *)
    | TFunction (_fparamsTODO, t) ->
        let* ty = aux t in
        Some (TyFun ([], ty) |> G.t)
    | _ -> None
  in
  aux t
