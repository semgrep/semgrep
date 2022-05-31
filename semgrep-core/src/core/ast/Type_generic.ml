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
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Common representation for (builtin) types across languages.
 *
 * In certain languages, a boolean declaration is using the type name
 * "boolean", in another language "Bool", in yet another "bool".
 * This is tedious to handle all those specifities during matching, so
 * the goal of this file is to factorize those specifities here.
 *
 * alt:
 *  - store this directly in AST_generic.ml and do the work in the
 *    xxx_to_generic.ml
 *
 * related:
 *  - Type.ml in deep-semgrep
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*
 * coupling: this is mostly used for typed metavariables like '($X: int)' to
 * match literals like 'f(1)', so most of the types below should have
 * a corresponding construct in AST_generic.literal
 *)
type builtin_type = TBool | TInt | TFloat | TNumber | TString
[@@deriving show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* less: should sanity check things by looking at [lang] *)
let builtin_type_of_ident _langTODO str =
  match str with
  | "int" -> Some TInt
  | "float" -> Some TFloat
  | "str"
  | "string"
  | "String" ->
      Some TString
  (* TS *)
  | "number" -> Some TNumber
  | "boolean" -> Some TBool
  | _ -> None

let builtin_type_of_type lang t =
  match t.G.t with
  (* for Python literal checking *)
  | G.TyExpr { e = G.N (G.Id ((str, _t), _idinfo)); _ } ->
      builtin_type_of_ident lang str
  (* for Java/Go/... literals *)
  | G.TyN (Id ((str, _t), _idinfo)) -> builtin_type_of_ident lang str
  | _ -> None
