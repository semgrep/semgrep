(* Yoann Padioleau
 *
 * Copyright (C) 2022, 2023 Semgrep Inc.
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
open Sexplib.Std
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Types for type inference.
 *
 * Why not simply reuse AST_generic.type_ ? Because here we want to make
 * sure type names are fully resolved! AST_generic is also meant to represent
 * types that people could actually write as part of programs. This is a more
 * abstract representation of types, divorced from the notion of a syntax tree.
 * For example, we don't include tokens here. We may introduce types here that
 * wouldn't appear as written types in a program. We might even introduce type
 * variables here, which would not be appropriate to include in AST_generic.
 *
 * In the future, we may also want to apply more complex normalizations to
 * simplify typing/naming.
 *
 * TODO: polymorphic types? type parameters are resolved names?
 *
 * These types are polymorphic to support the Pro Engine, which represents
 * resolved names differently.
 *
 * history: this used to be in semgrep-pro but was simpler to move it in
 * OSS because ???
 *)

(*****************************************************************************)
(* Visitor Helpers *)
(*****************************************************************************)

class virtual ['self] map_parent =
  object (_self : 'self)
    (* Could inherit from the AST_generic visitor but we just need this one
     * thing, and it's just a string list so there's not really a need to
     * recurse down. We should put alternate names in the type parameter anyway.
     * *)
    method visit_alternate_name _env x = x
    method visit_parsed_int _env x = x
  end

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type todo_kind = string option

(* Fully qualified name *)
and 'resolved name = 'resolved * 'resolved type_argument list

and 'resolved type_argument =
  | TA of 'resolved t
  (* Java: `?`, `? extends Foo`, `? super Foo` *)
  | TAWildcard of 'resolved type_arg_constraint option
  | OtherTypeArg of todo_kind

and 'resolved type_arg_constraint =
  (* Java: `? extends Foo` *)
  | TAUpper of 'resolved t
  (* Java: `? super Foo` *)
  | TALower of 'resolved t

and 'resolved t =
  | N of 'resolved name * (* alt names *) G.alternate_name list
  (* e.g. for unresolved types in core libraries
   * TODO? generalize and have just NotYetHandled of G.type_?
   * so at least we are as good as regular Semgrep for typing?
   *)
  | UnresolvedName of string * 'resolved type_argument list
  | Builtin of builtin_type
  | Null (* for null analysis, alt: TOption of t *)
  (* TODO: generalize to other containers? But then use a TyContainer
   * in SAST.ml? *)
  (* int option for the cases where we know the size of the array *)
  | Array of (Parsed_int.t[@name "parsed_int"]) option * 'resolved t
  | Function of 'resolved function_type
  | Pointer of 'resolved t
  (* todos (bailout) *)
  (* NoType is to avoid some Type.t option and use of let* everywhere.
   * See also of_opt() below.
   *)
  | NoType
  | Todo of todo_kind

and builtin_type =
  (* mimic SAST.literal *)
  | Int
  | Float
  | String
  | Bool
  | Number
  | OtherBuiltins of string

and 'resolved function_type = 'resolved parameter list * 'resolved t

and 'resolved parameter =
  | Param of 'resolved parameter_classic
  | OtherParam of todo_kind

and 'resolved parameter_classic = {
  (* the identifier can be useful to handle ArgKwd calls *)
  pident : string option;
  ptype : 'resolved t;
}
[@@deriving
  show { with_path = false },
    eq,
    sexp,
    visitors { variety = "map"; ancestors = [ "map_parent" ] }]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* TODO: should not be needed at some point *)
let todo_parameter = OtherParam (Some "todo_paramter")

let of_opt opt =
  match opt with
  | None -> NoType
  | Some t -> t

let rec to_name_opt lang ty =
  match ty with
  | N (n, _) -> Some n
  (* Currently, this function is only used to resolve the LHS of a DotAccess. In
   * go, DotAccess is overloaded to be both a field access (like `.` in C) and a
   * dereference plus a field access (like `->` in C). In most languages, either
   * all object types are implicitly pointers, or they require explicit
   * disambiguation which makes this unnecessary. To address this ambiguity,
   * look through a `Pointer` type when resolving the name. *)
  | Pointer ty when lang =*= Lang.Go -> to_name_opt lang ty
  | _else_ -> None

let mkt ?tok str =
  let tok =
    match tok with
    | Some tok -> tok
    | None -> Tok.unsafe_fake_tok str
  in
  G.TyN (G.Id ((str, tok), G.empty_id_info ())) |> G.t

let is_real_type = function
  | NoType
  | Todo _ ->
      false
  | _else_ -> true

(*****************************************************************************)
(* Converters *)
(*****************************************************************************)

let todo_kind_to_ast_generic_todo_kind (x : todo_kind) : G.todo_kind =
  match x with
  | Some s -> (s, Tok.unsafe_fake_tok s)
  | None -> ("TodoKind is None", Tok.unsafe_fake_tok "")

(* less: should sanity check things by looking at [lang]. but maybe users like
 * to write `bool` in a language that uses `boolean`, and we should allow that?
 *
 * NB: Conflates Java boxed types with primitives. This is probably fine for our
 * analysis.
 *
 * coupling: Inverse of ast_generic_type_of_builtin_type *)
let builtin_type_of_string _langTODO str =
  match str with
  | "Integer"
  | "int"
  | "long"
  | "Int"
  | "Long" ->
      Some Int
  | "float"
  | "double"
  | "Float"
  | "Double" ->
      Some Float
  | "str"
  | "string"
  | "String" ->
      Some String
  | "bool"
  | "boolean"
  | "Boolean" ->
      Some Bool
  (* TS *)
  | "number" -> Some Number
  | __else__ -> None

(* TODO: Check lang to get proper builtin type for more languages *)
let name_of_builtin_type lang t =
  match (lang, t) with
  | _, Int -> "int"
  | _, Float -> "float"
  | Lang.Java, String -> "String"
  | _, String -> "string"
  | Lang.Java, Bool -> "boolean"
  | _, Bool -> "bool"
  (* TS *)
  | _, Number -> "number"
  | _, OtherBuiltins str -> str

(* coupling: Inverse of builtin_type_of_string *)
let ast_generic_type_of_builtin_type ?tok lang t =
  let str = name_of_builtin_type lang t in
  mkt ?tok str

let builtin_type_of_type lang t =
  match t.G.t with
  (* for Python literal checking *)
  | G.TyExpr { e = G.N (G.Id ((str, _t), _idinfo)); _ } ->
      builtin_type_of_string lang str
  (* for Java/Go/... literals *)
  | G.TyN (Id ((str, _t), _idinfo)) -> builtin_type_of_string lang str
  | __else__ -> None

(* There is no function of_ast_generic_type_. The closest is
 * Naming_AST.type_of_expr  which converts an G.type_ to Type.t.
 *
 * If provided, `tok` is used in place of a fake tokens in most contexts where a
 * token is needed. This allows the resulting synthetic AST to be used in places
 * that require location information.
 * *)
let rec to_ast_generic_type_ ?tok lang
    (f : 'a -> G.alternate_name list -> G.name) (x : 'a t) : G.type_ option =
  let make_tok str =
    match tok with
    | Some tok -> tok
    | None -> Tok.unsafe_fake_tok str
  in
  match x with
  | N ((name, args), alts) -> (
      let n = f name alts in
      let t = G.TyN n |> G.t in
      match args with
      | [] -> Some t
      | xs ->
          let* targs = type_arguments lang f make_tok xs in
          Some (G.TyApply (t, Tok.unsafe_fake_bracket targs) |> G.t))
  | UnresolvedName (s, args) -> (
      let t = mkt ~tok:(make_tok s) s in
      match args with
      | [] -> Some t
      | xs ->
          let* targs = type_arguments lang f make_tok xs in
          Some (G.TyApply (t, Tok.unsafe_fake_bracket targs) |> G.t))
  | Null -> Some (mkt ~tok:(make_tok "null") "null")
  | Builtin x ->
      Some (ast_generic_type_of_builtin_type ~tok:(make_tok "") lang x)
  | Array (size, ty) ->
      let size = Option.map (fun pi -> G.L (G.Int pi) |> G.e) size in
      let* ty = to_ast_generic_type_ lang f ty in
      Some (G.TyArray (Tok.unsafe_fake_bracket size, ty) |> G.t)
  | Function (params, tret) ->
      let params =
        params
        |> List_.map (function
             | Param { pident; ptype } -> (
                 let topt = to_ast_generic_type_ lang f ptype in
                 match topt with
                 | Some t ->
                     let classic =
                       {
                         G.pname =
                           pident |> Option.map (fun s -> (s, make_tok s));
                         ptype = Some t;
                         pattrs = [];
                         pinfo = G.empty_id_info ();
                         pdefault = None;
                       }
                     in
                     G.Param classic
                 | _else_ ->
                     G.OtherParam
                       (("to_ast_generic_type for param", make_tok ""), []))
             | OtherParam x ->
                 G.OtherParam (todo_kind_to_ast_generic_todo_kind x, []))
      in
      let* tret = to_ast_generic_type_ lang f tret in
      Some (G.TyFun (params, tret) |> G.t)
  | Pointer ty ->
      let* ty = to_ast_generic_type_ lang f ty in
      Some (G.TyPointer (make_tok "Pointer", ty) |> G.t)
  | NoType
  | Todo _ ->
      None

and type_arguments lang f make_tok (xs : 'a type_argument list) :
    G.type_argument list option =
  match xs with
  | [] -> Some []
  | x :: xs ->
      let* x =
        match x with
        | TA t ->
            let* t = to_ast_generic_type_ lang f t in
            Some (G.TA t)
        | TAWildcard None -> Some (G.TAWildcard (make_tok "?", None))
        | TAWildcard (Some targ_constraint) ->
            let kind, t =
              match targ_constraint with
              | TAUpper t -> ((false, make_tok "extends"), t)
              | TALower t -> ((true, make_tok "super"), t)
            in
            let* t = to_ast_generic_type_ lang f t in
            Some (G.TAWildcard (make_tok "?", Some (kind, t)))
        | OtherTypeArg x ->
            Some (G.OtherTypeArg (todo_kind_to_ast_generic_todo_kind x, []))
      in
      let* xs = type_arguments lang f make_tok xs in
      Some (x :: xs)
