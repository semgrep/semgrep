(* Yoann Padioleau
 *
 * Copyright (C) 2022, 2023 r2c
 *
 *)
open Common
module G = AST_generic

let logger = Logging.get_logger [ __MODULE__ ]

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
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type todo_kind = string option [@@deriving show, eq]

(* Fully qualified name *)
type 'resolved name = 'resolved * 'resolved type_argument list
and 'resolved type_argument = TA of 'resolved t | OtherTypeArg of todo_kind

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
  | Array of int option * 'resolved t
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
[@@deriving show { with_path = false }, eq]

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

let mkt str =
  G.TyN (G.Id ((str, Tok.unsafe_fake_tok str), G.empty_id_info ())) |> G.t

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
 * coupling: Inverse of ast_generic_type_of_builtin_type *)
let builtin_type_of_string _langTODO str =
  match str with
  | "int" -> Some Int
  | "float" -> Some Float
  | "str"
  | "string"
  | "String" ->
      Some String
  | "bool"
  | "boolean" ->
      Some Bool
  (* TS *)
  | "number" -> Some Number
  | __else__ -> None

(* coupling: Inverse of builtin_type_of_string
 *
 * TODO: Check lang to get proper builtin type for more languages *)
let ast_generic_type_of_builtin_type lang t =
  match (lang, t) with
  | _, Int -> mkt "int"
  | _, Float -> mkt "float"
  (* TODO check lang for string type *)
  | _, String -> mkt "string"
  | Lang.Java, Bool -> mkt "boolean"
  | _, Bool -> mkt "bool"
  (* TS *)
  | _, Number -> mkt "number"
  | _, OtherBuiltins str -> mkt str

let builtin_type_of_type lang t =
  match t.G.t with
  (* for Python literal checking *)
  | G.TyExpr { e = G.N (G.Id ((str, _t), _idinfo)); _ } ->
      builtin_type_of_string lang str
  (* for Java/Go/... literals *)
  | G.TyN (Id ((str, _t), _idinfo)) -> builtin_type_of_string lang str
  | __else__ -> None

(* There is no function of_ast_generic_type_. The closest is
 * Naming_AST.type_of_expr  which converts an G.type_ to Type.t *)
let rec to_ast_generic_type_ lang (f : 'a -> G.alternate_name list -> G.name)
    (x : 'a t) : G.type_ option =
  match x with
  | N ((name, args), alts) -> (
      let n = f name alts in
      let t = G.TyN n |> G.t in
      match args with
      | [] -> Some t
      | xs ->
          let* targs = type_arguments lang f xs in
          Some (G.TyApply (t, Tok.unsafe_fake_bracket targs) |> G.t))
  | UnresolvedName (s, args) -> (
      let t = mkt s in
      match args with
      | [] -> Some t
      | xs ->
          let* targs = type_arguments lang f xs in
          Some (G.TyApply (t, Tok.unsafe_fake_bracket targs) |> G.t))
  | Null -> Some (mkt "null")
  | Builtin x -> Some (ast_generic_type_of_builtin_type lang x)
  | Array (size, ty) ->
      let size =
        Option.map
          (fun n ->
            G.L (G.Int (Some n, Tok.unsafe_fake_tok (string_of_int n))) |> G.e)
          size
      in
      let* ty = to_ast_generic_type_ lang f ty in
      Some (G.TyArray (Tok.unsafe_fake_bracket size, ty) |> G.t)
  | Function (params, tret) ->
      let params =
        params
        |> Common.map (function
             | Param { pident; ptype } -> (
                 let topt = to_ast_generic_type_ lang f ptype in
                 match topt with
                 | Some t ->
                     let classic =
                       {
                         G.pname =
                           pident
                           |> Option.map (fun s -> (s, Tok.unsafe_fake_tok s));
                         ptype = Some t;
                         pattrs = [];
                         pinfo = G.empty_id_info ();
                         pdefault = None;
                       }
                     in
                     G.Param classic
                 | _else_ ->
                     G.OtherParam
                       ( ( "to_ast_generic_type for param",
                           Tok.unsafe_fake_tok "" ),
                         [] ))
             | OtherParam x ->
                 G.OtherParam (todo_kind_to_ast_generic_todo_kind x, []))
      in
      let* tret = to_ast_generic_type_ lang f tret in
      Some (G.TyFun (params, tret) |> G.t)
  | Pointer ty ->
      let* ty = to_ast_generic_type_ lang f ty in
      Some (G.TyPointer (Tok.unsafe_fake_tok "Pointer", ty) |> G.t)
  | NoType
  | Todo _ ->
      None

and type_arguments lang f (xs : 'a type_argument list) :
    G.type_argument list option =
  match xs with
  | [] -> Some []
  | x :: xs ->
      let* x =
        match x with
        | TA t ->
            let* t = to_ast_generic_type_ lang f t in
            Some (G.TA t)
        | OtherTypeArg x ->
            Some (G.OtherTypeArg (todo_kind_to_ast_generic_todo_kind x, []))
      in
      let* xs = type_arguments lang f xs in
      Some (x :: xs)
