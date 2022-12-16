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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An Abstract Syntax Tree (AST) for OCaml.
 * (for a Concrete Syntax Tree see old/cst_ml_ml or ocaml-tree-sitter-semgrep).
 *
 * TODO:
 *  - lots of missing xxx bracket, lots of missing tok for correct l/r range
 *  - attributes at more places (@xxx)
 *  - extensions at many places (%xxx)
 *  - classes and objects
 *  - functors, module types, first-class modules
 *  - GADTs
 *  - see the XxxTodo
*)

(*****************************************************************************)
(* Names *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)
type tok = Parse_info.t
[@@deriving show] (* with tarzan *)

(* a shortcut to annotate some information with token/position information *)
type 'a wrap = 'a * tok
[@@deriving show] (* with tarzan *)

(* round(), square[], curly{}, angle<> brackets *)
type 'a bracket = tok * 'a * tok
[@@deriving show] (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Names  *)
(* ------------------------------------------------------------------------- *)

type ident = string wrap
[@@deriving show] (* with tarzan *)

(* a.k.a longident in the OCaml source.
 * TODO: Lident of string | LDot of t * string | Lapply of t * t
*)
type name = qualifier * ident
and qualifier = ident list
[@@deriving show] (* with tarzan *)

type todo_category = string wrap
[@@deriving show] (* with tarzan *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type type_ =
  | TyName of name (* include builtins *)
  | TyVar of ident (* 'a *)
  | TyAny of tok (* _ *)

  | TyFunction of type_ * type_
  | TyApp of type_ list * name (* less: could be merged with TyName *)

  | TyTuple of type_ list (* at least 2 *)

  (* sgrep-ext: *)
  | TyEllipsis of tok

  (* TODO:
   * - Rows, PolyVariants, inline record
   * - Object, Class
   * - Forall types ('a .)
   * - Package (first-class module?)
  *)
  | TyTodo of todo_category * type_ list

[@@deriving show { with_path = false} ] (* with tarzan *)

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)

(* start of big recursive type *)
type expr =
  | L of literal
  | Name of name

  (* note that Foo(1,2) is represented as Constructor(Foo, Some Tuple ...).
   * alt: Constructor of name * expr list bracket option
  *)
  | Constructor of name * expr option
  | PolyVariant of (tok (* '`' *) * ident) * expr option
  (* special case of Constr *)
  | Tuple of expr list
  | List  of expr list bracket

  (* The list can be empty. The brackets can also be fake. For example, in
   * 'match x with Foo -> 1; 2' you do not need the brackets around
   * '1; 2'.
   * Note that if the list has just one element, the ParenExpr construct
   * below will be used instead.
  *)
  | Sequence of expr list bracket (* begin/end, (), or do/done *)

  (* In most ASTs, and in most parsers, we get rid of those Paren
   * constructs because they are more useful in CSTs than ASTs. However,
   * for semgrep, and especially for autofix, parenthesis matter.
   * We used to have 'Tuple of expr list bracket', but this was not enough
   * because in code like Foo (1+2), there are no tuple, but
   * we still want to remember those parenthesis in the AST.
   * Thus, we keed those parenthesis in the AST and let ml_to_generic.ml
   * do the right thing depending on the context in which those
   * ParenExpr are used (for Tuple, for Constructor, or for regular grouping).
  *)
  | ParenExpr of expr bracket

  (* todo: Use AST_generic_.op? *)
  | Prefix of string wrap * expr
  | Infix of expr * string wrap * expr

  | Call of expr * argument list

  (* could be factorized with Prefix but it's not a usual prefix operator! *)
  | RefAccess of tok (* ! *) * expr
  | RefAssign of expr * tok (* := *) * expr

  (* less: lhs type to factorize xx and xx <- yy, for Array/String/BigArray *)
  (* special case of RefAccess and RefAssign *)
  | FieldAccess of expr * tok * name
  | FieldAssign of expr * tok * name * tok (* <- *) * expr

  (* we unsugar { x } in { x = x } *)
  | Record of expr option (* with *) * (name * expr) list bracket

  | New of tok * name
  | ObjAccess of expr * tok (* # *) * ident

  (* > 1 elt for mutually recursive let (let x and y and z) *)
  | LetIn of tok * rec_opt * let_binding list * expr
  | Fun of tok (* 'fun' *) * parameter list (* at least one *) * expr
  | Function of tok (* 'function' *) * match_case list

  | If of tok * expr * expr * expr option
  | Match of tok * expr * match_case list

  | Try of tok * expr * match_case list

  | While of tok * expr * expr
  | For of tok * ident * expr * for_direction * expr *   expr

  (* regular construct but also semgrep-ext: for Typed metavariables *)
  | TypedExpr of expr * tok (* : *) * type_

  (* sgrep-ext: *)
  | Ellipsis of tok
  | DeepEllipsis of expr bracket

  (* TODO:
   * - LocalOpen, LocalModule
   * - Array, BigArray (literals, access, set)
   * - Lazy, Assert,
   * - Object, ObjCopy
   * - Package (first-class module?)
   * - Coerce
   * - OCamlYacc value ($1)
  *)
  | ExprTodo of todo_category * expr list

(* Unit '()' used to be represented as a Constructor ("()"), and
 * 'true' (and 'false') as Constructor ("true"), but not anymore
*)
and literal =
  | Int    of int option wrap
  | Float  of float option wrap
  | Char   of string wrap
  | String of string wrap
  | Bool of bool wrap
  | Unit of tok * tok (* () or begin/end *)

and argument =
  | Arg of expr
  (* we unsugar ~x in ~x:x (same for ?x) *)
  | ArgKwd of ident * expr
  | ArgQuestion of ident  * expr

and match_case =
  pattern * match_action

and match_action = expr option (* when *) * tok (* -> *) * expr

and for_direction =
  | To of tok
  | Downto of tok

(* TODO: attribute? and keyword_attribute? *)
and rec_opt = tok option

(*****************************************************************************)
(* Patterns *)
(*****************************************************************************)
and pattern =
  | PatVar of ident
  | PatLiteral of literal (* can be signed *)

  (* alt: PatConstructor of name * pattern list bracket option *)
  | PatConstructor of name * pattern option
  | PatPolyVariant of (tok (* '`' *) * ident) * pattern option

  (* special cases of PatConstructor *)
  | PatConsInfix of pattern * tok (* :: *) * pattern
  (* some brackets are fake_info *)
  | PatTuple of pattern list bracket
  | PatList of pattern list bracket

  | PatUnderscore of tok
  (* TODO: can also have just ; _ and can have typed name *)
  (* we unsugar { x } in { x = x } *)
  | PatRecord of (name * pattern) list bracket

  | PatAs of pattern * ident
  (* OCaml disjunction patterns extension *)
  | PatDisj of pattern * pattern
  | PatTyped of pattern * tok * type_

  (* sgrep-ext: *)
  | PatEllipsis of tok

  (* TODO:
   * - LocalOpen,
   * - Array, BigArray,
   * - Lazy
  *)
  | PatTodo of todo_category * pattern list

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Let binding (global/local/function definition) *)
(* ------------------------------------------------------------------------- *)

(* Similar to the Fun vs Function dichotomy *)
and let_binding =
  | LetClassic of let_def
  | LetPattern of pattern * expr

(* was called fun_binding in the grammar *)
and let_def = {
  lname: ident;
  lparams: parameter list; (* can be empty *)
  lrettype: type_ option;
  lbody: expr;
}

and parameter =
  | Param of pattern
  (* ParamEllipsis can be done via ParamPat (PatEllipsis) *)
  (* TODO: Label parameter ~xxx ?xxx *)
  | ParamTodo of todo_category

[@@deriving show { with_path = false} ]  (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Type declaration *)
(* ------------------------------------------------------------------------- *)

(* TODO: keyword attribute: private, constraints *)
type type_declaration =
  | TyDecl of type_declaration_classic
  (* TODO: Extension (+=) decl, .. *)
  | TyDeclTodo of todo_category

and type_declaration_classic = {
  tname: ident;
  tparams: type_parameter list;
  tbody: type_def_kind;
}

(* TODO: can be far more complex now, with constraints on the type parameter *)
and type_parameter =
  | TyParam of ident (* a TyVar, e.g., 'a *)
  | TyParamTodo of todo_category

and type_def_kind =
  | AbstractType
  | CoreType of type_
  (* or type *)
  | AlgebraicType of constructor_decl list
  (* and type *)
  | RecordType   of field_decl list bracket
  | TdTodo of todo_category

and constructor_decl = ident * constructor_decl_kind

(* TODO: GADT : ..., Alias, InlineRecord def, etc. *)
and constructor_decl_kind =
  (* of ... *)
  type_ list

and field_decl = ident * type_ * mutable_opt

(* TODO: keyword attribute *)
and mutable_opt = tok option (* mutable *)
[@@deriving show { with_path = false} ] (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Class (and object) *)
(* ------------------------------------------------------------------------- *)
(* Nope ... represented via an ItemTodo "Class" *)

(* ------------------------------------------------------------------------- *)
(* Module *)
(* ------------------------------------------------------------------------- *)
type module_declaration = {
  mname: ident;
  (* TODO: mparams: for functors *)
  mbody: module_expr;
}

(* mutually recursive with item *)
and module_expr =
  | ModuleName of name (* alias *)
  | ModuleStruct of item list

  (* TODO: functor (def and app), abstract module *)
  | ModuleTodo of todo_category * module_expr list

(*****************************************************************************)
(* Attributes *)
(*****************************************************************************)

and attribute =
  | NamedAttr of (dotted_ident * item list) bracket
  (* TODO: also put keyword_attribute *)
and attributes = attribute list
and dotted_ident = ident list

(*****************************************************************************)
(* Toplevel *)
(*****************************************************************************)

(* Signature/Structure items *)
and item = {
  i: item_kind;
  iattrs: attributes;
}

(* could split in sig_item and struct_item but many constructions are
 * valid in both contexts.
*)
and item_kind =
  | Type of tok * type_declaration list (* mutually recursive *)

  | Exception of tok * ident * type_ list
  | External  of tok * ident * type_ * string wrap list (* primitive decls *)

  (* TODO: '!' option *)
  | Open of tok * name

  (* only in sig_item *)
  | Val of tok * ident * type_

  (* only in struct_item *)
  | Let of tok * rec_opt * let_binding list
  | TopExpr of expr

  | Module of tok * module_declaration

  (* TODO
   * - directives (#xxx)
   * - floating attributes ([@@@ ])
   * - include
   * - module type
  *)
  | ItemTodo of todo_category * item list

[@@deriving show { with_path = false} ] (* with tarzan *)

type program = item list
[@@deriving show ] (* with tarzan *)

(*****************************************************************************)
(* Any *)
(*****************************************************************************)
type partial =
  (* partial stmts *)
  | PartialIf of tok * expr
  | PartialMatch of tok * expr
  | PartialTry of tok * expr
  (* other *)
  | PartialLetIn of tok * rec_opt * let_binding list * tok (* in *)

[@@deriving show { with_path = false} ] (* with tarzan *)

type any =
  | T of type_
  | E of expr
  | P of pattern
  | MC of match_case

  | I of item
  | Id of ident
  | Pr of program

  | Partial of partial

[@@deriving show { with_path = false} ] (* with tarzan *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)

(* used when converting a module_path to a module name *)
let qualifier_to_name xs =
  match List.rev xs with
  | [] -> raise Common.Impossible
  | x::xs -> List.rev xs, x

(* TODO: rename id_to_expr *)
let name_of_id id = Name ([], id)

let mki x =
  { i = x; iattrs = [] }
