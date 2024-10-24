(* Yoann Padioleau
 *
 * Copyright (C) 2019-2024 Semgrep Inc.
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
 *
 * For a Concrete Syntax Tree see either
 *   - old/cst_ml_ml
 *   - ../tree-sitter/semgrep-ocaml/lib/CST.ml
 *
 * TODO:
 *  - lots of missing xxx bracket, lots of missing tok for correct l/r range
 *  - attributes at more places (@xxx)
 *  - extensions at many places (%xxx)
 *  - classes and objects (still work to do)
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

(* a shortcut to annotate some information with token/position information *)
type 'a wrap = 'a * Tok.t [@@deriving show]

(* round(), square[], curly{}, angle<> brackets *)
type 'a bracket = Tok.t * 'a * Tok.t [@@deriving show]

(* ------------------------------------------------------------------------- *)
(* Names  *)
(* ------------------------------------------------------------------------- *)

type ident = string wrap [@@deriving show]

(* a.k.a longident in the OCaml source.
 * TODO: Lident of string | LDot of t * string | Lapply of t * t
 *)
type name = qualifier * ident
and qualifier = ident list [@@deriving show]

type todo_category = string wrap [@@deriving show]

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type type_ =
  | TyName of name (* include builtins *)
  | TyVar of ident (* 'a *)
  | TyAny of Tok.t (* _ *)
  | TyFunction of type_ * type_
  | TyApp of type_ list bracket * name (* less: could be merged with TyName *)
  | TyTuple of type_ list (* at least 2 *)
  (* sgrep-ext: *)
  | TyEllipsis of Tok.t
  (* TODO:
   * - Rows, PolyVariants, inline record
   * - Object, Class
   * - Forall types ('a .)
   * - Package (first-class module?)
   *)
  | TyTodo of todo_category * type_ list
[@@deriving show { with_path = false }]

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
  | PolyVariant of (Tok.t (* '`' *) * ident) * expr option
  (* special case of Constr *)
  | Tuple of expr list
  | List of expr list bracket
  (* The list can be empty. The brackets can also be fake. For example, in
   * 'match x with Foo -> 1; 2' you do not need the brackets around
   * '1; 2'.
   * Note that if the list has just one element, the ParenExpr construct
   * below will be used instead.
   *)
  | Sequence of expr list bracket (* begin/end, (), or do/done *)
  (* we unsugar { x } in { x = x } *)
  | Record of expr option (* with *) * (name * expr) list bracket
  | Obj of object_
  | New of Tok.t * name
  (* todo: Use AST_generic_.op? *)
  | Prefix of string wrap * expr
  | Infix of expr * string wrap * expr
  | Call of expr * argument list
  (* could be factorized with Prefix but it's not a usual prefix operator! *)
  | RefAccess of Tok.t (* ! *) * expr
  | RefAssign of expr * Tok.t (* := *) * expr
  (* less: lhs type to factorize xx and xx <- yy, for Array/String/BigArray *)
  (* special case of RefAccess and RefAssign *)
  | FieldAccess of expr * Tok.t * name
  | FieldAssign of expr * Tok.t * name * Tok.t (* <- *) * expr
  | ObjAccess of expr * Tok.t (* # *) * ident
  (* > 1 elt for mutually recursive let (let x and y and z) *)
  | LetIn of Tok.t * rec_opt * let_binding list * expr
  | Fun of Tok.t (* 'fun' *) * parameter list (* at least one *) * expr
  | Function of Tok.t (* 'function' *) * match_case list
  (* those are expressions in OCaml! composability! no need for statement *)
  | If of Tok.t * expr * expr * expr option
  | Match of Tok.t * expr * match_case list
  | Try of Tok.t * expr * match_case list
  | While of Tok.t * expr * expr
  | For of Tok.t * ident * expr * for_direction * expr * expr
  (* regular construct but also semgrep-ext: for Typed metavariables *)
  | TypedExpr of expr * Tok.t (* : *) * type_
  (* OCaml 4.?? *)
  | LetOpen of Tok.t (* 'let' *) * name * Tok.t (* 'in' *) * expr
  | LocalOpen of name * Tok.t (* '.' *) * expr
  (* In most ASTs, and in most parsers, we get rid of those Paren
   * constructs because they are more useful in CSTs than ASTs. However,
   * for semgrep, and especially for autofix, parenthesis matter.
   * We used to have 'Tuple of expr list bracket', but this was not enough
   * because in code like Foo (1+2), there are no tuple, but
   * we still want to remember those parenthesis in the AST.
   * Thus, we keed those parenthesis in the AST and let ocaml_to_generic.ml
   * do the right thing depending on the context in which those
   * ParenExpr are used (for Tuple, for Constructor, or for regular grouping).
   *)
  | ParenExpr of expr bracket
  (* sgrep-ext: *)
  | Ellipsis of Tok.t
  | DeepEllipsis of expr bracket
  (* TODO:
   * - LocalModule
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
  | Int of Parsed_int.t
  | Float of float option wrap
  | Char of string wrap
  | String of string wrap (* TODO bracket *)
  | Bool of bool wrap
  | Unit of Tok.t * Tok.t (* () or begin/end *)

and argument =
  | Arg of expr
  (* we unsugar ~x in ~x:x (same for ?x) *)
  | ArgKwd of ident * expr
  | ArgQuestion of ident * expr
  | ArgTodo of todo_category

and match_case = pattern * match_action
and match_action = expr option (* when *) * Tok.t (* -> *) * expr
and for_direction = To of Tok.t | Downto of Tok.t

(* TODO: attribute? and keyword_attribute? *)
and rec_opt = Tok.t option

(* ------------------------------------------------------------------------- *)
(* object *)
(* ------------------------------------------------------------------------- *)
and object_ = {
  o_tok : Tok.t; (* 'object' *)
  (* TODO: self = (pattern * type_ option) bracket option *)
  o_body : class_field list;
}

and class_field =
  | Method of method_
  | InstanceVar of instance_variable
  (* TODO:
   * - inherit
   *)
  | CfldTodo of todo_category

and method_ = {
  (* TODO: override !, attrs *)
  m_tok : Tok.t; (* 'method' *)
  m_name : ident;
  m_params : parameter list;
  m_rettype : type_ option;
  m_body : expr option;
}

and instance_variable = {
  (* TODO: override !, attrs *)
  inst_tok : Tok.t; (* 'val' *)
  inst_name : ident;
  inst_type : type_ option;
  (* TODO: :> *)
  inst_expr : expr option;
}

(*****************************************************************************)
(* Patterns *)
(*****************************************************************************)
and pattern =
  | PatVar of ident
  | PatLiteral of literal (* can be signed *)
  (* alt: PatConstructor of name * pattern list bracket option *)
  | PatConstructor of name * pattern option
  | PatPolyVariant of (Tok.t (* '`' *) * ident) * pattern option
  (* special cases of PatConstructor *)
  | PatConsInfix of pattern * Tok.t (* :: *) * pattern
  (* some brackets are fake_info *)
  | PatTuple of pattern list bracket
  | PatList of pattern list bracket
  | PatUnderscore of Tok.t
  (* TODO: can also have just ; _ and can have typed name *)
  (* we unsugar { x } in { x = x } *)
  | PatRecord of (name * pattern) list bracket
  | PatAs of pattern * ident
  (* OCaml disjunction patterns extension *)
  | PatDisj of pattern * pattern
  | PatTyped of pattern * Tok.t * type_
  (* sgrep-ext: *)
  | PatEllipsis of Tok.t
  (* TODO:
   * - PatLocalOpen,
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
and let_binding = LetClassic of let_def | LetPattern of pattern * expr

(* was called fun_binding in the grammar *)
and let_def = {
  lname : ident;
  lparams : parameter list; (* can be empty *)
  lrettype : type_ option;
  lbody : expr;
  (* each individual 'let' can have its own attributes,
   * which forces to make let_def mutually recursive with attributes, which
   * is itself mutually recursive with item.
   *)
  lattrs : attributes;
}

and parameter =
  | Param of pattern
  (* ParamEllipsis can be done via ParamPat (PatEllipsis) *)
  (* TODO: Label parameter ~xxx ?xxx *)
  | ParamTodo of todo_category
[@@deriving show { with_path = false }]

(* ------------------------------------------------------------------------- *)
(* Type declaration *)
(* ------------------------------------------------------------------------- *)

(* old: the type below used to be a clearly separate 'type ...' and not 'and',
 * but with attributes in let_def, everything is now mutually recursive.
 *)

(* TODO: keyword attribute: private, constraints *)
and type_declaration =
  | TyDecl of type_declaration_classic
  (* TODO: Extension (+=) decl, .. *)
  | TyDeclTodo of todo_category

and type_declaration_classic = {
  tname : ident;
  tparams : type_parameters option;
  tbody : type_def_kind;
}

(* TODO: can be far more complex now, with constraints on the type parameter *)
and type_parameter =
  | TyParam of ident (* a TyVar, e.g., 'a *)
  | TyParamTodo of todo_category

(* alt: we could be more precise with
 * and type_parameters =
 * | NoTyParam
 * | OneTyParam of type_parameter
 * | TyParams of type_parameter list bracket
 * but anyway in the generic AST they are represented like below.
 *
 * The bracket can be '('')' for type defs, and '[', ']' for class defs.
 *)
and type_parameters = type_parameter list bracket

and type_def_kind =
  | AbstractType
  | CoreType of type_
  (* or type *)
  | AlgebraicType of constructor_decl list
  (* and type *)
  | RecordType of field_decl list bracket
  | TdTodo of todo_category

and constructor_decl = ident * constructor_decl_kind

(* TODO: GADT : ..., Alias, InlineRecord def, etc. *)
and constructor_decl_kind = (* of ... *)
  type_ list

and field_decl = ident * type_ * mutable_opt

(* TODO: keyword attribute *)
and mutable_opt = Tok.t option (* mutable *)
[@@deriving show { with_path = false }]

(* ------------------------------------------------------------------------- *)
(* Class (and object) *)
(* ------------------------------------------------------------------------- *)

(* This also used to be separate 'type ...' but is now mutually recursive
 * because of attributes in let_def.
 *)
and class_binding = {
  (* c_attrs: 'virtual' *)
  c_name : ident;
  c_tparams : type_parameters option;
  c_params : parameter list;
  (* TODO: c_rettype *)
  c_body : class_expr option; (* TODO: attributes *)
}

and class_expr =
  (* simple_class_expr *)
  | ClObj of object_
  (* TODO:
   * - ClassPathExp
   * - InstClassExp
   * - TypedClassExp
   * - ParenClassExp
   * - ClassFunc
   * - ClassApp
   * - LetClassApp
   * - LetOpenClass
   *)
  | ClTodo of todo_category * class_expr list
[@@deriving show { with_path = false }]

(* ------------------------------------------------------------------------- *)
(* Module *)
(* ------------------------------------------------------------------------- *)
and module_declaration = {
  mname : ident;
  (* TODO: mparams: for functors *)
  mbody : module_expr;
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
and attribute = NamedAttr of (dotted_ident * item list) bracket
(* TODO: also put keyword_attribute *)

and attributes = attribute list
and dotted_ident = ident list

(*****************************************************************************)
(* Toplevel *)
(*****************************************************************************)

(* Signature/Structure items.
 * Note that for Let, the attributes are actually in the nested
 * let_binding, so iattrs will be empty.
 *)
and item = { i : item_kind; iattrs : attributes }

(* could split in sig_item and struct_item but many constructions are
 * valid in both contexts.
 *)
and item_kind =
  | Type of Tok.t * type_declaration list (* mutually recursive *)
  | Exception of Tok.t * ident * type_ list
  | External of Tok.t * ident * type_ * string wrap list (* primitive decls *)
  (* TODO: '!' option, and the general open module_expr *)
  | Open of Tok.t * name
  (* only in sig_item *)
  | Val of Tok.t * ident * type_
  (* only in struct_item *)
  | Let of Tok.t * rec_opt * let_binding list
  | TopExpr of expr
  | Module of Tok.t * module_declaration
  | Class of Tok.t (* 'class' *) * class_binding list
  (* TODO
   * - directives (#xxx)
   * - floating attributes ([@@@ ])
   * - include
   * - module type
   *)
  | ItemTodo of todo_category * item list
[@@deriving show { with_path = false }]

type program = item list [@@deriving show]

(*****************************************************************************)
(* Any *)
(*****************************************************************************)
type partial =
  (* partial stmts *)
  | PartialIf of Tok.t * expr
  | PartialMatch of Tok.t * expr
  | PartialTry of Tok.t * expr
  (* other *)
  | PartialLetIn of Tok.t * rec_opt * let_binding list * Tok.t (* in *)
[@@deriving show { with_path = false }]

type any =
  | T of type_
  | E of expr
  | P of pattern
  | MC of match_case
  | I of item
  | Id of ident
  | Pr of program
  | Partial of partial
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)

(* used when converting a module_path to a module name *)
let qualifier_to_name xs =
  match List.rev xs with
  | [] -> raise Common.Impossible
  | x :: xs -> (List.rev xs, x)

(* TODO: rename id_to_expr *)
let name_of_id id = Name ([], id)
let mki x = { i = x; iattrs = [] }
