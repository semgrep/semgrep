(* Brandon Wu
 *
 * Copyright (C) 2024 r2c
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
(* Abstract Syntax Tree for QL.
 *)

(*****************************************************************************)
(* Names *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)

type tok = Tok.t [@@deriving show]

(* a shortcut to annotate some information with token/position information *)
type 'a wrap = 'a * tok [@@deriving show] (* with tarzan *)
type 'a bracket = tok * 'a * tok [@@deriving show] (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Ident *)
(* ------------------------------------------------------------------------- *)

type ident = string wrap [@@deriving show]

(* IdQualified is used when referring to modules *)
type name = Id of ident | IdQualified of qualified_info

and qualified_info = {
  name_last : ident * type_arguments option;
  name_middle : (ident * type_arguments option) list;
}

and dotted_ident = ident list

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)
and expr =
  | L of literal
  | N of name
  | IdSpecial of special wrap
  | Call of expr * type_arguments * argument list bracket
  | Aggregation of aggregation
  | Cast of type_ * expr
  | IfExp of expr (* test *) * expr (* body *) * expr (* orelse *)
  | BinOp of expr (* left *) * operator wrap (* op *) * expr (* right *)
  | UnaryOp of unaryop wrap (* op *) * expr (* operand *)
  | Quantified of quantifier wrap * formula_body bracket
  | Range of expr * tok (* .. *) * expr
  | Set of expr list bracket
  | ParenExpr of expr bracket
  | AnnotatedExpr of ident * ident * expr
  | DotAccess of expr * tok * dot_rhs
  (* sgrep: *)
  | Ellipsis of tok
  | MetavarEllipsis of ident
  | TypedMetavar of type_ * ident
  | DeepEllipsis of tok * expr * tok

and aggregation = {
  expr : expr;
  rank_exprs : expr list;
  body : aggregation_body bracket;
}

and aggregation_body = {
  vardecls : vardecl list;
  formula : expr option;
  as_exprs : (expr * ident option) list;
  agg_orderbys : (expr * direction wrap option) list;
}

and dot_rhs =
  | RhsPredicate of ident * argument list
  | RhsCast of type_
  | RhsSuper of tok

and formula_body =
  | Bare of expr
  (* This is silly but these two exprs are because you may have
     | e, but only up to twice
     e.g. forall(int x)
          forall(int x | e)
          forall(int x | e1 | e2)
  *)
  | Declared of vardecl list * (expr * expr option) option

and literal =
  | Int of Parsed_int.t
  | Float of float option wrap
  | String of string wrap
  | Bool of bool wrap

and special =
  | This
  | Result
  | NoneId
    (* https://codeql.github.com/docs/ql-language-reference/formulas/#none *)
  | Not

and unaryop = UAdd | USub
and quantifier = Forall | Exists | Forex

and operator =
  | In
  | InstanceOf
  (* logical *)
  | And
  | Or
  | Implies
  (* arithmetic *)
  | Add
  | Sub
  | Mult
  | Div
  | Mod
  (* comparison *)
  | Eq
  | NotEq
  | Lt
  | Gt
  | LtEq
  | GtEq

(* ------------------------------------------------------------------------- *)
(* Arguments *)
(* ------------------------------------------------------------------------- *)
and argument =
  | Arg of expr (* this can be Ellipsis for sgrep *)
  | ArgUnderscore of tok

and type_argument = name * Parsed_int.t option
and type_arguments = type_argument list

(* ------------------------------------------------------------------------- *)
(* Parameters *)
(* ------------------------------------------------------------------------- *)
and type_parameters = (type_argument * ident) list

(*****************************************************************************)
(* Type *)
(*****************************************************************************)
and type_ = name

(*****************************************************************************)
(* Statements *)
(*****************************************************************************)
and stmt =
  | ClassDef of class_definition
  | ModuleDef of module_definition
  | PredicateDef of predicate_definition
  | VarDecl of vardecl
  | NewType of tok (* 'newtype' *) * ident * type_definition_element list
  | TypeUnion of tok (* 'class' *) * ident * type_ list
  | ImportAs of tok * name (* name *) * ident option (* asname *)
  | Select of select
  | ExprStmt of expr (* value *)

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)
and vardecl = VardeclInit of type_ * ident | VardeclEllipsis of tok

(* ------------------------------------------------------------------------- *)
(* Select *)
(* ------------------------------------------------------------------------- *)
and select = {
  from : (tok (* 'from' *) * vardecl list) option;
  where : expr option;
  select : (expr * ident option) list;
  sel_orderbys : (expr * direction wrap option) list;
}

and direction = Asc | Desc

(* ------------------------------------------------------------------------- *)
(* Class *)
(* ------------------------------------------------------------------------- *)
and class_definition = tok (* 'class' *) * ident (* name *) * class_rhs

and class_rhs =
  | ClassBody of
      type_ list (* extends *)
      * type_ list (* instances of *)
      * stmt list bracket
  | ClassAlias of type_

(* ------------------------------------------------------------------------- *)
(* Predicate *)
(* ------------------------------------------------------------------------- *)
and predicate_definition = type_ option * ident * vardecl list * predicate_rhs

and predicate_rhs =
  | PredicateExpr of expr option
  | PredicateAlias of qualified_info * Parsed_int.t

and type_definition_element = ident * vardecl list * expr option

(*****************************************************************************)
(* Module *)
(*****************************************************************************)
and module_definition =
  tok (* 'module' *) * ident * type_parameters * module_rhs

and module_rhs = ModuleBody of stmt list | ModuleAlias of qualified_info

(*****************************************************************************)
(* Toplevel *)
(*****************************************************************************)
and program = stmt list [@@deriving show]

(*****************************************************************************)
(* Any *)
(*****************************************************************************)

(* This is mostly for semgrep to represent a pattern *)
type any = Pr of program | E of expr [@@deriving show { with_path = false }]
