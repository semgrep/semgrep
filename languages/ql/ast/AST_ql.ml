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

(* round(), square[], curly{}, angle<> brackets *)
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
  | Quantified of quantifier wrap * formula_body
  | Range of expr * tok (* .. *) * expr
  | Set of expr list bracket
  | ParenExpr of expr bracket
  | AnnotatedExpr of ident * ident * expr
  | DotAccess of expr * tok * dot_rhs
  (* sgrep: *)
  | Ellipsis of tok
  | MetavarEllipsis of ident
  | TypedMetavar of type_ * ident

and aggregation = {
  expr : expr;
  rank_exprs : expr list;
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
(* tree-sitter-python: (and Python2) allows any expression for the key, but
 * the official Python 2 grammar says "ast.c makes sure it's a NAME" *)
(* | ArgKwd of name (* arg *) * expr (* value *)
  (* TODO? just use ExprStar, and move PowInline in expr too? and just
 * say in which context those constructs can actually appear
 * (e.g., only in arg, or dict/set)
 *)
  | ArgStar of (* '*' *) tok * expr
  | ArgPow of (* '**' *) tok * expr
  (* TODO: merge with Tuple CompForIf, and actually there can be only 1
 * ArgComp in arguments *)
  | ArgComp of expr * for_if list *)

and type_argument = name * Parsed_int.t option
and type_arguments = type_argument list

(* ------------------------------------------------------------------------- *)
(* Parameters *)
(* ------------------------------------------------------------------------- *)
(* TODO: add bracket *)
and parameters = parameter list

and parameter =
  (* param_pattern is usually just a name.
   * TODO? merge with ParamDefault
   *)
  | Param of unit
(* | ParamDefault of (name * type_ option) * expr (* default value *)
  (* TODO: tree-sitter-python allows also a Subscript or Attribute instead
 * of just name, what is that?? *)
  | ParamStar of tok (* '*' *) * (name * type_ option)
  | ParamPow of tok (* '**' *) * (name * type_ option)
  (* python3: single star delimiter to force keyword-only arguments after.
 * reference: https://www.python.org/dev/peps/pep-3102/ *)
  | ParamSingleStar of tok
  (* python3: single slash delimiter to force positional-only arg prior. *)
  | ParamSlash of tok
  (* sgrep-ext: *)
  | ParamEllipsis of tok
 *)

and param_pattern = PatternName of ident
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
  | TypeUnion of ident * type_ list
  | ImportAs of tok * name (* name *) * ident option (* asname *)
  | Select of select
  | ExprStmt of expr (* value *)

and select = {
  from : (tok (* 'from' *) * vardecl list) option;
  where : expr option;
  select : (expr * ident option) list;
  sel_orderbys : (expr * direction wrap option) list;
}

and direction = Asc | Desc
and class_definition = tok (* 'class' *) * ident (* name *) * class_rhs

and class_rhs =
  | ClassBody of
      type_ list (* extends *)
      * type_ list (* instances of *)
      * stmt list bracket
  | ClassAlias of type_

and module_definition =
  tok (* 'module' *) * ident * type_parameters * module_rhs

and module_rhs = ModuleBody of stmt list | ModuleAlias of qualified_info
and predicate_definition = type_ option * ident * vardecl list * predicate_rhs

and predicate_rhs =
  | PredicateExpr of expr option
  | PredicateAlias of qualified_info * Parsed_int.t

and vardecl = type_ * ident
and type_definition_element = ident * vardecl list * expr option

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)

(*****************************************************************************)
(* Module *)
(*****************************************************************************)

(*****************************************************************************)
(* Toplevel *)
(*****************************************************************************)
and program = stmt list [@@deriving show]

(*****************************************************************************)
(* Any *)
(*****************************************************************************)
(* This is mostly for semgrep to represent a pattern *)
type any = Pr of program | E of expr [@@deriving show { with_path = false }]
