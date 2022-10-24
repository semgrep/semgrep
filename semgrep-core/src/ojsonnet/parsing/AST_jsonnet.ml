(* Yoann Padioleau
 *
 * Copyright (C) 2022 r2c
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A Concrete/Abstract Syntax Tree for Jsonnet.
 *
 * This CST is mostly derived from the tree-sitter-jsonnet grammar:
 * https://github.com/sourcegraph/tree-sitter-jsonnet
 * I tried to keep the original terms (e.g., LocalBind) instead of the
 * terms I use in AST_generic (e.g., Let).
 *
 * The main uses for this file are:
 *  - for Semgrep to allow people to use jsonnet patterns to match
 *    over jsonnet code
 *  - TODO: potentially for implementing a jsonnet interpreter in OCaml,
 *    so we can use it in osemgrep instead of having to bind to
 *    the jsonnet C library. This could allow in turn to provide
 *    better error messages when there is an error in a jsonnet
 *    semgrep rule. Indeed right now the error will be mostly
 *    reported on the resulting JSON.
 *)

(*****************************************************************************)
(* Token (leaf) *)
(*****************************************************************************)

type tok = Parse_info.t [@@deriving show]
type 'a wrap = 'a * tok [@@deriving show]
type 'a bracket = tok * 'a * tok [@@deriving show]

(*****************************************************************************)
(* Names *)
(*****************************************************************************)
type ident = string wrap [@@deriving show]

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)

(* Using a record from the start. This is not needed yet, but if
 * we implement a jsonnet interpreter, this might become useful.
 *)
type expr = { e : expr_kind }

and expr_kind =
  (* values *)
  | L of literal
  (* TODO: String of string_kind * string_content bracket *)
  | Object of field list bracket
  | Array of expr list bracket
  (* entities *)
  | Id of string wrap
  | IdSpecial of special wrap
  (* =~ Let *)
  | LocalBind of local_binding * tok (* ; *) * expr
  (* accesses *)
  (* TODO: ArrayComprenhension *)
  | DotAccess of expr * tok (* '.' *) * ident
  | ArrayAccess of expr * expr bracket
  (* TODO: | SliceAccess of expr *  *)
  (* TODO: SuperDot and SuperArrayAccess *)
  (* control flow *)
  | Call of expr * argument list bracket
  | UnaryOp of unary_op wrap * expr
  | BinaryOp of expr * binary_op wrap * expr
  | If of tok * expr * expr * (tok * expr) option
  (* TODO: expr { objinside } ?? *)
  | Lambda of function_definition
  (* TODO | Assert of  *)
  (* directives *)
  | Import of import
  (* TODO: ImportStr *)
  (* TODO: expr in super ?? *)
  | ParenExpr of expr bracket
  | TodoExpr of string wrap * expr list

and literal =
  | Null of tok
  | Bool of bool wrap
  (* for integers and floats; no difference in jsonnet *)
  | Number of string wrap

and special = Self of tok | Super of tok | Dollar of tok (* ??? *)
and argument = Arg of expr | NamedArg of ident * tok (* = *) * expr

(* alt: could reuse AST_generic_.ml but because we might want to
 * make a jsonnet interpreter, better to be as precise as possible
 *)
and unary_op = UPlus | UMinus | UBang | Utilde

and binary_op =
  | Plus
  | Minus
  | Mult
  | Div
  | Mod
  | LSL
  | LSR
  | Lt
  | LtE
  | Gt
  | GtE
  | Eq
  | NotEq
  (* TODO? in *)
  | And
  | Or
  | BitAnd
  | BitOr
  | BitXor

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Local binding *)
(* ------------------------------------------------------------------------- *)
(* TODO *)
and local_binding = unit

(* ------------------------------------------------------------------------- *)
(* Functions  *)
(* ------------------------------------------------------------------------- *)
and function_definition = unit

(* ------------------------------------------------------------------------- *)
(* Objects  *)
(* ------------------------------------------------------------------------- *)
and field = unit

(*****************************************************************************)
(* Directives *)
(*****************************************************************************)
and import = unit [@@deriving show { with_path = false }]

(*****************************************************************************)
(* Program *)
(*****************************************************************************)

type program = expr [@@deriving show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let e ekind = { e = ekind }
