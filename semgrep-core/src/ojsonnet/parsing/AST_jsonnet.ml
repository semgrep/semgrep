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
(* An Abstract Syntax Tree for Jsonnet (well kinda concrete actually).
 *
 * This AST/CST is mostly derived from the tree-sitter-jsonnet grammar:
 * https://github.com/sourcegraph/tree-sitter-jsonnet
 * I tried to keep the original terms (e.g., LocalBind) instead of the
 * terms I use in AST_generic (e.g., Let).
 * See also the excellent spec: https://jsonnet.org/ref/spec.html
 *
 * The main uses for this file are:
 *  - for Semgrep to allow people to use jsonnet patterns to match
 *    over jsonnet code
 *  - TODO: potentially for implementing a jsonnet interpreter in OCaml,
 *    so we can use it in osemgrep instead of having to write an OCaml
 *    binding to the jsonnet C library. This could allow in turn to provide
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
  | Object of field list bracket
  | Array of expr list bracket
  (* TODO: Object/ArrayComprenhension *)
  (* entities *)
  | Id of string wrap
  | IdSpecial of special wrap
  (* =~ Let *)
  | LocalBind of tok (* 'local' *) * local_binding list * tok (* ; *) * expr
  (* accesses *)
  | DotAccess of expr * tok (* '.' *) * ident
  | ArrayAccess of expr * expr bracket
  (* TODO: | SliceAccess of expr *  *)
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
  | Str of string_

and string_ = verbatim option * string_kind * string_content bracket
and verbatim = tok (* @ *)
and string_kind = SingleQuote | DoubleQuote | TripleBar (* a.k.a Text block *)

(* TODO? parse the inside? %x and so on? *)
and string_content = string wrap list

(* Super can appear only in DotAccess/ArrayAccess/InSuper
 * TODO? make special construct for those special Super case instead?
 * At the same time in the spec when they unsugar constructs to a
 * "core" language, then super is put at the expression level.
 *)
and special = Self | Super | Dollar (* ??? *)
and argument = Arg of expr | NamedArg of ident * tok (* = *) * expr

(* alt: we could reuse AST_generic_.ml, but because we might want to
 * make a jsonnet interpreter, better to be as precise as possible.
 *)
and unary_op = UPlus | UMinus | UBang | UTilde

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
and function_definition = {
  ftok : tok;
  fparams : parameter list bracket;
  fbody : expr;
}

and parameter = unit

(* ------------------------------------------------------------------------- *)
(* Objects  *)
(* ------------------------------------------------------------------------- *)
and field = unit
and fieldname = unit

(*****************************************************************************)
(* Directives *)
(*****************************************************************************)
and import = unit [@@deriving show { with_path = false }]

(*****************************************************************************)
(* Program *)
(*****************************************************************************)

type program = expr [@@deriving show]

(*****************************************************************************)
(* Any (mostly for semgrep) *)
(*****************************************************************************)
type any = E of expr

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let e ekind = { e = ekind }
