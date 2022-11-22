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
(* The "core" language subset of jsonnet
 *
 * See https://jsonnet.org/ref/spec.html#core
 *
 * This is mostly a copy-paste of AST_jsonnet.ml with a few constructs
 * removed thanks to Desugar_jsonnet.ml (see the "no xxx anymore" comment).
 *)

(*****************************************************************************)
(* Tokens (leaves) *)
(*****************************************************************************)

(* same than in AST_jsonnet.ml
 * TODO: handle origin of file to track imports
 *)
type tok = Parse_info.t [@@deriving show]
type 'a wrap = 'a * tok [@@deriving show]
type 'a bracket = tok * 'a * tok [@@deriving show]

(*****************************************************************************)
(* Names *)
(*****************************************************************************)
(* same than in AST_jsonnet.ml (but can contain '$') *)
type ident = string wrap [@@deriving show]

(*****************************************************************************)
(* Expr *)
(*****************************************************************************)

(* We use a record for expressions like we do in AST_generic.
 * Such a record could become useful for example to store the types of
 * each expressions for Check_jsonnet.ml
 *)
type expr = { e : expr_kind }

(* no Array slices, no complex Array or Object comprehension,
 * no DotAccess (use generalized ArrayAccess), no Assert, no ParenExpr,
 * no Import (expanded during desugaring), no TodoExpr.
 *)
and expr_kind =
  | L of literal
  | O of obj_inside bracket
  | A of arr_inside bracket
  (* entities *)
  | Id of string wrap
  | IdSpecial of special wrap
  | Local of tok (* 'local' *) * bind list * tok (* ; *) * expr
  (* accesses *)
  | ArrayAccess of expr * expr bracket
  (* control flow *)
  | Call of expr * argument list bracket
  | UnaryOp of unary_op wrap * expr
  | BinaryOp of expr * binary_op wrap * expr
  (* always an else now *)
  | If of tok * expr * expr * expr
  | Lambda of function_definition
  (* builtins *)
  | Error of tok (* 'error' *) * expr

(* ------------------------------------------------------------------------- *)
(* literals *)
(* ------------------------------------------------------------------------- *)
and literal =
  | Null of tok
  | Bool of bool wrap
  | Number of string wrap
  | Str of string_

and string_ = verbatim option * string_kind * string_content bracket
and verbatim = tok (* @ *)
and string_kind = SingleQuote | DoubleQuote | TripleBar (* a.k.a Text block *)
and string_content = string wrap list

(* ------------------------------------------------------------------------- *)
(* Calls *)
(* ------------------------------------------------------------------------- *)

(* no Dollar anymore *)
and special = Self | Super
and argument = Arg of expr | NamedArg of ident * tok (* = *) * expr
and unary_op = UPlus | UMinus | UBang | UTilde

(* no !=, ==, %, in *)
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
  | And
  | Or
  | BitAnd
  | BitOr
  | BitXor

and assert_ = tok (* 'assert' *) * expr * (tok (* ':' *) * expr) option

(* ------------------------------------------------------------------------- *)
(* Collections and comprehensions *)
(* ------------------------------------------------------------------------- *)
(* no ArrayComp *)
and arr_inside = Array of expr list
and 'a comprehension = 'a * for_comp * for_or_if_comp list

(* no CompIf *)
and for_or_if_comp = CompFor of for_comp
and for_comp = tok (* 'for' *) * ident * tok (* 'in' *) * expr

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Local binding *)
(* ------------------------------------------------------------------------- *)
(* we already desugar 'foo(x,y) = z' as 'foo = function(x, y) z' *)
and bind = B of ident * tok (* '=' *) * expr (* can be a Function *)

(* ------------------------------------------------------------------------- *)
(* Functions  *)
(* ------------------------------------------------------------------------- *)
and function_definition = {
  f_tok : tok;
  f_params : parameter list bracket;
  f_body : expr;
}

and parameter = P of ident * (tok (* '=' *) * expr) option

(* ------------------------------------------------------------------------- *)
(* Objects  *)
(* ------------------------------------------------------------------------- *)
and obj_inside = Object of obj_member list | ObjectComp of obj_comprehension

(* no OAssert anymore, no OLocal *)
and obj_member = OField of field

and field = {
  fld_name : field_name;
  (* no more PlusField *)
  fld_hidden : hidden wrap;
  (* can be a Lambda for methods *)
  fld_value : expr;
}

(* no FId, FStr, and FDynamic -> FExpr *)
and field_name = FExpr of expr bracket

(* =~ visibility *)
and hidden = Colon | TwoColons | ThreeColons
and obj_local = tok (* 'local' *) * bind

and obj_comprehension = {
  oc_locals1 : obj_local list;
  oc_comp :
    (expr bracket (* like FDynamic *) * tok (* : *) * expr) comprehension;
  (* after the comprehension elt but before the forspec *)
  oc_locals2 : obj_local list;
}
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Program *)
(*****************************************************************************)

type program = expr [@@deriving show]

(*****************************************************************************)
(* Any (for semgrep) *)
(*****************************************************************************)
type any = E of expr

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let e ekind = { e = ekind }
