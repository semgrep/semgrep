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
(* The "Core" language subset of Jsonnet.
 *
 * See https://jsonnet.org/ref/spec.html#core
 *
 * This is mostly a copy-paste of AST_jsonnet.ml with a few constructs
 * removed thanks to Desugar_jsonnet.ml (see the "no xxx anymore" comment).
 *)

(*****************************************************************************)
(* Tokens (leaves) *)
(*****************************************************************************)

(* Same types than in AST_jsonnet.ml
 * TODO: handle origin of file to track imports
 *)
type tok = Tok.t [@@deriving show]
type 'a wrap = 'a * tok [@@deriving show]
type 'a bracket = tok * 'a * tok [@@deriving show]
type todo_kind = string wrap [@@deriving show]

(*****************************************************************************)
(* Names *)
(*****************************************************************************)
(* same than in AST_jsonnet.ml (but can contain '$') *)
type ident = string wrap [@@deriving show]

(*****************************************************************************)
(* Expr *)
(*****************************************************************************)

(* Core expressions: No Array slices, no complex Array or Object comprehension,
 * no DotAccess (use generalized ArrayAccess), no Assert, no ParenExpr,
 * no Import (expanded during desugaring).
 *
 * alt: we could use a record for expressions like we do in AST_generic.
 * Such a record could become useful for example to store the types of
 * each expressions for Check_jsonnet.ml, but probably we can
 * make such a typechecker in a compositional way without having to modify
 * record fields.
 *)
type expr =
  | L of AST_jsonnet.literal
  | O of obj_inside bracket
  (* no complex arr_inside, no ArrayComp *)
  | Array of expr list bracket
  (* entities *)
  | Id of string wrap
  | IdSpecial of special wrap
  | Local of tok (* 'local' *) * bind list * tok (* ; *) * expr
  (* access (generalize DotAccess) *)
  | ArrayAccess of expr * expr bracket
  (* control flow *)
  | Call of expr * argument list bracket
  | UnaryOp of unary_op wrap * expr
  | BinaryOp of expr * binary_op wrap * expr
  (* always with an else now (Null if there was no else) *)
  | If of tok * expr * expr * expr
  | Lambda of function_definition
  (* builtins *)
  | Error of tok (* 'error' *) * expr
  (* TODO: what are constructs not yet handled? list them *)
  | ExprTodo of todo_kind * AST_jsonnet.expr

(* ------------------------------------------------------------------------- *)
(* Calls *)
(* ------------------------------------------------------------------------- *)

(* no Dollar anymore *)
and special = Self | Super

(* the NamedArg are supposed to be the last arguments *)
and argument = Arg of expr | NamedArg of ident * tok (* = *) * expr
and unary_op = AST_jsonnet.unary_op

(* no '!=', '==', '%', 'in' *)
and binary_op =
  | Plus
  | Minus
  | Mult
  | Div
  | LSL
  | LSR
  | Lt
  | LtE
  | Gt
  | GtE
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
and bind = B of ident * tok (* '=' *) * expr (* can be a Function *)

(* ------------------------------------------------------------------------- *)
(* Functions  *)
(* ------------------------------------------------------------------------- *)
and function_definition = {
  f_tok : tok;
  f_params : parameter list bracket;
  f_body : expr;
}

(* always with a default value now (Error if there was no default value) *)
and parameter = P of ident * tok (* '=' *) * expr

(* ------------------------------------------------------------------------- *)
(* Objects  *)
(* ------------------------------------------------------------------------- *)
and obj_inside =
  (* no OAssert anymore (moved up in Object), no OLocal *)
  | Object of obj_assert list * field list
  (* used also for Array comprehension *)
  | ObjectComp of obj_comprehension

and obj_assert = tok (* assert *) * expr

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
and hidden = AST_jsonnet.hidden

(* no more locals1 and locals2, no CompIf *)
and obj_comprehension = field_name * tok (* : *) * expr * for_comp

and for_comp = tok (* 'for' *) * ident * tok (* 'in' *) * expr
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Program *)
(*****************************************************************************)

type program = expr [@@deriving show]
