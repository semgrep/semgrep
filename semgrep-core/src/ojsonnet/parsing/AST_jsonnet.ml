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
(* An Abstract Syntax Tree (AST) for Jsonnet (well kinda concrete actually).
 *
 * This AST/CST is mostly derived from the tree-sitter-jsonnet grammar:
 * https://github.com/sourcegraph/tree-sitter-jsonnet
 * I tried to keep the original terms (e.g., Local, hidden) instead of the
 * terms we use in AST_generic (e.g., Let, annotation).
 *
 * See also the excellent spec: https://jsonnet.org/ref/spec.html,
 * and especially https://jsonnet.org/ref/spec.html#abstract_syntax
 *
 * There is also an ANTLR grammar here:
 * https://gist.github.com/ironchefpython/84380aa60871853dc86719dd598c35e4
 * used in https://github.com/sourcegraph/lsif-jsonnet
 *
 * The main uses for this file are:
 *  - for Semgrep to allow people to use Jsonnet patterns to match
 *    over Jsonnet code
 *  - potentially for implementing a Jsonnet interpreter in OCaml,
 *    so we can use it in osemgrep instead of having to write an OCaml
 *    binding to the Jsonnet C library. This could allow in turn to provide
 *    better error messages when there is an error in a Jsonnet
 *    semgrep rule. Indeed right now the error will be mostly
 *    reported on the resulting (also called "manifested") JSON.
 *    This could also enable certain holistic optimizations or
 *    static checks at some point.
 *)

(*****************************************************************************)
(* Tokens (leaves) *)
(*****************************************************************************)

type tok = Parse_info.t [@@deriving show]
type 'a wrap = 'a * tok [@@deriving show]
type 'a bracket = tok * 'a * tok [@@deriving show]

(*****************************************************************************)
(* Names *)
(*****************************************************************************)
(* very simple language, no qualified names here *)
type ident = string wrap [@@deriving show]

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)

(* Should we use a record for expressions like we do in AST_generic?
 * If we implement a Jsonnet interpreter, such a record could become useful
 * for example to store the types of each expressions?
 * Actually the Jsonnet spec defines an intermediate "Core" representation
 * where things are simplified and unsugared
 * (see https://jsonnet.org/ref/spec.html#core), so we probably do not
 * need to use a record here, but we could in an hypothetical Core_jsonnet.ml
 * file at some point.
 * old: when using a record:
 *   type expr = { e : expr_kind }
 *)
type expr = expr_kind

(* very simple language, just expressions! no statement, no class defs (but
 * some object defs) *)
and expr_kind =
  (* values *)
  | L of literal
  | O of obj_inside bracket
  | A of arr_inside bracket
  (* entities *)
  | Id of string wrap
  | IdSpecial of special wrap
  (* =~ Let. Note that a series of 'local x = 1; local y = 2; x+y'
   * will be interpreted as nested Local. There is no real
   * sequence operator in Jsonnet. The ';' is used only for
   * Local and for Assert.
   *)
  | Local of tok (* 'local' *) * bind list * tok (* ; *) * expr
  (* accesses *)
  | DotAccess of expr * tok (* '.' *) * ident
  | ArrayAccess of expr * expr bracket
  (* TODO: | SliceAccess of expr *  *)
  (* control flow *)
  | Call of expr * argument list bracket
  | UnaryOp of unary_op wrap * expr
  | BinaryOp of expr * binary_op wrap * expr
  | If of tok (* 'if' *) * expr * expr * (tok (* 'else' *) * expr) option
  (* TODO: expr { objinside } ?? *)
  (* TODO: expr in super ?? *)
  | Lambda of function_definition
  (* directives *)
  | I of import
  (* builtins *)
  | Assert of assert_ * tok (* ';' *) * expr
  | Error of tok (* 'error' *) * expr
  (* for the CST *)
  | ParenExpr of expr bracket
  | TodoExpr of string wrap * expr list

(* ------------------------------------------------------------------------- *)
(* literals *)
(* ------------------------------------------------------------------------- *)
and literal =
  | Null of tok
  | Bool of bool wrap
  (* for integers and floats; no difference in jsonnet *)
  | Number of string wrap
  | Str of string_

and string_ = verbatim option * string_kind * string_content bracket
and verbatim = tok (* @ *)
and string_kind = SingleQuote | DoubleQuote | TripleBar (* a.k.a Text block *)

(* The string can contain escape sequences and also special chars (e.g., '%')
 * which are interpreted in a special way by certain builtins (e.g., format).
 * There is no string interpolation in Jsonnet, but the '%' can
 * be used for a similar purpose.
 *)
and string_content = string wrap list

(* ------------------------------------------------------------------------- *)
(* Calls *)
(* ------------------------------------------------------------------------- *)

(* Super can appear only in DotAccess/ArrayAccess/InSuper.
 * alt: we could make special constructs for those special Super cases.
 * However, in the spec they unsugar those constructs to a
 * "core" language where super is at the expression level, so this is
 * probably simpler to have it here instead of in 3 special constructs.
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

and assert_ = tok (* 'assert' *) * expr * (tok (* ':' *) * expr) option

(* ------------------------------------------------------------------------- *)
(* Collections and comprehensions *)
(* ------------------------------------------------------------------------- *)
and arr_inside = Array of expr list | ArrayComp of expr comprehension
and 'a comprehension = 'a * for_comp * for_or_if_comp list
and for_or_if_comp = CompFor of for_comp | CompIf of if_comp
and for_comp = tok (* 'for' *) * ident * tok (* 'in' *) * expr
and if_comp = tok (* 'if' *) * expr

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

(* semgrep: LATER can have ParamEllipsis *)
and parameter = P of ident * (tok (* '=' *) * expr) option

(* ------------------------------------------------------------------------- *)
(* Objects  *)
(* ------------------------------------------------------------------------- *)
and obj_inside = Object of obj_member list | ObjectComp of obj_comprehension
and obj_member = OLocal of obj_local | OField of field | OAssert of assert_

and field = {
  fld_name : field_name;
  fld_attr : attribute option;
  fld_hidden : hidden wrap;
  (* can be a Lambda for methods *)
  fld_value : expr;
}

and field_name = FId of ident | FStr of string_ | FDynamic of expr bracket

(* =~ visibility *)
and hidden = Colon | TwoColons | ThreeColons

and attribute =
  (* concatenate fields, not valid for methods *)
  | PlusField of tok

and obj_local = tok (* 'local' *) * bind

and obj_comprehension = {
  oc_locals1 : obj_local list;
  oc_comp :
    (expr bracket (* like FDynamic *) * tok (* : *) * expr) comprehension;
  (* after the comprehension elt but before the forspec *)
  oc_locals2 : obj_local list;
}

(*****************************************************************************)
(* Directives *)
(*****************************************************************************)
and import =
  | Import of tok (* 'import' *) * string_ (* filename *)
  (* As opposed to Import, in ImportStr the content of the file
   * is not evaluated but just returned as a raw string (to be stored
   * in a local).
   *)
  | ImportStr of tok (* 'importstr' *) * string_ (* filename *)
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Program *)
(*****************************************************************************)

type program = expr [@@deriving show]

(*****************************************************************************)
(* Any (for semgrep) *)
(*****************************************************************************)
(* Right now there is just one case, but at some point we may want to
 * allow field patterns.
 *)
type any = E of expr

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let e ekind = (* old: when we use a record  { e = ekind } *) ekind
