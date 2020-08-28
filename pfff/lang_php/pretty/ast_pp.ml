(* Julien Verlaguet, Yoann Padioleau
 *
 * Copyright (C) 2011, 2012 Facebook
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
(*
 * This file is mostly a copy paste of ast_php_simple.ml but with some
 * additional constructors for comments or newlines so that we can
 * pretty print code while maintaining the comments and newlines
 * of the original file. Moreover a few things are now unsugared
 * in ast_php_simple.ml such as Encaps, InlineHtml, ShortArray,
 * that we must keep here because of the refactoring/pretty-printing context.
 *
 * coupling: that means each time we add a feature in PHP we need to extend
 * ast_php.ml, ast_php_simple.ml, and now ast_pp.ml :( Life is hard.
 *
 * Note that we assume all Newline below are esthetic newlines
 * (see the prelude comment in ast_pp_build.ml).
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

type esthetic =
  | Comment of string
  | Newline

type program = stmt list

(* ------------------------------------------------------------------------- *)
(* Statement *)
(* ------------------------------------------------------------------------- *)
and stmt =
  | StmtEsthet of esthetic

  | Expr of expr

  (* Can Noop be Block []? Maybe, but right now it's used to represent
   * empty else branch in If below, and changing Noop to Block []
   * introduces some regressions. So safer to keep Noop for now.
   *)
  | Noop
  | Block of stmt list

  | If of expr * stmt * stmt
  | Switch of expr * case list

  | While of expr * stmt list
  | Do of stmt list * expr
  | For of expr list * expr list * expr list * stmt list
  | Foreach of expr * expr * expr option * stmt list

  | Return of expr option
  | Break of expr option | Continue of expr option

  | Throw of expr
  | Try of stmt list * catch list * finally list

  | InlineHtml of string

  (* only at toplevel in most of our code *)
  | ClassDef of class_def
  | FuncDef of func_def
  | ConstantDef of constant_def

  | StaticVars of (string * expr option) list
  | Global of expr list

  and case =
  | CaseEsthet of esthetic

  | Case of expr * stmt list
  | Default of stmt list

  (* catch(Exception $exn) { ... } => ("Exception", "$exn", [...]) *)
  and catch = hint_type * string * stmt list
  and finally = stmt list

(* ------------------------------------------------------------------------- *)
(* Expression *)
(* ------------------------------------------------------------------------- *)
and expr =
  | Int of string
  | Double of string

  | String of string
  | Guil of encaps list
  | HereDoc of string * encaps list * string

  | Id of string

  | Array_get of expr * expr option

  | This
  | Obj_get of expr * expr
  | Class_get of expr * expr

  | Assign of Cst_php.binaryOp option * expr * expr
  | Infix of Cst_php.fixOp * expr
  | Postfix of Cst_php.fixOp * expr
  | Binop of Cst_php.binaryOp * expr * expr
  | Unop of Cst_php.unaryOp * expr

  | Call of expr * expr list

  | Ref of expr
  | Unpack of expr

  | Xhp of xml
  | ConsArray of array_value list
  | Collection of string * array_value list
  | List of expr list

  | New of expr * expr list
  | InstanceOf of expr * expr

  | CondExpr of expr * expr * expr
  | Cast of Cst_php.ptype * expr

  | Lambda of lambda_def

  and map_kind =
    | Map
    | StableMap

  and array_value =
    | Aval of expr
    | Akval of expr * expr

  and vector_value = expr
  and map_value = expr * expr
  and encaps =
    | EncapsString of string
    | EncapsVar of expr
    | EncapsCurly of expr
    | EncapsDollarCurly of expr
    | EncapsExpr of expr

  and xhp =
    | XhpText of string
    | XhpExpr of expr
    | XhpXml of xml

    and xml = {
      xml_tag: string list;
      xml_attrs: (string * xhp_attr) list;
      xml_body: xhp list;
    }

      and xhp_attr =
        | AttrString of encaps list
        | AttrExpr of expr

(* ------------------------------------------------------------------------- *)
(* Types *)
(* ------------------------------------------------------------------------- *)

and hint_type =
     | Hint of string
     | HintArray
     | HintQuestion of hint_type
     | HintTuple of hint_type list
     | HintCallback of hint_type list * (hint_type option)
     | HintVariadic of hint_type option

(* ------------------------------------------------------------------------- *)
(* Definitions *)
(* ------------------------------------------------------------------------- *)
and func_def = {
  f_ref: bool;
  f_name: string;
  f_params: parameter list;
  f_return_type: hint_type option;
  f_body: stmt list;
}

and parameter = {
  (* todo: modifiers *)
  p_type: hint_type option;
  p_ref: bool;
  p_name: string;
  p_default: expr option;
  p_variadic: bool;
}

and lambda_def = {
  l_ref: bool;
  l_params: parameter list;
  (* actually use parameter can't have a default value nor a type hint
   * so maybe we should use a more specific type *)
  l_use: parameter list;
  l_body: stmt list;
}

and constant_def = {
  cst_name: string;
  cst_body: expr option;
}
and class_def = {
  c_type: class_type;
  c_name: string;
  c_extends: hint_type list;
  c_implements: hint_type list;
  c_body: class_element list;
}

and class_element =
  | CEEsthet of esthetic
  | CEconst of bool (* is_abstract *) * constant_def list
  | CEdef of class_vars_def
  | CEmethod of method_def

and class_type =
  | ClassRegular
  | ClassFinal
  | ClassAbstract
  | ClassAbstractFinal
  | Interface
  | Trait

and class_vars_def = {
  cv_final: bool;
  cv_static: bool;
  cv_abstract: bool;
  cv_visibility: visibility;
  cv_type: hint_type option;
  cv_vars: cvar list;
}

and cvar = string * expr option

and method_def = {
  m_visibility: visibility;
  m_static: bool;
  m_final: bool;
  m_abstract: bool;
  m_ref: bool;
  m_name: string;
  m_params: parameter list;
  m_return_type: hint_type option;
  m_body: stmt list;
}

and visibility =
  | Novis
  | Public  | Private
  | Protected | Abstract

(* with tarzan *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let has_modifier cv =
  cv.cv_final ||
  cv.cv_static ||
  cv.cv_abstract ||
  cv.cv_visibility <> Novis

let rec is_string_key = function
  | [] -> true
  | Aval _ :: _ -> false
  | Akval (String _, _) :: rl -> is_string_key rl
  | _ -> false

let key_length_acc c = function
  | Aval _ -> c
  | Akval (String s, _) -> max (String.length s + 2) c
  | _ -> c

let key_length l =
  List.fold_left key_length_acc 0 l
