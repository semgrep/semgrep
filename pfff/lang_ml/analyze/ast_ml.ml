(* Yoann Padioleau
 *
 * Copyright (C) 2019, 2020 r2c
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
(* An Abstract Syntax Tree for OCaml.
 *
 * See cst_ml.ml for a Concrete Syntax Tree for OCaml (better for program
 * transformation purpose).
 *)

(*****************************************************************************)
(* The AST related types *)
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

type name = qualifier * ident
 and qualifier = ident list (* TODO: functor? *)
 [@@deriving show] (* with tarzan *)

type todo_category = string wrap
 [@@deriving show] (* with tarzan *)
(* ------------------------------------------------------------------------- *)
(* Types *)
(* ------------------------------------------------------------------------- *)

type type_ = 
  | TyName of name (* include builtins *)
  | TyVar of ident (* 'a *)

  | TyFunction of type_ * type_
  | TyApp of type_ list * name (* less: could be merged with TyName *)

  | TyTuple of type_ list (* at least 2 *)

  (* sgrep-ext: *)
  | TyEllipsis of tok

  | TyTodo of todo_category * type_ list

 [@@deriving show { with_path = false} ] (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Expressions *)
(* ------------------------------------------------------------------------- *)

(* start of big recursive type *)
type expr =
  | L of literal
  | Name of name

  | Constructor of name * expr option
  (* special case of Constr *)
  | Tuple of expr list
  | List  of expr list bracket

  (* can be empty *) 
  | Sequence of expr list

  | Prefix of string wrap * expr
  | Infix of expr * string wrap * expr

  | Call of expr * argument list

  (* could be factorized with Prefix but it's not a usual prefix operator! *)
  | RefAccess of tok (* ! *) * expr
  | RefAssign of expr * tok (* := *) * expr

  (* special case of RefAccess and RefAssign *)
  | FieldAccess of expr * tok * name
  | FieldAssign of expr * tok * name * tok (* <- *) * expr

  | Record of expr option (* with *) * (name * expr) list bracket

  | New of tok * name
  | ObjAccess of expr * tok (* # *) * ident
  

  (* > 1 elt for mutually recursive let (let x and y and z) *)
  | LetIn of tok * let_binding list * expr * rec_opt
  | Fun of tok (* 'fun' *) * parameter list (* at least one *) * expr
  | Function of tok (* 'function' *) * match_case list

  | If of tok * expr * expr * expr option
  | Match of tok * expr * match_case list

  | Try of tok * expr * match_case list 

  | While of tok * expr * expr
  | For of tok * ident * expr * for_direction * expr *   expr

  (* sgrep-ext: *)
  | Ellipsis of tok
  | DeepEllipsis of expr bracket

  | ExprTodo of todo_category * expr list

 and literal =
   | Int    of string wrap
   | Float  of string wrap
   | Char   of string wrap
   | String of string wrap

 and argument = 
   | Arg of expr
   | ArgKwd of ident * expr
   | ArgQuestion of ident  * expr

 and match_case =
  pattern * match_action

  and match_action = expr option (* when *) * tok (* -> *) * expr

 and for_direction =
  | To of tok
  | Downto of tok

 and rec_opt = tok option

(* ------------------------------------------------------------------------- *)
(* Patterns *)
(* ------------------------------------------------------------------------- *)
and pattern = 
  | PatVar of ident
  | PatLiteral of literal (* can be signed *)
  | PatConstructor of name * pattern option
 
  (* special cases of PatConstructor *)
  | PatConsInfix of pattern * tok (* :: *) * pattern
  | PatTuple of pattern list
  | PatList of pattern list bracket

  | PatUnderscore of tok
  | PatRecord of (name * pattern) list bracket

  | PatAs of pattern * ident
  (* ocaml disjunction patterns extension *)
  | PatDisj of pattern * pattern
  | PatTyped of pattern * type_

  (* sgrep-ext: *)
  | PatEllipsis of tok

  | PatTodo of todo_category * pattern list

(* ------------------------------------------------------------------------- *)
(* Let binding (global/local/function definition) *)
(* ------------------------------------------------------------------------- *)

and let_binding =
  | LetClassic of let_def
  | LetPattern of pattern * expr

 (* was called fun_binding in the grammar *)
 and let_def = {
   lname: ident;
   lparams: parameter list; (* can be empty *)
   lbody: expr;
 }

 and parameter = 
   | Param of pattern
   (* ParamEllipsis can be done via ParamPat (PatEllipsis) *)
   | ParamTodo of tok
 
 [@@deriving show { with_path = false} ]  (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Type declaration *)
(* ------------------------------------------------------------------------- *)

type type_declaration = {
  tname: ident;
  tparams: type_parameter list;
  tbody: type_def_kind;
}

 and type_parameter = ident (* a TyVar, e.g., 'a *)

 and type_def_kind =
   | AbstractType
   | CoreType of type_
   (* or type *)
   | AlgebraicType of (ident * type_ list) list
   (* and type *)
   | RecordType   of (ident * type_ * tok option (* mutable *)) list bracket

 [@@deriving show { with_path = false} ] (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Class *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Module *)
(* ------------------------------------------------------------------------- *)
type module_declaration = {
  mname: ident;
  mbody: module_expr;
}

  (* mutually recursive with item *)
  and module_expr =
  | ModuleName of name (* alias *)
  | ModuleStruct of item list

  | ModuleTodo of todo_category * module_expr list

(* ------------------------------------------------------------------------- *)
(* Signature/Structure items *)
(* ------------------------------------------------------------------------- *)

(* could split in sig_item and struct_item but many constructions are
 * valid in both contexts.
 *)
and item = 
  | Type of tok * type_declaration list (* mutually recursive *)

  | Exception of tok * ident * type_ list
  | External  of tok * ident * type_ * string wrap list (* primitive decls *)
      
  | Open of tok * name
      
  (* only in sig_item *)
  | Val of tok * ident * type_
      
  (* only in struct_item *)
  | Let of tok * rec_opt * let_binding list

  | Module of tok * module_declaration

  | ItemTodo of todo_category * item list

 [@@deriving show { with_path = false} ] (* with tarzan *)
      
type program = item list
 [@@deriving show ] (* with tarzan *)

(*****************************************************************************)
(* Any *)
(*****************************************************************************)

type any =
  | T of type_
  | E of expr
  | P of pattern

  | I of item
  | Pr of program

 [@@deriving show { with_path = false} ] (* with tarzan *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)

let str_of_ident (s,_) = s
let info_of_ident (_,info) = info

let ident_of_name (_, ident) = ident
let qualifier_of_name (qu, _) = 
  qu |> List.map str_of_ident |> Common.join "."
