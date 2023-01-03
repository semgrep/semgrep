(* Yoann Padioleau
 *
 * Copyright (C) 2014 Facebook
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
open Ast_c

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Normalized expressions (converted into instructions and lvalues/rvalues).
 *
 * Ast_cpp -> Ast_c -> Ast_cil ...
 *
 * We are doing flow-insensitive analysis so the goal here is just
 * to convert Ast_c.expr into something that is easier to work-on
 * to generate datalog facts.
 *
 * related work:
 *  - CIL :)
 *  - SIL
 *
 * See also pfff/mini/ast_minic.ml
 *)

(*****************************************************************************)
(* CIL-expr types *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Names *)
(* ------------------------------------------------------------------------- *)

(* for functions, constants, fields, builtins, types *)
type name = string wrap [@@deriving show]

(* for globals, locals, parameters *)
type var = name [@@deriving show]

(* ------------------------------------------------------------------------- *)
(* Lvalue *)
(* ------------------------------------------------------------------------- *)
(* Used to be inlined in expr (now called rvalue), but it is cleaner
 * to separate rvalue and lvalue. Note that 'Call' is not there, it's
 * not an lvalue (you can not do 'foo() = x' in C).
 *)
type lvalue =
  | Id of name (* actually a var or name *)
  | ObjField of var * name (* x->fld *)
  | ArrayAccess of var * var (* x[y] *)
  (* hmm mv? *)
  | DeRef of var (* *x *)
[@@deriving show { with_path = false }]
(* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Rvalue *)
(* ------------------------------------------------------------------------- *)
(* see ast_minic.ml for more comments about this CIL-like AST *)
type rvalue =
  | Int of int option wrap
  | Float of float option wrap
  | String of string wrap (* string or char *)
  | StaticCall of name * var list (* foo(...) *)
  | DynamicCall of (*Deref*) var * var list (* ( *f)(...) *)
  | BuiltinCall of name * var list (* e.g. v + 1 *)
  (* could be a lvalue, but weird to do (malloc(...)[x] = ...) *)
  | Alloc of type_ (* malloc(sizeof(type)) *)
  | AllocArray of var * type_ (* malloc(n*sizeof(type)) *)
  | Lv of lvalue
[@@deriving show { with_path = false }]
(* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Stmt *)
(* ------------------------------------------------------------------------- *)

type instr =
  | Assign of var (* or name *) * rvalue (* x = e *)
  | AssignAddress of var * lvalue (* except Deref (no sense to do &*x) *)
  | AssignLvalue of lvalue * var (* Except Id, done by Assign *)
[@@deriving show { with_path = false }]
(* with tarzan *)
