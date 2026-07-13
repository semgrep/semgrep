(*
   Copyright (c) 2026 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)

(** Hook slot for a static C/C++ preprocessor [#if] / [#elif] condition
    evaluator. When unset (the default), callers must treat every
    expression as [Unknown] and keep every branch. *)

(** Result of evaluating a preprocessor condition expression. *)
type t =
  | Truthy  (** Value is definitively non-zero. *)
  | Falsy  (** Value is definitively zero. *)
  | Unknown  (** Cannot be decided statically. *)

val hook_eval : (Ast_cpp.expr -> t) option Hook.t
(** When [Some evaluator] is installed, callers use [evaluator] to decide
    whether a [#if]/[#elif] branch is statically dead. Default is [None]. *)
