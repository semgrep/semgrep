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

(* Hook slot for an evaluator of C/C++ preprocessor #if / #elif
 * conditions. When unset, callers must treat every
 * expression as [Unknown] and keep every branch. *)

type t = Truthy | Falsy | Unknown

let hook_eval : (Ast_cpp.expr -> t) option Hook.t = Hook.create None
