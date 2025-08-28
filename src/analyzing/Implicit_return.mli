(*
   Copyright (c) 2023-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* True when the given language supports implicit returns. *)
val lang_supports_implicit_return : Lang.t -> bool

(* For the given AST, mark all expression nodes that may be executed before
 * exiting a function as "returning nodes".
 * See the 'is_implicit_return' expr field in AST_generic.
 *)
val mark_implicit_return : Lang.t -> AST_generic.program -> unit
