(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* Check metavariable-name constraints *)

val hook_is_kind :
  (Rule.metavar_name_kind -> AST_generic.expr -> bool) option Hook.t

val hook_module_resolver : (string list -> String.t Base.List.t) option Hook.t

(* Check whether `expr` satisfies the condition described in
   `metavar_cond_name`. *)
val find_name :
  Match_env.env -> AST_generic.expr -> Rule.metavar_cond_name -> bool
