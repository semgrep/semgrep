(*
   Copyright (c) 2021-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* Works by side effect on the generic AST by modifying its refs.
 * We pass the lang because some name resolution algorithm may be
 * specific to a language.
 *)
val resolve : Lang.t -> AST_generic.program -> unit

(* pro-scan hook *)
val pro_hook_normalize_ast_generic_type :
  (Lang.t -> AST_generic.type_ -> AST_generic.type_) option Hook.t
