(*
   Copyright (c) 2024-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* annotates the cfg with facts *)
val hook_annotate_facts : (IL.cfg -> unit) option Hook.t

(* checks if any of the facts satisfies the when condition (e) *)
val hook_facts_satisfy_e :
  (Metavariable.bindings -> AST_generic.facts -> AST_generic.expr -> bool)
  option
  Hook.t

(* TODO: Can't use Hook.ml yet as this is set in Pro_core_CLI.ml *)
val hook_path_sensitive : bool ref
val with_pro_hooks : (unit -> 'a) -> 'a
