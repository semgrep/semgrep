(*
   Copyright (c) 2022-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* Convert a generic AST (coming for example from Yaml_to_generic)
 * to an AST_jsonnet program. This is used in Rule_fetching import_callback
 * to convert a yaml program in a jsonnet program so jsonnet policy
 * can manipulate legacy YAML rules.
 *
 * This is the similar to the reverse of Manifest_jsonnet_to_AST_generic
 * (used by Parse_rule.ml), but here we produce an AST_jsonnet instead
 * of a Value_jsonnet.
 *
 * may raise Parse_info.Other_Error if the generic AST program contain
 * constructs that don't have an equivalent in AST_jsonnet.
 *)
val program : AST_generic.program -> AST_jsonnet.program
