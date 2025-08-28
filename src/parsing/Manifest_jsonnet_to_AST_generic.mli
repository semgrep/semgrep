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
(* Manifest a Jsonnet value to a generic AST program so we can
 * then parse this jsonnet rule in Parse_rule.ml. This is similar
 * to what we do to parse a YAML or JSON rule; in both cases
 * we first parse and create a generic AST before calling
 * Parse_rule.parse_generic_ast to finally get a Rule.t
 *)
val manifest_value : Value_jsonnet.t -> AST_generic.program
