(*
   Copyright (c) 2021-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* Note!!
   `program` and `any` will behave slightly differently on metavariables.
   `program` assumes a target, and `any` assumes a pattern.
*)

(* Parsing a YAML file.
 * This may raise Parse_info.Other_error.
 *)
val program : Fpath.t -> AST_generic.program

(* parsing a semgrep YAML pattern *)
val any : string -> AST_generic.any

(* internals used in Parse_rule.ml *)
val parse_yaml_file :
  is_target:bool ->
  Fpath.t (* origin *) ->
  string (* file content *) ->
  AST_generic.program
