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
