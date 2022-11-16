(* Parsing a YAML file.
 * This may raise Parse_info.Other_error.
 *)
val program : Common.filename -> AST_generic.program

(* parsing a semgrep YAML pattern *)
val any : string -> AST_generic.any

(* internals used in Parse_rule.ml *)
val parse_yaml_file : Common.filename -> string -> AST_generic.program
