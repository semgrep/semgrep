(* Manifest a Jsonnet value to a generic AST program so we can
 * then parse this jsonnet rule in Parse_rule.ml. This is similar
 * to what we do to parse a YAML or JSON rule; in both cases
 * we first parse and create a generic AST before calling
 * Parse_rule.parse_generic_ast to finally get a Rule.t
 *)
val manifest_value : Value_jsonnet.t -> AST_generic.program
