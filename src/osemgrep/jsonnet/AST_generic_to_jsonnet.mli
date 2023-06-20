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
