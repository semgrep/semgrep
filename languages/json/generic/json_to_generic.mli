(* The unescape_strings is used when we use Json_to_generic to parse
 * a semgrep rule written in JSON (instead of YAML) in which case
 * we need to do the same thing that Yojson does and unescape strings.
 *)
val program : ?unescape_strings:bool -> Ast_json.program -> AST_generic.program
val any : Ast_json.any -> AST_generic.any
