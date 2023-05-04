(* main entry point *)
val parse_and_resolve_name :
  ?parsing_cache_dir:string (* "" means no parsing cache *) ->
  string (* semgrep or AST generic version *) ->
  Lang.t ->
  Fpath.t ->
  AST_generic.program * Tok.location list

(* We store the version and the original path as well as whether parsing the
 * file was generating an exn in the marshalled value on disk. That
 * way we can do extra check when we read it back (e.g., making sure
 * the version is the same than the current program, otherwise
 * we could get some segfaults (unmarshalling is not type-safe in OCaml)).
 *)
type versioned_parse_result =
  string (* AST_generic.version *)
  * Fpath.t (* original path *)
  * (AST_generic.program * Tok.location list, Exception.t) Common.either

(* to add as a suffix to a filename *)
val binary_suffix : Fpath.ext (* .ast.binary *)
val is_binary_ast_filename : Fpath.t -> bool

(* used by semgrep-core -generate_ast_binary *)
val versioned_parse_result_of_file :
  string (* semgrep or AST generic version *) ->
  Lang.t ->
  Fpath.t ->
  versioned_parse_result
