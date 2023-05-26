(* Main entry point. Mostly a wrapper around
 * Parse_target.parse_and_resolve_name() but caching the parsed AST
 * in parsing_cache_dir if it is defined.
 *)
val parse_and_resolve_name :
  ?parsing_cache_dir:Fpath.t option ->
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
type ast_cached_value =
  ( (AST_generic.program * Tok.location list, Exception.t) Common.either,
    string (* AST_generic.version *)
    * Lang.t
    * Rpath.t (* original file *)
    * float (* mtime of the original file *) )
  Cache_disk.cached_value_on_disk

(* to add as a suffix to a filename *)
val binary_suffix : Fpath.ext (* .ast.binary *)
val is_binary_ast_filename : Fpath.t -> bool

(* used by semgrep-core -generate_ast_binary *)
val ast_cached_value_of_file :
  string (* semgrep or AST generic version *) ->
  Lang.t ->
  Fpath.t ->
  ast_cached_value
