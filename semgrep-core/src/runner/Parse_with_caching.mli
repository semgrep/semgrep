val parse_and_resolve_name :
  ?parsing_cache_dir:string (* "" means no parsing cache *) ->
  string (* semgrep or AST generic version *) ->
  Lang.t ->
  Common.filename ->
  AST_generic.program * Parse_info.token_location list
