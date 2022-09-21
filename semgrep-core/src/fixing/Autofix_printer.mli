val print_ast :
  lang:Lang.t ->
  metavars:Metavariable.bindings ->
  target_contents:string Lazy.t ->
  fix_pattern_ast:AST_generic.any ->
  fix_pattern:string ->
  AST_generic.any ->
  string option
