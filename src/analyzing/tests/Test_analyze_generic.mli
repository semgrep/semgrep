val actions :
  < Cap.exec ; Cap.tmp > ->
  parse_program:(Fpath.t -> AST_generic.program) ->
  Arg_.cmdline_actions
