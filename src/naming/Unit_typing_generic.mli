val tests :
  (string (* filename *) -> AST_generic.program) ->
  (Lang.t -> string -> 'a) ->
  Testutil.test list
