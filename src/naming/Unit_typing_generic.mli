val tests :
  (string (* filename *) -> AST_generic.program) ->
  (Lang.t -> string -> 'a) ->
  Alcotest_ext.test list
