val tests :
  (Fpath.t -> AST_generic.program) ->
  (Lang.t -> string -> 'a) ->
  Testo.test list
