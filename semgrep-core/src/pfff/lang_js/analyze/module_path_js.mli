val resolve_path :
  root:Common.dirname (* where to find node_modules *) ->
  pwd:Common.dirname (* pwd of importer *) ->
  string ->
  (* full path *)
  Common.filename option
