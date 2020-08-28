(*s: builtins_php.mli *)

val generate_php_stdlib: 
  src:Common.dirname (* dir with .idl.json files *) -> 
  (* phpmanual_dir:Common.dirname (* dir with php manual xml files *) ->  *)
  dest:Common.dirname (* e.g. data/php_stdlib/ *) ->
  unit

val actions: unit -> Common.cmdline_actions
(*x: builtins_php.mli *)
(*e: builtins_php.mli *)
