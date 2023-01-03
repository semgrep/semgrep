val build :
  ?verbose:bool ->
  (* for builtins_java.ml, tags_java.ml *)
  ?only_defs:bool ->
  Common.path ->
  Common.filename list ->
  Graph_code.t
