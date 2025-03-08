val parse : Fpath.t -> AST_yaml.document
val any : string -> AST_yaml.any

(* internals used by yaml_to_generic.ml *)
val parse_yaml_file : is_target:bool -> Fpath.t -> string -> AST_yaml.document
