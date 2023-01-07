val function_definition :
  Lang.t -> AST_generic.function_definition -> IL.name list * IL.stmt list

val stmt : Lang.t -> AST_generic.stmt -> IL.stmt list
val unique_name_of_entity : AST_generic.entity -> AST_generic.unique_name option
val var_of_id_info : AST_generic.ident -> AST_generic.id_info -> IL.name
