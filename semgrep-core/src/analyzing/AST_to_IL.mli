val function_definition :
  Lang.t -> AST_generic.function_definition -> IL.name list * IL.stmt list

val stmt : Lang.t -> AST_generic.stmt -> IL.stmt list

val name_of_entity : AST_generic.entity -> IL.name option
