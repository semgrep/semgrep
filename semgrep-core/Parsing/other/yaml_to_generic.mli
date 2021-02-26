type env = {
  str: string;
  file: Common.filename
}

val program: env -> AST_generic.program
val any: string -> AST_generic.any
