val blist_as_expression : AST_bash.blist -> AST_bash.expression option

val add_redirects_to_last_command_of_pipeline :
  AST_bash.pipeline -> AST_bash.redirect list -> AST_bash.pipeline

val concat_blists : AST_bash.blist list -> AST_bash.blist
