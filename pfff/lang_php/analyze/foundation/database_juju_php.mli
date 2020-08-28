
type database

val juju_db_of_files: 
  ?show_progress:bool ->
  Common.filename list -> database

val code_database_of_juju_db:
  database -> Env_interpreter_php.code_database
