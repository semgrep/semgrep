val parse : Fpath.t -> (Ast_go.program, Parser_go.token) Parsing_result.t
val parse_program : Fpath.t -> Ast_go.program
val any_of_string : string -> Ast_go.any
val program_of_string : < Cap.tmp > -> string -> Ast_go.program
