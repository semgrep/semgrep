
type env = {
    file: Common.filename;
    (* get the charpos (offset) in file given a line x col *)
    conv: (int * int, int) Hashtbl.t;
}

val line_col_to_pos: Common.filename -> (int * int, int) Hashtbl.t

val token: env -> Tree_sitter_run.Token.t -> Parse_info.t

val str: env -> Tree_sitter_run.Token.t -> string * Parse_info.t
