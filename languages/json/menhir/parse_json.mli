(* AST_json.program is actually an alias to AST_js.expr *)
val parse_program : Fpath.t -> AST_json.program

(* for semgrep pattern parsing *)
val any_of_string : string -> AST_json.any

(* return a precise AST this time (see AST_json.value) *)
val parse : Fpath.t -> AST_json.value
