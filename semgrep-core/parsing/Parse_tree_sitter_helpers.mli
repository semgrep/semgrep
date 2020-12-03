
type 'a env = {
  file: Common.filename;
  (* get the charpos (offset) in file given a line x col *)
  conv: (int * int, int) Hashtbl.t;
  extra: 'a;
}

type 'ast result = 'ast option * Tree_sitter_run.Tree_sitter_error.t list

val line_col_to_pos: Common.filename -> (int * int, int) Hashtbl.t

val token: 'a env -> Tree_sitter_run.Token.t -> Parse_info.t

val str: 'a env -> Tree_sitter_run.Token.t -> string * Parse_info.t

val combine_tokens: 'a env -> Tree_sitter_run.Token.t list -> Parse_info.t

(*
   Call a tree-sitter parser and then map the CST into an AST
   with the user-provided function. Takes care of error handling.
*)
val wrap_parser :
  (unit -> 'cst option * Tree_sitter_run.Tree_sitter_error.t list) ->
  ('cst -> 'ast) ->
  'ast result
