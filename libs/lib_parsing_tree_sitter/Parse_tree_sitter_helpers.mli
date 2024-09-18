type 'a env = {
  file : Fpath.t;
  (* Get the byte offset (0-based) in the source from a (line, column) pair.
     The line is 1-based, the column is 0-based.
     Raises Not_found.
     TODO: rename 'bytepos_of_line_col'
     TODO: return an option instead of raising an exception *)
  conv : int * int -> int;
  extra : 'a;
}

(* Take a file, return a lookup function that may raise Not_found.
   This is used to populate the field 'conv' above.
   TODO: rename 'bytepos_of_line_col' *)
val line_col_to_pos : Fpath.t -> int * int -> int

(* Take a string, return a lookup function that may raise Not_found.
   TODO: rename 'bytepos_of_line_col'
   TODO: explain what's special about patterns. It seems like it should
   work for any input string.
*)
val line_col_to_pos_pattern : string (* contents *) -> int * int -> int

(* Tree_sitter_run tokens to Tok.t converters *)
val token : 'a env -> Tree_sitter_run.Token.t -> Tok.t
val str : 'a env -> Tree_sitter_run.Token.t -> string * Tok.t
val debug_sexp_cst_after_error : Sexplib.Sexp.t -> unit

(*
   Call a tree-sitter parser and then map the CST into an AST
   with the user-provided function. Takes care of error handling but lets
   exceptions go through.

   Extras (e.g. comments, heredoc bodies, ...) must be injected into the AST
   at this stage.
*)
val wrap_parser :
  (unit -> ('cst, 'extra) Tree_sitter_run.Parsing_result.t) ->
  ('cst -> 'extra list -> 'ast) ->
  ('ast, unit) Tree_sitter_run.Parsing_result.t
