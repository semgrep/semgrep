type 'a env = {
  file : string;
  (* get the charpos (offset) in file given a (line, col) pair *)
  conv : int * int -> int;
  extra : 'a;
}

(* to fill in conv *)
val line_col_to_pos : string (* filename *) -> int * int -> int

(* to fill in conv for pattern parsing *)
val line_col_to_pos_str : string (* contents *) -> int * int -> int

(* Tree_sitter_run tokens to Tok.t converters *)
val token : 'a env -> Tree_sitter_run.Token.t -> Tok.t
val str : 'a env -> Tree_sitter_run.Token.t -> string * Tok.t
val debug_sexp_cst_after_error : Sexplib.Sexp.t -> unit

(*
   Call a tree-sitter parser and then map the CST into an AST
   with the user-provided function. Takes care of error handling but lets
   exceptions go through.
*)
val wrap_parser :
  (unit -> 'cst Tree_sitter_run.Parsing_result.t) ->
  ('cst -> 'ast) ->
  'ast Tree_sitter_run.Parsing_result.t
