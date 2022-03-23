type 'a env = {
  file : Common.filename;
  (* get the charpos (offset) in file given a line x col *)
  conv : (int * int, int) Hashtbl.t;
  extra : 'a;
}

val line_col_to_pos : Common.filename -> (int * int, int) Hashtbl.t
val token : 'a env -> Tree_sitter_run.Token.t -> Parse_info.t
val str : 'a env -> Tree_sitter_run.Token.t -> string * Parse_info.t

val str_if_wrong_content_temporary_fix :
  'a env -> Tree_sitter_run.Token.t -> string * Parse_info.t

(* Use Parse_info.combine_infos instead *)
val combine_tokens_DEPRECATED :
  'a env -> Tree_sitter_run.Token.t list -> Parse_info.t

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

val parse_number_literal : string * Parse_info.t -> AST_generic.literal
