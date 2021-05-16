type 'a env = {
  file : Common.filename;
  (* get the charpos (offset) in file given a line x col *)
  conv : (int * int, int) Hashtbl.t;
  extra : 'a;
}

val line_col_to_pos : Common.filename -> (int * int, int) Hashtbl.t

val token : 'a env -> Tree_sitter_run.Token.t -> Parse_info.t

val str : 'a env -> Tree_sitter_run.Token.t -> string * Parse_info.t

(* Use Parse_info.combine_infos instead *)
val combine_tokens_DEPRECATED :
  'a env -> Tree_sitter_run.Token.t list -> Parse_info.t

(* like int_of_string_opt, but also converts C octals like 0400 in
 * the right value. *)
val int_of_string_c_octal_opt : string -> int option

(*
   Call a tree-sitter parser and then map the CST into an AST
   with the user-provided function. Takes care of error handling.
*)
val wrap_parser :
  (unit -> 'cst Tree_sitter_run.Parsing_result.t) ->
  ('cst -> 'ast) ->
  'ast Tree_sitter_run.Parsing_result.t
