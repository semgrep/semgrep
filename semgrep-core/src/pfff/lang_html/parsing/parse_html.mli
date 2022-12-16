
type program_and_tokens = Ast_html.html_tree * Parser_html.token list

(* This is the main function *)
val parse:
  Common.filename -> program_and_tokens

val html_tree_of_string: string -> Ast_html.html_tree

(* using ocamlnet/netstring/nethtml.ml parser *)
(* val parse_simple_tree: Ast_html.html_raw -> Ast_html.html_tree2 *)
