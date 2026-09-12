(*
   Add grammar rules to ensure that no pattern or token() remains anonymous.

   The problem is that tree-sitter won't return a node if the pattern
   doesn't have a name. A rule consisting only of a pattern such as

     foo: $ => /[a-z]+/,

   allows the token to receive a name, 'foo' in this example.

   This is applied to some token() constructs as well.
*)
val work_around_missing_nodes : Tree_sitter_grammar.grammar -> Tree_sitter_grammar.grammar

type token_node_name =
  | Literal of string
  | Name of string

(* Get the name of the token node as it will appear in the CST. *)
val get_token_node_name : Tree_sitter_grammar.Rule_body.t -> token_node_name option
