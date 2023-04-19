open Ast_fuzzy

type 'tok hooks = { kind : 'tok -> Parse_info.token_kind; tokf : 'tok -> Tok.t }

exception Unclosed of string (* msg *) * Tok.t (* starting point *)

val mk_trees : 'tok hooks -> 'tok list -> trees
val mk_tokens : 'tok hooks -> 'tok list -> (Parse_info.token_kind * Tok.t) list
(* visitors, mappers, extractors, abstractors *)

type visitor_out = trees -> unit

type visitor_in = {
  ktree : (tree -> unit) * visitor_out -> tree -> unit;
  ktrees : (trees -> unit) * visitor_out -> trees -> unit;
  ktok : (tok -> unit) * visitor_out -> tok -> unit;
}

val default_visitor : visitor_in
val mk_visitor : visitor_in -> visitor_out
val toks_of_trees : trees -> tok list
val abstract_position_trees : trees -> trees
