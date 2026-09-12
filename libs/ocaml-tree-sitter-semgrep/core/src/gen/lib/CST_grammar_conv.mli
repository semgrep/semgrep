(*
   Representation of a CST, as derived from a grammar definition.
   This representation is meant to be straightforward to translate to
   OCaml type definitions.

   A raw grammar definition as expressed in a grammar.json file is readable
   and writable using the Tree_sitter_grammar module.

   The representation offered in this file is an irreversible view on
   a grammar.json file. In particular:
   - precedence annotations are removed
   - nested anonymous rules such as nested sequences and nested alternatives
     are flattened whenever possible
   - various fields from the original grammar are ignored
   - there's room for future modifications

   TODO: clarify what we intend to do with this
*)

val of_tree_sitter : Tree_sitter_grammar.grammar -> CST_grammar.t

(* Sort and group the rules based on interdependencies. This is already
   done as part of 'of_tree_sitter'.
*)
val tsort_rules :
  CST_grammar.rule list -> CST_grammar.rule list list
