(*
   Parse_javascript_tree_sitter_priv is fully accessible by
   Parse_typescript_tree_sitter which reuses much of it.

   This module, Parse_javascript_tree_sitter, is the public interface
   for everyone else to use.
*)

let parse = Parse_javascript_tree_sitter_priv.parse
