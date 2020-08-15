(*
   Match a parsed pattern against a parsed document.
*)

open AST
open Pattern_AST

(*
   Match a sequential pattern against a document tree.
   A "List" node in the document represents a branch in the document.
   It can be either skipped or followed.
*)
let rec match_ (pat : Pattern_AST.node list ) (doc : AST.node list) =
  match pat with
  | [] -> true
  | Dots :: _ -> failwith "support for '...' was not implemented"
  | Metavar _ :: _ -> failwith "support for '$VAR' was not implemented"
  | p :: next_pat ->
      match doc with
      | [] -> false
      | List doc_branch1 :: doc_branch2 ->
          match_ pat doc_branch1 || match_ pat doc_branch2
      | Atom d :: doc ->
          match p, d with
          | Word a, Word b when a = b -> match_ next_pat doc
          | Punct a, Punct b when a = b -> match_ next_pat doc
          | Byte a, Byte b when a = b -> match_ next_pat doc
          | _ -> false
