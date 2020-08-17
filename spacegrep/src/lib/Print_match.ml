(*
   Print debugging information to help follow what's going on during
   matching.
*)

open Printf
open Pattern_AST
open Doc_AST

let rec show_pat_node (pat_node : Pattern_AST.node) =
  match pat_node with
  | Atom (Word s) -> sprintf "Word '%s'" (String.escaped s)
  | Atom (Punct c) -> sprintf "Punct %C" c
  | Atom (Byte c) -> sprintf "Byte 0x%02x" (Char.code c)
  | Atom (Metavar s) -> sprintf "Metavar %s" s
  | List _ -> "List"
  | Dots -> "Dots"

let show_doc_node (doc_node : Doc_AST.node) =
  match doc_node with
  | Atom (Word s) -> sprintf "Word '%s'" (String.escaped s)
  | Atom (Punct c) -> sprintf "Punct %C" c
  | Atom (Byte c) -> sprintf "Byte 0x%02x" (Char.code c)
  | List _ -> "List"

let show_pat pat =
  match pat with
  | [] -> "[]"
  | node :: _ -> show_pat_node node

let show_doc doc =
  match doc with
  | [] -> "[]"
  | node :: _ -> show_doc_node node

let print (pat : Pattern_AST.t) (doc : Doc_AST.t) =
  printf "pat %s : doc %s\n"
    (show_pat pat) (show_doc doc)
