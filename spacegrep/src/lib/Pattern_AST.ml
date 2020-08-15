(*
   AST for a pattern to be matched against a document.
*)

type node =
  | Word of string
  | Punct of char
  | Byte of char
  | Metavar of string
  | Dots

(*
   A pattern is a flat sequence of tokens.
   Indentation in the input pattern is ignored.
*)
type t = node list
