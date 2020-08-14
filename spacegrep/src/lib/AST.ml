(*
   Type of our universal abstract syntax tree of a program.
*)

type atom =
  | Word of string (* ascii words [A-Za-z0-9_]+ *)
  | Punct of char (* ascii punctuation, including braces *)
  | Byte of char (* everything else, excluding ascii whitespace *)

type t =
  | Atom of atom
  | List of t list
