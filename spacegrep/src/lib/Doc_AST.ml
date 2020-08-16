(*
   Type of our universal abstract syntax tree of a program.
*)

type atom =
  | Word of string (* ascii words [A-Za-z0-9_]+ *)
  | Punct of char (* ascii punctuation, including braces *)
  | Byte of char (* everything else, excluding ascii whitespace *)

type node =
  | Atom of atom
  | List of node list

type t = node list

(*
   For convenience of implementation, a document is parsed as a pattern.
   Here we convert the pattern-specific constructs to normal document
   elements.
*)
let rec of_pattern (pat : Pattern_AST.t) : t =
  match pat with
  | [] -> []
  | Atom atom :: pat ->
      (match atom with
       | Word s -> Atom (Word s) :: of_pattern pat
       | Punct c -> Atom (Punct c) :: of_pattern pat
       | Byte c -> Atom (Byte c) :: of_pattern pat
       | Dots ->
           Atom (Punct '.') :: Atom (Punct '.') :: Atom (Punct '.')
           :: of_pattern pat
       | Metavar s ->
           Atom (Punct '$') :: Atom (Word s)
           :: of_pattern pat
      )
  | List pat1 :: pat2 -> List (of_pattern pat1) :: of_pattern pat2
