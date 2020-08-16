(*
   AST for a pattern to be matched against a document.
*)

type atom =
  | Word of string (* ascii words [A-Za-z0-9_]+ *)
  | Punct of char (* ascii punctuation, including braces *)
  | Byte of char (* everything else, excluding ascii whitespace *)
  | Metavar of string

type node =
  | Atom of atom
  | List of node list
  | Dots

type t = node list

(*
   Ignore the special meaning of Dots and Metavars.
   This is intended for pretty-printing documents using the same printer
   as for patterns.
   See also Doc_AST.of_pattern which does the same thing but maps to
   a different type.
*)
let rec as_doc (pat : t) : t =
  match pat with
  | [] -> []
  | Atom atom :: pat ->
      (match atom with
       | Word s -> Atom (Word s) :: as_doc pat
       | Punct c -> Atom (Punct c) :: as_doc pat
       | Byte c -> Atom (Byte c) :: as_doc pat
       | Metavar s ->
           Atom (Punct '$') :: Atom (Word s)
           :: as_doc pat
      )
  | Dots :: pat ->
      Atom (Punct '.') :: Atom (Punct '.') :: Atom (Punct '.')
      :: as_doc pat
  | List pat1 :: pat2 -> List (as_doc pat1) :: as_doc pat2
