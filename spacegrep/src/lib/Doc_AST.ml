(*
   Type of our universal abstract syntax tree of a program.
*)

type atom =
  | Word of string (* ascii words [A-Za-z0-9_]+ *)
  | Punct of char (* ascii punctuation, including braces *)
  | Byte of char (* everything else, excluding ascii whitespace *)

type node =
  | Atom of Loc.t * atom
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
  | Atom (loc, atom) :: pat ->
      (match atom with
       | Word s -> Atom (loc, Word s) :: of_pattern pat
       | Punct c -> Atom (loc, Punct c) :: of_pattern pat
       | Byte c -> Atom (loc, Byte c) :: of_pattern pat
       | Metavar s ->
           let (start, end_) = loc in
           let word_loc = (Loc.Pos.shift start 1, end_) in
           Atom (Loc.sub loc 0 1, Punct '$') :: Atom (word_loc, Word s)
           :: of_pattern pat
      )
  | Dots loc :: pat ->
      let pos0, pos3 = loc in
      let pos1 = Loc.Pos.shift pos0 1 in
      let pos2 = Loc.Pos.shift pos1 1 in
      let loc0 = pos0, pos1 in
      let loc1 = pos1, pos2 in
      let loc2 = pos2, pos3 in
      Atom (loc0, Punct '.')
      :: Atom (loc1, Punct '.')
      :: Atom (loc2, Punct '.')
      :: of_pattern pat
  | List pat1 :: pat2 -> List (of_pattern pat1) :: of_pattern pat2
