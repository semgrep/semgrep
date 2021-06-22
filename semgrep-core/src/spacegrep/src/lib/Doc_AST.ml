(*
   Type of our universal abstract syntax tree of a program.
*)

type atom =
  | Word of string (* ascii words [A-Za-z0-9_]+ *)
  | Punct of char (* ascii punctuation, including braces *)
  | Byte of char (* everything else, excluding ascii whitespace *)
[@@deriving show { with_path = false }]

type node = Atom of Loc.t * atom | List of node list
[@@deriving show { with_path = false }]

type t = node list [@@deriving show]

(*
   For convenience of implementation, a document is parsed as a pattern.
   Here we convert the pattern-specific constructs to normal document
   elements.

   This function is made tail-recursive to avoid stack overflows on
   large files.
*)
let rec of_pattern (pat : Pattern_AST.t) : t =
  List.fold_left of_pattern_node [] pat |> List.rev

and of_pattern_node acc pat_node =
  match pat_node with
  | End -> acc
  | Atom (loc, atom) -> (
      match atom with
      | Word s -> Atom (loc, Word s) :: acc
      | Punct c -> Atom (loc, Punct c) :: acc
      | Byte c -> Atom (loc, Byte c) :: acc
      | Metavar s ->
          let start, end_ = loc in
          let word_loc = (Loc.Pos.shift start 1, end_) in
          Atom (word_loc, Word s) :: Atom (Loc.sub loc 0 1, Punct '$') :: acc )
  | Dots (loc, None) ->
      (* ... *)
      let pos0, pos3 = loc in
      let pos1 = Loc.Pos.shift pos0 1 in
      let pos2 = Loc.Pos.shift pos1 1 in
      let loc0 = (pos0, pos1) in
      let loc1 = (pos1, pos2) in
      let loc2 = (pos2, pos3) in
      Atom (loc2, Punct '.')
      :: Atom (loc1, Punct '.')
      :: Atom (loc0, Punct '.')
      :: acc
  | Dots (loc, Some s) ->
      (* $...MVAR *)
      let pos0, pos5 = loc in
      let pos1 = Loc.Pos.shift pos0 1 in
      let pos2 = Loc.Pos.shift pos1 1 in
      let pos3 = Loc.Pos.shift pos2 1 in
      let pos4 = Loc.Pos.shift pos3 1 in
      let loc0 = (pos0, pos1) in
      let loc1 = (pos1, pos2) in
      let loc2 = (pos2, pos3) in
      let loc3 = (pos3, pos4) in
      let loc4 = (pos4, pos5) in
      Atom (loc4, Word s)
      :: Atom (loc3, Punct '.')
      :: Atom (loc2, Punct '.')
      :: Atom (loc1, Punct '.')
      :: Atom (loc0, Punct '$')
      :: acc
  | List pat -> List (of_pattern pat) :: acc

let rec to_pattern (doc : t) : Pattern_AST.t = List.map to_pat_node doc

and to_pat_node (node : node) : Pattern_AST.node =
  match node with
  | Atom (loc, atom) -> Atom (loc, to_pat_atom atom)
  | List nodes -> List (to_pattern nodes)

and to_pat_atom (atom : atom) : Pattern_AST.atom =
  match atom with Word s -> Word s | Punct c -> Punct c | Byte c -> Byte c

(* Equality function that disregards location. Meant for unit tests. *)
let eq a b = Pattern_AST.eq (to_pattern a) (to_pattern b)
