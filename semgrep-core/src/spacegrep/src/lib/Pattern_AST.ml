(*
   AST for a pattern to be matched against a document.
*)

type atom =
  | Word of string (* ascii words [A-Za-z0-9_]+ *)
  | Punct of char (* ascii punctuation, including braces *)
  | Byte of char (* everything else, excluding ascii whitespace *)
  | Metavar of string
[@@deriving show { with_path = false }, eq]

type node =
  | Atom of Loc.t * atom
  | List of node list
  | Dots of Loc.t
  | End (* used to mark the end of the root pattern, so as to distinguish
           it from the end of a sub-pattern. *)
[@@deriving show { with_path = false }, eq]

type t = node list [@@deriving show, eq]

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
  | End :: pat -> as_doc pat
  | Atom (loc, atom) :: pat -> (
      match atom with
      | Word s -> Atom (loc, Word s) :: as_doc pat
      | Punct c -> Atom (loc, Punct c) :: as_doc pat
      | Byte c -> Atom (loc, Byte c) :: as_doc pat
      | Metavar s ->
          let start, end_ = loc in
          let word_loc = (Loc.Pos.shift start 1, end_) in
          Atom (Loc.sub loc 0 1, Punct '$')
          :: Atom (word_loc, Word s)
          :: as_doc pat )
  | Dots loc :: pat ->
      let pos0, pos3 = loc in
      let pos1 = Loc.Pos.shift pos0 1 in
      let pos2 = Loc.Pos.shift pos1 1 in
      let loc0 = (pos0, pos1) in
      let loc1 = (pos1, pos2) in
      let loc2 = (pos2, pos3) in
      Atom (loc0, Punct '.')
      :: Atom (loc1, Punct '.')
      :: Atom (loc2, Punct '.')
      :: as_doc pat
  | List pat1 :: pat2 -> List (as_doc pat1) :: as_doc pat2

(* Equality function that disregards location. Meant for unit tests. *)
let rec eq a b =
  match (a, b) with
  | [], [] -> true
  | a_head :: a_tail, b_head :: b_tail ->
      ( match (a_head, b_head) with
      | Atom (_, a), Atom (_, b) -> a = b
      | List a, List b -> eq a b
      | Dots _, Dots _ -> true
      | End, End -> true
      | _ -> false )
      && eq a_tail b_tail
  | _ -> false
