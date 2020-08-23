(*
   Match a parsed pattern against a parsed document.
*)

val debug : bool ref

(*
   Match a pattern against a document. Return the list of all
   non-overlapping matches found by scanning the document from left to right.
*)
val search : Pattern_AST.t -> Doc_AST.t -> (Loc.t * Loc.t) list

(*
   Print the matched lines to stdout in a human-readable format.
*)
val print :
  ?highlight:bool ->
  Src_file.t -> (Loc.t * Loc.t) list -> unit
