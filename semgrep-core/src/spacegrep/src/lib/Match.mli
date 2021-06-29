(*
   Match a parsed pattern against a parsed document.
*)

val debug : bool ref

(*
  Unique identifier for a pattern among all the patterns specified
  on the command line.
*)
type pattern_id = int

(*
   A pair of locations (loc1, loc2)
     = ((pos1_start, pos1_end), (pos2_start, pos2_end))
   which is normally a pair of tokens.

   A region can be converted to a location by taking the first and last
   positions (pos1_start, pos2_end).
*)
type region = Loc.t * Loc.t

(*
  The name of a subpattern and the locations allowing its extraction
  from the source document.
*)
type capture = {
  value : string;
  (* source text, substring of the original document. *)
  loc : Loc.t;
}

(*
   A match between a pattern and a region of an input file.
   The captures are where some named subpatterns match.
*)
type match_ = {
  region : region;
  capture : capture;
  named_captures : (string * capture) list;
}

val timef : (unit -> 'a) -> 'a * float

(*
   Match a pattern against a document. Return the list of all
   non-overlapping matches found by scanning the document from left to right.

   By default, matching ascii words is case-sensitive. Set case_sensitive to
   false for case-insensitive matching on the ascii letters a-z/A-Z.
   This doesn't affect backreferences, e.g. the pattern '$A = $A'
   will match 'A = A' and 'a = a' as always, but not 'A = a', regardless
   of the case-sensitivity setting.
*)
val search :
  ?case_sensitive:bool ->
  Src_file.t ->
  Pattern_AST.t ->
  Doc_AST.t ->
  match_ list

(* Same as 'search', but also returns the time it took. *)
val timed_search :
  ?case_sensitive:bool ->
  Src_file.t ->
  Pattern_AST.t ->
  Doc_AST.t ->
  match_ list * float

(*
   Print the matched lines to stdout in a human-readable format.
*)
val print :
  ?highlight:bool ->
  ?print_optional_separator:(unit -> unit) ->
  Src_file.t ->
  match_ list ->
  unit

(*
   Print the results of matching multiple patterns against multiple documents.
*)
val print_nested_results :
  ?highlight:bool ->
  ?print_optional_separator:(unit -> unit) ->
  (Src_file.t * (pattern_id * match_ list * float) list * float * float) list ->
  (Src_file.t * Parse_pattern.error) list ->
  unit
