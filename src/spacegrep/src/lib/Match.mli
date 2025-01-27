(*
   Match a parsed pattern against a parsed document.
*)

val debug : bool ref

(* See function 'make_search_param' *)
type search_param = {
  no_skip_search : bool;
  case_sensitive : bool;
  ellipsis_max_span : int;
}

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
   Create search parameters, overriding defaults if desired.

   no_skip_search: disable an optimization. Default is false.
   See --help for details.

   case_sensitive:
   By default, matching ascii words is case-sensitive. Set case_sensitive to
   false for case-insensitive matching on the ascii letters a-z/A-Z.
   This doesn't affect backreferences, e.g. the pattern '$A = $A'
   will match 'A = A' and 'a = a' as always, but not 'A = a', regardless
   of the case-sensitivity setting.

   ellipsis_max_span: maximum number of newlines an ellipsis can match.
   The default is 10. Setting it to 0 forces matched tokens to be all
   on the same line.
*)
val create_search_param :
  ?no_skip_search:bool ->
  ?case_sensitive:bool ->
  ?ellipsis_max_span:int ->
  unit ->
  search_param

val default_search_param : search_param

(*
   Match a pattern against a document. Return the list of all
   non-overlapping matches found by scanning the document from left to right.
*)
val search :
  search_param -> Src_file.t -> Pattern_AST.t -> Doc_AST.t -> match_ list

(* Same as 'search', but also returns the time it took. *)
val timed_search :
  search_param ->
  Src_file.t ->
  Pattern_AST.t ->
  Doc_AST.t ->
  match_ list * float
