(* Parsing functions. They will raise exceptions if the input is malformed.

   The anchor is the pattern that matches the path from the git project
   root to the work folder, typically the one containing the gitignore file.

   The default selection mode is Ignore.
*)

val from_file :
  anchor:Glob.Pattern.t ->
  format:Gitignore.format ->
  source_kind:string ->
  Fpath.t ->
  Gitignore.path_selectors

val from_string :
  anchor:Glob.Pattern.t ->
  name:string ->
  source_kind:string ->
  string ->
  Gitignore.path_selectors

type parse_pattern_result = {
  compiled_pattern : Glob.Match.compiled_pattern;
  is_affected_by_middle_slash_option : bool;
}

(* Lower-level function that can be used to create custom matchers that
   combine multiple patterns.

   middle_slash_anchors_left: true by default as per the Gitignore spec.

   right_anchored: true by default. If false, '/**' will be appended to
   the pattern to match subpaths.
*)
val parse_pattern :
  ?middle_slash_anchors_left:bool ->
  ?right_anchored:bool ->
  source:Glob.Match.loc ->
  left_anchor:Glob.Pattern.t ->
  string ->
  parse_pattern_result
