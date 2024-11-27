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

(* Lower-level function that can be used to create custom matchers that
   combine multiple patterns. *)
val parse_pattern :
  source:Glob.Match.loc ->
  anchor:Glob.Pattern.t ->
  string ->
  Glob.Match.compiled_pattern
