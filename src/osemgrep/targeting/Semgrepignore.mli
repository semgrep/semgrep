(*
   Parse and interpret '.semgrepignore' files in addition to '.gitignore'
   files.

   The patterns they contain specify file paths to exclude from Semgrep scans.

   See the ml file for compatibility issues.
*)

(* Holds project root, and some cached data *)
type t

(*
   Initialize the data used to filter paths.
   The project_root path must be absolute and must exist. It is used to
   locate .gitignore and .semgrepignore files.

   This is an instanciation of Gitignore_filter.t specific to Semgrep.
*)
val create :
  ?include_patterns:string list ->
  ?cli_patterns:string list ->
  project_root:Fpath.t ->
  unit ->
  t

(*
   Pass a path to a file and determine whether it should be ignored for
   Semgrep scanning purposes.
   Paths must be absolute. The root '/' designates the root of the git project.
   The input path doesn't have to exist. Directories are identified by
   a trailing slash. It's important since some *ignore patterns only apply
   to directories.
*)
val select :
  t ->
  Fpath.t ->
  Gitignore_filter.status * Gitignore_syntax.selection_event list
