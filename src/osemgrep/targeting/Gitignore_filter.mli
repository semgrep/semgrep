(*
   Support for file tree filtering using the gitignore specification.

   This implements the full gitignore filtering specification, assuming
   the sources of gitignore patterns were already parsed and are provided
   as levels. The actual sources of gitignore patterns and levels
   are not specified here, allowing nonstandard sources such as
   .semgrepignore files.

   Specification: https://git-scm.com/docs/gitignore
*)

(* Any number of groups of path selectors. *)
type t

(* This should be clearer than a bool *)
type status = Not_ignored | Ignored

(*
   Create a gitignore filter meant to be reused to filter many target paths.

   gitignore_filenames: pairs (file kind, file name);
                        see Gitignore_files.create
*)
val create :
  ?gitignore_filenames:(string * string) list ->
  ?higher_priority_levels:Gitignore_level.t list ->
  ?lower_priority_levels:Gitignore_level.t list ->
  project_root:Fpath.t ->
  unit ->
  t

(*
   Examine a single absolute[1] path[2] and determine whether it is selected
   by the gitignore mechanism, i.e. ignored for git purposes.

   [1] The path must be absolute within the git project. For example,
   if the git project root is at /home/bob/fooproj, then
   the path to the file /home/bob/fooproj/bar
   must be given as /bar.

   [2] Paths to folders must have a trailing slash.

   Return whether the list of selection/deselection events that the path
   went through, in reverse order. The first element of the list, if any,
   determines whether the file is selected.
*)
val select :
  t ->
  Gitignore_syntax.selection_event list ->
  Ppath.t ->
  status * Gitignore_syntax.selection_event list
