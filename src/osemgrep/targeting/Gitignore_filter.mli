(*
   Support for file tree filtering using the gitignore specification.

   This implements the full gitignore filtering specification, assuming
   the sources of gitignore patterns were already parsed and are provided
   as levels. The actual sources of gitignore patterns and levels
   are not specified here, allowing nonstandard sources such as
   .semgrepignore files.

   Specification: https://git-scm.com/docs/gitignore
*)

(*
   A precedence level is a source of compiled gitignore patterns, such as a
   .gitignore file or the command line. Git has rules defining these
   precedence levels.
*)
type level = {
  (* Informational level kind *)
  level_kind : string;
  (* Informational text indicating where the gitignore patterns came from *)
  source_name : string;
  (* Sequence of path selectors derived from gitignore patterns *)
  patterns : Gitignore_syntax.path_selector list;
}

(* Any number of groups of path selectors. *)
type t

val create :
  ?gitignore_filenames:string list ->
  ?higher_priority_levels:level list ->
  ?lower_priority_levels:level list ->
  project_root:Fpath.t ->
  unit -> t

(*
   Examine a single absolute* path and determine whether it is selected by the
   gitignore mechanism, i.e. ignored for git purposes.

   *The path must be absolute within the git project. For example,
   if the git project root is at /home/bob/fooproj, then
   the path to the file /home/bob/fooproj/bar
   must be given as /bar.

   Return whether the list of selection/deselection events that the path
   went through, in reverse order. The first element of the list, if any,
   determines whether the file is selected.
*)
val select :
  t ->
  Gitignore_syntax.git_path ->
  bool * Gitignore_syntax.selection_event list
