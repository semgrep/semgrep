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
   A precedence level is a source of gitignore patterns, such as a
   .gitignore file or the command line. Git has rules defining these
   precedence levels.
*)
type level = {
  (* Informational level kind *)
  level_kind : string;
  (* Informational text indicating where the gitignore patterns came from *)
  source_name : string;
  (* Sequence of path selectors derived from gitignore patterns *)
  patterns : Gitignore_syntax.pattern list;
}

(* Any number of groups of path selectors. *)
type t = level list

(*
   A ternary state indicating whether a path is selected or deselected at
   the end of a level:
   - Selected: select the file (= ignore it for git purposes)
   - Deselected: deselect the file (= don't ignore it for git purposes)
   - Unmatched: move on to the next level to determine the fate of the path.
*)
type selection_result = Selected | Deselected | Unmatched

val select : t -> path -> selection_result
