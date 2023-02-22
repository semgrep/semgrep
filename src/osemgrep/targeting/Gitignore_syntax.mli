(*
   Implement the full gitignore syntax

   https://git-scm.com/docs/gitignore
*)

(*
   A ternary state indicating whether a path is selected or deselected at
   the end of a level:
   - Selected: select the file (= ignore it for git purposes)
   - Deselected: deselect the file (= don't ignore it for git purposes)
   - Unmatched: move on to the next level to determine the fate of the path.
*)
type selection_event =
  | Selected of Glob_matcher.loc
  | Deselected of Glob_matcher.loc

val show_selection_event : selection_event -> string

(* Show a list of selection events, one per line *)
val show_selection_events : selection_event list -> string

(* Path selector. *)
type path_selector = {
  loc : Glob_matcher.loc;
  (* The matcher tells whether a given path matches the pattern.
     For example, the pattern /foo matches the path /

     The result is the list of selection events, most recent first.
     The head of the list alone determines whether the path is selected.
     An empty list indicates that the path was not selected. *)
  matcher : Git_path.t -> selection_event option;
}

type t = path_selector list

(* Parsing functions. They will raise exceptions if the input is malformed.

   The anchor is the pattern that matches the path from the git project
   root to the work folder, typically the one containing the gitignore file.
*)
val from_string : anchor:Glob_matcher.pattern -> ?name:string -> string -> t
val from_file : anchor:Glob_matcher.pattern -> Fpath.t -> t
