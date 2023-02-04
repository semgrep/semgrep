(*
   Implement the full gitignore syntax

   https://git-scm.com/docs/gitignore
*)

(* The location of a pattern, for logging and troubleshooting. *)
type loc = {
  (* File name or other source location name useful to a human reader
     in error messages. *)
  source_name : string;
  (* Line number, starting from 1. *)
  line_number : int;
  line_contents : string;
}

(*
   A ternary state indicating whether a path is selected or deselected at
   the end of a level:
   - Selected: select the file (= ignore it for git purposes)
   - Deselected: deselect the file (= don't ignore it for git purposes)
   - Unmatched: move on to the next level to determine the fate of the path.
*)
type selection_event = Selected of loc | Deselected of loc

(* Path selector. *)
type path_selector = {
  loc : loc;
  (* The matcher tells whether a given path matches the pattern.
     For example, the pattern /foo matches the path /

     The result is the list of selection events, most recent first.
     The head of the list alone determines whether the path is selected.
     An empty list indicates that the path was not selected. *)
  matcher : Fpath.t -> selection_event option;
}

type t = path_selector list

(* Parsing functions. They will raise exceptions if the input is malformed. *)
val from_string : ?name:string -> string -> t
val from_file : string -> t
