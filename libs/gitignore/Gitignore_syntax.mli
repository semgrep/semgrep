(*
   Implement the full gitignore syntax

   https://git-scm.com/docs/gitignore

   This is mostly the syntax supported by libs/globbing/ with the addition
   of negator (!) at the beginning of a line to "deselect" a file.
*)

(* content of a .gitignore *)
type t = path_selector list

and path_selector = {
  loc : Glob.Match.loc;
  (* The matcher tells whether a given path matches the pattern.
     For example, the pattern '/f*' matches the path '/foo'.

     The result comes with a selection event in case of a match. *)
  matcher : Ppath.t -> selection_event option;
}

(*
   An event representing where a pattern matched a path. The sequence
   of selection events forms a trace that's useful to understand why
   particular path was gitignored (selected) or not gitignored (deselected).

   - Selected: select the file (= ignore it for git purposes)
     e.g. a file selected via 'generated/*.c'
   - Deselected: deselect the file (= don't ignore it for git purposes)
     e.g. a file deselected via '!generated/main.c'
*)
and selection_event =
  | Selected of Glob.Match.loc
  | Deselected of Glob.Match.loc

val show_selection_event : selection_event -> string

(*
   Show a trace of selection events, one per line, oldest first
   (unlike the list which has the most recent first).
*)
val show_selection_events : selection_event list -> string

(* Parsing functions. They will raise exceptions if the input is malformed.

   The anchor is the pattern that matches the path from the git project
   root to the work folder, typically the one containing the gitignore file.

   The default selection mode is Ignore.
*)
val from_string :
  anchor:Glob.Pattern.t -> name:string -> kind:string -> string -> t

val from_file : anchor:Glob.Pattern.t -> kind:string -> Fpath.t -> t

(* Internals.
   Remove the leading exclamation mark from the string, returning
   'Some new_string' if successful, 'None' otherwise.
*)
val remove_negator : string -> string option

(* Lower-level function that can be used to create custom matchers that
   combine multiple patterns. *)
val parse_pattern :
  source:Glob.Match.loc ->
  anchor:Glob.Pattern.t ->
  string ->
  Glob.Match.compiled_pattern
