(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*
   Content of a .gitignore represented directly as matching functions.
*)
type path_selectors = path_selector list

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

and selection_events = selection_event list

(*
   A level as defined in the gitignore specification.

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
  patterns : path_selectors;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Remove the leading exclamation mark from the string, returning
   'Some new_string' if successful, 'None' otherwise.
   Used in Parse_gitignore but also in Include_filter
*)
let remove_negator (str : string) : string option =
  if String.length str >= 1 && str.[0] = '!' then Some (Str.string_after str 1)
  else None

(*****************************************************************************)
(* Dumpers *)
(*****************************************************************************)

let show_selection_event (x : selection_event) : string =
  match x with
  | Selected loc -> Printf.sprintf "ignored at %s" (Glob.Match.show_loc loc)
  | Deselected loc ->
      Printf.sprintf "de-ignored at %s" (Glob.Match.show_loc loc)

(*
   Show a trace of selection events, one per line, oldest first
   (unlike the list which has the most recent first).
*)
let show_selection_events (xs : selection_event list) : string =
  List.rev xs
  |> Common.map (fun x -> show_selection_event x ^ "\n")
  |> String.concat ""
