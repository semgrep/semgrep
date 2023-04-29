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

(*
   A project usually contains a toplevel .gitignore, but each subdir
   of this project can also contain a "refining" .gitignore that
   takes precedence.
   Here we cache all those .gitignore.
   TODO? why we use a cache? Why not loading all those .gitiginore at once?
*)
type gitignores_cache = {
  project_root : Fpath.t;
  (* gitignore_filenames is the list of pairs (file kind, file name)
     for gitignore files. The file kind is the conventional file kind
     chosen be the user e.g. "semgrepignore" for ".semgrepignore" files.
     The default is ["gitignore", ".gitignore"]. In Semgrep, we use
     [".gitignore"; ".semgrepignore"]. The order of the file names defines
     the order in which multiples file in the same folder are loaded,
     as if they were concatenated. Multiple files found in the same folder
     are treated as part of the same level, i.e. the fate of a file is unknown
     until the patterns of all the gitignore files in the folder were scanned.
  *)
  gitignore_filenames : (string (* kind *) * string (* file name *)) list;
  cache : (string, level option) Hashtbl.t;
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
