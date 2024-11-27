(*
   Main data structures to support gitignore file filtering.

   Specification: https://git-scm.com/docs/gitignore
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*
   Content of a .gitignore, represented directly as matching (selecting)
   functions. A .gitignore contains a list of patterns, where
   each pattern is essentially a "path selector".

   See also Parse_gitignore.from_file().
*)
type path_selectors = path_selector list

and path_selector = {
  loc : Glob.Match.loc;
  (* The matcher tells whether a given path matches the pattern.
     For example, the pattern '/f*' matches the path '/foo'.

     The result comes with a selection event in case of a match.
     Note the use of Ppath below, not Fpath.
  *)
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
  (* Informational level kind (ex: ??) *)
  level_kind : string;
  (* Informational text indicating where the gitignore patterns came from
   * (ex: ??) *)
  source_name : string;
  (* Sequence of path selectors derived from gitignore patterns *)
  patterns : path_selectors;
}

(* The legacy semgrepignore format adds ':include' directives. *)
type format = Gitignore | Legacy_semgrepignore

(* 'gitignore_filename' exists because we want to support reading gitignore
   patterns from files other than '.gitignore' such as '.semgrepignore'.

   The order of the file names defines
   the order in which multiples file in the same folder are loaded,
   as if they were concatenated. Multiple files found in the same folder
   are treated as part of the same level, i.e. the fate of a file is unknown
   until the patterns of all the gitignore files in the folder were scanned.
*)
type gitignore_filename = {
  (* The file kind is the name for the conventional file kind
     chosen by the user (e.g. "semgrepignore" for ".semgrepignore" files).
     This is used in error messages and such.
  *)
  source_kind : string;
  (* The filename is e.g. '.gitignore' or '.semgrepignore'.
  *)
  filename : string;
  (* Specify if the format is strict Gitignore or some extension. *)
  format : format;
}

let default_gitignore_filename =
  { source_kind = "gitignore"; filename = ".gitignore"; format = Gitignore }

(*
   A project usually contains a single toplevel .gitignore, but each subdir
   of this project can also contain a "refining" .gitignore that
   takes precedence. Here we cache all those .gitignore. The cache
   also support non-standard ignore files (e.g., .semgrepignore).

   See also Gitignores_cache.create() and Gitignores_cache.load()
*)
type gitignores_cache = {
  project_root : Fpath.t;
  (* List of gitignore-like files to look for in each of the project's
     source folders. *)
  gitignore_filenames : gitignore_filename list;
  (* TODO? why we use a cache? Why not loading all those .gitiginore at once?*)
  cache : (string, level option) Hashtbl.t;
}

(*
   This represents the full gitignore filtering inputs, assuming
   the sources of gitignore patterns were already parsed and are provided
   as levels. The actual sources of gitignore patterns and levels
   are not specified here, allowing nonstandard sources (e.g.,
   .semgrepignore files).

   See also Gitignore_filter.create() and Gitignore_filter.select().
*)
type filter = {
  project_root : Fpath.t;
  higher_priority_levels : level list;
  gitignore_file_cache : gitignores_cache;
  lower_priority_levels : level list;
}

(* Final result of a gitignore filter (clearer than a bool) *)
type status = Not_ignored | Ignored [@@deriving show]

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
  |> List_.map (fun x -> show_selection_event x ^ "\n")
  |> String.concat ""
