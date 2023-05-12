(*
   Parse and interpret '.semgrepignore' files in addition to '.gitignore'
   files.

   The patterns they contain specify file paths to exclude from Semgrep scans.

   See the ml file for compatibility issues.
*)

(* Holds project root, and some cached data *)
type t
type exclusion_mechanism = Gitignore_and_semgrepignore | Only_semgrepignore

(*
   Initialize the data used to filter paths.
   The project_root path must exist. It is used to
   locate .gitignore and .semgrepignore files.

   This is an instanciation of Gitignore_filter.t specific to Semgrep.

   Use Git_project.find_project_root to determine the root of the
   git project.
*)
val create :
  ?include_patterns:
    (* TODO: ignore folders containing a '.git' file (in list_files only?)
       ?ignore_folder_if_contains:string list ->
    *)
    string list ->
  ?cli_patterns:string list ->
  exclusion_mechanism:exclusion_mechanism ->
  project_root:Fpath.t ->
  unit ->
  t

(*
   Pass a path to a file and determine whether it should be ignored for
   Semgrep scanning purposes.

   The input path must be either absolute with the root '/' designating
   the root of the git project or relative to the project root.

   The input path doesn't have to exist. Directories are identified by
   a trailing slash. It's important since some *ignore patterns only apply
   to directories.

   Use Git_project.find_project_root to determine the root of the
   git project and obtain valid git paths (paths relative to the project root).
*)
val select : t -> Ppath.t -> Gitignore.status * Gitignore.selection_event list

(* TODO: list project files without relying on 'git ls-files'.
      This will allow de-excluding some files using negated patterns in
      semgrepignore files.

   type list_result = {
     (* List of the files that were not ignored. *)
     selected_files: Fpath.t list;

     (* List of the files and folders that were ignored. Children of ignored
        folders aren't listed since we don't know about their existence. *)
     ignored_files:
      (Fpath.t * Gitignore_filter.status * Gitignore_syntax.selection_event list)
      list;
   }

   val list_files :
      ?include_folders:bool ->
      ?include_symlinks:bool ->
      t ->
      Git_path.t list ->
      list_result
*)
