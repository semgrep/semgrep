(*
   Parse and interpret '.semgrepignore' files in addition to '.gitignore'
   files.

   The patterns they contain specify file paths to exclude from Semgrep scans.

   See the ml file for compatibility issues.
*)

(*
   We have to support the legacy built-in semgrepignore patterns
   when scanning for source code but we want something different
   or empty when scanning for secrets.

   The 'Empty' case is useful for testing.

   TODO: combine the built-in semgrepignore files into one once
   we have an extended syntax with sections.
*)
type builtin_semgrepignore = Empty | Semgrep_scan_legacy
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
  ?cli_patterns:string list ->
  builtin_semgrepignore:builtin_semgrepignore ->
  exclusion_mechanism:exclusion_mechanism ->
  project_root:Fpath.t ->
  unit ->
  Gitignore.filter
