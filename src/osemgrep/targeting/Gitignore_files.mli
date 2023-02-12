(*
   Load .gitignore (or .semgrepignore) files
*)

(* Cache parsed gitignore files *)
type t

(* Initialize the cache. The project root must be given as an absolute path.

   gitignore_filenames is the list of file names for gitignore files.
   The default is [".gitignore"]. In Semgrep, we use
   [".gitignore"; ".semgrepignore"]. The order of the file names defines
   the order in which multiples file in the same folder are loaded,
   as if they were concatenated. Multiple files found in the same folder
   are treated as part of the same level, i.e. the fate of a file is unknown
   until the patterns of all the gitignore files in the folder were scanned.
*)
val create :
  ?gitignore_filenames:string list ->
  project_root:Fpath.t ->
  unit -> t

(*
   Load the .gitignore files applicable to a given target file in the given
   folder.
*)
val load :
  Fpath.t ->
  t ->
  level
