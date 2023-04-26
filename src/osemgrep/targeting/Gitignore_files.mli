(*
   Load .gitignore (or .semgrepignore) files
*)

(* Cache parsed gitignore files *)
type t

(* Initialize the cache for a project defined by the project root folder.

   gitignore_filenames is the list of pairs (file kind, file name)
   for gitignore files. The file kind is the conventional file kind
   chosen be the user e.g. "semgrepignore" for ".semgrepignore" files.
   The default is ["gitignore", ".gitignore"]. In Semgrep, we use
   [".gitignore"; ".semgrepignore"]. The order of the file names defines
   the order in which multiples file in the same folder are loaded,
   as if they were concatenated. Multiple files found in the same folder
   are treated as part of the same level, i.e. the fate of a file is unknown
   until the patterns of all the gitignore files in the folder were scanned.
*)
val create :
  ?gitignore_filenames:(string * string) list ->
  project_root:Fpath.t ->
  unit ->
  t

(*
   Load the .gitignore files applicable to target files in the given
   folder.
*)
val load : t -> Ppath.t -> Gitignore_level.t option
