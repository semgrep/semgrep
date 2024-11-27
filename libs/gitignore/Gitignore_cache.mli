(* Initialize the cache for a project defined by the project root folder.
   See the doc in Gitignore.ml about gitignore_filenames for more information
   on the ?gitignore_filenames parameter below.
*)
val create :
  ?gitignore_filenames:Gitignore.gitignore_filename list ->
  project_root:Fpath.t ->
  unit ->
  Gitignore.gitignores_cache

(*
   Load (or get it back from the cache) the .gitignore files applicable to
   target files in the given folder.
*)
val load :
  Gitignore.gitignores_cache ->
  Ppath.t (* directory *) ->
  Gitignore.level option
