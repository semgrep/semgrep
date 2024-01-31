(*
   Various utilities to deal with git projects.
*)

(*
   The result of searching for the project root from the filesystem path
   of a scanning root.
*)
type scanning_root_info = {
  (* Physical, absolute path to the project root in the local filesystem
     + original path to be preferred in messages to the user. *)
  project_root : Rfpath.t;
  (* Path of a Semgrep scanning root express within the project, relative to
     the project root. *)
  inproject_path : Ppath.t;
}

(*
   Find a project root even if the given path is not within a git project.

   'force_root' can be used to impose the project root rather than guessing
   it. It's useful for testing the gitignore mechanism and stay within
   the confines of the test folder.

   This returns the project root and the path relative to that root just like
   'find_git_project_root'.

   If a git project root is not found, the project kind is 'Other_project'
   and the project root is what 'fallback_project_root' specifies.

   To keep things simple, the default value of 'fallback_project_root' is
   set to the current folder '.', resolved to a physical path.
*)
val find_any_project_root :
  ?fallback_root:Rfpath.t ->
  ?force_root:Project.kind * Rfpath.t ->
  Rfpath.t ->
  Project.kind * scanning_root_info

(*
   Provide a similar result as 'find_git_project_root' but don't look
   for a git project root. Instead, use the project root provided
   by 'project_root' which defaults to the current directory.
*)
val force_project_root :
  ?project_root:Rfpath.t -> Rfpath.t -> scanning_root_info
