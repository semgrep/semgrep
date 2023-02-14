(*
   Load and cache .gitignore (or .semgrepignore) files
*)

type t = {
  project_root: Fpath.t;
  gitignore_filenames: string list;
  cache: (Fpath.t, Gitignore_level.t list) Hashtbl.t;
}
