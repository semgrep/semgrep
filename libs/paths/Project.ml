(*
   The type of project.

   A git project must work with the 'git' commands.

   A gitignore project is a fake git project. It doesn't have a valid
   '.git/' folder but its '.gitignore' files should be read as part
   of the semgrepignore mechanism.
*)
type kind = Git_project | Gitignore_project | Other_project [@@deriving show]

(* A project has a kind and a root path. It is better to use
 * a Real path here to canonicalize. In any case, you should not
 * use the project root rpath to build fpaths; you should use
 * a ppath associated with a scanning root fpath.
 *)
type t = { kind : kind; path : Rpath.t }
