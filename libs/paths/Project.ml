(*
   The type of project.

   A git project must work with the 'git' commands.

   A gitignore project is a fake git project. It doesn't have a valid
   '.git/' folder but its '.gitignore' files should be read as part
   of the semgrepignore mechanism.
*)
type kind = Git_project | Gitignore_project | Other_project [@@deriving show]

(* A project has a kind and a root path. *)
type t = { kind : kind; path : Rfpath.t } [@@deriving show]
