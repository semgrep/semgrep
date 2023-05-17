type kind = Git_project | Other_project [@@deriving show]

(* a project has a kind and a root path *)
type t = kind * Fpath.t
