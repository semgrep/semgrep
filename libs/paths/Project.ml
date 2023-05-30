type kind = Git_project | Other_project [@@deriving show]

(* A project has a kind and a root path. It is better to use
 * a Real path here to canonicalize. In any case, you should not
 * use the project root rpath to build fpaths; you should use
 * a ppath associated with a scanning root fpath.
 *)
type t = kind * Rpath.t
