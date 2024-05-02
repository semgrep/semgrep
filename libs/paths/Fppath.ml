(*
   Path information for a file in a git project, which includes:
   - a path in the file system
   - the path relative to the project root
*)

type t = { fpath : Fpath.t; ppath : Ppath.t } [@@deriving show]

let compare a b = Fpath.compare a.fpath b.fpath
