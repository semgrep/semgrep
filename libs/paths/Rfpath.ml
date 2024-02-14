(*
   Realpath + original user-friendly path
*)

type t = { fpath : Fpath.t; rpath : Rpath.t; cwd : Rpath.t }
[@@deriving show, eq]

let of_fpath fpath =
  { fpath; rpath = Rpath.of_fpath fpath; cwd = Rpath.getcwd () }

let of_string s = s |> Fpath.v |> of_fpath
let to_fpath x = x.fpath
let to_rpath x = x.rpath

let getcwd () =
  let cwd = Rpath.getcwd () in
  { fpath = Fpath.v "."; rpath = cwd; cwd }

let is_valid (x : t) =
  Fpath.is_rel x.fpath && String.equal (Sys.getcwd ()) (Rpath.to_string x.cwd)
