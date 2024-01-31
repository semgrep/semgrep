(*
   Realpath + original user-friendly path
*)

type t = { fpath : Fpath.t; rpath : Rpath.t } [@@deriving show, eq]

let of_fpath fpath = { fpath; rpath = Rpath.of_fpath fpath }
let of_string s = s |> Fpath.v |> of_fpath
let to_fpath x = x.fpath
let to_rpath x = x.rpath
let getcwd () = { fpath = Fpath.v "."; rpath = Rpath.getcwd () }
