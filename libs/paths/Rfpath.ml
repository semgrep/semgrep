(*
   Realpath + original user-friendly path
*)

type t = { fpath : Fpath.t; rpath : Rpath.t; cwd : Rpath.t }
[@@deriving show, eq]

let of_fpath_exn fpath =
  let rpath = Rpath.of_fpath_exn fpath in
  { fpath; rpath; cwd = Rpath.getcwd () }

let of_string_exn path = of_fpath_exn (Fpath.v path)

let of_fpath fpath =
  match Rpath.of_fpath fpath with
  | Ok rpath -> Ok { fpath; rpath; cwd = Rpath.getcwd () }
  | Error _msg as err -> err

let of_string s = s |> Fpath.v |> of_fpath

let of_strings paths =
  paths
  |> List.partition_map (fun path ->
         match of_string path with
         | Ok rpath -> Left rpath
         | Error msg -> Right (path, msg))

let of_fpaths paths =
  paths
  |> List.partition_map (fun fpath ->
         match of_fpath fpath with
         | Ok rpath -> Left rpath
         | Error msg -> Right (fpath, msg))

let to_fpath x = x.fpath
let to_rpath x = x.rpath

let getcwd () =
  let cwd = Rpath.getcwd () in
  { fpath = Fpath.v "."; rpath = cwd; cwd }

let is_valid (x : t) =
  Fpath.is_rel x.fpath && String.equal (Sys.getcwd ()) (Rpath.to_string x.cwd)
