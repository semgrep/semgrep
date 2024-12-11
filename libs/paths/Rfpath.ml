(*
   Realpath + original user-friendly path
*)

module Log = Log_paths.Log

type t = { fpath : Fpath.t; rpath : Rpath.t; cwd : Rpath.t }
[@@deriving show, eq]

let create ~fpath ~rpath =
  let res = { fpath; rpath; cwd = Rpath.getcwd () } in
  Log.debug (fun m ->
      m "[Unix.getcwd()=%s] new rfpath: %s" (Unix.getcwd ()) (show res));
  res

let of_fpath_exn fpath =
  let rpath = Rpath.of_fpath_exn fpath in
  create ~fpath ~rpath

let of_string_exn path = of_fpath_exn (Fpath.v path)

let of_fpath fpath =
  match Rpath.of_fpath fpath with
  | Ok rpath -> Ok (create ~fpath ~rpath)
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

let log_missing_path path msg =
  Log.warn (fun m -> m "Cannot obtain realpath for %S: %s" path msg)

let of_strings_with_warnings paths =
  let res, missing = of_strings paths in
  missing |> List.iter (fun (path, msg) -> log_missing_path path msg);
  res

let of_fpaths_with_warnings fpaths =
  let res, missing = of_fpaths fpaths in
  missing
  |> List.iter (fun (fpath, msg) ->
         log_missing_path (Fpath.to_string fpath) msg);
  res

let to_fpath x = x.fpath
let to_rpath x = x.rpath
let getcwd () = create ~fpath:(Fpath.v ".") ~rpath:(Rpath.getcwd ())

let is_valid (x : t) =
  Fpath.is_rel x.fpath && String.equal (Sys.getcwd ()) (Rpath.to_string x.cwd)

let parent x =
  let rparent =
    match Rpath.parent x.rpath with
    | None -> x.rpath
    | Some x -> x
  in
  if UFile.is_lnk x.fpath then
    (* The fpath becomes an ugly physical path *)
    { fpath = Rpath.to_fpath rparent; rpath = rparent; cwd = x.cwd }
  else
    (* dangerous!
       This is only correct if fpath is not a symlink. However, it doesn't have
       to be a physical path. *)
    { fpath = Fpath.parent x.fpath; rpath = rparent; cwd = x.cwd }
