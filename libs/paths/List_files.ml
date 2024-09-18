open Fpath_.Operators
module Log = Log_paths.Log

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(*
   List files recursively in a safe, efficient, and portable manner.

   Replaces the functions in libs/commons/ that use external UNIX commands
   such as 'find'.
*)

(*************************************************************************)
(* Helpers *)
(*************************************************************************)

let with_dir_handle path func =
  let dir = Unix.opendir !!path in
  Common.protect ~finally:(fun () -> Unix.closedir dir) (fun () -> func dir)

(* Read the names found in a directory, excluding "." and "..". *)
let read_dir_entries path =
  with_dir_handle path (fun dir ->
      let rec loop acc =
        try
          let name = Unix.readdir dir in
          let acc =
            if
              name = Filename.current_dir_name (* "." *)
              || name = Filename.parent_dir_name (* ".." *)
            then acc
            else name :: acc
          in
          loop acc
        with
        | End_of_file -> List.rev acc
      in
      loop [])

let read_dir_entries_fpath path = read_dir_entries path |> List_.map Fpath.v

let rec iter_dir_entries func dir names =
  List.iter (iter_dir_entry func dir) names

and iter_dir_entry func dir name =
  let path = Fpath.add_seg dir name in
  iter func path

(*************************************************************************)
(* Entry points *)
(*************************************************************************)

and iter func path =
  let stat =
    try Some (Unix.lstat !!path) with
    | Unix.Unix_error (_error_kind, _func, _info) ->
        (* Ignore all errors. Should we ignore less? *)
        None
  in
  match stat with
  | Some { Unix.st_kind = S_DIR; _ } -> iter_dir func path
  | Some stat (* regular file, symlink, etc. *) -> func path stat
  | None -> ()

and iter_dir func dir =
  let names = read_dir_entries dir in
  iter_dir_entries func dir names

let fold_left func init path =
  let acc = ref init in
  iter (fun path stat -> acc := func !acc path stat) path;
  !acc

let list_with_stat path =
  fold_left (fun acc path stat -> (path, stat) :: acc) [] path |> List.rev

let list path = list_with_stat path |> List_.map fst

(* python: Target.files_from_filesystem *)
let list_regular_files ?(keep_root = false) root_path =
  list_with_stat root_path
  |> List_.filter_map (fun (path, (stat : Unix.stats)) ->
         Log.debug (fun m -> m "root: %s path: %s" !!root_path !!path);
         if keep_root && path = root_path then Some path
         else
           match stat.st_kind with
           | Unix.S_REG -> Some path
           | _else_ -> None)
