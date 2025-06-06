open Fpath_.Operators

let readdir (_caps : Cap.FS.readdir) handle = Unix.readdir handle |> Fpath.v

(* helpers *)

let with_dir_handle (path : Fpath.t) func =
  let dir = UUnix.opendir !!path in
  Common.protect ~finally:(fun () -> UUnix.closedir dir) (fun () -> func dir)

(* Read the names found in a directory, excluding "." and "..". *)
let read_dir_entries (caps : < Cap.readdir ; .. >) path =
  with_dir_handle path (fun dir ->
      let rec loop acc =
        (* alt: use Sys.readdir which already filters "." and ".." *)
        match readdir caps#readdir dir with
        | name
          when Fpath.is_current_dir name (* "." *)
               || Fpath.is_parent_dir name (* ".." *) ->
            loop acc
        | name -> loop (name :: acc)
        | exception End_of_file -> List.rev acc
      in
      loop [])

let is_empty_dir (path : Fpath.t) : bool =
  (* note that Sys.readdir already filters the "." and ".." entries *)
  Array.length (USys.readdir !!path) = 0

(* also in Testo.ml, Testutil_files.ml and autofix-printing-stats *)
let with_chdir (caps : < Cap.chdir ; .. >) (path : Fpath.t) func =
  let orig_cwd = UUnix.getcwd () in
  CapSys.chdir caps#chdir !!path;
  Common.protect ~finally:(fun () -> CapSys.chdir caps#chdir orig_cwd) func
