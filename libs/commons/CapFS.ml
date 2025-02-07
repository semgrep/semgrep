open Fpath_.Operators

let readdir _caps = Unix.readdir

(* helpers *)

let with_dir_handle path func =
  let dir = UUnix.opendir !!path in
  Common.protect ~finally:(fun () -> UUnix.closedir dir) (fun () -> func dir)

(* Read the names found in a directory, excluding "." and "..". *)
let read_dir_entries (caps : < Cap.readdir ; .. >) path =
  with_dir_handle path (fun dir ->
      let rec loop acc =
        try
          (* alt: use Sys.readdir which already filters "." and ".." *)
          let name = readdir caps#readdir dir in
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
