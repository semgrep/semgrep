(*
   Iterate over a file tree.

   Originally copied from the dune-deps project.
   Copyright (c) 2020 Martin Jambon
   Copyright (c) 2020 r2c
*)

type visit_tracker = {
  was_visited : string -> bool;
  mark_visited : string -> unit;
}

let memoize f =
  let tbl = Hashtbl.create 100 in
  fun x ->
    let run =
      match Hashtbl.find_opt tbl x with
      | Some run -> run
      | None ->
          let run = lazy (f x) in
          Hashtbl.add tbl x run;
          run
    in
    Lazy.force run

(* Cache the results of the 'stat' syscall to speed things up.
   (due to calling it multiple times on the same path, and having
   possibly a lot of paths, and not so great caching at the OS level). *)
let stat = memoize Unix.stat

(* This is to avoid visiting the same file or directory multiple times.

   It can happen if the same folder or overlapping folders are specified
   on the command line. It can also happen due to cycles introduced
   by symbolic links.
*)
let create_visit_tracker () =
  let tbl = Hashtbl.create 100 in
  let get_id path =
    try Some (stat path).st_ino with
    | _ -> None
  in
  let was_visited path =
    match get_id path with
    | None -> true
    | Some id -> Hashtbl.mem tbl id
  in
  let mark_visited path =
    match get_id path with
    | None -> ()
    | Some id -> Hashtbl.replace tbl id ()
  in
  { was_visited; mark_visited }

let get_file_kind path =
  try Some (stat path).st_kind with
  | _ -> None

let compare_filenames = String.compare

(* Scan from a single root. *)
let fold_one ~accept_file_name ~accept_dir_name visit_tracker f acc root =
  let rec fold acc path =
    if visit_tracker.was_visited path then acc
    else (
      visit_tracker.mark_visited path;
      let name = Filename.basename path in
      match get_file_kind path with
      | Some Unix.S_DIR ->
          if not (accept_dir_name name) then acc
          else
            let children =
              let a = Sys.readdir path in
              (* sort elements so as to obtain reproducible test results
                 and overall minimize surprises. *)
              Array.sort compare_filenames a;
              Array.to_list a
              |> List_.map (fun name -> Filename.concat path name)
            in
            List.fold_left fold acc children
      | Some Unix.S_REG
      | Some Unix.S_FIFO ->
          if accept_file_name name then f acc path else acc
      | None
      | Some _ ->
          (* leave broken symlinks and special files alone *)
          acc)
  in
  fold acc root

let fold ?(excluded_paths = []) ?(accept_file_name = fun _file -> true)
    ?(accept_dir_name = fun _dir -> true) f acc roots =
  let visit_tracker = create_visit_tracker () in
  List.iter visit_tracker.mark_visited excluded_paths;
  List.fold_left
    (fun acc root ->
      fold_one ~accept_file_name ~accept_dir_name visit_tracker f acc root)
    acc roots

let iter ?excluded_paths ?accept_file_name ?accept_dir_name f roots =
  fold ?excluded_paths ?accept_file_name ?accept_dir_name
    (fun () file -> f file)
    () roots

let list ?excluded_paths ?accept_file_name ?accept_dir_name roots =
  fold ?excluded_paths ?accept_file_name ?accept_dir_name
    (fun acc file -> file :: acc)
    [] roots
  |> List.rev
