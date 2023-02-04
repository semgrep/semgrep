(*
   Lazy file tree used for path filtering.
*)

let ( / ) = Fpath.( / )
let ( // ) = Fpath.( // )

type file_kind = Dir | Reg | Lnk

type file = {
  path : Fpath.t;
  abs_path : Fpath.t;
  kind : file_kind;
  parent : file option Lazy.t;
  children : file list Lazy.t;
}

type t = {
  (* An absolute path to an existing directory. *)
  cwd : Fpath.t;
  (* Cache of info about files, keyed by absolute paths.
     They're converted to strings to ensure the default Hashtbl.hash
     will work because Fpath doesn't provide Fpath.hash.
     The current implementation of FPath.to_string is
     the identity so it doesn't hurt performance. *)
  file_cache : (string, file option) Hashtbl.t;
}

let cwd x = x.cwd

(* Use this only on validated paths *)
let internal_path_of_string str =
  match Fpath.of_string str with
  | Ok res -> res
  | Error (`Msg _msg) -> assert false

let create ?cwd () =
  let system_cwd = internal_path_of_string (Unix.getcwd ()) in
  let cwd =
    match cwd with
    | Some input_cwd -> (
        let abs_cwd = system_cwd // input_cwd in
        let abs_cwd_str = Fpath.to_string abs_cwd in
        let cwd =
          try Unix.realpath abs_cwd_str |> internal_path_of_string with
          | Unix.Unix_error _ ->
              failwith ("not a valid directory: " ^ abs_cwd_str)
        in
        (* need to check that it's a directory. *)
        match (Unix.lstat (Fpath.to_string cwd)).st_kind with
        | S_DIR ->
            (* make absolute if needed *)
            system_cwd // cwd
        | __else__ -> failwith ("not a valid directory: " ^ abs_cwd_str))
    | None -> system_cwd
  in
  { cwd; file_cache = Hashtbl.create 1000 }

let with_cache table key compute =
  match Hashtbl.find_opt table key with
  | Some res -> res
  | None ->
      let res = compute () in
      Hashtbl.add table key res;
      res

let with_dir_handle dir_path func =
  let dir = Unix.opendir (Fpath.to_string dir_path) in
  Fun.protect ~finally:(fun () -> Unix.closedir dir) (fun () -> func dir)

(*
   Read the names found in a directory, excluding "." and "..", sorted
   in alphabetical order (bytewise).

   The result includes the paths extending the input dir_path. If dir_path
   is absolute, then the result is a list of absolute paths.
*)
let read_dir_entries dir_path =
  with_dir_handle dir_path (fun dir ->
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
        | End_of_file ->
            List.sort String.compare acc
            |> Common.map (fun basename -> dir_path / basename)
      in
      loop [])

let rec resolve t path =
  let abs_path = t.cwd // path in
  let abs_path_str = Fpath.to_string abs_path in
  with_cache t.file_cache abs_path_str (fun () ->
      let kind =
        match (Unix.lstat abs_path_str).st_kind with
        | S_DIR -> Some Dir
        | S_REG -> Some Reg
        | S_LNK -> Some Lnk
        | S_CHR
        | S_BLK
        | S_FIFO
        | S_SOCK ->
            None
      in
      let parent = lazy (compute_parent t abs_path) in
      match kind with
      | None -> None
      | Some kind ->
          let children =
            match kind with
            | Reg
            | Lnk ->
                lazy []
            | Dir -> lazy (compute_children t abs_path)
          in
          Some { path; abs_path; kind; parent; children })

and compute_parent t abs_path =
  let ppath = Fpath.parent abs_path in
  if Fpath.equal ppath abs_path then None else resolve t ppath

and compute_children t abs_dir_path =
  read_dir_entries abs_dir_path |> List.filter_map (resolve t)

let parent file = Lazy.force file.parent
let children file = Lazy.force file.children

let descendants file =
  let rec descendants acc file =
    List.fold_left descendants (file :: acc) (children file)
  in
  descendants [] file |> List.rev
