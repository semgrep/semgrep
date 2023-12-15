(*
   Generic functions used by more than one module in this library
*)

open Printf

(* safe version of List.map for ocaml < 5 *)
let list_map f l = List.rev_map f l |> List.rev

(* safe version of List.flatten *)
let list_flatten ll =
  List.fold_left (fun acc l -> List.rev_append l acc) [] ll |> List.rev

let rec make_dir_if_not_exists ?(recursive = false) dir =
  match (Unix.stat dir).st_kind with
  | S_DIR -> ()
  | S_REG
  | S_CHR
  | S_BLK
  | S_LNK
  | S_FIFO
  | S_SOCK ->
      failwith
        (sprintf
           "File %S already exists but is not a folder as required by the \
            testing setup."
           dir)
  | exception Unix.Unix_error (ENOENT, _, _) ->
      let parent = Filename.dirname dir in
      if parent = dir then
        (* dir is something like "." or "/" *)
        failwith
          (sprintf
             "Folder %S doesn't exist and has no parent that we could create."
             dir)
      else if recursive then (
        make_dir_if_not_exists ~recursive parent;
        Sys.mkdir dir 0o777)
      else if Sys.file_exists parent then Sys.mkdir dir 0o777
      else
        failwith
          (sprintf "The parent folder of %S doesn't exist (current folder: %S)"
             dir (Sys.getcwd ()))
