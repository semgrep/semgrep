(* Martin Jambon
 *
 * Copyright (C) 2022-2024 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Fpath_.Operators
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Utilities for creating, scanning, and deleting a hierarchy
   of test files.
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t =
  | Dir of string * t list
  | File of string * string
  | Symlink of string * string

(* if you prefer a curried syntax *)
let file name : t = File (name, "")
let dir name entries : t = Dir (name, entries)
let symlink name dest : t = Symlink (name, dest)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let get_name = function
  | Dir (name, _)
  | File (name, _)
  | Symlink (name, _) ->
      name

let rec sort xs =
  List_.map sort_one xs
  |> List.sort (fun a b -> String.compare (get_name a) (get_name b))

and sort_one x =
  match x with
  | Dir (name, xs) -> Dir (name, sort xs)
  | File _
  | Symlink _ ->
      x

(*****************************************************************************)
(* Helpers independent of 't' but useful when working on file trees *)
(*****************************************************************************)
(* TODO move all those functions in UFile.ml *)

let is_dir path =
  match (UUnix.lstat !!path).st_kind with
  | S_DIR -> true
  | _ -> false

let is_file path =
  match (UUnix.lstat !!path).st_kind with
  | S_REG -> true
  | _ -> false

let is_symlink path =
  match (UUnix.lstat !!path).st_kind with
  | S_LNK -> true
  | _ -> false

let mkdir ?(root = USys.getcwd () |> Fpath.v) path =
  if Fpath.is_rel root then
    invalid_arg
      (spf "Testutil_files.mkdir: root must be an absolute path: %s" !!root);
  let rec mkdir path =
    let abs_path = root // path in
    let str = Fpath.to_string abs_path in
    if not (USys.file_exists str) then (
      let parent = Fpath.parent path in
      mkdir parent;
      UUnix.mkdir str 0o777)
  in
  let root_s = Fpath.to_string root in
  if not (USys.file_exists root_s) then
    failwith ("Testutil_files.mkdir: root folder doesn't exist: " ^ root_s);
  mkdir path

let get_dir_entries path =
  let dir = UUnix.opendir !!path in
  Common.protect
    ~finally:(fun () -> Unix.closedir dir)
    (fun () ->
      let acc = ref [] in
      try
        while true do
          acc := Unix.readdir dir :: !acc
        done;
        assert false
      with
      | End_of_file ->
          List.rev !acc
          |> List.filter (function
               | ".."
               | "." ->
                   false
               | _ -> true))

let remove path =
  let rec remove path =
    match (UUnix.lstat !!path).st_kind with
    | S_DIR ->
        let names = get_dir_entries path in
        List.iter (fun name -> remove (path / name)) names;
        UUnix.rmdir !!path
    | _other -> USys.remove !!path
  in
  if USys.file_exists !!path then remove path

let with_chdir dir f =
  let dir_s = Fpath.to_string dir in
  let orig = UUnix.getcwd () in
  Common.protect
    ~finally:(fun () -> UUnix.chdir orig)
    (fun () ->
      UUnix.chdir dir_s;
      f ())

let init_rng = lazy (URandom.self_init ())

let create_tempdir () =
  let rec loop n =
    if n > 10 then
      failwith "Can't create a temporary test folder with a random name";
    let name = spf "test-%x" (URandom.bits ()) in
    let path = UTmp.get_temp_dir_name () / name in
    if USys.file_exists !!path then loop (n + 1)
    else (
      UUnix.mkdir !!path 0o777;
      path)
  in
  Lazy.force init_rng;
  loop 1

let with_tempdir ?(persist = false) ?(chdir = false) func =
  let dir = create_tempdir () in
  Common.protect
    ~finally:(fun () -> if not persist then remove dir)
    (fun () -> if chdir then with_chdir dir (fun () -> func dir) else func dir)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

(* List the paths of regular files.
   Sorry, the implementation below with fold_left is a little tricky. *)
let flatten ?(root = Fpath.v ".") ?(include_dirs = false) files =
  let rec flatten acc files = List.fold_left flatten_one acc files
  and flatten_one (acc, dir) file =
    match file with
    | Dir (name, entries) ->
        let path = dir / name in
        let acc = if include_dirs then path :: acc else acc in
        let acc, _last_dir = flatten (acc, path) entries in
        (acc, dir)
    | File (name, _contents) ->
        let file = dir / name in
        (file :: acc, dir)
    | Symlink (name, _dest) ->
        let file = dir / name in
        (file :: acc, dir)
  in
  let acc, _dir = flatten ([], root) files in
  List.rev acc
  |> (* remove the leading "./" *)
  List_.map Fpath.normalize

let print_files files =
  flatten files |> List.iter (fun path -> UPrintf.printf "%s\n" !!path)

let rec write root files = List.iter (write_one root) files

and write_one root file =
  match file with
  | Dir (name, entries) ->
      let dir = root / name in
      if not (USys.file_exists !!dir) then UUnix.mkdir !!dir 0o777;
      write dir entries
  | File (name, contents) ->
      let path = root / name in
      UFile.write_file ~file:path contents
  | Symlink (name, dest) ->
      let path = !!(root / name) in
      UUnix.symlink dest path

let read root =
  let rec read path =
    let name = Fpath.basename path in
    match (UUnix.lstat !!path).st_kind with
    | S_DIR ->
        let names = get_dir_entries path in
        Dir (name, List_.map (fun name -> read (path / name)) names)
    | S_REG -> File (name, UFile.read_file path)
    | S_LNK -> Symlink (name, UUnix.readlink !!path)
    | _other ->
        failwith ("Testutil_files.read: unsupported file type: " ^ !!path)
  in
  match (UUnix.stat !!root).st_kind with
  | S_DIR ->
      let names = get_dir_entries root in
      List_.map (fun name -> read (root / name)) names
  | _other ->
      failwith ("Testutil_files.read: root must be a directory: " ^ !!root)

let with_tempfiles ?chdir ?persist ?(verbose = false) files func =
  with_tempdir ?persist ?chdir (fun root ->
      (* files are automatically deleted as part of the cleanup done by
         'with_tempdir'. *)
      let files = sort files in
      if verbose then (
        UPrintf.printf "--- begin input files ---\n";
        print_files files;
        UPrintf.printf "--- end input files ---\n");
      write root files;
      func root)

(*****************************************************************************)
(* Inline tests *)
(*****************************************************************************)

let () =
  Testo.test "Testutil_files" (fun () ->
      with_tempdir ~chdir:true (fun root ->
          assert (read root =*= []);
          assert (read (Fpath.v ".") =*= []);
          let tree =
            [
              File ("a", "hello");
              File ("b", "yo");
              Symlink ("c", "a");
              Dir ("d", [ File ("e", "42"); Dir ("empty", []) ]);
            ]
          in
          write root tree;
          let tree2 = read root in
          assert (sort tree2 =*= sort tree);

          let paths = flatten tree |> Fpath_.to_strings in
          List.iter UStdlib.print_endline paths;
          assert (paths =*= [ "a"; "b"; "c"; "d/e" ])))
