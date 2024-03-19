(* Austin Theriault
 *
 * Copyright (C) Semgrep, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(* Commentary *)
(* This is like git_wrapper but using pure ocaml https://github.com/mirage/ocaml-git. *)
(* That way we don't have to shell out *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

module Store = Git_unix.Store
module Hash = Store.Hash
module Value = Git.Value.Make (Hash)
module Commit = Git.Commit.Make (Hash)
module Tree = Git.Tree.Make (Hash)
module Blob = Git.Blob.Make (Hash)
module User = Git.User

let tags = Logs_.create_tags [ __MODULE__ ]

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type object_table = (Value.hash, Value.t) Hashtbl.t
type hash = Hash.t [@@deriving show, eq, ord]
type commit = Commit.t [@@deriving show, eq, ord]
type blob = Blob.t [@@deriving show, eq, ord]
type author = User.t [@@deriving show, eq, ord]

type blob_with_extra = { blob : blob; path : Fpath.t; size : int }
[@@deriving show]

(*****************************************************************************)
(* Reexports *)
(*****************************************************************************)

let commit_digest = Commit.digest
let commit_author = Commit.author
let hex_of_hash = Hash.to_hex
let blob_digest = Blob.digest
let string_of_blob = Blob.to_string

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

(* A lot of this is from https://github.com/mirage/ocaml-git *)
let load_store ?(path = Fpath.v (Sys.getcwd ())) () = Store.v path

let load_store_exn ?(path = Fpath.v (Sys.getcwd ())) () =
  match%lwt load_store ~path () with
  | Ok store -> Lwt.return store
  | Error (`Reference_not_found _) ->
      failwith "Reference not found. Are you in a git repository?"
  | Error (`Not_found _) -> failwith "Not found. Are you in a git repository?"
  | Error (`Msg msg) -> failwith msg
  | _ -> failwith "Unknown error"

let objects_of_store (store : Store.t) : object_table Lwt.t =
  Logs.debug (fun m -> m ~tags "reading objects");
  let%lwt contents = Store.contents store in
  Lwt.return (contents |> List.to_seq |> Hashtbl.of_seq)

let tree_of_commit (objects : object_table) commit =
  commit |> Commit.tree
  |> Hashtbl_.find_some objects (fun obj ->
         match obj with
         | Git.Value.Tree tree -> tree
         | _ ->
             failwith
               "Not a tree! Shouldn't happen, as we read a tree from a commit \
                from a store.")

let rec blobs_of_tree ?(path_prefix = "") (objects : object_table)
    (tree : Tree.t) : blob_with_extra list =
  tree |> Tree.to_list
  |> List_.map (blobs_of_entry ~path_prefix objects)
  |> List.flatten

and blobs_of_entry ?(path_prefix = "") (objects : object_table) :
    Tree.entry -> blob_with_extra list = function
  | { perm = `Exec; name = path_segment; node = hash }
  | { perm = `Everybody; name = path_segment; node = hash }
  | { perm = `Normal; name = path_segment; node = hash } ->
      let blob =
        Hashtbl_.find_some objects
          (fun obj ->
            match obj with
            | Git.Value.Blob blob -> blob
            | _ ->
                failwith
                  "Not a blob! Shouldn't happen, as we read a blob from a tree \
                   from a store.")
          hash
      in
      (* If youre on a 32bit machine trying to scan files with blobs > 2gb you deserve the error this could cause *)
      let size = blob |> Blob.length |> Int64.to_int in
      let path = Filename.concat path_prefix path_segment |> Fpath.v in
      [ { blob; path; size } ]
  | { perm = `Dir; name = path_segment; node = hash } ->
      let tree =
        Hashtbl_.find_some objects
          (fun obj ->
            match obj with
            | Git.Value.Tree tree -> tree
            | _ ->
                failwith
                  "Not a tree! Shouldn't happen, as we read a tree from a tree \
                   from a store.")
          hash
      in
      let path = Filename.concat path_prefix path_segment in
      blobs_of_tree ~path_prefix:path objects tree
  | { perm = `Link; _ }
  | { perm = `Commit; _ } ->
      []

let blobs_by_commit objects commits =
  commits
  |> List_.map (fun commit ->
         let tree = tree_of_commit objects commit in
         (commit, tree))
  |> List_.map (fun (commit, tree) ->
         let blobs = blobs_of_tree objects tree in
         (commit, blobs))

let commit_blobs_by_date store =
  Logs.debug (fun m -> m ~tags "loading objects from store");
  let%lwt objects = objects_of_store store in
  Logs.debug (fun m -> m ~tags "loaded objects");
  Logs.debug (fun m -> m ~tags "getting commits");
  let commits =
    Hashtbl.fold
      (fun _hash value acc ->
        match value with
        | Git.Value.Commit commit -> commit :: acc
        | _ -> acc)
      objects []
  in
  Logs.debug (fun m -> m ~tags "got commits");
  Logs.debug (fun m -> m ~tags "sorting commits");
  let commits_by_date = List.sort Commit.compare_by_date commits in
  Logs.debug (fun m -> m ~tags "sorted commits");
  let blobs_by_commit = blobs_by_commit objects commits_by_date in
  Logs.debug (fun m -> m ~tags "got blobs by commit");
  Lwt.return blobs_by_commit
