(*
   Copyright (c) 2024-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Unit tests for Git_wrapper
*)

open Common
open Printf
open Fpath_.Operators

let t = Testo.create

let test_user_identity () =
  Testutil_git.with_git_repo ~verbose:true
    [ File ("empty", "") ]
    (fun _cwd ->
      let not_found =
        Git_wrapper.config_get_exn "xxxxxxxxxxxxxxxxxxxxxxxxxxx"
      in
      Alcotest.(check (option string)) "missing entry" None not_found;
      let user_name = Git_wrapper.config_get_exn "user.name" in
      Alcotest.(check (option string))
        "default user name" (Some "Tester") user_name;
      let user_email = Git_wrapper.config_get_exn "user.email" in
      Alcotest.(check (option string))
        "default user email" (Some "tester@example.com") user_email;
      Git_wrapper.config_set_exn "user.name" "nobody";
      let nobody = Git_wrapper.config_get_exn "user.name" in
      Alcotest.(check (option string)) "new user name" (Some "nobody") nobody)

(* Stress test for git ls-files to reproduce Windows EBADF issue; see
   SAF-2358. This number of iterations probably won't catch the error
   we were seeing, but I'm leaving this here in case the issue keeps
   popping up. Setting iterations = 10000 consistently failed before
   the patch in #5268. *)
let test_ls_files_stress () =
  let iterations = 10 in
  Testutil_git.with_git_repo ~verbose:false
    [ File ("test.txt", "hello") ]
    (fun cwd ->
      for i = 1 to iterations do
        match Git_wrapper.ls_files ~cwd [] with
        | Ok _files -> ()
        | Error msg ->
            Alcotest.fail (sprintf "ls_files failed on iteration %d: %s" i msg)
      done;
      printf "ls_files stress test: %d iterations passed\\n" iterations)

let test_one_missing_history_object () =
  let module Hash = Git.Hash.Make (Digestif.SHA1) in
  let module Commit = Git.Commit.Make (Hash) in
  let missing_tree = Hash.digest_string "missing tree" in
  let user : Git.User.t =
    { name = "Tester"; email = "tester@example.com"; date = (0L, None) }
  in
  let commit =
    Commit.make ~tree:missing_tree ~author:user ~committer:user (Some "test")
  in
  let objects = Base.Hashtbl.Poly.create () in
  Base.Hashtbl.set objects ~key:(Commit.digest commit)
    ~data:(Git.Value.Commit commit);
  let commits =
    Git_wrapper.commit_blobs_by_date (ROHashtbl.Base.of_hashtbl objects)
  in
  match commits with
  | [ (actual_commit, []) ] ->
      Alcotest.(check bool)
        "commit with a missing tree is retained" true
        (Git_wrapper.equal_commit commit actual_commit)
  | _ -> Alcotest.fail "expected one commit with no resolved blobs"

let test_memoized_tree_paths () =
  let module Hash = Git.Hash.Make (Digestif.SHA1) in
  let module Blob = Git.Blob.Make (Hash) in
  let module Tree = Git.Tree.Make (Hash) in
  let module Commit = Git.Commit.Make (Hash) in
  let blob = Blob.of_string "secret" in
  let blob_hash = Blob.digest blob in
  let subtree = Tree.v [ Tree.entry ~name:"value.txt" `Normal blob_hash ] in
  let subtree_hash = Tree.digest subtree in
  let root path = Tree.v [ Tree.entry ~name:path `Dir subtree_hash ] in
  let root_a = root "a" in
  let root_b = root "b" in
  let user : Git.User.t =
    { name = "Tester"; email = "tester@example.com"; date = (0L, None) }
  in
  let commit tree message =
    Commit.make ~tree:(Tree.digest tree) ~author:user ~committer:user
      (Some message)
  in
  let commit_a = commit root_a "a" in
  let commit_b = commit root_b "b" in
  let objects = Base.Hashtbl.Poly.create () in
  let add hash value = Base.Hashtbl.set objects ~key:hash ~data:value in
  add blob_hash (Git.Value.Blob blob);
  add subtree_hash (Git.Value.Tree subtree);
  add (Tree.digest root_a) (Git.Value.Tree root_a);
  add (Tree.digest root_b) (Git.Value.Tree root_b);
  add (Commit.digest commit_a) (Git.Value.Commit commit_a);
  add (Commit.digest commit_b) (Git.Value.Commit commit_b);
  let paths =
    Git_wrapper.commit_blobs_by_date (ROHashtbl.Base.of_hashtbl objects)
    |> List.concat_map snd
    |> List.map (fun (blob : Git_wrapper.blob_with_extra) -> blob.path)
    |> List.sort Fpath.compare
  in
  let fpath = Alcotest.testable Fpath.pp Fpath.equal in
  Alcotest.(check (list fpath))
    "shared subtree paths retain their distinct prefixes"
    [ Fpath.v "a/value.txt"; Fpath.v "b/value.txt" ]
    paths

let test_dirty_lines () =
  let relative_file = Fpath.v "test.txt" in
  Testutil_git.with_git_repo ~verbose:false
    [ File ("test.txt", "one\ntwo\nthree\nfour\nfive\n") ]
    (fun cwd ->
      UFile.write_file
        ~file:Fpath.(cwd / "test.txt")
        "one\nnew two\nnew three\nfour\nfive\nsix\n";
      let actual = Git_wrapper.dirty_lines_of_file_exn ~cwd relative_file in
      Alcotest.(check (option (array (pair int int))))
        "single- and multi-line ranges"
        (Some [| (2, 4); (6, 7) |])
        actual)

let tests =
  [
    t ?skipped:Testutil.skip_on_windows "user identity" test_user_identity;
    t "ls_files stress test" test_ls_files_stress;
    t "skip one missing history object" test_one_missing_history_object;
    t "memoized tree paths" test_memoized_tree_paths;
    t ?skipped:Testutil.skip_on_windows "dirty lines" test_dirty_lines;
    t "get git project root" (fun () ->
        let cwd = Sys.getcwd () |> Fpath.v in
        match Git_wrapper.project_root_for_files_in_dir cwd with
        | Some root -> printf "found git project root: %s\n" !!root
        | None ->
            Alcotest.fail
              (spf "couldn't find a git project root for current directory %s"
                 (Sys.getcwd ())));
    t "fail to get git project root" (fun () ->
        (* A standard folder that we know is not in a git repo *)
        let cwd = Filename.get_temp_dir_name () |> Fpath.v in
        match Git_wrapper.project_root_for_files_in_dir cwd with
        | Some root ->
            Alcotest.fail
              (spf "we found a git project root with cwd = %s: %s" !!cwd !!root)
        | None -> printf "found no git project root as expected\n");
  ]
