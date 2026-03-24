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

let tests =
  [
    t ?skipped:Testutil.skip_on_windows "user identity" test_user_identity;
    t "ls_files stress test" test_ls_files_stress;
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
