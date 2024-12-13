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
      let not_found = Git_wrapper.config_get "xxxxxxxxxxxxxxxxxxxxxxxxxxx" in
      Alcotest.(check (option string)) "missing entry" None not_found;
      let user_name = Git_wrapper.config_get "user.name" in
      Alcotest.(check (option string))
        "default user name" (Some "Tester") user_name;
      let user_email = Git_wrapper.config_get "user.email" in
      Alcotest.(check (option string))
        "default user email" (Some "tester@example.com") user_email;
      Git_wrapper.config_set "user.name" "nobody";
      let nobody = Git_wrapper.config_get "user.name" in
      Alcotest.(check (option string)) "new user name" (Some "nobody") nobody)

let tests =
  [
    t "user identity" test_user_identity;
    t "get git project root" (fun () ->
        let cwd = USys.getcwd () |> Fpath.v in
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
