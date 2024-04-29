(*
   Unit tests for Git_wrapper
*)

open Common
open Printf
open Fpath_.Operators

(* Precisely mask the hexadecimal part in a temp folder name
   such as 'test-1e92745e'. This is printed by some tests as a relative
   path that is not automatically detected by Testo.mask_temp_paths. *)
let mask_test_dirname =
  Testo.mask_pcre_pattern ~replace:(fun _ -> "<HEX>") "test-([a-f0-9]{1,8})"

let normalize =
  [
    Testutil_git.mask_temp_git_hash;
    Testutil.mask_temp_paths ();
    mask_test_dirname;
  ]

let t = Testo.create
let capture = Testo.create ~checked_output:(Testo.stdout ()) ~normalize

(*
   List repo files relative to 'cwd' which can be the root of the git repo,
   some other folder in the git repo, or outside the git repo.

   The scanning roots themselves can be absolute or relative paths. They
   are relative to the chosen current directory 'cwd'.
*)
let test_ls_files_relative ~mk_cwd ~mk_scanning_root () =
  let repo_files =
    Testutil_files.
      [
        dir "a" [ dir "b" [ file "target" ] ]; dir "x" [ dir "y" [ file "z" ] ];
      ]
  in
  Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
      let project_root = Rpath.getcwd () in
      let cwd = mk_cwd ~project_root in
      Testutil_files.with_chdir (Rpath.to_fpath cwd) (fun () ->
          let scanning_root = mk_scanning_root ~project_root in
          printf "project root: %s\n" (Rpath.to_string project_root);
          printf "cwd: %s\n" (Sys.getcwd ());
          printf "scanning root: %s\n" (Fpath.to_string scanning_root);
          let file_list =
            Git_wrapper.ls_files_relative ~project_root
              [ mk_scanning_root ~project_root ]
          in
          printf "file list:\n";
          file_list
          |> List.iter (fun path -> printf "  %s\n" (Fpath.to_string path))))

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
    capture "ls-files from project root"
      (test_ls_files_relative
         ~mk_cwd:(fun ~project_root -> project_root)
         ~mk_scanning_root:(fun ~project_root:_ -> Fpath.v "."));
    capture "ls-files from outside the project, relative scanning root"
      (test_ls_files_relative
         ~mk_cwd:(fun ~project_root ->
           project_root |> Rpath.to_fpath |> Fpath.parent |> Rpath.of_fpath_exn)
         ~mk_scanning_root:(fun ~project_root ->
           project_root |> Rpath.to_fpath |> Fpath.basename |> Fpath.v));
    capture "ls-files from outside the project, absolute scanning root"
      (test_ls_files_relative
         ~mk_cwd:(fun ~project_root ->
           project_root |> Rpath.to_fpath |> Fpath.parent |> Rpath.of_fpath_exn)
         ~mk_scanning_root:(fun ~project_root -> project_root |> Rpath.to_fpath));
    capture "ls-files from project subfolder"
      (test_ls_files_relative
         ~mk_cwd:(fun ~project_root ->
           Fpath.((project_root |> Rpath.to_fpath) / "a") |> Rpath.of_fpath_exn)
         ~mk_scanning_root:(fun ~project_root:_ -> Fpath.v "."));
    capture "ls-files from project subfolder, relative scanning root"
      (test_ls_files_relative
         ~mk_cwd:(fun ~project_root ->
           Fpath.((project_root |> Rpath.to_fpath) / "x") |> Rpath.of_fpath_exn)
         ~mk_scanning_root:(fun ~project_root:_ -> Fpath.v "../a"));
    capture "ls-files from project subfolder, absolute scanning root"
      (test_ls_files_relative
         ~mk_cwd:(fun ~project_root ->
           Fpath.((project_root |> Rpath.to_fpath) / "x") |> Rpath.of_fpath_exn)
         ~mk_scanning_root:(fun ~project_root ->
           Fpath.(Rpath.to_fpath project_root / "a")));
    t "get git project root" (fun () ->
        let cwd = USys.getcwd () |> Fpath.v in
        match Git_wrapper.get_project_root_for_files_in_dir cwd with
        | Some root -> printf "found git project root: %s\n" !!root
        | None ->
            Alcotest.fail
              (spf "couldn't find a git project root for current directory %s"
                 (Sys.getcwd ())));
    t "fail to get git project root" (fun () ->
        (* A standard folder that we know is not in a git repo *)
        let cwd = Filename.get_temp_dir_name () |> Fpath.v in
        match Git_wrapper.get_project_root_for_files_in_dir cwd with
        | Some root ->
            Alcotest.fail
              (spf "we found a git project root with cwd = %s: %s" !!cwd !!root)
        | None -> printf "found no git project root as expected\n");
  ]
