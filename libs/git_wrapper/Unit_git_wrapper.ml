(*
   Unit tests for Git_wrapper
*)

open Common
open Printf
open Fpath_.Operators

(* Mask lines like this one:
   [main (root-commit) 45e8b46] Add all the files
*)
let mask_temp_git_hash =
  Testo.mask_line ~after:"[main (root-commit) " ~before:"]" ()

(* Precisely mask the hexadecimal part in a temp folder name
   such as 'test-1e92745e' *)
let mask_test_dirname =
  Testo.mask_pcre_pattern ~mask:"test-<HEX>" "test-[a-f0-9]{8}"

let t =
  Testo.create ~checked_output:Stdout
    ~mask_output:
      [ mask_temp_git_hash; mask_test_dirname; Testo.mask_temp_paths () ]

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
  Git_wrapper.with_git_repo repo_files (fun () ->
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

let tests =
  [
    t "ls-files from project root"
      (test_ls_files_relative
         ~mk_cwd:(fun ~project_root -> project_root)
         ~mk_scanning_root:(fun ~project_root:_ -> Fpath.v "."));
    t "ls-files from outside the project, relative scanning root"
      (test_ls_files_relative
         ~mk_cwd:(fun ~project_root ->
           project_root |> Rpath.to_fpath |> Fpath.parent |> Rpath.of_fpath)
         ~mk_scanning_root:(fun ~project_root ->
           project_root |> Rpath.to_fpath |> Fpath.basename |> Fpath.v));
    t "ls-files from outside the project, absolute scanning root"
      (test_ls_files_relative
         ~mk_cwd:(fun ~project_root ->
           project_root |> Rpath.to_fpath |> Fpath.parent |> Rpath.of_fpath)
         ~mk_scanning_root:(fun ~project_root -> project_root |> Rpath.to_fpath));
    t "ls-files from project subfolder"
      (test_ls_files_relative
         ~mk_cwd:(fun ~project_root ->
           Fpath.((project_root |> Rpath.to_fpath) / "a") |> Rpath.of_fpath)
         ~mk_scanning_root:(fun ~project_root:_ -> Fpath.v "."));
    t "ls-files from project subfolder, relative scanning root"
      (test_ls_files_relative
         ~mk_cwd:(fun ~project_root ->
           Fpath.((project_root |> Rpath.to_fpath) / "x") |> Rpath.of_fpath)
         ~mk_scanning_root:(fun ~project_root:_ -> Fpath.v "../a"));
    t "ls-files from project subfolder, absolute scanning root"
      (test_ls_files_relative
         ~mk_cwd:(fun ~project_root ->
           Fpath.((project_root |> Rpath.to_fpath) / "x") |> Rpath.of_fpath)
         ~mk_scanning_root:(fun ~project_root ->
           Fpath.(Rpath.to_fpath project_root / "a")));
    t "get git project root" (fun () ->
        match Git_wrapper.get_project_root () with
        | Some _ -> ()
        | None ->
            Alcotest.fail
              (spf "couldn't find a git project root for current directory %s"
                 (Sys.getcwd ())));
    t "fail to get git project root" (fun () ->
        (* A standard folder that we know is not in a git repo *)
        let cwd = Filename.get_temp_dir_name () |> Fpath.v in
        match Git_wrapper.get_project_root ~cwd () with
        | Some _ ->
            Alcotest.fail
              (spf "we found a git project root with cwd = %s" !!cwd)
        | None -> ());
  ]
