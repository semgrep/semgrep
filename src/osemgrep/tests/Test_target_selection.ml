(*
   Test target selection on git repos with osemgrep.
*)

open Printf

let with_git_repo (files : Testutil_files.t list) func =
  Testutil_files.with_tempfiles_verbose files (fun path ->
      Testutil_files.with_chdir path (fun () ->
          Git_wrapper.init ();
          Git_wrapper.add ~force:true [ Fpath.v "." ];
          Git_wrapper.commit "Add all the files";
          func ()))

let run_osemgrep caps argv =
  printf "RUN %s\n%!" (argv |> Array.to_list |> String.concat " ");
  CLI.main caps argv

let osemgrep_ls caps =
  let exit_code =
    run_osemgrep caps [| "semgrep"; "scan"; "--experimental"; "--x-ls"; "." |]
  in
  Alcotest.(check int) "exit code" 0 (Exit_code.to_int exit_code)

let concat_lines lines = String.concat "\n" lines ^ "\n"
let gitignore lines : Testutil_files.t = File (".gitignore", concat_lines lines)

let semgrepignore lines : Testutil_files.t =
  File (".semgrepignore", concat_lines lines)

(* The repo_name will be included in the final test name, so don't worry about
   making the test name unique. *)
type repo_with_tests = {
  repo_name : string;
  repo_files : Testutil_files.t list;
  tests : (string * (Cap.all_caps -> unit)) list;
}

let test_list_from_project_root =
  ("list target files from project root", fun caps -> osemgrep_ls caps)

let test_list_targets_from_subdir cwd =
  let func caps =
    Testutil_files.with_chdir cwd (fun () ->
        printf "cwd: %s\n" (Fpath.to_string cwd);
        osemgrep_ls caps)
  in
  let name = "list target files from " ^ Fpath.to_string cwd in
  (name, func)

(*
   A list of git repo definitions and tests to run on them.
*)
let repos_with_tests : repo_with_tests list =
  let open Testutil_files in
  [
    {
      repo_name = "simple-semgrepignore";
      repo_files =
        [
          file "a"; file "b"; file "c"; gitignore [ "a" ]; semgrepignore [ "b" ];
        ];
      tests = [ test_list_from_project_root ];
    };
    {
      repo_name = "no-semgrepignore";
      repo_files = [ file "a"; gitignore [ "a" ] ];
      tests = [ test_list_from_project_root ];
    };
    {
      repo_name = "gitignore-deignore";
      repo_files =
        [
          gitignore [ "bin/*"; "!bin/ignore-me-not" ];
          dir "bin" [ file "ignore-me"; file "ignore-me-not" ];
        ];
      tests = [ test_list_from_project_root ];
    };
    {
      repo_name = "nested-repo";
      repo_files = [ dir "a" [ dir "b" [ file "target" ] ] ];
      tests =
        [
          test_list_from_project_root;
          (* subfolder that doesn't contain the target directly *)
          test_list_targets_from_subdir (Fpath.v "a");
          (* subfolder that contains the target *)
          test_list_targets_from_subdir (Fpath.v "a/b");
        ];
    };
  ]

let mask_output =
  [
    Testo.mask_line ~after:"Initialized empty Git repository in" ();
    Testo.mask_line ~after:"[main (root-commit) " ~before:"]" ();
    Testo.mask_pcre_pattern "/test-[a-f0-9]+";
  ]

(*
   Create a list of tests for each test repo.
*)
let tests caps : Testo.test list =
  repos_with_tests
  |> List_.map (fun { repo_name; repo_files; tests } ->
         tests
         |> List_.map (fun (test_name, test_func) ->
                Testo.create
                  ~category:[ "target selection on real git repos"; repo_name ]
                  ~checked_output:Stdout ~mask_output test_name (fun () ->
                    with_git_repo repo_files (fun () -> test_func caps))))
  |> List_.flatten
