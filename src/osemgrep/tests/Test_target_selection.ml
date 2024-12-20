(*
   Test target selection on git repos with osemgrep.
*)

open Printf

(*
   List targets by invoking Find_targets.get_targets directly.
*)
let list_targets_internal ?(conf = Find_targets.default_conf) ?roots () =
  let roots =
    match roots with
    | None -> [ Scanning_root.of_string "." ]
    | Some roots -> roots
  in
  let selected, _errors, _skipped = Find_targets.get_target_fpaths conf roots in
  printf "Target files:\n";
  selected |> List.iter (fun fpath -> printf "  %s\n" (Fpath.to_string fpath))

let run_osemgrep caps argv =
  printf "RUN %s\n%!" (argv |> Array.to_list |> String.concat " ");
  CLI.main caps argv

(*
   List targets by going through the full semgrep command.
*)
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
  tests : (string * (CLI.caps -> unit)) list;
}

let test_list_from_project_root =
  ( "list target files from project root (internal)",
    fun _caps -> list_targets_internal () )

let test_cli_list_from_project_root =
  ("list target files from project root", fun caps -> osemgrep_ls caps)

let test_list_targets_from_subdir ?roots cwd =
  let func _caps =
    Testutil_files.with_chdir cwd (fun () ->
        printf "cwd: %s\n" (Sys.getcwd ());
        list_targets_internal ?roots ())
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
      tests = [ test_list_from_project_root; test_cli_list_from_project_root ];
    };
    {
      repo_name = "no-semgrepignore";
      repo_files = [ file "a"; gitignore [ "a" ] ];
      tests = [ test_list_from_project_root; test_cli_list_from_project_root ];
    };
    {
      repo_name = "gitignore-deignore";
      repo_files =
        [
          gitignore [ "bin/*"; "!bin/ignore-me-not" ];
          dir "bin" [ file "ignore-me"; file "ignore-me-not" ];
        ];
      tests = [ test_list_from_project_root; test_cli_list_from_project_root ];
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

let normalize =
  [
    Testo.mask_line ~after:"Initialized empty Git repository in" ();
    Testutil_git.mask_temp_git_hash;
    Testo.mask_pcre_pattern "/test-[a-f0-9]+";
    Testutil.mask_temp_paths ();
  ]

(*
   Create a list of tests for each test repo.
*)
let tests caps : Testo.t list =
  repos_with_tests
  |> List_.map (fun { repo_name; repo_files; tests } ->
         tests
         |> List_.map (fun (test_name, test_func) ->
                Testo.create
                  ~category:[ "target selection on real git repos"; repo_name ]
                  ~checked_output:(Testo.stdout ()) ~normalize test_name
                  (fun () ->
                    Testutil_git.with_git_repo ~verbose:true
                      ~honor_gitignore:false repo_files (fun _cwd ->
                        test_func caps))))
  |> List_.flatten
