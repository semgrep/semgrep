(*
   Test target selection on git repos with osemgrep.
*)

open Printf
module T = Alcotest_ext

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

let repos : (string * Testutil_files.t list) list =
  let open Testutil_files in
  [
    ( "simple-semgrepignore",
      [ file "a"; file "b"; file "c"; gitignore [ "a" ]; semgrepignore [ "b" ] ]
    );
    ("no-semgrepignore", [ file "a"; gitignore [ "a" ] ]);
    ( "gitignore deignore",
      [
        gitignore [ "bin/*"; "!bin/ignore-me-not" ];
        dir "bin" [ file "ignore-me"; file "ignore-me-not" ];
      ] );
  ]

let tests caps =
  repos
  |> List_.map (fun (repo_name, (files : Testutil_files.t list)) ->
         T.create
           ~category:[ "target selection on real git repos" ]
           ~checked_output:Stdout
           ~mask_output:
             [
               T.mask_line ~after:"Initialized empty Git repository in" ();
               T.mask_line ~after:"[main (root-commit) " ~before:"]" ();
               T.mask_pcre_pattern "/test-[a-f0-9]+";
             ]
           repo_name
           (fun () -> with_git_repo files (fun () -> osemgrep_ls caps)))
