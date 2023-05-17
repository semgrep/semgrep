open Printf
open File.Operators
open Testutil_files (* file/dir/symlink *)
module F = Testutil_files

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* similar to F.with_tempfiles_verbose but takes a unit so
 * can be conveniently used inside a Testutil.test
 *)
let test_with_files (files : F.t list) func () =
  F.with_tempfiles_verbose files func

(*****************************************************************************)
(* The tests *)
(*****************************************************************************)

let tests =
  Testutil.pack_tests "Ppath"
    [
      ( "find project root",
        test_with_files
          [
            symlink "proj_link" "proj";
            dir "proj"
              [ dir ".git" []; dir "a" [ dir ".git" []; dir "b" [ file "c" ] ] ];
          ]
          (fun test_root ->
            printf "Test relative target paths, from outside the project\n";
            let target_path = Fpath.v "proj_link" / "a" in
            let expected_proj_root = Fpath.v "." in
            (match Git_project.find_any_project_root target_path with
            | Git_project, _, _ -> assert false
            | Other_project, proj_root, path_to_a ->
                printf "Obtained non-git project root: %s\n" !!proj_root;
                Alcotest.(check string) "equal" !!expected_proj_root !!proj_root;
                Alcotest.(check string)
                  "equal" "/proj_link/a"
                  (Ppath.to_string path_to_a));

            printf "Test absolute target paths\n";
            assert (Fpath.is_abs test_root);
            let target_path = test_root / "proj_link" / "a" in
            let expected_proj_root = test_root / "proj_link" in
            (match Git_project.find_any_project_root target_path with
            | Other_project, _, _ -> assert false
            | Git_project, proj_root, path_to_a ->
                printf "Obtained git project root: %s\n" !!proj_root;
                Alcotest.(check string) "equal" !!expected_proj_root !!proj_root;
                Alcotest.(check string) "equal" "/a" (Ppath.to_string path_to_a));

            printf "Test relative target paths, from inside the project\n";
            F.with_chdir
              (Fpath.v "proj_link" / "a")
              (fun () ->
                let target_path = Fpath.v "b" in
                let expected_proj_root = Unix.getcwd () |> Fpath.v in
                match Git_project.find_any_project_root target_path with
                | Other_project, _, _ -> assert false
                | Git_project, proj_root, path_to_b ->
                    printf "Expected git project root: %s\n"
                      !!expected_proj_root;
                    printf "Obtained git project root: %s\n" !!proj_root;
                    Alcotest.(check string)
                      "equal" !!expected_proj_root !!proj_root;
                    Alcotest.(check string)
                      "equal" "/b"
                      (Ppath.to_string path_to_b))) );
    ]
