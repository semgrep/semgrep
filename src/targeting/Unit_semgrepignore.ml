(*
   Unit tests for our semgrepignore implementation
*)

open Printf
open Testutil_files (* file/dir/symlink *)
module F = Testutil_files

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let t = Testo.create

(*
   In these tests, the file hierarchy must contain the
   .gitignore and .semgrepignore files but the target files are not
   needed. Tests that check for the detection of target files are
   in Unit_find_targets.ml.

   Similar to Unit_gitignore.test_filter, but using Semgrepignore.create
   and the includes/excludes extra parameters.
*)
let test_filter ?excludes:cli_patterns (files : F.t list) selection () =
  F.with_tempdir ~chdir:true (fun root ->
      let files = F.sort files in
      printf "--- All files ---\n";
      print_files files;
      F.write root files;
      let files2 = F.read root |> F.sort in
      assert (files2 = files);
      printf "--- Filtered files ---\n";
      let filter =
        Semgrepignore.create ?cli_patterns ~default_semgrepignore_patterns:Empty
          ~exclusion_mechanism:
            { use_gitignore_files = true; use_semgrepignore_files = true }
          ~project_root:root ()
      in
      let error = ref false in
      selection
      |> List.iter (fun (path, should_be_selected) ->
             let path = Ppath.of_string_for_tests path in
             let status, selection_events =
               Gitignore_filter.select filter path
             in
             printf "Selection events for ppath %s:\n"
               (Ppath.to_string_for_tests path);
             print_string (Gitignore.show_selection_events selection_events);
             if should_be_selected then (
               match status with
               | Not_ignored ->
                   printf "[OK] ppath %s: not ignored\n"
                     (Ppath.to_string_for_tests path)
               | Ignored ->
                   printf "[FAIL] ppath %s: ignored\n"
                     (Ppath.to_string_for_tests path);
                   error := true)
             else
               match status with
               | Not_ignored ->
                   printf "[FAIL] ppath %s: not ignored\n"
                     (Ppath.to_string_for_tests path);
                   error := true
               | Ignored ->
                   printf "[OK] ppath %s: ignored\n"
                     (Ppath.to_string_for_tests path));
      if !error then Alcotest.fail "there were some unexpected results")

(*****************************************************************************)
(* The tests *)
(*****************************************************************************)

let tests =
  Testo.categorize "Semgrepignore"
    [
      t "gitignore + semgrepignore"
        (test_filter
           [
             File (".gitignore", "*.c");
             File (".semgrepignore", "!hello.*");
             file "hello.c";
             file "hello.ml";
           ]
           [ ("/hello.ml", true); ("/hello.c", true); ("/generated.c", false) ]);
      t "semgrepignore alone"
        (test_filter
           [ File (".semgrepignore", "hello.*"); file "hello.c"; file "bye.c" ]
           [ ("/hello.c", false); ("/bye.c", true) ]);
      t "legacy semgrepignore with :include"
        (test_filter
           [
             File
               ( ".semgrepignore",
                 "a\n  :include   subdir/extra-semgrepignore \n" );
             file "a";
             file "b";
             file "c";
             dir "subdir"
               [
                 (* exclude only the 'b' file at the root, not the one in
                    this folder *)
                 File ("extra-semgrepignore", "/b\n");
                 file "a";
                 file "b";
               ];
           ]
           [
             ("/a", false);
             ("/b", false);
             ("/c", true);
             ("/subdir/a", false);
             ("/subdir/b", true);
           ]);
      t "deep semgrepignore + gitignore"
        (test_filter
           [
             File (".gitignore", "a");
             dir "dir"
               [ File (".semgrepignore", "b"); file "a"; file "b"; file "c" ];
             file "a";
             file "b";
           ]
           [
             ("/a", false);
             ("/b", true);
             ("/dir/a", false);
             ("/dir/b", false);
             ("/dir/c", true);
           ]);
      t "excludes"
        (test_filter ~excludes:[ "*.ml" ] []
           [
             ("/a.ml", false);
             ("/a.c", true);
             ("/b/a.ml", false);
             ("/b/a.c", true);
           ]);
    ]
