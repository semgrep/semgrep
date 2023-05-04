(*
   Unit tests for our semgrepignore implementation
*)

open Printf
open Testutil_files (* file/dir/symlink *)
module F = Testutil_files

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*
   In these tests, the file hierarchy must contain the
   .gitignore and .semgrepignore files but the target files are not
   needed.
   Similar to Unit_gitignore.test_filter, but using Semgrepignore.create
   and the includes/excludes extra parameters.
*)
let test_filter ?includes:include_patterns ?excludes:cli_patterns
    (files : F.t list) selection () =
  F.with_tempdir ~chdir:true (fun root ->
      let files = F.sort files in
      printf "--- All files ---\n";
      print_files files;
      F.write root files;
      let files2 = F.read root |> F.sort in
      assert (files2 = files);
      printf "--- Filtered files ---\n";
      let filter =
        Semgrepignore.create ?include_patterns ?cli_patterns
          ~exclusion_mechanism:Gitignore_and_semgrepignore ~project_root:root ()
      in
      let error = ref false in
      selection
      |> List.iter (fun (path, should_be_selected) ->
             let path = Ppath.of_string path in
             let status, selection_events =
               Common.save_excursion Glob.Match.debug true (fun () ->
                   Semgrepignore.select filter path)
             in
             printf "Selection events for path %s:\n" (Ppath.to_string path);
             print_string (Gitignore.show_selection_events selection_events);
             if should_be_selected then (
               match status with
               | Not_ignored ->
                   printf "[OK] %s: not ignored\n" (Ppath.to_string path)
               | Ignored ->
                   printf "[FAIL] %s: ignored\n" (Ppath.to_string path);
                   error := true)
             else
               match status with
               | Not_ignored ->
                   printf "[FAIL] %s: not ignored\n" (Ppath.to_string path);
                   error := true
               | Ignored -> printf "[OK] %s: ignored\n" (Ppath.to_string path));
      assert (not !error))

(*****************************************************************************)
(* The tests *)
(*****************************************************************************)

let tests =
  Testutil.pack_tests "Semgrepignore"
    [
      ( "gitignore + semgrepignore",
        test_filter
          [
            File (".gitignore", "*.c");
            File (".semgrepignore", "!hello.*");
            file "hello.c";
            file "hello.ml";
          ]
          [ ("/hello.ml", true); ("/hello.c", true); ("/generated.c", false) ]
      );
      ( "semgrepignore alone",
        test_filter
          [ File (".semgrepignore", "hello.*"); file "hello.c"; file "bye.c" ]
          [ ("/hello.c", false); ("/bye.c", true) ] );
      ( "deep semgrepignore + gitignore",
        test_filter
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
          ] );
      ( "includes",
        test_filter ~includes:[ "*.ml" ] []
          [
            ("/a.ml", true);
            ("/a.c", false);
            ("/b/a.ml", true);
            ("/b/a.c", false);
          ] );
      ( "excludes",
        test_filter ~excludes:[ "*.ml" ] []
          [
            ("/a.ml", false);
            ("/a.c", true);
            ("/b/a.ml", false);
            ("/b/a.c", true);
          ] );
      ( "includes + excludes",
        test_filter ~includes:[ "/src" ] ~excludes:[ "*.c" ] []
          [
            ("/a.ml", false);
            ("/a.c", false);
            ("/src/a.ml", true);
            ("/src/a.c", false);
          ] );
      ( "includes + excludes + semgrepignore",
        test_filter ~includes:[ "/src" ] ~excludes:[ "*.c" ]
          [
            (* command-line level includes/excludes take precedence over
               gitignore/semgrepignore files, so this is ignored. *)
            File (".semgrepignore", "!b.*");
          ]
          [
            ("/a.ml", false);
            ("/a.c", false);
            ("/src/a.ml", true);
            ("/src/a.c", false);
            ("/src/b.c", false);
          ] );
    ]
