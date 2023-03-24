(*
   Unit tests for our gitignore and semgrepignore implementation
*)

open Printf
open File.Operators
module F = Testutil_files

let file name : F.t = File (name, "")
let dir name entries : F.t = Dir (name, entries)
let symlink name dest : F.t = Symlink (name, dest)

let print_files files =
  F.flatten files |> List.iter (fun path -> printf "%s\n" !!path)

let test_with_files (files : F.t list) func () =
  F.with_tempdir ~chdir:true (fun root ->
      let files = F.sort files in
      printf "Input files:\n";
      print_files files;
      F.write root files;
      (* Nice listing of the real file tree.
         Don't care if the 'tree' command is unavailable. *)
      Sys.command (sprintf "tree -a '%s'" !!root) |> ignore;
      func root)

let test_list (files : F.t list) () =
  test_with_files files
    (fun root ->
      let files2 = F.read root |> F.sort in
      printf "Output files:\n";
      print_files files2;
      assert (files2 = files))
    ()

(*
   In these tests, the file hierarchy must contain the
   .gitignore and .semgrepignore files but the target files are not
   needed.
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
             let path = Git_path.of_string path in
             let status, selection_events =
               Common.save_excursion Glob_matcher.debug true (fun () ->
                   Semgrepignore.select filter path)
             in
             printf "Selection events for path %s:\n" (Git_path.to_string path);
             print_string
               (Gitignore_syntax.show_selection_events selection_events);
             if should_be_selected then (
               match status with
               | Not_ignored ->
                   printf "[OK] %s: not ignored\n" (Git_path.to_string path)
               | Ignored ->
                   printf "[FAIL] %s: ignored\n" (Git_path.to_string path);
                   error := true)
             else
               match status with
               | Not_ignored ->
                   printf "[FAIL] %s: not ignored\n" (Git_path.to_string path);
                   error := true
               | Ignored ->
                   printf "[OK] %s: ignored\n" (Git_path.to_string path));
      assert (not !error))

let tests =
  Testutil.pack_tests "Semgrepignore"
    [
      ("list one file", test_list [ file "a" ]);
      ( "list hierarchy",
        test_list
          [
            file "a";
            file "b";
            symlink "c" "a";
            dir "dir" [ file "d"; symlink "e" "f"; dir "g" [] ];
          ] );
      ( "simple gitignore",
        test_filter
          [ File (".gitignore", "*.c"); file "hello.c"; file "hello.ml" ]
          [ ("/hello.ml", true); ("/hello.c", false) ] );
      ( "relative paths",
        test_filter
          [ File (".gitignore", "*.c"); file "hello.c"; file "hello.ml" ]
          [ ("hello.ml", true); ("hello.c", false) ] );
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
      ( "deep gitignore",
        test_filter
          [ dir "dir" [ File (".gitignore", "a"); file "a" ]; file "a" ]
          [ ("/a", true); ("/dir/a", false) ] );
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
      ( "ignore directories only",
        test_filter
          [
            File (".gitignore", "a/");
            dir "dir" [ file "a" ];
            dir "a" [ file "b" ];
          ]
          [
            ("/a/", false);
            ("/a", true);
            ("/dir/a", true);
            ("/dir/a/", false);
            ("/a/b", false);
          ] );
      ( "absolute patterns",
        test_filter
          [
            (* [!] 'b/c' is treated as anchored just like '/b/c' because it
               contains a slash in the middle, as per the gitignore spec. *)
            File (".gitignore", "/a\nb/c");
            dir "a" [ file "b" ];
            dir "b" [ file "a"; file "c"; file "d"; dir "b" [ file "c" ] ];
          ]
          [
            ("/a/b", false);
            ("/b/a", true);
            ("/b/c", false);
            ("/b/d", true);
            ("/b/b/c", true);
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
                  (Git_path.to_string path_to_a));

            printf "Test absolute target paths\n";
            assert (Fpath.is_abs test_root);
            let target_path = test_root / "proj_link" / "a" in
            let expected_proj_root = test_root / "proj_link" in
            (match Git_project.find_any_project_root target_path with
            | Other_project, _, _ -> assert false
            | Git_project, proj_root, path_to_a ->
                printf "Obtained git project root: %s\n" !!proj_root;
                Alcotest.(check string) "equal" !!expected_proj_root !!proj_root;
                Alcotest.(check string)
                  "equal" "/a"
                  (Git_path.to_string path_to_a));

            printf "Test relative target paths, from inside the project\n";
            F.with_chdir
              (Fpath.v "proj_link" / "a")
              (fun _cwd ->
                let target_path = Fpath.v "b" in
                let expected_proj_root = Fpath.v ".." in
                match Git_project.find_any_project_root target_path with
                | Other_project, _, _ -> assert false
                | Git_project, proj_root, path_to_b ->
                    printf "Obtained git project root: %s\n" !!proj_root;
                    Alcotest.(check string)
                      "equal" !!expected_proj_root !!proj_root;
                    Alcotest.(check string)
                      "equal" "/b"
                      (Git_path.to_string path_to_b))) );
    ]
