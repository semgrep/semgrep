(*
   Unit tests for our gitignore implementation
*)

open Printf
module F = Testutil_files
open Testutil_files (* file/dir/symlink *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* ?? *)
let test_list (files : t list) () =
  F.with_tempfiles_verbose files (fun root ->
      let files2 = read root |> sort in
      printf "Output files:\n";
      print_files files2;
      assert (files2 = files))

(*
   In these tests, the file hierarchy must contain the
   .gitignore files but the target files are not needed.
*)
let test_filter (files : F.t list) selection () =
  F.with_tempdir ~chdir:true (fun root ->
      let files = F.sort files in
      printf "--- All files ---\n";
      print_files files;
      F.write root files;
      let files2 = F.read root |> F.sort in
      assert (files2 = files);
      printf "--- Filtered files ---\n";
      let filter = Gitignore_filter.create ~project_root:root () in
      let error = ref false in
      selection
      |> List.iter (fun (path, should_be_selected) ->
             let path = Ppath.of_string_for_tests path in
             let status, selection_events =
               Common.save_excursion Glob.Match.debug true (fun () ->
                   let selection_events = [] in
                   (* TODO *)
                   Gitignore_filter.select filter selection_events path)
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
  Testutil.pack_tests "Gitignore"
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
      ( "deep gitignore",
        test_filter
          [ dir "dir" [ File (".gitignore", "a"); file "a" ]; file "a" ]
          [ ("/a", true); ("/dir/a", false) ] );
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
    ]
