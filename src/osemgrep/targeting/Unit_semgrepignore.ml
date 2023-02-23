(*
   Unit tests for our gitignore and semgrepignore implementation
*)

open Printf
module F = Testutil_files

let file name : F.t = File (name, "")
let dir name entries : F.t = Dir (name, entries)
let symlink name dest : F.t = Symlink (name, dest)

let print_files files =
  F.flatten files
  |> List.iter (fun path -> printf "%s\n" (Fpath.to_string path))

let test_list (files : F.t list) () =
  F.with_tempdir ~chdir:true (fun root ->
      let files = F.sort files in
      printf "Input files:\n";
      print_files files;
      F.write root files;
      let files2 = F.read root |> F.sort in
      printf "Output files:\n";
      print_files files2;
      assert (files2 = files))

let test_filter ?includes ?excludes (files : F.t list) selection () =
  F.with_tempdir ~chdir:true (fun root ->
      let files = F.sort files in
      printf "--- All files ---\n";
      print_files files;
      F.write root files;
      let files2 = F.read root |> F.sort in
      assert (files2 = files);
      printf "--- Filtered files ---\n";
      let filter =
        Semgrepignore.create ?includes ?excludes ~project_root:root ()
      in
      let error = ref false in
      selection
      |> List.iter (fun (rel_path, should_be_selected) ->
             let path = Fpath.v rel_path in
             assert (Fpath.is_abs path);
             let status, selection_events = Semgrepignore.select filter path in
             printf "Selection events for path %s:\n" (Fpath.to_string path);
             print_string
               (Gitignore_syntax.show_selection_events selection_events);
             if should_be_selected then (
               match status with
               | Not_ignored ->
                   printf "[OK] %s: selected\n" (Fpath.to_string path)
               | Ignored ->
                   printf "[FAIL] %s: not selected\n" (Fpath.to_string path);
                   error := true)
             else
               match status with
               | Not_ignored ->
                   printf "[FAIL] %s: selected\n" (Fpath.to_string path);
                   error := true
               | Ignored ->
                   printf "[OK] %s: not selected\n" (Fpath.to_string path));
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
    ]
