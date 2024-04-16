(*
   Unit tests for our gitignore implementation
*)

open Printf
module F = Testutil_files

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let concat_lines lines = String.concat "\n" lines ^ "\n"
let gitignore lines : Testutil_files.t = File (".gitignore", concat_lines lines)

(* Test that Testutil_files works as it should *)
let test_list (files : F.t list) () =
  F.with_tempfiles ~verbose:true files (fun root ->
      let files2 = F.read root |> F.sort in
      printf "Output files:\n";
      F.print_files files2;
      assert (files2 = files))

(*
   In these tests, the file hierarchy must contain the
   .gitignore files but the target files are not needed.
*)
let test_filter (files : F.t list) () =
  F.with_tempdir ~chdir:true (fun root ->
      let files = F.sort files in
      printf "--- All files ---\n";
      F.print_files files;
      F.write root files;
      let files2 = F.read root |> F.sort in
      assert (files2 = files);
      printf "--- Filtered files ---\n";
      let filter = Gitignore_filter.create ~project_root:root () in
      files |> F.flatten
      |> List.iter (fun path ->
             assert (Fpath.is_rel path);
             let path = Ppath.of_relative_fpath path in
             let status, selection_events =
               (* Glob.Match.run is supposed to print detailed logs on which
                  path is matched against which pattern. Requires Debug
                  log level. *)
               Gitignore_filter.select filter path
             in
             printf "Selection events for path %s:\n"
               (Ppath.to_string_for_tests path);
             print_string (Gitignore.show_selection_events selection_events);
             match status with
             | Not_ignored ->
                 printf "SEL ppath %s\n" (Ppath.to_string_for_tests path)
             | Ignored ->
                 printf "IGN ppath %s\n" (Ppath.to_string_for_tests path)))

(*****************************************************************************)
(* The tests *)
(*****************************************************************************)

let t =
  Testo.create ~checked_output:(Testo.stdout ())
    ~normalize:[ Testutil.mask_temp_paths () ]

let tests =
  let open F in
  Testo.categorize "Gitignore"
    [
      t "list one file" (test_list [ file "a" ]);
      t "list hierarchy"
        (test_list
           [
             file "a";
             file "b";
             symlink "c" "a";
             dir "dir" [ file "d"; symlink "e" "f"; dir "g" [] ];
           ]);
      t "simple gitignore"
        (test_filter [ gitignore [ "*.c" ]; file "hello.c"; file "hello.ml" ]);
      t "relative paths"
        (test_filter [ gitignore [ "*.c" ]; file "hello.c"; file "hello.ml" ]);
      t "unanchored"
        (test_filter [ gitignore [ "a" ]; dir "dir" [ file "a" ]; file "a" ]);
      t "deep gitignore"
        (test_filter [ dir "dir" [ gitignore [ "a" ]; file "a" ]; file "a" ]);
      t "ignore directories only"
        (test_filter
           [ gitignore [ "a/" ]; dir "dir" [ file "a" ]; dir "a" [ file "b" ] ]);
      t "absolute patterns"
        (test_filter
           [
             (* [!] 'b/c' is treated as anchored just like '/b/c' because it
                contains a slash in the middle, as per the gitignore spec. *)
             gitignore [ "/a"; "b/c" ];
             dir "a" [ file "b" ];
             dir "b" [ file "a"; file "c"; file "d"; dir "b" [ file "c" ] ];
           ]);
      (* unanchored patterns should not match if the parent dir hasn't been
         matched, and the path is excluded *)
      t "excluded patterns"
        (test_filter
           [
             (* 'a/' excludes any folder or subfolder named 'a' *)
             gitignore [ "a/"; "!dir/a"; "!a/b" ];
             (* '/dir/a' is not ignored because '!dir/a' overrides 'a/'
                (therefore '/dir/a/b' is not ignored either) *)
             dir "dir" [ dir "a" [ file "b" ] ];
             (* /a/b is ignored because its parent is ignored;
                '!a/b' can do nothing about it. *)
             dir "a" [ file "b" ];
           ]);
      t "anchored with trailing wildcard"
        (test_filter
           [
             gitignore [ "dir/*" ];
             dir "dir" [ file "ignore-me" ];
             (* The slash in 'dir/*' anchors the pattern *)
             dir "sub" [ dir "dir" [ file "ignore-me-not" ] ];
           ]);
      t "ignore all but one"
        (test_filter
           [
             gitignore [ "dir/*"; "!dir/ignore-me-not" ];
             dir "dir" [ file "ignore-me"; file "ignore-me-not" ];
           ]);
    ]
