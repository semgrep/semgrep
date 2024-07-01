(*
   Test List_files
*)

open Fpath_.Operators
open Testutil_paths
module TP = Testutil_paths

let t = Testo.create

let test_regular_file_as_root () =
  TP.with_file_tree
    (File ("hello", Regular "yo"))
    (fun workspace ->
      assert (List_files.list (workspace / "hello") = [ workspace / "hello" ]))

let test_empty_dir_as_root () =
  TP.with_file_tree
    (Dir ("empty", []))
    (fun workspace -> assert (List_files.list (workspace / "empty") = []))

(* Because file listings are not guaranteed to be in any particular order. *)
let compare_path_lists expected actual =
  let sort x =
    List.sort Fpath.compare x |> List_.map ( !! ) |> String.concat "\n"
  in
  Alcotest.(check string) "equal" (sort expected) (sort actual)

let test_regular_files () =
  with_file_tree
    (Dir
       ( "root",
         [
           File ("a", Regular "");
           File ("b", Regular "");
           Dir ("c", [ File ("d", Regular "") ]);
         ] ))
    (fun workspace ->
      compare_path_lists
        [
          workspace / "root" / "a";
          workspace / "root" / "b";
          workspace / "root" / "c" / "d";
        ]
        (List_files.list workspace))

let test_symlinks () =
  with_file_tree
    (Dir
       ( "root",
         [
           File ("a", Regular "");
           File ("b", Symlink "doesnt_exist");
           File ("c", Symlink "a");
         ] ))
    (fun workspace ->
      compare_path_lists
        [
          workspace / "root" / "a";
          workspace / "root" / "b";
          workspace / "root" / "c";
        ]
        (List_files.list workspace))

let test_ignore_symlinks () =
  with_file_tree
    (Dir
       ( "root",
         [
           File ("a", Regular "");
           File ("b", Symlink "doesnt_exist");
           File ("c", Symlink "a");
         ] ))
    (fun workspace ->
      compare_path_lists
        [ workspace / "root" / "a" ]
        (List_files.list_regular_files workspace))

let test_symlink_as_root () =
  with_file_tree
    (File ("a", Symlink "b"))
    (fun workspace ->
      let root_path = workspace / "a" in
      compare_path_lists [ root_path ]
        (List_files.list_regular_files ~keep_root:true root_path))

let tests =
  Testo.categorize_suites "List_files"
    [
      Testo.categorize "list"
        [
          t "regular_file_as_root" test_regular_file_as_root;
          t "empty_dir_as_root" test_empty_dir_as_root;
          t "regular_files" test_regular_files;
          t "symlinks" test_symlinks;
          t "ignore_symlinks" test_ignore_symlinks;
          t "symlink_as_root" test_symlink_as_root;
        ];
    ]
