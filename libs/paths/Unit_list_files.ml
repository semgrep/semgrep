(*
   Test List_files
*)

open Fpath_.Operators
open Testutil_paths
module TP = Testutil_paths

let t = Testo.create

let test_regular_file_as_root (caps : < Cap.readdir ; .. >) () =
  TP.with_file_tree
    (File ("hello", Regular "yo"))
    (fun workspace ->
      assert (
        List_files.list caps (workspace / "hello") = [ workspace / "hello" ]))

let test_empty_dir_as_root (caps : < Cap.readdir ; .. >) () =
  TP.with_file_tree
    (Dir ("empty", []))
    (fun workspace -> assert (List_files.list caps (workspace / "empty") = []))

(* Because file listings are not guaranteed to be in any particular order. *)
let compare_path_lists expected actual =
  let sort x =
    List.sort Fpath.compare x |> List_.map ( !! ) |> String.concat "\n"
  in
  Alcotest.(check string) "equal" (sort expected) (sort actual)

let test_regular_files (caps : < Cap.readdir ; .. >) () =
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
        (List_files.list caps workspace))

let test_symlinks (caps : < Cap.readdir ; .. >) () =
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
        (List_files.list caps workspace))

let test_ignore_symlinks (caps : < Cap.readdir ; .. >) () =
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
        (List_files.list_regular_files caps workspace))

let test_symlink_as_root (caps : < Cap.readdir ; .. >) () =
  with_file_tree
    (File ("a", Symlink "b"))
    (fun workspace ->
      let root_path = workspace / "a" in
      compare_path_lists [ root_path ]
        (List_files.list_regular_files ~keep_root:true caps root_path))

let tests (caps : < Cap.readdir ; .. >) =
  Testo.categorize_suites "List_files"
    [
      Testo.categorize "list"
        [
          t "regular_file_as_root" (test_regular_file_as_root caps);
          t "empty_dir_as_root" (test_empty_dir_as_root caps);
          t "regular_files" (test_regular_files caps);
          t "symlinks" (test_symlinks caps);
          t "ignore_symlinks" (test_ignore_symlinks caps);
          t "symlink_as_root" (test_symlink_as_root caps);
        ];
    ]
