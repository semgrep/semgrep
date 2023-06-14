(*
   Test List_files
*)

(* warning 37 [unused-constructor]): constructor X is never used to build
   values. *)
[@@@warning "-37"]

open Printf
open File.Operators

type file_tree = Dir of string * file_tree list | File of string * file_kind
and file_kind = Regular of string | Symlink of string

let write_file path data =
  let oc = open_out_bin !!path in
  output_string oc data;
  close_out oc

let rec create_files parent (x : file_tree) =
  match x with
  | Dir (name, files) ->
      let dir_path = parent / name in
      Unix.mkdir !!dir_path 0o777;
      List.iter (create_files dir_path) files
  | File (name, Regular data) ->
      let path = parent / name in
      write_file path data
  | File (name, Symlink target_path) ->
      let link_path = parent / name in
      Unix.symlink target_path !!link_path

let rec delete_files parent (x : file_tree) =
  match x with
  | Dir (name, files) ->
      let dir_path = parent / name in
      List.iter (delete_files dir_path) files;
      Sys.rmdir !!dir_path
  | File (name, _) -> Sys.remove !!(parent / name)

(*
   Create a temporary file tree as specified. The user-specified function
   takes the root folder as argument and can assume that all the files
   were created according to the specification.
   Files are deleted automatically.
*)
let with_file_tree tree func =
  let workspace =
    Fpath.v (Filename.get_temp_dir_name ())
    / sprintf "test-list_files-%i" (Random.bits ())
  in
  Unix.mkdir !!workspace 0o777;
  Fun.protect
    ~finally:(fun () ->
      try Sys.rmdir !!workspace with
      | _ -> ())
    (fun () ->
      create_files workspace tree;
      Fun.protect
        ~finally:(fun () -> delete_files workspace tree)
        (fun () -> func workspace))

let test_regular_file_as_root () =
  with_file_tree
    (File ("hello", Regular "yo"))
    (fun workspace ->
      assert (List_files.list (workspace / "hello") = [ workspace / "hello" ]))

let test_empty_dir_as_root () =
  with_file_tree
    (Dir ("empty", []))
    (fun workspace -> assert (List_files.list (workspace / "empty") = []))

(* Because file listings are not guaranteed to be in any particular order. *)
let compare_path_lists expected actual =
  let sort x =
    List.sort Fpath.compare x |> Common.map ( !! ) |> String.concat "\n"
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
  Testutil.pack_suites "List_files"
    [
      Testutil.pack_tests "list"
        [
          ("regular_file_as_root", test_regular_file_as_root);
          ("empty_dir_as_root", test_empty_dir_as_root);
          ("regular_files", test_regular_files);
          ("symlinks", test_symlinks);
          ("ignore_symlinks", test_ignore_symlinks);
          ("symlink_as_root", test_symlink_as_root);
        ];
    ]
