(*
   Unit tests for the Src_file module
*)

open Printf
open Tree_sitter_run

(*
   Check that the behavior wrt to newlines is correct and the same
   whether we read from a file or from a string.
*)
let test_load_file () =
  let load_file data =
    let file, oc = Filename.open_temp_file "ocaml-tree-sitter-core-test" "" in
    Fun.protect
      (fun () ->
         Fun.protect
           (fun () ->
              output_string oc data
           )
           ~finally:(fun () -> close_out_noerr oc);
         Src_file.load_file file
      )
      ~finally:(fun () -> Sys.remove file)
  in
  let check expected_lines data =
    let lines_from_string = (Src_file.load_string data).lines in
    let lines_from_file = (load_file data).lines in
    Alcotest.(check bool) (sprintf "from_string %S" data) true
      (expected_lines = lines_from_string);
    Alcotest.(check bool) (sprintf "from_file [%S]" data) true
      (expected_lines = lines_from_file)
  in
  check [| "" |] "";
  check [| "\n"; "" |] "\n";
  check [| "a\n"; "" |] "a\n";
  check [| "a\n"; "b\r\n"; "c" |] "a\nb\r\nc";
  check [| "a\r\n"; "" |] "a\r\n"

let test_get_region () =
  let open Loc in
  let input = "012\n45\r\n89\n" in
  let src = Src_file.load_string input in
  let pos0 = { row = 0; column = 0 } in
  let pos1 = { row = 0; column = 1 } in
  let _pos2 = { row = 0; column = 2 } in
  let pos3 = { row = 0; column = 3 } in
  let pos4 = { row = 1; column = 0 } in
  let pos5 = { row = 1; column = 1 } in
  let pos6 = { row = 1; column = 2 } in
  let pos7 = { row = 1; column = 3 } in
  let pos8 = { row = 2; column = 0 } in
  let pos9 = { row = 2; column = 1 } in
  let pos10 = { row = 2; column = 2 } in
  let pos11 = { row = 2; column = 3 } in
  let pos12 = { row = 2; column = 3 } in
  let check expected start end_ =
    let actual = Src_file.get_region src start end_ in
    Alcotest.(check string) (sprintf "equal %S" expected) expected actual
  in
  check "" pos0 pos0;
  check "" pos1 pos0; (* invalid bounds *)
  check "" pos11 pos12; (* invalid bounds *)
  check "0" pos0 pos1;
  check "12" pos1 pos3;
  check "45" pos4 pos6;
  check "45\r" pos4 pos7;
  check "45\r\n8" pos4 pos9;
  check "9" pos9 pos10;
  check "9\n" pos9 pos11;
  check "89\n" pos8 pos12; (* invalid bounds *)
  check "\n" pos3 pos4;
  check "\n4" pos3 pos5;
  check "\n45\r\n" pos3 pos8

let test = "Src_file", [
  "load_file", `Quick, test_load_file;
  "get_region", `Quick, test_get_region;
]
