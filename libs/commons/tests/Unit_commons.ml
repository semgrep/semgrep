(*
   Test suite for this folder.
*)

open Printf

let t = Testo.create

(*
   This only checks the correctness of the results of the map function.
   For a benchmark, we could use and adapt
   https://gist.github.com/mjambon/95cfc818b7d2cb93fe5212495c3ab885

   We check:
   - correctness of the map result
   - left-to-right order
*)
let test_common_map =
  let list_lengths =
    [
      0;
      1;
      2;
      3;
      4;
      5;
      6;
      7;
      8;
      9;
      10;
      (* Lengths over the threshold beyond which the implementation switches
         to not using the stack. *)
      10_000;
      10_001;
      10_002;
      10_003;
      10_004;
      10_005;
    ]
  in
  let test len =
    let input = List.init len (fun i -> i) in
    let f x = x + 1 in
    let acc = ref [] in
    let expected_output = List_.map f input in
    let output =
      List_.map
        (fun x ->
          let res = f x in
          acc := res :: !acc;
          res)
        input
    in
    let order = List.rev !acc in
    assert (output = expected_output);
    assert (order = expected_output)
  in
  let tests =
    List_.map
      (fun len ->
        let name = sprintf "list length = %i" len in
        let test () = test len in
        t name test)
      list_lengths
  in
  tests

(*
   Check that both Windows (CRLF) and Unix line endings (LF) are removed
   when reading a line. This is a problem with Stdlib.input_line, which
   leaves a trailing '\r' when reading a file from Windows.
*)
let test_cat () =
  let contents = "hello\r\nworld\n!" in
  let file = Filename.temp_file "test_cat_" ".txt" in
  let oc = open_out_bin file in
  output_string oc contents;
  close_out oc;
  match UFile.Legacy.cat file with
  | [ "hello"; "world"; "!" ] -> ()
  | lines ->
      List.iteri (fun i line -> eprintf "line %i: %S\n" i line) lines;
      flush stderr;
      assert false

let test_readable () =
  Alcotest.(check string)
    "same string" "Bar.java"
    (Filename_.readable ~root:"." "./Bar.java");
  Alcotest.(check string)
    "same string" "Bar.java"
    (Filename_.readable ~root:"." "Bar.java");
  Alcotest.(check string)
    "same string" "a/b/Bar.java"
    (Filename_.readable ~root:"." "a/b/Bar.java");
  Alcotest.(check string)
    "same string" "Bar.java"
    (Filename_.readable ~root:"/a/b/" "/a/b/Bar.java");
  Alcotest.(check string)
    "same string" "Bar.java"
    (Filename_.readable ~root:"/a/b/" "/a/b//Bar.java");
  ()

let with_file contents f =
  let file, oc = Filename.open_temp_file "test_pfff_read_file_" ".dat" in
  Common.protect
    ~finally:(fun () ->
      close_out_noerr oc;
      Sys.remove file)
    (fun () ->
      output_string oc contents;
      close_out oc;
      f file)

(* We don't have tests for reading from named pipes (FIFO) because it would
   too complicated (involves two processes) and name pipes are useful mostly
   with Bash's process substitution feature.
*)
let test_read_file () =
  (* test file smaller than one filesystem block (most likely) *)
  let data = String.make 200 'A' in
  with_file data (fun file -> assert (UFile.Legacy.read_file file = data));
  (* test file larger than one filesystem block (most likely) *)
  let data = String.make 100_000 'A' in
  with_file data (fun file -> assert (UFile.Legacy.read_file file = data));
  (* test empty file *)
  let data = "" in
  with_file data (fun file -> assert (UFile.Legacy.read_file file = data));
  (* test partial read *)
  let data = String.make 8192 'A' in
  let max_len = 100 in
  with_file data (fun file ->
      assert (UFile.Legacy.read_file ~max_len file = String.sub data 0 max_len));
  (* test 0-length read *)
  let data = String.make 8192 'A' in
  let max_len = 0 in
  with_file data (fun file ->
      assert (UFile.Legacy.read_file ~max_len file = String.sub data 0 max_len))

let tests =
  Testo.categorize_suites "commons"
    [
      Testo.categorize_suites "common"
        [
          Testo.categorize "map" test_common_map;
          [ t "cat" test_cat ];
          [ t "readable" test_readable ];
          [ t "read_file" test_read_file ];
        ];
    ]
