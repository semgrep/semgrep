(*
   Test suite for this folder.
*)

open Printf

(*
   This only checks the correctness of the results of the map function.
   For a benchmark, we could use and adapt
   https://gist.github.com/mjambon/95cfc818b7d2cb93fe5212495c3ab885

   We check:
   - correctness of the map result
   - left-to-right order
*)
let test_common_map =
  let list_lengths = [
    0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10;
    (* Lengths over the threshold beyond which the implementation switches
       to not using the stack. *)
    10_000; 10_001; 10_002; 10_003; 10_004; 10_005;
  ]
  in
  let test len =
    let input = List.init len (fun i -> i) in
    let f x = x + 1 in
    let acc = ref [] in
    let expected_output = List.map f input in
    let output =
      Common.map (fun x ->
        let res = f x in
        acc := res :: !acc;
        res
      ) input
    in
    let order = List.rev !acc in
    assert (output = expected_output);
    assert (order = expected_output)
  in
  let tests =
    List.map (fun len ->
      let name = sprintf "list length = %i" len in
      let test () = test len in
      (name, test)
    ) list_lengths
  in
  tests

(*
   Check that both Windows (CRLF) and Unix line endings (LF) are removed
   when reading a line. This is a problem with Stdlib.input_line, which
   leaves a trailing '\r' when reading a file from Windows.
*)
let test_cat () =
  let contents = "\
hello\r\n\
world\n\
!"
  in
  let file = Filename.temp_file "test_cat_" ".txt" in
  let oc = open_out_bin file in
  output_string oc contents;
  close_out oc;
  match Common.cat file with
  | ["hello"; "world"; "!"] -> ()
  | lines ->
      List.iteri (fun i line -> eprintf "line %i: %S\n" i line) lines;
      flush stderr;
      assert false

let test_readable () =
  Alcotest.(check string) "same string" "Bar.java"
    (Common.readable ~root:"." "./Bar.java");
  Alcotest.(check string) "same string" "Bar.java"
    (Common.readable ~root:"." "Bar.java");
  Alcotest.(check string) "same string" "a/b/Bar.java"
    (Common.readable ~root:"." "a/b/Bar.java");
  Alcotest.(check string) "same string" "Bar.java"
    (Common.readable ~root:"/a/b/" "/a/b/Bar.java");
  Alcotest.(check string) "same string" "Bar.java"
    (Common.readable ~root:"/a/b/" "/a/b//Bar.java");
  ()

let with_file contents f =
  let file, oc = Filename.open_temp_file "test_pfff_read_file_" ".dat" in
  Fun.protect
    ~finally:(fun () ->
      close_out_noerr oc;
      Sys.remove file
    )
    (fun () ->
       output_string oc contents;
       close_out oc;
       f file
    )

(* We don't have tests for reading from named pipes (FIFO) because it would
   too complicated (involves two processes) and name pipes are useful mostly
   with Bash's process substitution feature.
*)
let test_read_file () =
  (* test file smaller than one filesystem block (most likely) *)
  let data = String.make 200 'A' in
  with_file data (fun file ->
    assert (Common.read_file file = data)
  );
  (* test file larger than one filesystem block (most likely) *)
  let data = String.make 100_000 'A' in
  with_file data (fun file ->
    assert (Common.read_file file = data)
  );
  (* test empty file *)
  let data = "" in
  with_file data (fun file ->
    assert (Common.read_file file = data)
  );
  (* test partial read *)
  let data = String.make 8192 'A' in
  let max_len = 100 in
  with_file data (fun file ->
    assert (Common.read_file ~max_len file = String.sub data 0 max_len)
  );
  (* test 0-length read *)
  let data = String.make 8192 'A' in
  let max_len = 0 in
  with_file data (fun file ->
    assert (Common.read_file ~max_len file = String.sub data 0 max_len)
  )

(* TODO: we should use Unix.realpath! but only available in 4.13 *)
let realpath s = Common.fullpath s

let test_path_conversion () =
  let check_path path =
    FPath.to_string (FPath.of_string path) = realpath path
  in
  let data = String.make 150 'v' in
  assert (check_path ".");
  assert (check_path "..");
  assert (check_path "../..");
  assert (FPath.to_string (FPath.of_string "/") = realpath "/");
  with_file data (fun file ->
    let path = FPath.of_string file in
    let max_len = 24 in
    assert (FPath.to_string path = realpath file);
    assert (FPath.read_file path = data);
    assert (FPath.cat path = [data]);
    assert (FPath.read_file ~max_len path = String.sub data 0 max_len);
    assert (FPath.file_exists path);
    assert (not (FPath.is_directory path))
  );
  assert (FPath.to_string (FPath.(of_string "." / "." / ".." / "." / "..")) = realpath "../..")


let tests =
  Testutil.pack_suites "commons" [
    Testutil.pack_suites "common" [
      Testutil.pack_tests "map" test_common_map;
      ["cat", test_cat];
      ["readable", test_readable];
      ["read_file", test_read_file];
      ["path_conversion", test_path_conversion]
    ]
  ]
