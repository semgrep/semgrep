let t = Testo.create

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Unit tests for our UFile, and UTmp modules. *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let with_string str f =
  UTmp.with_temp_file ~contents:str (fun fpath ->
      (* note that this uses open_in_bin internally *)
      UFile.with_open_in fpath (fun chan -> f chan))

(*****************************************************************************)
(* The tests *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests =
  Testo.categorize "File"
    [
      t "input_line LF" (fun () ->
          with_string "foo\nbar\n" (fun chan ->
              let str1 = input_line chan in
              let str2 = input_line chan in
              Alcotest.(check (list string))
                __LOC__ [ "foo"; "bar" ] [ str1; str2 ]));
      t "input_line CRLF" (fun () ->
          with_string "foo\r\nbar\r\n" (fun chan ->
              let str1 = input_line chan in
              let str2 = input_line chan in
              (* when using open_in_bin, there is no translation so we
               * still get those \r
               *)
              Alcotest.(check (list string))
                __LOC__ [ "foo\r"; "bar\r" ] [ str1; str2 ]));
      (* this is used sometimes in Windows and it's ugly *)
      t "input_line CR" (fun () ->
          with_string "foo\rbar\r" (fun chan ->
              (* OCaml input_line does not recognize those a CR as a newline *)
              let str1 = input_line chan in
              Alcotest.(check (list string)) __LOC__ [ "foo\rbar\r" ] [ str1 ]));
      t "Common.input_text_line CRLF" (fun () ->
          with_string "foo\r\nbar\r\n" (fun chan ->
              (* Common.input_text_line will perform some translation
               * and remove \r regardless of how the file was opened
               * (via open_in_bin or open_in)
               *)
              let str1 = Common.input_text_line chan in
              let str2 = Common.input_text_line chan in
              Alcotest.(check (list string))
                __LOC__ [ "foo"; "bar" ] [ str1; str2 ]));
    ]
