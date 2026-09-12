(*
   Test the Snippet module.

   TODO: test the 'extract' function
*)

open Printf
open Tree_sitter_run

let check_format_with_color (lines, expected_res) =
  let res = Snippet.format ~style:Color lines in
  printf "Highlighted result:\n%s" res;
  (* This is for copy-pasting the expected string into the OCaml test: *)
  printf "Same, OCaml-escaped: %S\n" res;
  printf "           Expected: %S\n" expected_res;
  Alcotest.(check string) "equal" expected_res res

let check_format_without_color (lines, expected_res) =
  let res = Snippet.format ~style:Text lines in
  printf "Highlighted result:\n%s" res;
  (* This is for copy-pasting the expected string into the OCaml test: *)
  printf "Same, OCaml-escaped: %S\n" res;
  printf "           Expected: %S\n" expected_res;
  Alcotest.(check string) "equal" expected_res res

let test_format_with_color () =
  (* The expected output is meant to be copy pasted from the test output. *)
  let data : (Snippet.t * string) list = [
    [
      [Normal "A"; Highlight "H"; Normal "B\n" ]
    ], "A\027[1;4;31mH\027[0mB\n";
    [
      [Normal "A\n"];
      [Normal "B"; Highlight "H"; Normal "C\n" ];
      [Normal "D\n"];
    ], "A\nB\027[1;4;31mH\027[0mC\nD\n";
    [
      [Normal "A\n"];
      [Highlight "H\n"];
      [Normal "B\n"];
    ], "A\n\027[1;4;31mH\n\027[0mB\n";
    (* Special case of highlighting a newline.
       Note that highlighting empty regions is handled by the 'extract'
       function by creating nonempty Highlight fragments for one character. *)
    [
      [Normal "highlight newline:"; Highlight "\n"];
      [Normal "N\n"]
    ], "highlight newline:\027[1;4;31m \n\027[0mN\n";
  ] in
  data
  |> List.iter check_format_with_color

let test_format_without_color () =
  (* The expected output is meant to be copy pasted from the test output. *)
  let data : (Snippet.t * string) list = [
    [
      [Normal "A"; Highlight "H"; Normal "B\n" ]
    ], "AHB\n ^ \n";
    [
      [Normal "A\n"];
      [Normal "B"; Highlight "H"; Normal "C\n" ];
      [Normal "D\n"];
    ], "A\nBHC\n ^ \nD\n";
    [
      [Normal "A\n"];
      [Highlight "H\n"];
      [Normal "B\n"];
    ], "A\nH\n^\nB\n";
    [
      [Normal "highlight newline:"; Highlight "\n"];
      [Normal "N\n"]
    ], "highlight newline: \n                  ^\nN\n";
  ] in
  data
  |> List.iter check_format_without_color

let highlight
    ~data ?(start_row = 0) ?(start_column = 0) ?end_row ?end_column () =
  let src = Src_file.load_string ~src_name:"test input" data in
  let end_row = Option.value end_row ~default:start_row in
  let end_column = Option.value end_column ~default:start_column in
  let start_pos : Loc.pos = { row = start_row; column = start_column } in
  let end_pos : Loc.pos = { row = end_row; column = end_column } in
  Snippet.extract ~start_pos ~end_pos src
  |> Snippet.format ~style:Text

let check_extract
    ~data ?start_row ?start_column ?end_row ?end_column
    expected_res =
  let res = highlight ~data ?start_row ?start_column ?end_row ?end_column () in
  printf "Highlighted result:\n%s" res;
  printf "Same, OCaml-escaped: %S\n" res;
  printf "           Expected: %S\n" expected_res;
  Alcotest.(check string) "equal" expected_res res

let test_extract () =
  check_extract
    ~data:"<- this"
    "<- this\n^      \n";
  check_extract
    ~data:"->this<-"
    ~start_column:2
    ~end_column:6
    "->this<-\n  ^^^^  \n";
  check_extract
    ~data:"this empty region: -><-"
    ~start_column:21
    ~end_column:21
    "this empty region: -><-\n                     ^ \n";
  check_extract
    ~data:"empty location at the end of next line:\n->\n"
    ~start_row:1
    ~start_column:2
    ~end_column:2
    "empty location at the end of next line:\n-> \n  ^\n\n";
  check_extract
    ~data:"empty location at the end of next line, no newline:\n->"
    ~start_row:1
    ~start_column:2
    ~end_column:2
    "empty location at the end of next line, no newline:\n-> \n  ^\n";
  check_extract
    ~data:"\240\159\152\129 <- this 4-byte emoji is incorrectly highlighted"
    ~end_column:4
    "\240\159\152\129 <- this 4-byte emoji is incorrectly highlighted\n^^^^                                                \n"

let test = "Snippet", [
  "highlight snippet with color", `Quick, test_format_with_color;
  "highlight snippet without color", `Quick, test_format_without_color;
  "extract and highlight snippet", `Quick, test_extract;
]
