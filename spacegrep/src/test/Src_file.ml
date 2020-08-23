(*
   Test the formatting of matching results.
*)

open Spacegrep

let highlight s =
  "[" ^ s ^ "]"

let run_one input start end_ expected_output =
  let output = Src_file.insert_highlight highlight input start end_ in
  Alcotest.(check string) "equal" expected_output output

let corpus = [
  (* name, input, start, end, expected output *)
  "empty", "", 0, 0, "[]";
  "no newline", "a", 0, 1, "[a]";
  "trailing newline", "a\n", 0, 1, "[a]\n";
  "sub", "abc\n", 1, 2, "a[b]c\n";
  "multiline", "012\n45\n789", 1, 8, "0[12]\n[45]\n[7]89";
  "empty lines", "012\n\n\n678\n", 0, 8, "[012]\n[]\n[]\n[67]8\n";
]

let test =
  let suite =
    List.map (fun (name, input, start, end_, expected_output) ->
      name, `Quick, (fun () -> run_one input start end_ expected_output)
    ) corpus
  in
  "Src_file", suite
