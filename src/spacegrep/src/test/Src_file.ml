(*
   Test the formatting of matching results.
*)

open Spacegrep

let t = Testo.create
let highlight s = "[" ^ s ^ "]"

let test_highlight input start end_ expected_output =
  let output = Src_file.insert_highlight highlight input start end_ in
  Alcotest.(check string) "equal" expected_output output

let highlight_corpus =
  [
    (* name, input, start, end, expected output *)
    ("empty", "", 0, 0, "[]");
    ("no newline", "a", 0, 1, "[a]");
    ("trailing newline", "a\n", 0, 1, "[a]\n");
    ("sub", "abc\n", 1, 2, "a[b]c\n");
    ("multiline", "012\n45\n789", 1, 8, "0[12]\n[45]\n[7]89");
    ("empty lines", "012\n\n\n678\n", 0, 8, "[012]\n[]\n[]\n[67]8\n");
  ]

let rec find_word_locations word (ast : Doc_AST.node list) =
  List.concat_map (find_word_locations_in_node word) ast

and find_word_locations_in_node word node =
  match node with
  | Atom (loc, Word s) when s = word -> [ loc ]
  | Atom _ -> []
  | List l -> find_word_locations word l

let loc_of_word word input =
  let lexbuf = Lexing.from_string input in
  let ast = Parse_doc.of_lexbuf lexbuf in
  match find_word_locations word ast with
  | [ loc ] -> loc
  | _ -> assert false

(*
   Obtain correct token locations by parsing a document and searching
   for the words in the AST. This is easy to use but assumes locations
   set in the AST are correct.
*)
let test_lines_of_range input start_word end_word expected_output =
  let start_loc = loc_of_word start_word input in
  let end_loc = loc_of_word end_word input in
  let src = Src_file.of_string input in
  let lines = Src_file.list_lines_of_loc_range src start_loc end_loc in
  Alcotest.(check (list string)) "equal" expected_output lines

let lines_of_range_corpus =
  [
    (* test name, input, start word, end word, expected output *)
    ("simple", "a\n", "a", "a", [ "a" ]);
    ("no trailing newline", "a", "a", "a", [ "a" ]);
    ("partial", "a\nb c d e\nf\n", "c", "d", [ "b c d e" ]);
    ("multiline", "a\nb c\nd e\nf\n", "c", "d", [ "b c"; "d e" ]);
  ]

let test =
  Testo.categorize_suites "Src_file"
    [
      Testo.categorize "highlight"
        (List_.map
           (fun (name, input, start, end_, expected_output) ->
             t name (fun () -> test_highlight input start end_ expected_output))
           highlight_corpus);
      Testo.categorize "lines_of_range"
        (List_.map
           (fun (name, input, start_word, end_word, expected_output) ->
             t name (fun () ->
                 test_lines_of_range input start_word end_word expected_output))
           lines_of_range_corpus);
    ]
