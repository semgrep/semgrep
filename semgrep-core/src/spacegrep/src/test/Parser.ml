(*
   Test document parsing.
*)

open Spacegrep

let run_debug input expected_output =
  let output =
    input |> Src_file.of_string
    |> Parse_pattern.of_src ~is_doc:true
    |> Result.get_ok |> Pattern_AST.as_doc |> Print.Debug.to_string
  in
  Alcotest.(check string) "equal" expected_output output

let run_pretty input expected_output =
  let output =
    input |> Src_file.of_string
    |> Parse_pattern.of_src ~is_doc:true
    |> Result.get_ok |> Pattern_AST.as_doc |> Print.to_string
  in
  Alcotest.(check string) "equal" expected_output output

let debug_corpus =
  [
    ("empty", "", "");
    ("atom", "a", "Word 'a'\n");
    ("hello", "hello, world\n", "Word 'hello'\nPunct ','\nWord 'world'\n");
    ("indent", "a\n    b\n", "Word 'a'\nList (\n  Word 'b'\n)\n");
    ("paragraph", "a b\nc d\n", "Word 'a'\nWord 'b'\nWord 'c'\nWord 'd'\n");
    ( "parens",
      "a (b [c{}]) d\n",
      "Word 'a'\n\
       Punct '('\n\
       List (\n\
      \  Word 'b'\n\
      \  Punct '['\n\
      \  List (\n\
      \    Word 'c'\n\
      \    Punct '{'\n\
      \    Punct '}'\n\
      \  )\n\
      \  Punct ']'\n\
       )\n\
       Punct ')'\n\
       Word 'd'\n" );
    ( "nothing to close",
      "a ) b\nc )\n) d\n)\n",
      "Word 'a'\n\
       Punct ')'\n\
       Word 'b'\n\
       Word 'c'\n\
       Punct ')'\n\
       Punct ')'\n\
       Word 'd'\n\
       Punct ')'\n" );
    ( "not closed",
      "a ( b\nc (\n( d\n(\n",
      "Word 'a'\n\
       Punct '('\n\
       Word 'b'\n\
       Word 'c'\n\
       Punct '('\n\
       Punct '('\n\
       Word 'd'\n\
       Punct '('\n" );
    ("mismatched", "(]\n", "Punct '('\nPunct ']'\n");
    ( "mismatched outside",
      "a ( {b} ] c\n",
      "Word 'a'\n\
       Punct '('\n\
       Punct '{'\n\
       List (\n\
      \  Word 'b'\n\
       )\n\
       Punct '}'\n\
       Punct ']'\n\
       Word 'c'\n" );
    ( "mismatched inside",
      "a ( {b] ) c\n",
      "Word 'a'\n\
       Punct '('\n\
       Punct '{'\n\
       Word 'b'\n\
       Punct ']'\n\
       Punct ')'\n\
       Word 'c'\n" );
    ( "mismatched sequence",
      "a ( ] b { ) c\n",
      "Word 'a'\n\
       Punct '('\n\
       Punct ']'\n\
       Word 'b'\n\
       Punct '{'\n\
       Punct ')'\n\
       Word 'c'\n" );
    ( "mismatched after",
      "a ( ) b { ] c\n",
      "Word 'a'\n\
       Punct '('\n\
       Punct ')'\n\
       Word 'b'\n\
       Punct '{'\n\
       Punct ']'\n\
       Word 'c'\n" );
  ]

let pretty_corpus =
  [
    ( "text parens multiline",
      "Je t'aime (moi\nnon plus).\n",
      "Je\nt\n'\naime\n(\nmoi\nnon\nplus\n)\n.\n" );
    ( "text parens same line",
      "Je t'aime (moi non plus).\n",
      "Je\nt\n'\naime\n(\n  moi\n  non\n  plus\n)\n.\n" );
  ]

let test =
  let suite =
    List.map
      (fun (name, input, expected_output) ->
        (name, `Quick, fun () -> run_debug input expected_output))
      debug_corpus
    @ List.map
        (fun (name, input, expected_output) ->
          (name, `Quick, fun () -> run_pretty input expected_output))
        pretty_corpus
  in
  ("Parser", suite)
