(*
   Test pattern matching.
*)

open Printf
open Spacegrep

let t = Testo.create
let word s = Pattern_AST.(Atom (Loc.dummy, Word s))
let punct c = Pattern_AST.(Atom (Loc.dummy, Punct c))
let byte c = Pattern_AST.(Atom (Loc.dummy, Byte c))
let dots = Pattern_AST.(Dots (Loc.dummy, None))
let metavar s = Pattern_AST.(Atom (Loc.dummy, Metavar s))

let test_pattern_parser () =
  let open Pattern_AST in
  let pat_str =
    "Hello, (world).\n  foo\n  ...\n  bar\n\nmore things ...\n$Var_0\n"
  in
  let pat_src = Src_file.of_string pat_str in
  let pat = Parse_pattern.of_src pat_src |> Result.get_ok in
  assert (
    Pattern_AST.eq pat
      [
        word "Hello";
        punct ',';
        punct '(';
        word "world";
        punct ')';
        punct '.';
        List [ word "foo"; dots; word "bar" ];
        word "more";
        word "things";
        dots;
        metavar "Var_0";
        End;
      ])

(*
   Compare the structure of two unparsed documents.
*)
let doc_eq doc1_str doc2_str =
  let doc1 = Src_file.of_string doc1_str |> Parse_doc.of_src in
  let doc2 = Src_file.of_string doc2_str |> Parse_doc.of_src in
  Doc_AST.eq doc1 doc2

let matches_eq expected_doc_strings matches =
  let doc_strings =
    List_.map (fun (x : Match.match_) -> x.capture.value) matches
  in
  printf "=== expected matches ===\n";
  List.iter (fun s -> printf "%s\n--\n" s) expected_doc_strings;
  printf "=== actual matches ===\n";
  List.iter
    (fun (match_ : Match.match_) -> printf "%s\n--\n" match_.capture.value)
    matches;
  List.length expected_doc_strings = List.length doc_strings
  && List.for_all2 doc_eq expected_doc_strings doc_strings

let search param pat doc_src doc =
  let matches = Match.search param doc_src pat doc in
  Print_match.print doc_src matches;
  matches

let search_str param pat_str doc_str =
  let pat =
    Src_file.of_string pat_str |> Parse_pattern.of_src |> Result.get_ok
  in
  let doc_src = Src_file.of_string doc_str in
  let doc = Parse_doc.of_src doc_src in
  search param pat doc_src doc

let check_match_count param pat_str doc_str expected_num_matches =
  let matches = search_str param pat_str doc_str in
  let num_matches = List.length matches in
  Alcotest.(check int) "number of matches" expected_num_matches num_matches

let check_matches param pat_str doc_str expected_matches =
  let matches = search_str param pat_str doc_str in
  Alcotest.(check bool) "matches" true (matches_eq expected_matches matches)

type expectation = Count of int | Matches of string list

let check_matching param pat_str doc_str expectation =
  match expectation with
  | Count expected_num_matches ->
      check_match_count param pat_str doc_str expected_num_matches
  | Matches expected_matches ->
      check_matches param pat_str doc_str expected_matches

let matcher_corpus =
  [
    (* title, #matches, pattern, document *)
    ("empty pattern", Count 1, "", "x");
    ("word", Matches [ "hello" ], "hello", "hello");
    ("simple fail", Count 0, "a", "ab cd");
    ("sequence", Count 1, "a b", "a b c");
    ("stutter", Count 1, "a b", "a a b");
    ("search", Matches [ "a"; "a"; "a" ], "a", "42 a\n, b, a, c a");
    ("cover parenthesized block", Count 1, "a (b c) d", "a (b c) d");
    ("cover indented block", Count 1, "a b c d e", "a\n  b\n    c\n  d\ne\n");
    ("indented pattern", Count 0, "a\n  b\nc\n", "a b c");
    ("indented dots", Count 2, "{\n  ...\n}\n", "{}\n\n{\n}\n");
    ("multiple matches", Count 3, "a", "a a a");
    ("just dots", Count 1, "...", "a b");
    ("dots", Matches [ "a x y b" ], "a...b", "a x y b");
    ("unnecessary dots", Count 1, "a...b", "a b");
    ("overnumerous dots", Count 1, "a ... ... c", "a b c");
    ("double dots", Count 1, "a...b...c", "a x b x x c");
    ("trailing dots", Matches [ "a b" ], "a ...", "a b");
    ("dots in subblock mismatch", Count 0, "a\n  ...\n  b\n", "a\n  x\nb\n");
    ("dots in subblock match", Count 1, "a\n  ...\n  b\n", "a\n  x\n  b\nc\n");
    ("trailing dots in subblock", Count 1, "a\n  b\n  ...\n", "a\n  b\n  c\nd\n");
    ( "missing trailing dots in subblock",
      Count 0,
      "a\n  b\nd\n",
      "a\n  b\n  c\nd\n" );
    ("dots max span", Count 1, "0 ... 10", "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10");
    ( "dots overflow",
      Count 0,
      "0 ... 12",
      "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12" );
    ( "dots overflow barely",
      Count 0,
      "0 ... 12",
      "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n\n12" );
    ( "consecutive dots",
      Count 1,
      "0 ... ... 11",
      "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11" );
    ( "double dots overflow",
      Count 0,
      "0 ... ... 21",
      "0\n\
       1\n\
       2\n\
       3\n\
       4\n\
       5\n\
       6\n\
       7\n\
       8\n\
       9\n\
       10\n\
       11\n\
       12\n\
       13\n\
       14\n\
       15\n\
       16\n\
       17\n\
       18\n\
       19\n\
       20\n\
       21" );
    ( "trailing dots extravaganza",
      Matches [ "0 1 2 3 4 5" ],
      "0 ...",
      "0\n1\n2\n3\n4\n5\n" );
    ( "trailing dots limit",
      Matches [ "0 1 2 3 4 5 6 7 8 9 10" ],
      "0 ...",
      "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12" );
    ( "code match",
      Count 1,
      "function foo(x, y) { x = 42; ... y = x + 3; ... }",
      "/* This program does nothing useful. */\n\
       function foo(x, y) {\n\
      \  x = 42;\n\
      \  z = sqrt(x);\n\
      \  y = x + 3;\n\
      \  return x + y + z;\n\
       }\n" );
    ( "match in same block",
      Count 1,
      "{\n  a: 42\n  ...\n  b: 17\n  ...\n}\n",
      "[ { a: 42, x: 1, b: 17 }, {} ]" );
    ( "mismatch due to different blocks",
      Count 0,
      "{\n  a: 42\n  ...\n  b: 17\n  ...\n}\n",
      "[ { a: 42 }, { b: 17 } ]" );
    ("metavariable", Count 1, "$X $X", "yo yo");
    ("metavariable mismatch", Count 0, "$X $X", "a b");
    ("metavariable scope", Count 1, "a\n  $X\nb\n  $X\n", "a\n  x\nb\n  x\n");
    ("multiple metavariables", Count 1, "$X $Y $X $Y", "a b a b");
    ("dots metavariable", Count 1, "a $...X b $...X", "a x y b x y");
    ("dots metavariable mismatch", Count 0, "a $...X b $...X", "a x y b u v");
    ("multiple dots metavariable", Count 1, "a $...X b $...Y c", "a x b x x c");
    ("overnumerous dots metavariable", Count 1, "a $...X ... c", "a b c");
    ("trailing dots metavariable", Matches [ "a b c" ], "a $...X", "a b c");
    ("nested matches", Matches [ "a b b a"; "b b" ], "$A ... $A", "a b b a");
    ( "prefer shorter match",
      Matches [ "function foo" ],
      "function ... foo",
      "function x function foo" );
    ( "prefer shorter match 2",
      Matches [ "a 2 b 2"; "a 1 b 1" ],
      "a $N ... b $N",
      "a 1  a 2 b 2  a 1 b 1" );
    ("overlapping matches", Matches [ "1 2"; "2 3" ], "$A $B", "1 2 3");
    ( "shortest overlapping matches",
      Matches [ "1 2"; "2 3" ],
      "$A ... $B",
      "1 2 3" );
    ( "overlapping matches with closed end",
      Matches [ "a b" ],
      "a ... b",
      "a a b" );
    ("overlapping matches with open end", Matches [ "a a b" ], "a ...", "a a b");
    ("leading dots", Matches [ "a b" ], "... b", "a b c");
    ("match block start", Matches [ "a"; "b" ], "... $X", "a\n  b\n  c\nd\ne\n");
    ("match everything", Matches [ "a b c" ], "...", "a b c");
    ("case-sensitive", Matches [ "foo" ], "foo", "Foo or foo");
  ]

let matcher_corpus_case_insensitive =
  [ ("case-insensitive", Matches [ "Foo"; "foo" ], "foo", "Foo or foo") ]

let matcher_corpus_same_line_ellipsis =
  [ ("same-line ellipsis", Matches [ "a b" ], "a ...", "x\ny a b\nc\nd") ]

let matcher_corpus_two_line_ellipsis =
  [ ("two-line ellipsis", Matches [ "a b\nc" ], "a ...", "x\ny a b\nc\nd") ]

let create_matcher_suite ?expected_outcome param matcher_corpus =
  List_.map
    (fun (name, expectation, pat_str, doc_str) ->
      Testo.create ?expected_outcome name (fun () ->
          check_matching param pat_str doc_str expectation))
    matcher_corpus

let matcher_suite =
  let param = Match.create_search_param () in
  create_matcher_suite param matcher_corpus

let matcher_suite_case_insensitive =
  let param = Match.create_search_param ~case_sensitive:false () in
  create_matcher_suite param matcher_corpus_case_insensitive

let matcher_suite_same_line_ellipsis =
  let param = Match.create_search_param ~ellipsis_max_span:0 () in
  create_matcher_suite param matcher_corpus_same_line_ellipsis

let matcher_suite_two_line_ellipsis =
  let param = Match.create_search_param ~ellipsis_max_span:1 () in
  create_matcher_suite param matcher_corpus_two_line_ellipsis

let test =
  Testo.categorize "Matcher"
    ([ t "pattern parser" test_pattern_parser ]
    @ matcher_suite @ matcher_suite_case_insensitive
    @ matcher_suite_same_line_ellipsis @ matcher_suite_two_line_ellipsis)
