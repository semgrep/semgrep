(*
   Test pattern matching.
*)

open Printf
open Spacegrep

let word s = Pattern_AST.(Atom (Loc.dummy, Word s))
let punct c = Pattern_AST.(Atom (Loc.dummy, Punct c))
let byte c = Pattern_AST.(Atom (Loc.dummy, Byte c))
let dots = Pattern_AST.(Dots Loc.dummy)
let metavar s = Pattern_AST.(Atom (Loc.dummy, Metavar s))

let test_pattern_parser () =
  let open Pattern_AST in
  let pat_str = "\
Hello, (world).
  foo
  ...
  bar

more things ...
$Var_0
"
  in
  let pat_src = Src_file.of_string pat_str in
  let pat = Parse_pattern.of_src pat_src in
  assert (
    Pattern_AST.eq pat
      [
        word "Hello"; punct ','; punct '(';
        word "world"; punct ')'; punct '.';

        List [word "foo"; dots; word "bar"];

        word "more"; word "things"; dots;
        metavar "Var_0";
        End;
      ]
  )

(*
   Compare the structure of two unparsed documents.
*)
let doc_eq doc1_str doc2_str =
  let doc1 = Src_file.of_string doc1_str |> Parse_doc.of_src in
  let doc2 = Src_file.of_string doc2_str |> Parse_doc.of_src in
  Doc_AST.eq doc1 doc2

let matches_eq expected_doc_strings matches =
  let doc_strings =
    List.map (fun (x : Match.match_) -> x.capture.value) matches in
  printf "=== expected matches ===\n";
  List.iter (fun s ->
    printf "%s\n--\n" s
  ) expected_doc_strings;
  printf "=== actual matches ===\n";
  List.iter (fun (match_ : Match.match_) ->
    printf "%s\n--\n" match_.capture.value
  ) matches;
  (List.length expected_doc_strings = List.length doc_strings
   && List.for_all2 doc_eq expected_doc_strings doc_strings)

let search pat doc_src doc =
  let matches = Match.search doc_src pat doc in
  Match.print doc_src matches;
  matches

let search_str pat_str doc_str =
  let pat = Src_file.of_string pat_str |> Parse_pattern.of_src in
  let doc_src = Src_file.of_string doc_str in
  let doc = Parse_doc.of_src doc_src in
  search pat doc_src doc

let check_match_count pat_str doc_str expected_num_matches =
  let matches = search_str pat_str doc_str in
  let num_matches = List.length matches in
  Alcotest.(check int) "number of matches" expected_num_matches num_matches

let check_matches pat_str doc_str expected_matches =
  let matches = search_str pat_str doc_str in
  Alcotest.(check bool) "matches" true (matches_eq expected_matches matches)

type expectation = Count of int | Matches of string list

let check_matching pat_str doc_str expectation =
  match expectation with
  | Count expected_num_matches ->
      check_match_count pat_str doc_str expected_num_matches
  | Matches expected_matches ->
      check_matches pat_str doc_str expected_matches

let matcher_corpus = [
  (* title, #matches, pattern, document *)
  "empty pattern", Count 1, "", "x";
  "word", Matches ["hello"], "hello", "hello";
  "simple fail", Count 0, "a", "ab cd";
  "sequence", Count 1, "a b", "a b c";
  "stutter", Count 1, "a b", "a a b";
  "search", Matches ["a"; "a"; "a"], "a", "42 a\n, b, a, c a";
  "cover parenthesized block", Count 1, "a (b c) d", "a (b c) d";

  "cover indented block", Count 1, "a b c d e",
  "\
a
  b
    c
  d
e
";

  "indented pattern", Count 0,
  "\
a
  b
c
",
  "a b c";

  "indented dots", Count 2,
  "\
{
  ...
}
",
  "\
{}

{
}
";

  "multiple matches", Count 3, "a", "a a a";
  "just dots", Count 1, "...", "a b";
  "dots", Matches ["a x y b"], "a...b", "a x y b";
  "unnecessary dots", Count 1, "a...b", "a b";
  "overnumerous dots", Count 1, "a ... ... c", "a b c";
  "double dots", Count 1, "a...b...c", "a x b x x c";
  "trailing dots", Matches ["a b"], "a ...", "a b";

  "dots in subblock mismatch", Count 0,
  "\
a
  ...
  b
",
  "\
a
  x
b
";

  "dots in subblock match", Count 1,
  "\
a
  ...
  b
",
  "\
a
  x
  b
c
";

  "trailing dots in subblock", Count 1,
  "\
a
  b
  ...
",
  "\
a
  b
  c
d
";

  "missing trailing dots in subblock", Count 0,
  "\
a
  b
d
",
  "\
a
  b
  c
d
";

  "dots max span", Count 1, "0 ... 10",
  "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10";

  "dots overflow", Count 0, "0 ... 12",
  "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12";

  "dots overflow barely", Count 0, "0 ... 12",
  "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n\n12";

  "double dots", Count 1, "0 ... ... 11",
  "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11";

  "double dots overflow", Count 0, "0 ... ... 21",
  "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n\
   10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n20\n21";

  "trailing dots", Matches ["0 1 2 3 4 5"], "0 ...",
  "0\n1\n2\n3\n4\n5\n";

  "trailing dots limit", Matches ["0 1 2 3 4 5 6 7 8 9 10"], "0 ...",
  "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12";

  "code match", Count 1,
  "function foo(x, y) { x = 42; ... y = x + 3; ... }",
  "\
/* This program does nothing useful. */
function foo(x, y) {
  x = 42;
  z = sqrt(x);
  y = x + 3;
  return x + y + z;
}
";

  "match in same block", Count 1,
  "\
{
  a: 42
  ...
  b: 17
  ...
}
",
  "[ { a: 42, x: 1, b: 17 }, {} ]";

  "mismatch due to different blocks", Count 0,
  "\
{
  a: 42
  ...
  b: 17
  ...
}
",
  "[ { a: 42 }, { b: 17 } ]";

  "metavariable", Count 1, "$X $X", "yo yo";
  "metavariable mismatch", Count 0, "$X $X", "a b";
  "metavariable scope", Count 1,
  "\
a
  $X
b
  $X
",
  "\
a
  x
b
  x
";

  "multiple metavariables", Count 1,
  "$X $Y $X $Y",
  "a b a b";
]

let matcher_suite =
  List.map (fun (name, expectation, pat_str, doc_str) ->
    name, `Quick, (fun () -> check_matching pat_str doc_str expectation)
  ) matcher_corpus

let test = "Matcher", [
  "pattern parser", `Quick, test_pattern_parser;
] @ matcher_suite
