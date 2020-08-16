(*
   Test pattern matching.
*)

open Spacegrep

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
  let pat = Parse_pattern.of_string pat_str in
  assert (
    pat =
    [
      Atom (Word "Hello"); Atom (Punct ','); Atom (Punct '(');
      Atom (Word "world"); Atom (Punct ')'); Atom (Punct '.');

      List [Atom (Word "foo"); Atom Dots; Atom (Word "bar")];

      Atom (Word "more"); Atom (Word "things"); Atom Dots;
      Atom (Metavar "Var_0")
    ]
  )

let run matches pat_str doc_str =
  let pat = Parse_pattern.of_string pat_str in
  let doc = Parse_doc.of_string doc_str in
  Alcotest.(check bool) "match" matches (Match.search pat doc)

let matcher_corpus = [
  "empty", true, "", "";
  "word", true, "hello", "hello";
  "simple fail", false, "a", "ab cd";
  "sequence", true, "a b", "a b c";
  "cover parenthesized block", true, "a (b c) d", "a (b c) d";

  "cover indented block", true, "a b c d e",
  "\
a
  b
    c
  d
e
";

  "indented pattern", false,
  "\
a
  b
c
",
  "a b c";

  "just dots", true, "...", "a b";
  "dots", true, "a...b", "a x y b";
  "unnecessary dots", true, "a...b", "a b";
  "double dots", true, "a...b...c", "a x b x x c";
  "trailing dots", true, "a ...", "a b";

  "dots in subblock mismatch", false,
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

  "dots in subblock match", true,
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
d
";

  "trailing dots in subblock", true,
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

  "code match", true,
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

  "match in same block", true,
  "\
{
  a: 42
  ...
  b: 17
  ...
}
",
  "[ { a: 42, x: true, b: 17 }, {} ]";

  "mismatch due to different blocks", false,
  "\
{
  a: 42
  ...
  b: 17
  ...
}
",
  "[ { a: 42 }, { b: 17 } ]";
]

let matcher_suite =
  List.map (fun (name, matches, pat_str, doc_str) ->
    name, `Quick, (fun () -> run matches pat_str doc_str)
  ) matcher_corpus

let test = "Matcher", [
  "pattern parser", `Quick, test_pattern_parser;
] @ matcher_suite
