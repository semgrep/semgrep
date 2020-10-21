(*
   Test pattern matching.
*)

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

let search pat doc_src doc =
  let matches = Match.search pat doc in
  Match.print doc_src matches;
  List.length matches

let run num_matches pat_str doc_str =
  let pat = Src_file.of_string pat_str |> Parse_pattern.of_src in
  let doc_src = Src_file.of_string doc_str in
  let doc = Parse_doc.of_src doc_src in
  Alcotest.(check int) "number of matches" num_matches (search pat doc_src doc)

let matcher_corpus = [
  "empty pattern", 1, "", "x";
  "word", 1, "hello", "hello";
  "simple fail", 0, "a", "ab cd";
  "sequence", 1, "a b", "a b c";
  "stutter", 1, "a b", "a a b";
  "cover parenthesized block", 1, "a (b c) d", "a (b c) d";

  "cover indented block", 1, "a b c d e",
  "\
a
  b
    c
  d
e
";

  "indented pattern", 0,
  "\
a
  b
c
",
  "a b c";

  "multiple matches", 3, "a", "a a a";
  "just dots", 1, "...", "a b";
  "dots", 1, "a...b", "a x y b";
  "unnecessary dots", 1, "a...b", "a b";
  "overnumerous dots", 1, "a ... ... c", "a b c";
  "double dots", 1, "a...b...c", "a x b x x c";
  "trailing dots", 1, "a ...", "a b";

  "dots in subblock mismatch", 0,
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

  "dots in subblock match", 1,
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

  "trailing dots in subblock", 1,
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

  "missing trailing dots in subblock", 0,
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

  "code match", 1,
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

  "match in same block", 1,
  "\
{
  a: 42
  ...
  b: 17
  ...
}
",
  "[ { a: 42, x: 1, b: 17 }, {} ]";

  "mismatch due to different blocks", 0,
  "\
{
  a: 42
  ...
  b: 17
  ...
}
",
  "[ { a: 42 }, { b: 17 } ]";

  "metavariable", 1, "$X $X", "yo yo";
  "metavariable mismatch", 0, "$X $X", "a b";
  "metavariable scope", 1,
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

  "multiple metavariables", 1,
  "$X $Y $X $Y",
  "a b a b";
]

let matcher_suite =
  List.map (fun (name, matches, pat_str, doc_str) ->
    name, `Quick, (fun () -> run matches pat_str doc_str)
  ) matcher_corpus

let test = "Matcher", [
  "pattern parser", `Quick, test_pattern_parser;
] @ matcher_suite
