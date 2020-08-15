(*
   Test pattern matching.
*)

open Spacegrep

let test_pattern_parser () =
  let open Pattern_AST in
  let pat_str = "\
Hello, (world).
  something

more things ...
$VARx
"
  in
  let pat = Parse_pattern.of_string pat_str in
  assert (
    pat =
    [
      Word "Hello"; Punct ','; Punct '('; Word "world"; Punct ')'; Punct '.';
      Word "something";
      Word "more"; Word "things"; Dots;
      Metavar "$VAR"; Word "x"
    ]
  )

let run matches pat_str doc_str =
  let pat = Parse_pattern.of_string pat_str in
  let doc = Parse.of_string doc_str in
  Alcotest.(check bool) "match" matches (Match.match_ pat doc)

let matcher_corpus = [
  "empty", true, "", "";
  "word", true, "hello", "hello";
  "simple fail", false, "a", "ab cd";
  "sequence", true, "a b", "a b c";
  "ignore child block", true, "a d", "a (b c) d";
  "sequence in child block", true, "a (b c", "a (b c) d";
]

let matcher_suite =
  List.map (fun (name, matches, pat_str, doc_str) ->
    name, `Quick, (fun () -> run matches pat_str doc_str)
  ) matcher_corpus

let test = "Matcher", [
  "pattern parser", `Quick, test_pattern_parser;
] @ matcher_suite
