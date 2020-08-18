(*
   Document parsing.
*)

open Spacegrep

let run_debug input expected_output =
  let output =
    input
    |> Src_file.of_string
    |> Parse_pattern.of_src ~is_doc:true
    |> Pattern_AST.as_doc
    |> Print.Debug.to_string
  in
  Alcotest.(check string) "equal" expected_output output

let run_pretty input expected_output =
  let output =
    input
    |> Src_file.of_string
    |> Parse_pattern.of_src ~is_doc:true
    |> Pattern_AST.as_doc
    |> Print.to_string
  in
  Alcotest.(check string) "equal" expected_output output

let debug_corpus = [
  "empty",
  "",
  "";

  "atom",
  "a",
  "\
Word 'a'
";

  "hello",
  "hello, world\n",
  "\
Word 'hello'
Punct ','
Word 'world'
";

  "indent",
  "\
a
    b
",
  "\
Word 'a'
List (
  Word 'b'
)
";

  "paragraph",
  "\
a b
c d
",
  "\
Word 'a'
Word 'b'
Word 'c'
Word 'd'
";

  "parens",
  "\
a (b [c{}]) d
",
  "\
Word 'a'
Punct '('
List (
  Word 'b'
  Punct '['
  List (
    Word 'c'
    Punct '{'
    List (
    )
    Punct '}'
  )
  Punct ']'
)
Punct ')'
Word 'd'
";

  "nothing to close",
  "\
a ) b
c )
) d
)
",
  "\
Word 'a'
Punct ')'
Word 'b'
Word 'c'
Punct ')'
Punct ')'
Word 'd'
Punct ')'
";

  "not closed",
  "\
a ( b
c (
( d
(
",
  "\
Word 'a'
Punct '('
Word 'b'
Word 'c'
Punct '('
Punct '('
Word 'd'
Punct '('
";

  "mismatched",
  "\
(]
",
  "\
Punct '('
Punct ']'
";

  "mismatched outside",
  "\
a ( {b} ] c
",
  "\
Word 'a'
Punct '('
Punct '{'
List (
  Word 'b'
)
Punct '}'
Punct ']'
Word 'c'
";

  "mismatched inside",
  "\
a ( {b] ) c
",
  "\
Word 'a'
Punct '('
Punct '{'
Word 'b'
Punct ']'
Punct ')'
Word 'c'
";

  "mismatched sequence",
  "\
a ( ] b { ) c
",
  "\
Word 'a'
Punct '('
Punct ']'
Word 'b'
Punct '{'
Punct ')'
Word 'c'
";

  "mismatched after",
  "\
a ( ) b { ] c
",
  "\
Word 'a'
Punct '('
List (
)
Punct ')'
Word 'b'
Punct '{'
Punct ']'
Word 'c'
";
]

let pretty_corpus = [
  "text parens multiline",
  "\
Je t'aime (moi
non plus).
",
  "\
Je
t
'
aime
(
moi
non
plus
)
.
";

  "text parens same line",
  "\
Je t'aime (moi non plus).
",
  "\
Je
t
'
aime
(
  moi
  non
  plus
)
.
"
]

let test =
  let suite =
    List.map (fun (name, input, expected_output) ->
      name, `Quick, (fun () -> run_debug input expected_output)
    ) debug_corpus
    @
    List.map (fun (name, input, expected_output) ->
      name, `Quick, (fun () -> run_pretty input expected_output)
    ) pretty_corpus
  in
  "Parser", suite
