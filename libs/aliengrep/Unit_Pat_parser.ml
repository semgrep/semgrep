(* Unit tests for Pat_AST *)

open Pat_AST

let uconf = Conf.default_uniline_conf
let mconf = Conf.default_multiline_conf
let ast = Alcotest.testable (Fmt.of_to_string Pat_AST.show) ( = )

let check conf pat expected_ast =
  let res = Pat_parser.from_string conf pat in
  Alcotest.(check ast) "equal" expected_ast res

let test_literal_match () =
  check uconf "a bc!" [ Word "a"; Word "bc"; Other "!" ]

let test_parentheses () =
  check uconf "([x])"
    [ Bracket ('(', [ Bracket ('[', [ Word "x" ], ']') ], ')') ];
  check uconf "(})" [ Bracket ('(', [ Other "}" ], ')') ];
  check uconf "(" [ Other "(" ];
  check uconf "}" [ Other "}" ];
  check uconf "(}" [ Other "("; Other "}" ];
  check uconf "[(}]" [ Bracket ('[', [ Other "("; Other "}" ], ']') ];
  (* Uniline mode treats quotes as brackets *)
  check uconf "''" [ Bracket ('\'', [], '\'') ];
  check uconf "'ab'" [ Bracket ('\'', [ Word "ab" ], '\'') ];
  check uconf {|'a"b"'|}
    [ Bracket ('\'', [ Word "a"; Bracket ('"', [ Word "b" ], '"') ], '\'') ];
  (* Multiline mode doesn't treat quotes as brackets *)
  check mconf {|'a"b"'|}
    [ Other "'"; Word "a"; Other {|"|}; Word "b"; Other {|"|}; Other "'" ]

let test_metavariables () =
  check uconf "$A $A $BB" [ Metavar "A"; Metavar "A"; Metavar "BB" ]

let test_ellipsis () = check uconf "a ... b" [ Word "a"; Ellipsis; Word "b" ]

let test_long_ellipsis () =
  check uconf "a .... b" [ Word "a"; Long_ellipsis; Word "b" ]

let test_multiline () = ()

let tests =
  [
    ("literal_match", test_literal_match);
    ("parentheses", test_parentheses);
    ("metavariables", test_metavariables);
    ("ellipsis", test_ellipsis);
    ("long ellipsis", test_long_ellipsis);
    ("multiline", test_multiline);
  ]
