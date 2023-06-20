(* Unit tests for Pat_AST *)

open Pat_AST

let slconf = Conf.default_singleline_conf
let mlconf = Conf.default_multiline_conf
let ast = Alcotest.testable (Fmt.of_to_string Pat_AST.show) ( = )

let check conf pat expected_ast =
  let res = Pat_parser.from_string conf pat in
  Alcotest.(check ast) "equal" expected_ast res

let test_literal_match () =
  check slconf "a bc!" [ Word "a"; Word "bc"; Other "!" ]

let test_parentheses () =
  check slconf "([x])"
    [ Bracket ('(', [ Bracket ('[', [ Word "x" ], ']') ], ')') ];
  check slconf "(})" [ Bracket ('(', [ Other "}" ], ')') ];
  check slconf "(" [ Other "(" ];
  check slconf "}" [ Other "}" ];
  check slconf "(}" [ Other "("; Other "}" ];
  check slconf "[(}]" [ Bracket ('[', [ Other "("; Other "}" ], ']') ];
  (* Uniline mode treats quotes as brackets *)
  check slconf "''" [ Bracket ('\'', [], '\'') ];
  check slconf "'ab'" [ Bracket ('\'', [ Word "ab" ], '\'') ];
  check slconf {|'a"b"'|}
    [ Bracket ('\'', [ Word "a"; Bracket ('"', [ Word "b" ], '"') ], '\'') ];
  (* Multiline mode doesn't treat quotes as brackets *)
  check mlconf {|'a"b"'|}
    [ Other "'"; Word "a"; Other {|"|}; Word "b"; Other {|"|}; Other "'" ]

let test_metavariables () =
  check slconf "$A $A $BB" [ Metavar "A"; Metavar "A"; Metavar "BB" ]

let test_ellipsis () = check slconf "a ... b" [ Word "a"; Ellipsis; Word "b" ]

let test_long_ellipsis () =
  check slconf "a .... b" [ Word "a"; Long_ellipsis; Word "b" ]

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
