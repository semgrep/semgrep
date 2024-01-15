(* All the unit test suites for aliengrep. *)

let tests =
  Testo.categorize_suites "aliengrep"
    [ Unit_Pat_parser.tests; Unit_Match.tests ]
