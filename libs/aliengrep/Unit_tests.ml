(* All the unit test suites for aliengrep. *)

let tests =
  Alcotest_ext.pack_suites "aliengrep"
    [ Unit_Pat_parser.tests; Unit_Match.tests ]
