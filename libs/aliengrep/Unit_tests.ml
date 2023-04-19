(* All the unit test suites for aliengrep. *)

let tests =
  Testutil.pack_suites "aliengrep" [ Unit_Pat_parser.tests; Unit_Match.tests ]
