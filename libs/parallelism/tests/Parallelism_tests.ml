(* All unit tests for the parallelism library. *)

let tests =
  Testo.categorize_suites "Parallelism"
    [ Unit_Concurrent.tests; Unit_SharedMemo.tests; Unit_Hook.tests ]
