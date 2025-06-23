(* All unit tests for the parallelism library. *)

let tests base =
  Testo.categorize_suites "Parallelism"
    [ Unit_Domains.tests base; Unit_SharedMemo.tests base ]
