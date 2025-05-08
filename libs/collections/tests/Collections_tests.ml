(* All unit tests for the collections library *)
let tests =
  Testo.categorize_suites "Collections"
    [ Unit_Hashtbl_.tests; Unit_List_.tests ]
