(*
   All unit tests for the commons library
*)
let tests =
  Testo.categorize_suites "Commons"
    [
      Unit_Hashtbl_.tests;
      Unit_immutable_buffer.tests;
      Unit_Regex.tests;
      Unit_Legacy_Regex.tests;
      Unit_regexp_engine.tests;
      Unit_String_.tests;
      Unit_List_.tests;
    ]
