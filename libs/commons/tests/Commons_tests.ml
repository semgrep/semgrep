(*
   All unit tests for the commons library
*)
let tests =
  Testo.categorize_suites "Commons"
    [
      Unit_Hashtbl_.tests;
      Unit_immutable_buffer.tests;
      Unit_Pcre_.tests;
      Unit_Pcre2_.tests;
      Unit_regexp_engine.tests;
      Unit_String_.tests;
      Unit_List_.tests;
      Unit_File.tests;
    ]
