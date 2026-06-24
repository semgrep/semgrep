(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   All unit tests for the commons library
*)
let tests =
  Testo.categorize_suites "Commons"
    [
      Unit_immutable_buffer.tests;
      Unit_Pcre2_.tests;
      Unit_regexp_engine.tests;
      Unit_String_.tests;
      Unit_File.tests;
      Unit_Random_.tests;
      Unit_external.tests;
      Unit_Common.tests;
      Unit_Logs_.tests;
      Unit_UCmd.tests;
    ]
