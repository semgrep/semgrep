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
   Entrypoint to run the unit tests from the command line.
*)

let tests () : Testo.t list =
  Spacegrep.Match.debug := true;
  Testo.categorize_suites "spacegrep"
    [ File_type.test; Parser.test; Matcher.test; Src_file.test; Comment.test ]
