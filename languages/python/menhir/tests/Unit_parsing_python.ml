(*
   Copyright (c) 2022-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
open Fpath_.Operators

let t = Testo.create

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(* ran from the root of the semgrep repository *)
let tests_path = Fpath.v "tests"

let match_statements () =
  Parse_python.parse_program
    (tests_path / "parsing" / "python" / "pattern_matching.py")
  |> ignore;
  Parse_python.program_of_string "match = case\ncase(match)\n" |> ignore

let tests =
  Testo.categorize "parsing_python"
    [
      t "Python structural pattern matching" match_statements;
      t "regression files" (fun () ->
          let dir = tests_path / "python" / "parsing" in
          let files = Common2.glob (dir / "*.py") in
          files
          |> List.iter (fun file ->
              Testutil.run !!file (fun () ->
                  Parse_python.parse_program file |> ignore)));
    ]
