(*
   Copyright (c) 2026 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* Unit tests for Parsed_int *)

let t = Testo.create
let fake_tok = Tok.unsafe_fake_tok ""
let int64_opt_testable = Alcotest.(option int64)

let check_parse msg expected s =
  let result, _ = Parsed_int.parse (s, fake_tok) in
  Alcotest.check int64_opt_testable msg expected result

let test_parse_underscore_separators () =
  check_parse "decimal with underscores" (Some 1_000_000L) "1_000_000";
  check_parse "hex with underscores between digits only" (Some 0xDEAD_BEEFL)
    "0xDEAD_BEEF";
  (* Minimal cases: underscore immediately after the radix prefix (PEP 515). *)
  check_parse "hex with underscore after 0x prefix" (Some 1L) "0x_1";
  check_parse "hex with underscore after prefix and between digits"
    (Some 0xDEAD_BEEFL) "0x_dead_beef";
  check_parse "octal with underscore after 0o prefix" (Some 493L) "0o_755";
  check_parse "binary with underscore after 0b prefix" (Some 170L)
    "0b_1010_1010"

let test_parse_invalid () =
  (* Python: `_12345` is a name (ast.parse -> Name), not an integer literal. *)
  check_parse "leading underscore is not an int literal" None "_12345";
  check_parse "not an int" None "not_an_int";
  check_parse "empty string" None ""

let tests =
  Testo.categorize "Parsed_int"
    [
      t "parse numeric literals with underscore separators"
        test_parse_underscore_separators;
      t "parse invalid int" test_parse_invalid;
    ]
