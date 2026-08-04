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

let check_parse_c_octal msg expected s =
  let result, _ = Parsed_int.parse_c_octal (s, fake_tok) in
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

(* [Parsed_int.parse] is the entry point for languages whose grammar has no
   C-style octal (Apex, Terraform, R). Leading zeros must be interpreted as
   decimal, not routed through the octal path. *)
let test_parse_leading_zero_is_decimal () =
  check_parse "08 is decimal 8" (Some 8L) "08";
  check_parse "0700 is decimal 700, not octal 448" (Some 700L) "0700";
  check_parse "09 is decimal 9" (Some 9L) "09"

let test_parse_c_octal () =
  (* Actual octals: leading 0 + only octal digits after. *)
  check_parse_c_octal "single zero" (Some 0L) "0";
  check_parse_c_octal "0700 is octal 448" (Some 448L) "0700";
  check_parse_c_octal "017 is octal 15" (Some 15L) "017";
  (* Legacy octals may use underscore digit separators (e.g. Go, Java). *)
  check_parse_c_octal "01_0 is octal 8, not decimal 10" (Some 8L) "01_0";
  check_parse_c_octal "0_700 with leading separator" (Some 448L) "0_700";
  (* An unprefixed leading-zero tail with a non-octal digit returns [None];
     C-style-octal callers should reject these as invalid rather than
     silently reinterpreting them as decimal. *)
  check_parse_c_octal "078 has a non-octal 8 and returns None" None "078";
  check_parse_c_octal "08 has a non-octal 8 and returns None" None "08";
  (* Non-octal leading-zero literals must be recognized via their explicit
     prefix ([0x], [0b], [0o]) rather than being interpreted as octal. *)
  check_parse_c_octal "hex zero" (Some 0L) "0x0";
  check_parse_c_octal "hex nonzero" (Some 31L) "0x1F";
  check_parse_c_octal "binary zero" (Some 0L) "0b0";
  check_parse_c_octal "binary nonzero" (Some 5L) "0b101"

let tests =
  Testo.categorize "Parsed_int"
    [
      t "parse numeric literals with underscore separators"
        test_parse_underscore_separators;
      t "parse invalid int" test_parse_invalid;
      t "parse treats leading zero as decimal"
        test_parse_leading_zero_is_decimal;
      t "parse_c_octal distinguishes octal from hex/binary" test_parse_c_octal;
    ]
