(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Unit tests for Parsed_float
*)

let t = Testo.create
let fake_tok = Tok.unsafe_fake_tok ""
let float_opt_testable = Alcotest.(option (float 0.0001))

let test_parse_java_suffixes () =
  (* Java float suffix (f/F) *)
  let result, _ = Parsed_float.parse ("0.5f", fake_tok) in
  Alcotest.check float_opt_testable "0.5f should parse to 0.5" (Some 0.5) result;

  let result, _ = Parsed_float.parse ("0.5F", fake_tok) in
  Alcotest.check float_opt_testable "0.5F should parse to 0.5" (Some 0.5) result;

  (* Java double suffix (d/D) *)
  let result, _ = Parsed_float.parse ("1.5d", fake_tok) in
  Alcotest.check float_opt_testable "1.5d should parse to 1.5" (Some 1.5) result;

  let result, _ = Parsed_float.parse ("1.5D", fake_tok) in
  Alcotest.check float_opt_testable "1.5D should parse to 1.5" (Some 1.5) result;

  (* No suffix *)
  let result, _ = Parsed_float.parse ("2.5", fake_tok) in
  Alcotest.check float_opt_testable "2.5 should parse to 2.5" (Some 2.5) result

let test_parse_cpp_suffixes () =
  (* C/C++ float suffix (f/F) *)
  let result, _ = Parsed_float.parse ("3.14f", fake_tok) in
  Alcotest.check float_opt_testable "3.14f should parse to 3.14" (Some 3.14)
    result;

  (* C/C++ long double suffix (l/L) *)
  let result, _ = Parsed_float.parse ("2.71L", fake_tok) in
  Alcotest.check float_opt_testable "2.71L should parse to 2.71" (Some 2.71)
    result;

  let result, _ = Parsed_float.parse ("2.71l", fake_tok) in
  Alcotest.check float_opt_testable "2.71l should parse to 2.71" (Some 2.71)
    result

let test_parse_no_suffix () =
  let result, _ = Parsed_float.parse ("123.456", fake_tok) in
  Alcotest.check float_opt_testable "123.456 should parse" (Some 123.456) result;

  let result, _ = Parsed_float.parse ("0.0", fake_tok) in
  Alcotest.check float_opt_testable "0.0 should parse to 0.0" (Some 0.0) result;

  let result, _ = Parsed_float.parse ("1.0", fake_tok) in
  Alcotest.check float_opt_testable "1.0 should parse to 1.0" (Some 1.0) result

let test_parse_rust_suffixes () =
  (* Rust f32 suffix *)
  let result, _ = Parsed_float.parse ("0.5f32", fake_tok) in
  Alcotest.check float_opt_testable "0.5f32 should parse to 0.5" (Some 0.5)
    result;

  (* Rust f64 suffix *)
  let result, _ = Parsed_float.parse ("1.5f64", fake_tok) in
  Alcotest.check float_opt_testable "1.5f64 should parse to 1.5" (Some 1.5)
    result

let test_parse_invalid () =
  (* Invalid float literals should return None *)
  let result, _ = Parsed_float.parse ("not_a_float", fake_tok) in
  Alcotest.check float_opt_testable "not_a_float should fail to parse" None
    result;

  let result, _ = Parsed_float.parse ("", fake_tok) in
  Alcotest.check float_opt_testable "empty string should fail to parse" None
    result

let tests =
  Testo.categorize "Parsed_float"
    [
      t "parse Java float/double suffixes" test_parse_java_suffixes;
      t "parse C/C++ float/long double suffixes" test_parse_cpp_suffixes;
      t "parse Rust float suffixes" test_parse_rust_suffixes;
      t "parse float without suffix" test_parse_no_suffix;
      t "parse invalid float" test_parse_invalid;
    ]
