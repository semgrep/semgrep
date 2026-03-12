open Parsed_int
open Tok

let test_parse_c_octal_hex () =
  let tok = Tok.unsafe_fake_tok "test" in
  let (v1, _) = parse_c_octal ("0x1", tok) in
  let (v2, _) = parse_c_octal ("0x01", tok) in
  Alcotest.(check (option int64))
    "hex literals should normalize"
    v1
    v2

let tests =
  [
    Alcotest.test_case
      "parse_c_octal hex normalization"
      `Quick
      test_parse_c_octal_hex;
  ]