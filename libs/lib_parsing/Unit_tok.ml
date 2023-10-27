(*
   Unit tests for Tok
*)

let test_end_pos_of_loc () =
  let test ~str ~start ~expected =
    let line, column, bytepos = start in
    let expected_line, expected_col, expected_charpos = expected in
    let loc : Tok.location =
      { str; pos = { bytepos; line; column; file = "test.txt" } }
    in
    let line, col, charpos = Tok.end_pos_of_loc loc in
    assert (line = expected_line);
    assert (col = expected_col);
    assert (charpos = expected_charpos)
  in
  test ~str:"a" ~start:(1, 0, 0) ~expected:(1, 1, 1);
  test ~str:"a\n" ~start:(1, 0, 0) ~expected:(1, 2, 2);
  test ~str:"a\nb" ~start:(1, 0, 0) ~expected:(2, 1, 3);
  test ~str:"a\n\n" ~start:(1, 0, 0) ~expected:(2, 1, 3);
  test ~str:"\n    line1\n    line2\n" ~start:(2, 11, 17) ~expected:(4, 10, 38)

let tests =
  Testutil.pack_tests "tok" [ ("end_pos_of_loc", test_end_pos_of_loc) ]
