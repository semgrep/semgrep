(*
   Unit tests for Tok
*)

let t = Testo.create

let token_testable : Tok.t Alcotest.testable =
  let pp fmt tok : unit =
    Common.save_excursion Tok.pp_full_token_info true (fun () -> Tok.pp fmt tok)
  in
  Alcotest.testable pp Tok.equal

let check_token msg expected result =
  Alcotest.check token_testable msg expected result

let test_end_pos_of_loc () =
  let test ~str ~start ~expected =
    let line, column, bytepos = start in
    let expected_line, expected_col, expected_charpos = expected in
    let loc : Tok.location =
      { str; pos = { bytepos; line; column; file = Fpath.v "test.txt" } }
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

let test_combine_sparse_toks () =
  let tok str line column bytepos : Tok.t =
    let pos : Pos.t = { bytepos; line; column; file = Fpath_.fake_file } in
    OriginTok { str; pos }
  in
  let test tokens expected_combined_token =
    let first_tok, other_toks =
      match tokens with
      | x :: xs -> (x, xs)
      | [] -> assert false
    in
    let res =
      Tok.combine_sparse_toks ~ignorable_newline:"\\\n" ~ignorable_blank:'-'
        first_tok other_toks
    in
    match res with
    | None -> assert false
    | Some res -> check_token "equal" expected_combined_token res
  in
  (*
     Here, we test only realistic cases of token sequences that
     are ordered by byte position and don't overlap.
  *)
  (* tok arguments: string, line, column, bytepos *)
  test [ tok "a" 1 0 0 ] (tok "a" 1 0 0);
  test [ tok "a" 1 1 1 ] (tok "a" 1 1 1);
  test [ tok "a" 1 0 0; tok "b" 1 1 1 ] (tok "ab" 1 0 0);
  test [ tok "a" 1 0 0; tok "b" 1 2 2 ] (tok "a-b" 1 0 0);
  test [ tok "a" 1 0 0; tok "b" 1 3 3 ] (tok "a--b" 1 0 0);
  test [ tok "a" 1 0 0; tok "b" 2 0 2 ] (tok "a\\\nb" 1 0 0);
  test [ tok "a" 1 0 0; tok "b" 2 0 3 ] (tok "a-\\\nb" 1 0 0);
  test [ tok "a" 1 0 0; tok "b" 2 1 3 ] (tok "a\\\n-b" 1 0 0);
  test [ tok "a" 1 0 0; tok "b" 2 1 5 ] (tok "a--\\\n-b" 1 0 0);
  test [ tok "a\n" 1 0 0; tok "b" 2 1 3 ] (tok "a\n-b" 1 0 0)

let tests =
  Testo.categorize "Tok"
    [
      t "end_pos_of_loc" test_end_pos_of_loc;
      t "combine_sparse_toks" test_combine_sparse_toks;
    ]
