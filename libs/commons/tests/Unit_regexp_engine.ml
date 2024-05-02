(*
   Unit tests for SPcre
*)

let t = Testo.create

let test_remove_eos_assertions () =
  let check (input, expected_output) =
    let output = Pcre2_.remove_end_of_string_assertions_from_string input in
    Alcotest.(check (option string)) input expected_output output
  in
  List.iter check
    [
      ("", Some "");
      ("A", Some "A");
      ("AA", Some "AA");
      ("AAA", Some "AAA");
      ("AAAA", Some "AAAA");
      ({|\$|}, Some {|\$|});
      ({|A\$|}, Some {|A\$|});
      ({|AA\$|}, Some {|AA\$|});
      ({|AAA\$|}, Some {|AAA\$|});
      ({|AAAA\$|}, Some {|AAAA\$|});
      ("^", Some "");
      ("$", Some "");
      ({|\A|}, Some "");
      ({|\Z|}, Some "");
      ({|\z|}, Some "");
      ("^$", Some "");
      ({|^\Z|}, Some "");
      ({|\A$|}, Some "");
      ({|\A\Z|}, Some "");
      ("^A$", Some "A");
      ("^AA$", Some "AA");
      ("^AAA$", Some "AAA");
      ("^^", None);
      ("$$", None);
      ({|A\A|}, Some {|A\A|});
      ("[$]*", None);
      ("(?:^)", None);
      ({|\\A|}, Some {|\\A|});
      ({|(?<!.|\n)|}, None);
      (* DIY beginning-of-string assertion = \A *)
      ({|(?!.|\n)|}, None) (* DIY end-of-string assertion = \z *);
    ]

let tests =
  Testo.categorize "regexp engine"
    [ t "remove eos assertions" test_remove_eos_assertions ]
