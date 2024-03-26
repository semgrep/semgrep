(*
   Unit tests for Pcre_
*)

let t = Testo.create

let test_match_limit_ok () =
  let rex = Legacy_regex.regexp "(a+)+$" in
  match Legacy_regex.pmatch ~rex "aaaaaaaaaaaaaaaaa!" with
  | Ok _ -> ()
  | Error Pcre.MatchLimit ->
      Alcotest.fail "should not have failed with error MatchLimit"
  | Error _ -> Alcotest.fail "unexpected error"

let test_match_limit_fail () =
  let rex = Legacy_regex.regexp "(a+)+$" in
  match Legacy_regex.pmatch ~rex "aaaaaaaaaaaaaaaaaa!" with
  | Ok _ -> Alcotest.fail "should have failed with error MatchLimit"
  | Error Legacy_regex.MatchLimit -> ()
  | Error _ -> Alcotest.fail "unexpected error"

let test_register_exception_printer () =
  (* This is a little dirty since we can't undo it. *)
  Legacy_regex.register_exception_printer ();

  let msg =
    try
      ignore (Legacy_regex.regexp "???");
      Alcotest.fail "should have failed to compile the regexp"
    with
    | e -> Printexc.to_string e
  in
  Alcotest.(check string)
    "equal" "Pcre.Error(Pcre.BadPattern(\"nothing to repeat\", pos=0))" msg

let tests =
  Testo.categorize "pcre settings"
    [
      t "match limit ok" test_match_limit_ok;
      t "match limit fail" test_match_limit_fail;
      t "exception printer" test_register_exception_printer;
    ]
