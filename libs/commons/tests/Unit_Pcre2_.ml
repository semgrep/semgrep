(*
   Unit tests for Regex
*)

let t = Testo.create

let test_match_limit_ok () =
  let rex = Pcre2_.regexp "(a+)+$" in
  match Pcre2_.pmatch ~rex "aaaaaaaaaaaaaaaaa!" with
  | Ok _ -> ()
  | Error Pcre2.MatchLimit ->
      Alcotest.fail "should not have failed with error MatchLimit"
  | Error _ -> Alcotest.fail "unexpected error"

let test_register_exception_printer () =
  (* This is a little dirty since we can't undo it. *)
  Pcre2_.register_exception_printer ();

  let msg =
    try
      ignore (Pcre2_.regexp "???");
      Alcotest.fail "should have failed to compile the regexp"
    with
    | e -> Printexc.to_string e
  in
  Alcotest.(check string)
    "equal"
    "Pcre2.Error((Pcre2.BadPattern (\"quantifier does not follow a repeatable \
     item\", 0)))"
    msg

let tests =
  Testo.categorize "pcre2 settings"
    [
      t "match limit ok" test_match_limit_ok;
      t "exception printer" test_register_exception_printer;
    ]
