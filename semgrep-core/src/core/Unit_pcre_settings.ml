(*
   Unit tests for Pcre_settings
*)

let test_register_exception_printer () =
  (* This is a little dirty since we can't undo it. *)
  Pcre_settings.register_exception_printer ();

  let msg =
    let rex = Pcre_settings.regexp "(a+)+$" in
    try Pcre.pmatch ~rex "aaaaaaaaaaaaaaaaaaaaaaaaaa!" |> assert false
    with e -> Printexc.to_string e
  in
  Alcotest.(check string) "equal" "Pcre.Error(MatchLimit)" msg

let tests =
  Testutil.pack_tests "pcre settings"
    [ ("exception printer", test_register_exception_printer) ]
