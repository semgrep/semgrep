(*
   Test regexp parsing on individual files in /tests
*)


open Common

let test_valid_files dialect rel_path () =
  let dir = Config_pfff.tests_path rel_path in
  let files = Common2.glob (spf "%s/*.regexp" dir) in
  files |> List.iter (fun file ->
    try
      let _ = Parse.parse ~conf:(Dialect.conf dialect) file in
      ()
    with exn ->
      Alcotest.failf "it should correctly parse %s (exn = %s)"
        file (Common.exn_to_s exn)
  )

let test_invalid_files dialect rel_path () =
  let dir = Config_pfff.tests_path rel_path in
  let files = Common2.glob (spf "%s/*.regexp" dir) in
  files |> List.iter (fun file ->
    try
      let _ast = Parse.file ~conf:(Dialect.conf dialect) file in
      Alcotest.failf "it should have thrown a Parse_error %s" file
    with
    | Parse_info.Parsing_error _ -> ()
    | exn -> Alcotest.failf "throwing wrong exn %s on %s"
               (Common.exn_to_s exn) file
  )

let tests =
  Testutil.pack_suites "regexp parsing" [
    Testutil.pack_tests "pcre" [
      "valid files",
      test_valid_files Dialect.PCRE "regexp/pcre/parsing";
      "invalid files",
      test_invalid_files Dialect.PCRE "regexp/pcre/parsing_errors";
    ];
    Testutil.pack_tests "pcre_extended" [
      "valid files",
      test_valid_files Dialect.PCRE_extended
        "regexp/pcre_extended/parsing";
      "invalid files",
      test_invalid_files Dialect.PCRE_extended
        "regexp/pcre_extended/parsing_errors";
    ];
    Testutil.pack_tests "perl_xx" [
      "valid files",
      test_valid_files Dialect.Perl_xx "regexp/perl_xx/parsing";
      "invalid files",
      test_invalid_files Dialect.Perl_xx "regexp/perl_xx/parsing_errors";
    ];
  ]
