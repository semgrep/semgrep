let repr = Python_str_repr.repr

let make_test ?unicode_version (message, input, expected) =
  ( message,
    `Quick,
    fun () ->
      Alcotest.(check string) message expected (repr ?unicode_version input) )

let test_repr =
  List_.map make_test
    [
      ("empty", "", "''");
      ("simple", "a", "'a'");
      ("one single quote", {|'|}, {|"'"|});
      ("one single double quote", {|"|}, {|'"'|});
      ("tab character", "tab\tthis", {|'tab\tthis'|});
      ("line feed", "new\nline", {|'new\nline'|});
      ( "carraige return line feed",
        "Host: example.com\r\n",
        {|'Host: example.com\r\n'|} );
      ("non-printable ascii", "\x00\x7f\x20\x10", {|'\x00\x7f \x10'|});
      ( "mixed quotes",
        "\"I told him 'no' back then.\"",
        {|'"I told him \'no\' back then."'|} );
      ("mixed quotes, mainly single quotes", "'''\"'''", {|'\'\'\'"\'\'\''|});
      ("mixed quotes, mainly double quotes", "\"\"\"'\"\"\"", {|'"""\'"""'|});
    ]

let test_repr_unicode =
  List_.map make_test
    [ ("unicode 'æ'", "æ", "'æ'"); ("unicode '\u{80}'", "\u{80}", "'\\x80'") ]

let test_repr_unicode_version =
  [
    make_test ~unicode_version:(13, 0)
      ("'\\u061d' unicode version 13.0.0", "\u{061d}", "'\\u061d'");
    (* assuming uucp unicode version >= 14.0.0 *)
    make_test ("'\\u061d' unicode version >= 14.0.0", "\u{061d}", "'\u{061d}'");
  ]

let test_suites =
  [
    ("repr", test_repr);
    ("repr unicode", test_repr_unicode);
    ("repr unicode versions", test_repr_unicode_version);
  ]

let () = Alcotest.run "python-str-repr" test_suites
