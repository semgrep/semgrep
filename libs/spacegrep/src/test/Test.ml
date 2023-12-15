(*
   Entrypoint to run the unit tests from the command line.
*)

let tests () : Alcotest_ext.test list =
  Spacegrep.Match.debug := true;
  Alcotest_ext.pack_suites "spacegrep"
    [ File_type.test; Parser.test; Matcher.test; Src_file.test; Comment.test ]
