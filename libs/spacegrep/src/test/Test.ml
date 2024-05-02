(*
   Entrypoint to run the unit tests from the command line.
*)

let tests () : Testo.t list =
  Spacegrep.Match.debug := true;
  Testo.categorize_suites "spacegrep"
    [ File_type.test; Parser.test; Matcher.test; Src_file.test; Comment.test ]
