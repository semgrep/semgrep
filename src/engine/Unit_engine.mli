(*
   The test suites are defined from files found in the file system relative
   to the current location. Having them created on demand allows running
   'dune utop' from any location.
*)
val tests : unit -> Testutil.test list

(* Can be used from other test code to concisely run Semgrep *)
val match_pattern :
  lang:Lang.t ->
  hook:(Pattern_match.t -> unit) ->
  file:Fpath.t ->
  pattern:string ->
  fix_pattern:string option ->
  Pattern_match.t list

(*
   Generate a test suite for a list of languages.
   Two main folders are expected:
   - test_pattern_path: folder containing one subfolder per language
   - polyglot_pattern_path: folder containing patterns shared by multiple
     languages.

   Each language is specified as a triple:
   - language of type Lang.t e.g. Lang.Ruby
   - subdirectory containing the test cases e.g. "ruby"
   - language extension of the target files e.g. ".rb"

   Each language folder contains pairs of files:
   - foo.rb: target file for the test case "foo".
   - foo.sgrep: target file for the test case "foo". If missing, it must
     exist in the polyglot folder.
*)
val make_lang_regression_tests :
  test_pattern_path:Fpath.t ->
  polyglot_pattern_path:Fpath.t ->
  with_caching:bool ->
  (Lang.t * string * string) list ->
  Testutil.test list
