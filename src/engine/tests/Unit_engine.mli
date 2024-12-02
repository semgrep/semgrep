(*
   The test suites are defined from files found in the file system relative
   to the current location. Having them created on demand allows running
   'dune utop' from any location.
*)
val tests : unit -> Testo.t list

type fix_type =
  | Fix of string
  | FixRegex of
      (* regex *) string * (* count *) int option * (* replacement *) string
  | NoFix

(* Can be used from other test code to concisely run Semgrep *)
val match_pattern :
  lang:Lang.t ->
  hook:(Core_match.t -> unit) ->
  file:Fpath.t ->
  pattern:string ->
  fix:fix_type ->
  Core_match.t list

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

  Used in Unit_pro_languages.ml
*)
val make_lang_regression_tests :
  test_pattern_path:Fpath.t ->
  polyglot_pattern_path:Fpath.t ->
  (Lang.t * string * string) list ->
  Testo.t list

(* coupling: https://semgrep.dev/docs/language-support/
 * See also https://r2c.quip.com/FOAuA4ThzULc/How-to-promote-a-language-
 *)
type maturity_level = GA | Beta | Experimental
[@@deriving show { with_path = false }]

val make_maturity_tests :
  ?lang_exn:(Lang.t * string list) list (* skip list *) ->
  Lang.t ->
  string (* dir *) ->
  string (* ext *) ->
  maturity_level ->
  Testo.t list
