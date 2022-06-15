open Common
open Testutil
module E = Semgrep_error_code

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Unit (and integration) tests exercising the parsers.
 *
 * See also the parsing tests in pfff/tests/
 *)

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

(* ran from _build/default/tests/ hence the '..'s below *)
let tests_path = "../../../tests"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let parsing_tests_for_lang files lang =
  files
  |> Common.map (fun file ->
         ( Filename.basename file,
           fun () ->
             Parse_target.parse_and_resolve_name_fail_if_partial lang file
             |> ignore ))

let partial_parsing_tests_for_lang files lang =
  files
  |> Common.map (fun file ->
         ( Filename.basename file,
           fun () ->
             let { Parse_target.skipped_tokens = errs; _ } =
               Parse_target.parse_and_resolve_name lang file
             in
             if errs = [] then
               Alcotest.fail
                 "it should parse partially the file (with some errors)" ))

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

(* This differs from pfff/tests/<lang>/parsing because here we also use
 * tree-sitter to parse; certain files do not parse with pfff but parses here
 *)
let lang_parsing_tests () =
  (* TODO: infer dir and ext from lang using Lang helper functions *)
  let pack_parsing_tests_for_lang lang dir ext =
    let slang = Lang.show lang in
    pack_tests slang
      (let dir = Filename.concat (Filename.concat tests_path dir) "parsing" in
       let files = Common2.glob (spf "%s/*%s" dir ext) in
       parsing_tests_for_lang files lang)
  in
  pack_suites "lang parsing testing"
    [
      (* languages with only a tree-sitter parser *)
      pack_parsing_tests_for_lang Lang.Bash "bash" ".bash";
      pack_parsing_tests_for_lang Lang.Csharp "csharp" ".cs";
      pack_parsing_tests_for_lang Lang.Dockerfile "dockerfile" ".dockerfile";
      pack_parsing_tests_for_lang Lang.Lua "lua" ".lua";
      pack_parsing_tests_for_lang Lang.Rust "rust" ".rs";
      pack_parsing_tests_for_lang Lang.Swift "swift" ".swift";
      pack_parsing_tests_for_lang Lang.Kotlin "kotlin" ".kt";
      pack_parsing_tests_for_lang Lang.Hack "hack" ".hack";
      (* here we have both a Pfff and tree-sitter parser *)
      pack_parsing_tests_for_lang Lang.Java "java" ".java";
      pack_parsing_tests_for_lang Lang.Go "go" ".go";
      pack_parsing_tests_for_lang Lang.Ruby "ruby" ".rb";
      pack_parsing_tests_for_lang Lang.Js "js" ".js";
      pack_parsing_tests_for_lang Lang.Scala "scala" ".scala";
      pack_parsing_tests_for_lang Lang.Html "html" ".html";
      pack_parsing_tests_for_lang Lang.Vue "vue" ".vue";
      pack_parsing_tests_for_lang Lang.Cpp "cpp" ".cpp";
      (* a few parsing tests where we expect some partials
       * See cpp/parsing_partial/
       *)
      pack_tests "C++ partial parsing"
        (let dir = Filename.concat tests_path "cpp/parsing_partial" in
         let files = Common2.glob (spf "%s/*.cpp" dir) in
         let lang = Lang.Cpp in
         partial_parsing_tests_for_lang files lang);
      pack_parsing_tests_for_lang Lang.Php "php" ".php";
      pack_parsing_tests_for_lang Lang.R "r" ".r";
    ]

(* It's important that our parsers generate classic parsing errors
 * exns (e.g., Parsing_error, Lexical_error), otherwise semgrep
 * will report some "Fatal error" and abort.
 *)
let parsing_error_tests () =
  let dir = Filename.concat tests_path "OTHER/parsing_errors" in
  pack_tests "Parsing error detection"
    (let tests = Common2.glob (spf "%s/*" dir) in
     tests
     |> Common.map (fun file ->
            ( Filename.basename file,
              fun () ->
                try
                  let lang = List.hd (Lang.langs_of_filename file) in
                  let res = Parse_target.just_parse_with_lang lang file in
                  if res.Parse_target.skipped_tokens = [] then
                    Alcotest.fail
                      "it should raise a standard parsing error exn or return \
                       partial errors "
                with
                | Parse_info.Lexical_error _
                | Parse_info.Parsing_error _ ->
                    () )))

let parsing_rules_tests () =
  let dir = Filename.concat tests_path "OTHER/rule_formats" in
  pack_tests "Parsing rules"
    (let tests =
       Common2.glob (spf "%s/*.yaml" dir) @ Common2.glob (spf "%s/*.json" dir)
       (* skipped for now to avoid adding jsonnet as a dependency in our
        * CI: Common2.glob (spf "%s/*.jsonnet" dir)
        *)
     in
     tests
     |> Common.map (fun file ->
            (Filename.basename file, fun () -> Parse_rule.parse file |> ignore)))

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

let tests () =
  List.flatten
    [ lang_parsing_tests (); parsing_error_tests (); parsing_rules_tests () ]
