open Common
open Fpath_.Operators
module E = Core_error

let t = Testo.create

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Unit (and integration) tests exercising the parsers *)

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

(* ran from the root of the semgrep repository *)
let tests_path = Fpath.v "tests"
let tests_path_parsing = tests_path / "parsing"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type error_tolerance = Strict | Missing_tokens | Partial_parsing

(*
   Strict parsing: errors due to missing (inserted) tokens are not tolerated
   in these tests.
*)
let parsing_tests_for_lang files lang =
  files
  |> List_.map (fun file ->
         Testo.create ~tags:(Test_tags.tags_of_lang lang)
           (Filename.basename file) (fun () ->
             Parse_target.parse_and_resolve_name_strict lang (Fpath.v file)
             |> ignore))

(* Parsing is expected to succeed with only tolerable parsing errors
   (assumed to be "missing token" nodes inserted by tree-sitter) *)
let missing_tokens_tests_for_lang files lang =
  files
  |> List_.map (fun file ->
         Testo.create ~tags:(Test_tags.tags_of_lang lang)
           (Filename.basename file) (fun () ->
             let { Parsing_result2.errors; tolerated_errors; _ } =
               Parse_target.parse_and_resolve_name lang (Fpath.v file)
             in
             (match errors with
             | [] -> ()
             | _ ->
                 Alcotest.fail
                   ("parsing errors: " ^ Parsing_result2.format_errors errors));
             match tolerated_errors with
             | [] -> Alcotest.fail "no 'missing token' errors. Was it fixed?"
             | _ :: _ ->
                 print_endline
                   ("tolerated errors: "
                   ^ Parsing_result2.format_errors tolerated_errors)))

(* Parsing is expected to fail with at least one parsing error. *)
let partial_parsing_tests_for_lang files lang =
  files
  |> List_.map (fun file ->
         Testo.create ~tags:(Test_tags.tags_of_lang lang)
           ~expected_outcome:
             (Should_fail
                "tree-sitter parsing error is expected: skipped tokens or \
                 missing tokens") (Filename.basename file) (fun () ->
             let { Parsing_result2.errors; tolerated_errors; _ } =
               Parse_target.parse_and_resolve_name lang (Fpath.v file)
             in
             let all_errors = errors @ tolerated_errors in
             match all_errors with
             | [] -> ()
             | _ ->
                 Alcotest.fail
                   ("parsing errors: "
                   ^ Parsing_result2.format_errors all_errors)))

let parsing_tests_for_lang error_tolerance files lang =
  match error_tolerance with
  | Strict -> parsing_tests_for_lang files lang
  | Missing_tokens -> missing_tokens_tests_for_lang files lang
  | Partial_parsing -> partial_parsing_tests_for_lang files lang

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

let pack_parsing_tests_for_lang ?(error_tolerance = Strict) lang =
  let slang = Lang.show lang in
  let dir = Lang.to_lowercase_alnum lang in
  let dir = tests_path_parsing / dir in
  let exts = Lang.ext_of_lang lang in
  let dir, subcategory =
    match error_tolerance with
    | Strict -> (dir, None)
    | Missing_tokens -> (dir / "parsing_missing", Some "missing tokens")
    | Partial_parsing -> (dir / "parsing_partial", Some "partial parsing")
  in
  let is_dir f = f = "parsing_missing" || f = "parsing_partial" in
  let check_ext file =
    if
      not
        (List.exists
           (fun ext ->
             let regex = spf {|.*%s$|} (Str.quote ext) in
             file =~ regex)
           exts)
    then
      failwith (spf "Unrecognized extention for file %s for lang %s" file slang)
  in
  (* Get all files then check extentions *)
  let files =
    Common2.glob (spf "%s/*" !!dir) |> List.filter (fun f -> not (is_dir f))
  in
  if files =*= [] then failwith (spf "Empty set of parsing tests for %s" slang);
  List.iter check_ext files;
  let tests = parsing_tests_for_lang error_tolerance files lang in
  (match subcategory with
  | None -> tests
  | Some cat -> Testo.categorize cat tests)
  |> Testo.categorize slang

(* Note that here we also use tree-sitter to parse; certain files were not
 * parsing with pfff but parses here
 *)
let lang_parsing_tests () =
  Testo.categorize_suites "lang parsing"
    [
      (* languages with only a tree-sitter parser *)
      pack_parsing_tests_for_lang Lang.Bash;
      pack_parsing_tests_for_lang Lang.Csharp;
      pack_parsing_tests_for_lang Lang.Dockerfile;
      pack_parsing_tests_for_lang Lang.Lua;
      pack_parsing_tests_for_lang Lang.Move_on_aptos;
      pack_parsing_tests_for_lang Lang.Move_on_sui;
      pack_parsing_tests_for_lang Lang.Circom;
      pack_parsing_tests_for_lang Lang.Rust;
      pack_parsing_tests_for_lang Lang.Cairo;
      pack_parsing_tests_for_lang Lang.Swift;
      pack_parsing_tests_for_lang Lang.Kotlin;
      pack_parsing_tests_for_lang Lang.Hack;
      pack_parsing_tests_for_lang Lang.Html;
      pack_parsing_tests_for_lang Lang.Xml;
      pack_parsing_tests_for_lang Lang.Vue;
      pack_parsing_tests_for_lang Lang.R;
      pack_parsing_tests_for_lang Lang.Solidity;
      pack_parsing_tests_for_lang Lang.Julia;
      pack_parsing_tests_for_lang Lang.Jsonnet;
      pack_parsing_tests_for_lang Lang.Dart;
      (* here we have both a Pfff and tree-sitter parser *)
      pack_parsing_tests_for_lang Lang.Java;
      pack_parsing_tests_for_lang Lang.Go;
      pack_parsing_tests_for_lang Lang.Ruby;
      pack_parsing_tests_for_lang Lang.Js;
      pack_parsing_tests_for_lang Lang.C;
      pack_parsing_tests_for_lang Lang.Cpp;
      pack_parsing_tests_for_lang Lang.Php;
      pack_parsing_tests_for_lang Lang.Ocaml;
      (* recursive descent parser *)
      pack_parsing_tests_for_lang Lang.Scala;
      pack_parsing_tests_for_lang Lang.Clojure;
      pack_parsing_tests_for_lang Lang.Protobuf;
      pack_parsing_tests_for_lang Lang.Promql;
      pack_parsing_tests_for_lang Lang.Terraform;
      (* a few parsing tests where we expect some partials
       * See cpp/parsing_partial/
       *)
      pack_parsing_tests_for_lang ~error_tolerance:Partial_parsing Lang.Cpp;
      (* a few parsing tests where we rely on "missing tokens" being
         inserted by tree-sitter.
         See cpp/parsing_missing/
      *)
      pack_parsing_tests_for_lang ~error_tolerance:Missing_tokens Lang.C;
      pack_parsing_tests_for_lang ~error_tolerance:Missing_tokens Lang.Cpp;
    ]

(* It's important that our parsers generate classic parsing errors
 * exns (e.g., Parsing_error, Lexical_error), otherwise semgrep
 * will report some "Fatal error" and abort.
 *)
let parsing_error_tests () =
  let dir = tests_path / "parsing_errors" in
  let tags_of_file file =
    match Fpath.to_string file with
    (* For some reason, we can get a `Parsing_error.Syntax_error
       from this test in JS, despite the fact that the `try` makes this
       blatantly impossible.
       I suspect a `jsoo` bug.
    *)
    | file when file =~ ".*/foo.c" -> [ Test_tags.todo_js ]
    | _ -> []
  in
  Testo.categorize "Parsing error detection"
    (let tests = Common2.glob (spf "%s/*" !!dir) in
     tests |> Fpath_.of_strings
     |> List_.map (fun file ->
            t ~tags:(tags_of_file file) (Fpath.basename file) (fun () ->
                try
                  let lang = Lang.lang_of_filename_exn file in
                  let res = Parse_target.just_parse_with_lang lang file in
                  if res.skipped_tokens =*= [] then
                    Alcotest.fail
                      "it should raise a standard parsing error exn or return \
                       partial errors "
                with
                | Parsing_error.Lexical_error _
                | Parsing_error.Syntax_error _ ->
                    ())))

let parsing_rules_tests () =
  let dir = tests_path / "rule_formats" in
  Testo.categorize "Parsing rules"
    (let tests =
       Common2.glob (spf "%s/*.yaml" !!dir)
       @ Common2.glob (spf "%s/*.json" !!dir)
       (* skipped for now to avoid adding jsonnet as a dependency in our
        * CI: Common2.glob (spf "%s/*.jsonnet" dir)
        *)
     in
     tests |> Fpath_.of_strings
     |> List_.map (fun file ->
            t (Fpath.basename file) (fun () -> Parse_rule.parse file |> ignore)))

let parsing_rules_with_atd_tests () =
  let dir = tests_path / "rules_v2" in
  let tests1 =
    Common2.glob (spf "%s/*.yaml" !!dir) @ Common2.glob (spf "%s/*.json" !!dir)
  in
  let dir = tests_path / "syntax_v2" in
  let tests2 =
    Common2.glob (spf "%s/*.yaml" !!dir) @ Common2.glob (spf "%s/*.json" !!dir)
  in
  Testo.categorize "Parsing rules with rule_schema_v2.atd"
    (tests1 @ tests2 |> Fpath_.of_strings
    |> List_.map (fun file ->
           t !!file (fun () ->
               Parse_rules_with_atd.parse_rules_v2 file |> ignore)))

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

let tests () =
  List_.flatten
    [
      lang_parsing_tests ();
      parsing_error_tests ();
      parsing_rules_tests ();
      parsing_rules_with_atd_tests ();
    ]
