(*
   Copyright (c) 2022-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
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
let tests_path_parsing_missing = tests_path / "parsing_missing"
let tests_path_parsing_partial = tests_path / "parsing_partial"
let tests_path_parsing_todo = tests_path / "parsing_todo"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type error_tolerance = Strict | Missing_tokens | Partial_parsing | Todo

(* Recommended except if it's too big *)
let snapshot_ast (lang : Lang.t) =
  match lang with
  | Jsonnet ->
      (* spends forever trying to dump an AST (?) *)
      false
  | Yaml ->
      (* needed to catch incorrect parsing of scalars such as mistaking
         'nan' for a float. *)
      true
  | _ ->
      (* Lots of snapshots for all languages.
         For many languages, snapshots were added long after the parsing
         tests existed. We used to only check whether an exception
         was raised. Now we can catch non-fatal changes in how
         the input program is interpreted. *)
      true

(* Golden AST output may live next to a fixture as [foo.ast.expect] (same
   sidecar convention as maturity tests: replace the source extension). When
   absent, fall back to Testo's default hash-based snapshot under
   [tests/snapshots/semgrep-core/]. *)
let checked_output_for_snapshot file =
  let ast_expect_path file = Fpath.set_ext "ast.expect" (Fpath.v file) in
  let expect_path = ast_expect_path file in
  if Sys_.Fpath.exists expect_path then
    Testo.stdout ~expected_stdout_path:expect_path ()
  else Testo.stdout ()

(*
   Strict parsing: errors due to missing (inserted) tokens are not tolerated
   in these tests.
*)
let parsing_tests_for_lang ?expected_outcome files lang =
  files
  |> List.map (fun file ->
      let checked_output =
        (* Due to a bug in Testo, we can't approve the output of
              a test that fails due to an exception.
              TODO: fix it (I thought I already did but I guess I didn't)
           *)
        if snapshot_ast lang && Option.is_none expected_outcome then
          Some (checked_output_for_snapshot file)
        else None
      in
      Testo.create ?checked_output ?expected_outcome
        ~tags:(Test_tags.tags_of_lang lang) (Filename.basename file) (fun () ->
          let ast =
            Parse_target.parse_and_resolve_name_strict lang (Fpath.v file)
          in
          print_string (AST_generic.show_program ast)))

(* Parsing is expected to succeed with only tolerable parsing errors
   (assumed to be "missing token" nodes inserted by tree-sitter) *)
let missing_tokens_tests_for_lang files lang =
  files
  |> List.map (fun file ->
      Testo.create ~tags:(Test_tags.tags_of_lang lang) (Filename.basename file)
        (fun () ->
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
  |> List.map (fun file ->
      Testo.create ~tags:(Test_tags.tags_of_lang lang)
        ~expected_outcome:
          (Should_fail
             "tree-sitter parsing error is expected: skipped tokens or missing \
              tokens") (Filename.basename file) (fun () ->
          let { Parsing_result2.errors; tolerated_errors; _ } =
            Parse_target.parse_and_resolve_name lang (Fpath.v file)
          in
          let all_errors = errors @ tolerated_errors in
          match all_errors with
          | [] -> ()
          | _ ->
              Alcotest.fail
                ("parsing errors: " ^ Parsing_result2.format_errors all_errors)))

let parsing_tests_for_lang error_tolerance files lang =
  match error_tolerance with
  | Strict -> parsing_tests_for_lang files lang
  | Missing_tokens -> missing_tokens_tests_for_lang files lang
  | Partial_parsing -> partial_parsing_tests_for_lang files lang
  | Todo ->
      parsing_tests_for_lang ~expected_outcome:(Should_fail "to do") files lang

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

let pack_parsing_tests_for_lang ?(error_tolerance = Strict) lang =
  let slang = Lang.show lang in
  let dir = Lang.to_lowercase_alnum lang in
  let exts = Lang.exts_of_lang lang in
  let dir, subcategory =
    match error_tolerance with
    | Strict -> (tests_path_parsing / dir, None)
    | Missing_tokens -> (tests_path_parsing_missing / dir, Some "missing tokens")
    | Partial_parsing ->
        (tests_path_parsing_partial / dir, Some "partial parsing")
    | Todo -> (tests_path_parsing_todo / dir, None)
  in
  let check_ext file =
    if
      not
        (List.exists
           (fun ext ->
             let regex = spf {|.*%s$|} (Str.quote ext) in
             file =/~ regex)
           exts)
    then
      failwith
        (spf "Unrecognized extension for file %s for lang %s" !!file slang)
  in
  (* Get all files then check extensions. Markdown files are documentation
     about the test corpus (e.g. a coverage README), not test inputs, so we
     skip them rather than treat them as parse targets. *)
  let pattern = dir / "**" / "*" in
  let files =
    Common2.glob pattern
    |> List_.exclude (fun file ->
        Fpath.has_ext ".md" file || Fpath.has_ext "ast.expect" file)
  in
  if files =*= [] then
    failwith (spf "Empty set of parsing tests for %s at %s" slang !!pattern);
  List.iter check_ext files;
  let file_strings = List.map Fpath.to_string files in
  let tests = parsing_tests_for_lang error_tolerance file_strings lang in
  (match subcategory with
    | None -> tests
    | Some cat -> Testo.categorize cat tests)
  |> Testo.categorize slang

(* Note that here we also use tree-sitter to parse; certain files were not
 * parsing with pfff but parses here
 *)
let lang_parsing_tests langs_with_error_tolerance : Testo.t list =
  langs_with_error_tolerance
  |> List.map (fun (lang, error_tolerance) ->
      pack_parsing_tests_for_lang ~error_tolerance lang)
  |> Testo.categorize_suites "lang parsing"

(* It's important that our parsers generate classic parsing errors
 * exns (e.g., Parsing_error, Lexical_error), otherwise semgrep
 * will report some "Fatal error" and abort.
 *)
let parsing_error_tests () =
  let dir = tests_path / "parsing_errors" in
  let tags_of_file file =
    match file with
    | _ -> []
  in
  Testo.categorize "Parsing error detection"
    (Common2.glob (dir / "*")
    |> List.map (fun file ->
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
       Common2.glob (dir / "*.yaml") @ Common2.glob (dir / "*.json")
       (* skipped for now to avoid adding jsonnet as a dependency in our
        * CI: Common2.glob (spf "%s/*.jsonnet" dir)
        *)
     in
     tests
     |> List.map (fun file ->
         t (Fpath.basename file) (fun () ->
             let res = Parse_rule.parse file in
             match res with
             | Ok _ -> ()
             | Error err ->
                 failwith
                   (spf "error %s while parsing %s" (Rule_error.show err) !!file))))

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

let make_tests langs_with_tolerance =
  List_.flatten
    [
      lang_parsing_tests langs_with_tolerance;
      parsing_error_tests ();
      parsing_rules_tests ();
    ]

let langs_with_error_tolerance =
  [
    (* languages with only a tree-sitter parser *)
    (Lang.Bash, Strict);
    (Lang.Csharp, Strict);
    (Lang.Dockerfile, Strict);
    (Lang.Lua, Strict);
    (Lang.Lua, Todo);
    (Lang.Move_on_aptos, Strict);
    (Lang.Circom, Strict);
    (Lang.Rust, Strict);
    (Lang.Cairo, Strict);
    (Lang.Swift, Strict);
    (Lang.Kotlin, Strict);
    (Lang.Hack, Strict);
    (Lang.Html, Strict);
    (Lang.Xml, Strict);
    (Lang.R, Strict);
    (Lang.Solidity, Strict);
    (Lang.Julia, Strict);
    (Lang.Jsonnet, Strict);
    (Lang.Dart, Strict);
    (Lang.Fga, Strict);
    (* here we have both a Pfff and tree-sitter parser *)
    (Lang.Java, Strict);
    (Lang.Go, Strict);
    (Lang.Ruby, Strict);
    (Lang.Js, Strict);
    (Lang.C, Strict);
    (Lang.Cpp, Strict);
    (Lang.Php, Strict);
    (Lang.Python, Strict);
    (* t-strings (PEP 750, 3.14) are not yet supported by the bundled
       tree-sitter-python grammar. See tests/parsing_todo/python/. *)
    (Lang.Python, Todo);
    (Lang.Ocaml, Strict);
    (* recursive descent parser *)
    (Lang.Scala, Strict);
    (Lang.Clojure, Strict);
    (Lang.Protobuf, Strict);
    (Lang.Protobuf, Todo);
    (Lang.Promql, Strict);
    (Lang.Terraform, Strict);
    (Lang.Yaml, Strict);
    (* a few parsing tests where we expect some partials
     * See cpp/parsing_partial/
     *)
    (Lang.Cpp, Partial_parsing);
    (* a few parsing tests where we rely on "missing tokens" being
       inserted by tree-sitter.
       See cpp/parsing_missing/
    *)
    (Lang.C, Missing_tokens);
    (Lang.Cpp, Missing_tokens);
    (Lang.Cpp, Todo);
  ]

let tests () = make_tests langs_with_error_tolerance
