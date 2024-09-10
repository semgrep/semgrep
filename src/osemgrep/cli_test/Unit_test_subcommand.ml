(* Yoann Padioleau
 *
 * Copyright (C) 2024 Semgrep, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

let t = Testo.create

module F = Testutil_files

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Testing end-to-end (e2e) the test subcommand.
 * Since `semgrep test` is itself a test, we are actually most interested in
 * the `--matching-diagnosis` flag, and its associated output.
 *)

(*****************************************************************************)
(* Test cases *)
(*****************************************************************************)

let unexpected_match_rule_content =
  {|
rules:
  - id: no-foo-unless-good
    match:
      all:
      - pattern: foo(...)
      - not: foo(goood)
    message: "don't allow foo unless it's good"
    languages: [python]
    severity: ERROR
|}

let unexpected_match_test_content =
  {|
# ruleid: no-foo-unless-good
foo()
# ok: no-foo-unless-good
foo(good)
|}

let unexpected_match_multiple_rule_content =
  {|
rules:
  - id: no-foo-bar-unless-good
    match:
      any:
        - all:
          - pattern: foo(...)
          - not: foo(goood)
        - all:
          - pattern: bar(...)
          - not: bar(goood)
    message: "don't allow foo/bar unless it's good"
    languages: [python]
    severity: ERROR
|}

let unexpected_match_multiple_test_content =
  {|
# ruleid: no-foo-bar-unless-good
foo()
# ok: no-foo-bar-unless-good
foo(good)
# ruleid: no-foo-bar-unless-good
bar()
# ok: no-foo-bar-unless-good
bar(good)
|}

let unexpected_no_match_rule_content =
  {|
rules:
  - id: no-foo-unless-good
    match:
      all:
        - pattern: foo(...)
        - not: foo($X)
    message: "don't allow foo unless it's good"
    languages: [python]
    severity: ERROR
|}

let unexpected_no_match_test_content =
  {|
# ruleid: no-foo-unless-good
foo(bad)
# ruleid: no-foo-unless-good
foo(good)
|}

let _unexpected_no_match_never_rule_content =
  {|
rules:
  - id: no-foo-unless-good
    match:
      pattern: nonexistent
    message: "don't allow foo unless it's good"
    languages: [python]
    severity: ERROR
|}

let unexpected_no_match_redundant_rule_content =
  {|
rules:
  - id: no-foo-unless-good
    match:
      all:
        - pattern: foo(...)
        - not: foo($X)
        - not: $Y
    message: "don't allow foo unless it's good"
    languages: [python]
    severity: ERROR
|}

let normalize = [ Testutil_logs.mask_time ]

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

let mk_matching_explanation_tests (caps : Test_subcommand.caps) =
  let tests =
    [
      ( "matching diagnosis unexpected match",
        unexpected_match_rule_content,
        unexpected_match_test_content );
      ( "matching diagnosis unexpected match (multiple)",
        unexpected_match_multiple_rule_content,
        unexpected_match_multiple_test_content );
      ( "matching diagnosis unexpected no match",
        unexpected_no_match_rule_content,
        unexpected_no_match_test_content );
      (* TODO: started to fail at https://github.com/semgrep/semgrep-proprietary/pull/2199
            ( "matching diagnosis unexpected no match (never matched)",
              unexpected_no_match_never_rule_content,
              unexpected_no_match_test_content );
      *)
      ( "matching diagnosis unexpected no match (redundant not)",
        unexpected_no_match_redundant_rule_content,
        unexpected_no_match_test_content );
    ]
  in
  List_.map
    (fun (test_name, rule, test_content) ->
      t ~checked_output:(Testo.stdxxx ()) ~normalize test_name (fun () ->
          Logs.app (fun m -> m "Snapshot for %s" test_name);
          let files =
            [ F.File ("test.yaml", rule); F.File ("test.py", test_content) ]
          in
          Testutil_files.with_tempfiles ~verbose:true ~chdir:true files
            (fun _cwd ->
              let exit_code =
                Test_subcommand.main caps
                  [| "semgrep-test"; "."; "--matching-diagnosis" |]
              in
              Exit_code.Check.findings exit_code)))
    tests

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests (caps : < Test_subcommand.caps >) =
  Testo.categorize "Osemgrep Test (e2e)" (mk_matching_explanation_tests caps)
