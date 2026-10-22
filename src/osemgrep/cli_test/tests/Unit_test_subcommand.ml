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

let t = Testo.create ?skipped:Testutil.skip_on_windows

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

let mk_matching_explanation_tests () =
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
  List.map
    (fun (test_name, rule, test_content) ->
      t ~checked_output:(Testo.stdxxx ()) ~normalize test_name (fun () ->
          (* TODO: please do not capture logs so as to avoid flaky results *)
          Logs_.with_setup ~level:None @@ fun () ->
          Logs.app (fun m -> m "Snapshot for %s" test_name);
          let files =
            [ F.File ("test.yaml", rule); F.File ("test.py", test_content) ]
          in
          Testutil_files.with_tempfiles ~verbose:true ~chdir:true files
            (fun _cwd ->
              let exit_code =
                Test_subcommand.main
                  [| "semgrep-test"; "."; "--matching-diagnosis" |]
              in
              Exit_code.Check.findings exit_code)))
    tests

(* Test both sides of an intentional finding change, and ensure that the
   override remains an exact expectation rather than suppressing failures. *)
let mk_expectations_tests () =
  let entry ~path ~id ~engine lines =
    Printf.sprintf {|{"path":%S,"rule_id":%S,"engine":%S,"lines":%s}|} path id
      engine lines
  in
  let run ~pro rule source entries check =
    let files =
      [
        F.Dir
          ("rules", [ F.File ("test.yaml", rule); F.File ("test.py", source) ]);
        F.Dir
          ( "expectations",
            [ F.File ("overrides.json", "[" ^ String.concat "," entries ^ "]") ]
          );
      ]
    in
    F.with_tempfiles ~chdir:true files (fun _ ->
        Hook.with_hook_set Test_subcommand.hook_pro_init (fun () -> ())
        @@ fun () ->
        Hook.with_hook_set Test_subcommand.hook_pro_scan (fun config _ ->
            Core_scan.scan config)
        @@ fun () ->
        let args =
          [
            "semgrep-test";
            "./rules";
            "--override-expectations";
            "expectations/overrides.json";
          ]
          @ if pro then [ "--pro" ] else []
        in
        Test_subcommand.main (Array.of_list args) |> check)
  in
  let matching entries check =
    run ~pro:false unexpected_match_rule_content unexpected_match_test_content
      entries check
  in
  [
    t "external expectations accept added findings" (fun () ->
        matching
          [
            entry ~path:"../rules/test.py" ~id:"no-foo-unless-good"
              ~engine:"OSS" "[3,5]";
          ]
          Exit_code.Check.ok);
    t "external expectations accept removed findings" (fun () ->
        run ~pro:false _unexpected_no_match_never_rule_content
          unexpected_no_match_test_content
          [
            entry ~path:"../rules/test.py" ~id:"no-foo-unless-good"
              ~engine:"OSS" "[]";
          ]
          Exit_code.Check.ok);
    t "external expectations still reject unexpected findings" (fun () ->
        matching
          [
            entry ~path:"../rules/test.py" ~id:"no-foo-unless-good"
              ~engine:"OSS" "[]";
          ]
          Exit_code.Check.findings);
    t "external expectations still reject missing findings" (fun () ->
        matching
          [
            entry ~path:"../rules/test.py" ~id:"no-foo-unless-good"
              ~engine:"OSS" "[3,5,7]";
          ]
          Exit_code.Check.findings);
    t "external expectations preserve annotations by default" (fun () ->
        matching [] Exit_code.Check.findings);
    t "external expectations are scoped to a target" (fun () ->
        matching
          [
            entry ~path:"../rules/other.py" ~id:"no-foo-unless-good"
              ~engine:"OSS" "[3,5]";
          ]
          Exit_code.Check.findings);
    t "external expectations are scoped to a rule" (fun () ->
        matching
          [
            entry ~path:"../rules/test.py" ~id:"another-rule" ~engine:"OSS"
              "[3,5]";
          ]
          Exit_code.Check.findings);
    t "external expectations are scoped to an engine" (fun () ->
        run ~pro:true unexpected_match_rule_content
          unexpected_match_test_content
          [
            entry ~path:"../rules/test.py" ~id:"no-foo-unless-good"
              ~engine:"OSS" "[3,5]";
          ]
          Exit_code.Check.findings);
    t "external expectations support all engines independently" (fun () ->
        run ~pro:true unexpected_match_rule_content
          unexpected_match_test_content
          [
            entry ~path:"../rules/test.py" ~id:"no-foo-unless-good"
              ~engine:"OSS" "[3,5]";
            entry ~path:"../rules/test.py" ~id:"no-foo-unless-good"
              ~engine:"Pro" "[3,5]";
            entry ~path:"../rules/test.py" ~id:"no-foo-unless-good"
              ~engine:"Deep" "[3,5]";
          ]
          Exit_code.Check.ok);
    t "external expectations return load errors" (fun () ->
        let valid_entry =
          entry ~path:"../rules/test.py" ~id:"no-foo-unless-good" ~engine:"OSS"
            "[3]"
        in
        let invalid_entry =
          entry ~path:"../rules/test.py" ~id:"no-foo-unless-good" ~engine:"OSS"
            "[0]"
        in
        [
          None;
          Some "{";
          Some "{}";
          Some ("[" ^ invalid_entry ^ "]");
          Some ("[" ^ valid_entry ^ "," ^ valid_entry ^ "]");
        ]
        |> List.iter (fun contents ->
            let files =
              match contents with
              | None -> []
              | Some contents -> [ F.File ("overrides.json", contents) ]
            in
            F.with_tempfiles ~chdir:true files (fun _ ->
                match Test_expectations.load (Fpath.v "overrides.json") with
                | Ok _ -> failwith "expected a load error"
                | Error (`Msg message) ->
                    if
                      not
                        (String.starts_with
                           ~prefix:
                             "Invalid test expectations in overrides.json: "
                           message)
                    then failwith "load error omitted file context")));
    t "external expectations reject malformed entries" (fun () ->
        [
          [
            entry ~path:"../rules/test.py" ~id:"no-foo-unless-good"
              ~engine:"Typo" "[3]";
          ];
          [
            entry ~path:"../rules/test.py" ~id:"no-foo-unless-good"
              ~engine:"OSS" "[0]";
          ];
          [
            entry ~path:"../rules/test.py" ~id:"no-foo-unless-good"
              ~engine:"OSS" "[-1]";
          ];
          [
            entry ~path:"../rules/test.py" ~id:"no-foo-unless-good"
              ~engine:"OSS" {|["3"]|};
          ];
          [
            entry ~path:"../rules/test.py" ~id:"no-foo-unless-good"
              ~engine:"OSS" "[3]";
            entry ~path:"../rules/test.py" ~id:"no-foo-unless-good"
              ~engine:"OSS" "[5]";
          ];
          [ {|{"paths":"typo"}|} ];
        ]
        |> List.iter (fun entries ->
            match matching entries (fun _ -> ()) with
            | () -> failwith "invalid expectations accepted"
            | exception Error.Semgrep_error _ -> ()));
  ]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests () =
  Testo.categorize "Osemgrep Test (e2e)"
    (mk_matching_explanation_tests () @ mk_expectations_tests ())
