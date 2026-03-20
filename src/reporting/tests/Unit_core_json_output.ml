(* Nat Mote
 *
 * Copyright (C) 2019-2024 Semgrep Inc.
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

module Out = Semgrep_output_v1_j

let key = Alcotest.testable Core_json_output.pp_key ( = )
let key_not_equal = Alcotest.testable Core_json_output.pp_key ( <> )

let make_core_match ?(check_id = "fake-rule-id") ?annotated_rule_id
    ?(src = "unchanged") () : Out.core_match =
  let annotated_rule_id = Option.value annotated_rule_id ~default:check_id in
  let metadata : JSON.t =
    JSON.(
      Object
        [
          ( "semgrep.dev",
            Object
              [
                ("src", String src);
                ("rule", Object [ ("rule_name", String annotated_rule_id) ]);
              ] );
        ])
  in
  let extra : Out.core_match_extra =
    Out.
      {
        message = None;
        metadata = Some (JSON.to_yojson metadata);
        severity = None;
        metavars = [];
        fix = None;
        engine_kind = `OSS;
        dataflow_trace = None;
        is_ignored = false;
        sca_match = None;
        validation_state = None;
        historical_info = None;
        extra_extra = None;
      }
  in
  Out.
    {
      check_id = Rule_ID.of_string_exn check_id;
      path = Fpath.v "/fake/path/to/target";
      start = { line = 1; col = 1; offset = 1 };
      end_ = { line = 1; col = 2; offset = 2 };
      extra;
    }

let make_test_case test_name key_testable msg match1 match2 =
  Testo.create test_name (fun () ->
      let key1 = Core_json_output.test_core_unique_key match1 in
      let key2 = Core_json_output.test_core_unique_key match2 in
      Alcotest.(check key_testable msg key1 key2))

let test_core_unique_key =
  Testo.categorize "test_core_unique_key"
  @@ [
       (let match1 = make_core_match ~check_id:"rule1" () in
        let match2 = make_core_match ~check_id:"rule1" () in
        make_test_case "same-rule-matches" key "keys should match" match1 match2);
       (let match1 = make_core_match ~check_id:"rule1" () in
        let match2 = make_core_match ~check_id:"rule2" () in
        make_test_case "different-rule-matches" key_not_equal
          "keys should not match" match1 match2);
       (let match1 =
          make_core_match ~check_id:"orig-rule-name" ~src:"new-rule" ()
        in
        let match2 =
          make_core_match ~check_id:"mangled-rule-name" ~src:"previous-scan"
            ~annotated_rule_id:"orig-rule-name" ()
        in
        make_test_case "previous-scan match deduplication" key_not_equal
          "keys should not match" match1 match2);
     ]

let tests = Testo.categorize "Core_json_output" test_core_unique_key
