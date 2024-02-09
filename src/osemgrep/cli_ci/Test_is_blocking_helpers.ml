(* Sal Olivares
 *
 * Copyright (C) 2023 Semgrep, Inc.
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

open Ci_subcommand
open JSON

let t = Testo.create

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Helper to create JSON for validation state actions with named arguments *)
let json_of_validation_state_actions ~valid ~invalid ~error =
  json_of_string
    (Printf.sprintf
       {|{"dev.semgrep.validation_state.actions": {"valid": "%s", "invalid": "%s", "error":"%s"}}|}
       valid invalid error)

(* Helper to create JSON for findings with direct actions and validation state actions using named arguments *)
let json_of_finding_with_actions ~validation_state ~direct_action ~valid
    ~invalid ~error =
  json_of_string
    (Printf.sprintf
       {|{"validation_state": "%s", "dev.semgrep.actions": ["%s"], "dev.semgrep.validation_state.actions": {"valid": "%s", "invalid": "%s", "error":"%s"}}|}
       validation_state direct_action valid invalid error)

(* Helper to create JSON for actions, parsed into a JSON object *)
let json_of_actions action =
  json_of_string (Printf.sprintf {|{"dev.semgrep.actions": ["%s"]}|} action)

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let tests =
  Testo.categorize_suites "Is Blocking Testing"
    [
      Testo.categorize "rule_is_blocking"
        [
          t "Rule is blocking" (fun () ->
              let test_cases =
                [
                  ( "valid should block",
                    json_of_validation_state_actions ~valid:"block"
                      ~invalid:"monitor" ~error:"comment",
                    true );
                  ( "invalid should block",
                    json_of_validation_state_actions ~valid:"monitor"
                      ~invalid:"block" ~error:"comment",
                    true );
                  ( "error should block",
                    json_of_validation_state_actions ~valid:"monitor"
                      ~invalid:"comment" ~error:"block",
                    true );
                  ( "invalid/error should block",
                    json_of_validation_state_actions ~valid:"monitor"
                      ~invalid:"block" ~error:"block",
                    true );
                  ("actions should block", json_of_actions "block", true);
                ]
              in
              List.iter
                (fun (name, json, expected) ->
                  Alcotest.(check bool) name expected (rule_is_blocking json))
                test_cases);
          t "Rule is non blocking" (fun () ->
              let test_cases =
                [
                  ( "validation_state_actions should not block",
                    json_of_validation_state_actions "comment" "monitor"
                      "comment",
                    false );
                  ("actions should not block", json_of_actions "monitor", false);
                ]
              in
              List.iter
                (fun (name, json, expected) ->
                  Alcotest.(check bool) name expected (rule_is_blocking json))
                test_cases);
        ];
      Testo.categorize "finding_is_blocking"
        [
          t "Finding states and actions" (fun () ->
              let test_cases =
                [
                  ( "confirmed valid should block",
                    json_of_finding_with_actions
                      ~validation_state:"CONFIRMED_VALID"
                      ~direct_action:"monitor" ~valid:"block" ~invalid:"monitor"
                      ~error:"comment",
                    true );
                  ( "confirmed invalid should block",
                    json_of_finding_with_actions
                      ~validation_state:"CONFIRMED_INVALID"
                      ~direct_action:"monitor" ~valid:"monitor" ~invalid:"block"
                      ~error:"comment",
                    true );
                  ( "validation error should block",
                    json_of_finding_with_actions
                      ~validation_state:"VALIDATION_ERROR"
                      ~direct_action:"monitor" ~valid:"monitor"
                      ~invalid:"comment" ~error:"block",
                    true );
                  ( "no validator (treated as valid) should block",
                    json_of_finding_with_actions
                      ~validation_state:"NO_VALIDATOR" ~direct_action:"monitor"
                      ~valid:"block" ~invalid:"monitor" ~error:"comment",
                    true );
                  ( "unrelated state should not block",
                    json_of_finding_with_actions
                      ~validation_state:"UNRELATED_STATE"
                      ~direct_action:"monitor" ~valid:"block" ~invalid:"block"
                      ~error:"block",
                    false );
                  ( "direct action should block",
                    json_of_finding_with_actions ~validation_state:"ANY_STATE"
                      ~direct_action:"block" ~valid:"monitor" ~invalid:"monitor"
                      ~error:"comment",
                    true );
                  ( "no blocking action or state should not block",
                    json_of_finding_with_actions
                      ~validation_state:"CONFIRMED_VALID"
                      ~direct_action:"monitor" ~valid:"monitor"
                      ~invalid:"monitor" ~error:"comment",
                    false );
                ]
              in
              List.iter
                (fun (name, json, expected) ->
                  Alcotest.(check bool) name expected (finding_is_blocking json))
                test_cases);
        ];
    ]
