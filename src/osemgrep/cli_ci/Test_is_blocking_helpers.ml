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
(* Unit tests *)
(*****************************************************************************)

let tests =
  Testo.categorize_suites "Is Blocking Testing"
    [
      Testo.categorize "rule_is_blocking"
        [
          t "Rule is blocking" (fun () ->
              let blocking_validation_state_actions_1 =
                json_of_string
                  {|{"dev.semgrep.validation_state.actions": {"valid": "block", "invalid": "monitor", "error":"comment"}}|}
              in
              let blocking_validation_state_actions_2 =
                json_of_string
                  {|{"dev.semgrep.validation_state.actions": {"valid": "monitor", "invalid": "block", "error":"comment"}}|}
              in
              let blocking_validation_state_actions_3 =
                json_of_string
                  {|{"dev.semgrep.validation_state.actions": {"valid": "monitor", "invalid": "comment", "error":"block"}}|}
              in
              let blocking_validation_state_actions_4 =
                json_of_string
                  {|{"dev.semgrep.validation_state.actions": {"valid": "monitor", "invalid": "block", "error":"block"}}|}
              in

              let blocking_actions =
                json_of_string {|{"dev.semgrep.actions": ["block"]}|}
              in

              Alcotest.(check bool)
                "valid should block" true
                (rule_is_blocking blocking_validation_state_actions_1);
              Alcotest.(check bool)
                "invalid should block" true
                (rule_is_blocking blocking_validation_state_actions_2);
              Alcotest.(check bool)
                "error should block" true
                (rule_is_blocking blocking_validation_state_actions_3);
              Alcotest.(check bool)
                "invalid/error should block" true
                (rule_is_blocking blocking_validation_state_actions_4);

              Alcotest.(check bool)
                "actions should block" true
                (rule_is_blocking blocking_actions));
          t "Rule is non blocking" (fun () ->
              let non_blocking_validation_state_actions =
                json_of_string
                  {|{"dev.semgrep.validation_state.actions": {"valid": "comment", "invalid": "monitor", "error":"comment"}}|}
              in
              let non_blocking_actions =
                json_of_string {|{"dev.semgrep.actions": ["monitor"]}|}
              in

              Alcotest.(check bool)
                "validation_state_actions should not block" false
                (rule_is_blocking non_blocking_validation_state_actions);
              Alcotest.(check bool)
                "actions should not block" false
                (rule_is_blocking non_blocking_actions));
        ];
      Testo.categorize "finding_is_blocking"
        [
          t "Finding with confirmed valid state is blocking" (fun () ->
              let json =
                json_of_string
                  {|{"validation_state": "CONFIRMED_VALID", "dev.semgrep.validation_state.actions": {"valid": "block", "invalid": "monitor", "error":"comment"}}|}
              in
              Alcotest.(check bool)
                "confirmed valid should block" true (finding_is_blocking json));
          t "Finding with confirmed invalid state is blocking" (fun () ->
              let json =
                json_of_string
                  {|{"validation_state": "CONFIRMED_INVALID", "dev.semgrep.validation_state.actions": {"valid": "monitor", "invalid": "block", "error":"comment"}}|}
              in
              Alcotest.(check bool)
                "confirmed invalid should block" true (finding_is_blocking json));
          t "Finding with validation error state is blocking" (fun () ->
              let json =
                json_of_string
                  {|{"validation_state": "VALIDATION_ERROR", "dev.semgrep.validation_state.actions": {"valid": "monitor", "invalid": "comment", "error":"block"}}|}
              in
              Alcotest.(check bool)
                "validation error should block" true (finding_is_blocking json));
          t "Finding with no validator state is treated as valid" (fun () ->
              let json =
                json_of_string
                  {|{"validation_state": "NO_VALIDATOR", "dev.semgrep.validation_state.actions": {"valid": "block", "invalid": "monitor", "error":"comment"}}|}
              in
              Alcotest.(check bool)
                "no validator (treated as valid) should block" true
                (finding_is_blocking json));
          t "Finding with unrelated validation state does not block" (fun () ->
              let json =
                json_of_string
                  {|{"validation_state": "UNRELATED_STATE", "dev.semgrep.validation_state.actions": {"valid": "block", "invalid": "block", "error":"block"}}|}
              in
              Alcotest.(check bool)
                "unrelated state should not block" false
                (finding_is_blocking json));
          t "Finding with direct block action" (fun () ->
              let json =
                json_of_string {|{"dev.semgrep.actions": ["block"]}|}
              in
              Alcotest.(check bool)
                "direct action should block" true (finding_is_blocking json));
          t "Finding with no blocking action or state" (fun () ->
              let json =
                json_of_string
                  {|{"validation_state": "CONFIRMED_VALID", "dev.semgrep.validation_state.actions": {"valid": "monitor", "invalid": "monitor", "error":"comment"}, "dev.semgrep.actions": ["monitor"]}|}
              in
              Alcotest.(check bool)
                "no blocking action or state should not block" false
                (finding_is_blocking json));
        ];
    ]
