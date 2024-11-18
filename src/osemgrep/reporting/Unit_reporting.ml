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
open Common
module Out = Semgrep_output_v1_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

let t = Testo.create

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

(* If you add an entry, double check pysemgrep returns similar things by
 * copy pasting the rule, write a target with a match, then
 * use --debug on the rule with a match and inspect the logs (we now log the
 * 'match_key=' in rule_match.py so just look at the logs).
 * coupling: see also test_match_based_id.py
 *)
let string_of_formulas_expectations =
  [
    (* e2e/rules/eqeq.yaml 1st rule *)
    ( "basic rule",
      {|
rules:
  - id: assert-eqeq-is-ok
    pattern: $X == $X
    message: "possibly useless comparison but in eq function"
    languages: [python]
    severity: ERROR
|},
      [ ("$X", "1") ],
      "1 == 1" );
    (* e2e/rules/eqeq.yaml 2nd rule *)
    ( "many patterns",
      {|
rules:
  - id: eqeq-is-bad
    patterns:
      - pattern-not-inside: |
          def __eq__(...):
              ...
      - pattern-not-inside: assert(...)
      - pattern-not-inside: assertTrue(...)
      - pattern-not-inside: assertFalse(...)
      - pattern-either:
          - pattern: $X == $X
          - pattern: $X != $X
          - patterns:
              - pattern-inside: |
                  def __init__(...):
                      ...
              - pattern: self.$X == self.$X
      - pattern-not: 1 == 1
    message: "useless comparison operation `$X == $X` or `$X != $X`"
    languages: [python]
    severity: ERROR
    metadata:
      shortlink: https://sg.run/xyz1
      source: https://semgrep.dev/r/eqeq-bad
|},
      [ ("$X", "a+b") ],
      "a+b != a+b a+b == a+b def __init__(...):\n\
      \    ...\n\
      \ self.a+b == self.a+b 1 == 1 assert(...) assertFalse(...) \
       assertTrue(...) def __eq__(...):\n\
      \    ...\n" );
    (* e2e/rules/taint_trace.yaml TODO: wrong generation! *)
    ( "taint with labels (WRONG TOFIX)",
      {|
rules:
  - id: taint-trace
    message: found an error
    languages:
      - cpp
      - c
    severity: WARNING
    mode: taint
    metadata:
      interfile: true
    pattern-sources:
      - label: USER_CONTROLLED
        patterns:
          - pattern: SOURCE()
      - label: SCALAR
        requires: USER_CONTROLLED
        patterns:
          - pattern-either:
              - pattern: $LHS + $RHS
          - focus-metavariable:
              - $RHS
              - $LHS
    pattern-sinks:
      - requires: USER_CONTROLLED and SCALAR
        patterns:
          - pattern-either:
              - pattern: SINK(<... $SRC ...>)
          - focus-metavariable: $SRC
|},
      [ ("$RHS", "res1"); ("$SRC", "res2") ],
      (* TODO: this is wrong; osemgrep generate what is below but it should generate
       * instead:
       * "$LHS res1 $LHS + res1 SCALAR USER_CONTROLLED SOURCE() USER_CONTROLLED res2 SINK(<... res2 ...>) USER_CONTROLLED and SCALAR"
       * to take into account the label/requore/focus-metavariable in the rule
       *)
      "$LHS + res1 SINK(<... res2 ...>) SOURCE()" );
  ]

let test_string_of_formulas _caps =
  Testo.categorize "string_of_formulas"
    (string_of_formulas_expectations
    |> List_.map (fun (title, rule, mvars, expected) ->
           t title (fun () ->
               UTmp.with_temp_file ~contents:rule (fun file ->
                   match Parse_rule.parse file with
                   | Ok [ rule ] ->
                       let mvars =
                         mvars
                         |> List_.map (fun (mvar, mvalue_str) ->
                                ( mvar,
                                  Out.
                                    {
                                      abstract_content = mvalue_str;
                                      propagated_value = None;
                                      (* not used by Metavar_replacement *)
                                      start = { line = 0; col = 0; offset = 0 };
                                      end_ = { line = 0; col = 0; offset = 0 };
                                    } ))
                       in
                       let res =
                         Semgrep_hashing_functions
                         .match_formula_interpolated_str rule (Some mvars)
                       in
                       Alcotest.(check string) __LOC__ expected res
                   | _ ->
                       failwith
                         (spf "could not parse or more than one rule for %s"
                            title)))))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests caps =
  Testo.categorize_suites "Osemgrep reporting" [ test_string_of_formulas caps ]
