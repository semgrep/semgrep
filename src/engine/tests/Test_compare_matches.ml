open Common
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Helper functions to use in testing code.
 *
 * Note that this should be used only for the simple "sgrep" tests
 * as in tests/patterns/. For real "rule" tests, which use
 * the ruleid: annotations, look at Test_subcommand.ml.
 *
 * This module is mostly a copy-paste of Error_code.compare_actual_to_expected
 * but with Semgrep_error_code.error instead of Error_code.t for the error type.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let default_error_regexp = ".*\\(ERROR\\|MATCH\\):"

let location_of_pm { Core_match.range_loc; _ } =
  let { Tok.pos = { line; file; _ }; _ }, _ = range_loc in
  (file, line)

let location_of_core_error (err : Core_error.t) =
  match err.loc with
  | Some loc -> (loc.Tok.pos.file, loc.Tok.pos.line)
  (* TODO(andre) Is there something easier to debug, failwith? *)
  | None -> (Fpath_.fake_file, -1)

let (expected_error_lines_of_files :
      ?regexp:string ->
      ?ok_regexp:string ->
      Fpath.t list ->
      (Fpath.t * int) (* line *) list) =
 fun ?(regexp = default_error_regexp) ?ok_regexp test_files ->
  test_files
  |> List.concat_map (fun file ->
         UFile.cat file |> List_.index_list_1
         |> List_.filter_map (fun (s, idx) ->
                (* Right now we don't care about the actual error messages. We
                 * don't check if they match. We are just happy to check for
                 * correct lines error reporting.
                 *)
                if
                  s =~ regexp
                  (* This is so that we can mark a line differently for OSS/Pro,
                     e.g. `ruleid: deepok: example_rule_id` *)
                  && Option.fold ~none:true
                       ~some:(fun ok_regexp -> not (s =~ ok_regexp))
                       ok_regexp
                  (* + 1 because the comment is one line before *)
                then Some (file, idx + 1)
                else None))

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let plural n = if n >= 2 then "s" else ""

let compare_actual_to_expected ~to_location actual_findings
    expected_findings_lines =
  let actual_findings = List_.map to_location actual_findings in
  (* diff report *)
  let _common, only_in_expected, only_in_actual =
    Common2.diff_set_eff expected_findings_lines actual_findings
  in

  only_in_expected
  |> List.iter (fun (src, l) ->
         UCommon.pr2 (spf "this one finding is missing: %s:%d" !!src l));
  only_in_actual
  |> List.iter (fun (src, l) ->
         UCommon.pr2 (spf "this one finding was not expected: %s:%d" !!src l));
  let false_negatives = List.length only_in_expected in
  let false_positives = List.length only_in_actual in
  let num_errors = false_negatives + false_positives in
  let msg =
    spf
      "The test failed to produce exactly the expected findings: %i false \
       negative%s, %i false positive%s."
      false_negatives (plural false_negatives) false_positives
      (plural false_positives)
  in
  match num_errors with
  | 0 -> Stdlib.Ok ()
  | n -> Error (n, msg)

let compare_actual_to_expected_for_alcotest ~to_location actual expected =
  match compare_actual_to_expected ~to_location actual expected with
  | Ok () -> ()
  | Error (_num_errors, msg) -> Alcotest.fail msg
