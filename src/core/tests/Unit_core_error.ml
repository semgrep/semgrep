(*
   Copyright (c) 2026 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* Tests for the helpers of [Core_error] that count errors of a given type. *)

module E = Core_error
module Out = Semgrep_output_v1_t

let rid = Rule_ID.of_string_exn

(* Errors are told apart by their message, as the real ones are *)
let mk ?rule typ msg =
  E.mk_error ?rule_id:(Option.map rid rule)
    ~loc:(Loc.first_loc_of_file (Fpath.v "a.c"))
    ~msg typ

let errors =
  E.ErrorSet.of_list
    [
      mk ~rule:"a" Out.Timeout "1";
      mk ~rule:"a" Out.Timeout "2";
      mk ~rule:"b" Out.Timeout "3";
      mk ~rule:"c" Out.TimeoutDuringInterfile "4";
      mk Out.Timeout "no rule";
    ]

let test_count () =
  Alcotest.(check int) "Timeout" 4 (E.count_of_type Out.Timeout errors);
  Alcotest.(check int)
    "TimeoutDuringInterfile" 1
    (E.count_of_type Out.TimeoutDuringInterfile errors);
  Alcotest.(check int) "none" 0 (E.count_of_type Out.Timeout E.ErrorSet.empty)

let test_rule_ids () =
  Alcotest.(check (list string))
    "rules with repeats, errors without rule skipped" [ "a"; "a"; "b" ]
    (E.rule_ids_of_type Out.Timeout errors
    |> List.map Rule_ID.to_string |> List.sort String.compare)

let test_rule_counts () =
  let counts =
    E.rule_counts_of_type Out.Timeout errors
    |> List.map (fun (r, n) -> (Rule_ID.to_string r, n))
  in
  Alcotest.(check (list (pair string int)))
    "most frequent first"
    [ ("a", 2); ("b", 1) ]
    counts;
  let tied =
    E.ErrorSet.of_list
      [ mk ~rule:"z" Out.Timeout "1"; mk ~rule:"b" Out.Timeout "2" ]
    |> E.rule_counts_of_type Out.Timeout
    |> List.map (fun (r, _) -> Rule_ID.to_string r)
  in
  Alcotest.(check (list string)) "ties broken by rule ID" [ "b"; "z" ] tied

let tests =
  Testo.categorize "Core_error"
    [
      Testo.create "count_of_type" test_count;
      Testo.create "rule_ids_of_type" test_rule_ids;
      Testo.create "rule_counts_of_type" test_rule_counts;
    ]
