(*s: semgrep/reporting/JSON_report.ml *)
(*s: pad/r2c copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
*)
(*e: pad/r2c copyright *)
open Common
open AST_generic
module V = Visitor_AST

module PI = Parse_info
module R = Mini_rule (* TODO: use MR instead *)
module E = Error_code
module J = JSON
module MV = Metavariable
module RP = Report

open Pattern_match

(*****************************************************************************)
(* Unique ID *)
(*****************************************************************************)
(*s: function [[JSON_report.string_of_resolved]] *)
let string_of_resolved = function
  | Global -> "Global"
  | Local -> "Local"
  | Param -> "Param"
  | EnclosedVar -> "EnclosedVar"
  | ImportedEntity _ -> "ImportedEntity"
  | ImportedModule _ -> "ImportedModule"
  | TypeName -> "TypeName"
  | Macro -> "Macro"
  | EnumConstant -> "EnumConstant"
(*e: function [[JSON_report.string_of_resolved]] *)

(*s: function [[JSON_report.unique_id]] *)
(* Returning scoping-aware information about a metavariable, so that
 * the callers of sgrep (sgrep-lint) can check if multiple metavariables
 * reference the same entity, or reference exactly the same code.
 * See pfff/.../Naming_AST.ml for more information.
 *
 * TODO: provide a typed interface for the json object because it's really
 *       hard to see how to produce correct output. 'Semgrep.atd' in
 *       spacegrep already has definitions for about every type except this
 *       one.
*)
let unique_id any =
  match any with
  | E (N (Id ((str, _tok), { id_resolved = {contents = Some (resolved, sid)}; _}))) ->
      J.Object [
        "type", J.String "id";
        "value", J.String str;
        "kind", J.String (string_of_resolved resolved);
        (* single unique id *)
        "sid", J.Int sid;
      ]
  (* not an Id, return a md5sum of its AST as a "single unique id" *)
  | _ ->

      (* todo? note that if the any use a parameter, or a local,
       * as in foo(x): return complex(x), then they will have different
       * md5sum because the parameter will be different! We may
       * want to abstract also the resolved information in those cases.
      *)
      let any = AST_generic_helpers.abstract_for_comparison_any any in
      (* alt: Using the AST dumper should work also.
       * let v = Meta_AST.vof_any any in
       * let s = OCaml.string_of_v v in
      *)
      let s = Marshal.to_string any [] in
      let md5 = Digest.string s in
      J.Object [
        "type", J.String "AST";
        "md5sum", J.String (Digest.to_hex md5);
      ]
(*e: function [[JSON_report.unique_id]] *)

(*****************************************************************************)
(* JSON *)
(*****************************************************************************)

(*s: function [[JSON_report.json_range]] *)
let json_range min_loc max_loc =
  (* pfff (and Emacs) have the first column at index 0, but not r2c *)
  let adjust_column x = x + 1 in

  let len_max = String.length max_loc.PI.str in
  J.Object [
    "line", J.Int min_loc.PI.line;
    "col", J.Int (adjust_column min_loc.PI.column);
    "offset", J.Int min_loc.PI.charpos;
  ],
  J.Object [
    "line", J.Int max_loc.PI.line;
    "col", J.Int (adjust_column (max_loc.PI.column + len_max));
    "offset", J.Int (max_loc.PI.charpos + len_max);
  ]
(*e: function [[JSON_report.json_range]] *)


(*s: function [[JSON_report.range_of_any]] *)
let range_of_any any =
  let min_loc, max_loc = V.range_of_any any in
  let (startp, endp) = json_range min_loc max_loc in
  startp, endp
(*e: function [[JSON_report.range_of_any]] *)

(*s: function [[JSON_report.json_metavar]] *)
let json_metavar startp (s, mval) =
  let any = MV.mvalue_to_any mval in
  let (startp, endp) =
    try
      range_of_any any
    with Parse_info.NoTokenLocation _exn ->
      raise (Parse_info.NoTokenLocation (spf
                                           "NoTokenLocation with metavar %s, close location = %s"
                                           s (J.string_of_json startp)))
  in
  s, J.Object [
    "start", startp;
    "end", endp;
    "abstract_content", J.String (
      any
      |> V.ii_of_any |> List.filter PI.is_origintok
      |> List.sort Parse_info.compare_pos
      |> List.map PI.str_of_info
      |> Matching_report.join_with_space_if_needed
    );
    "unique_id", unique_id any
  ]
(*e: function [[JSON_report.json_metavar]] *)


(*s: function [[JSON_report.match_to_json]] *)
(* similar to pfff/h_program-lang/R2c.ml *)
let match_to_json x =
  try
    let min_loc, max_loc = x.range_loc in
    let startp, endp = json_range min_loc max_loc in
    Left (J.Object [
      "check_id", J.String x.rule_id.id;
      "path", J.String x.file;
      "start", startp;
      "end", endp;
      "extra", J.Object [
        "message", J.String x.rule_id.message;
        "metavars", J.Object (x.env |> List.map (json_metavar startp));
      ]
    ])
  (* raised by min_max_ii_by_pos in range_of_any when the AST of the
   * pattern in x.code or the metavar does not contain any token
  *)
  with Parse_info.NoTokenLocation s ->
    let loc = Parse_info.first_loc_of_file x.file in
    let s = spf "NoTokenLocation with pattern %s, %s"
        x.rule_id.pattern_string s in
    let err = E.mk_error_loc loc (E.MatchingError s) in
    Right err
[@@profiling]
(*e: function [[JSON_report.match_to_json]] *)



let json_time_of_profiling_data profiling_data =
  [
    "time", J.Object [
      "targets", J.Array (
        List.map (fun { RP.file = target; parse_time; match_time; run_time } ->
          J.Object [
            "path", J.String target;
            "parse_time", J.Float parse_time;
            "match_time", J.Float match_time;
            "run_time", J.Float run_time;
          ]
        ) profiling_data.RP.file_times
      );
      "rule_parse_time", J.Float profiling_data.RP.rule_parse_time
    ]
  ]

let json_fields_of_matches_and_errors files res =
  let (matches, new_errs) =
    Common.partition_either match_to_json res.RP.matches in
  let errs = new_errs @ res.RP.errors in
  let count_errors = (List.length errs) in
  let count_ok = (List.length files) - count_errors in
  let time_field =
    match res.RP.rule_profiling with
    | None -> []
    | Some x -> json_time_of_profiling_data x
  in
  [ "matches", J.Array (matches);
    "errors", J.Array (errs |> List.map R2c.error_to_json);
    "stats", J.Object [
      "okfiles", J.Int count_ok;
      "errorfiles", J.Int count_errors;
    ];
  ] @ time_field
[@@profiling]

let json_of_profile_info profile_start =
  let now = Unix.gettimeofday () in
  (* total time, but excluding J.string_of_json time that comes after *)
  (* partial copy paste of Common.adjust_profile_entry *)
  Hashtbl.add !Common._profile_table "TOTAL"
    (ref (now -. profile_start), ref 1);

  (* partial copy paste of Common.profile_diagnostic *)
  let xs =
    Hashtbl.fold (fun k v acc -> (k,v)::acc) !Common._profile_table []
    |> List.sort (fun (_k1, (t1,_n1)) (_k2, (t2,_n2)) -> compare t2 t1)
  in
  xs |> List.map (fun (k, (t, cnt)) ->
    k, J.Object [
      "time", J.Float !t;
      "count", J.Int !cnt;
    ]
  ) |> (fun xs -> J.Object xs)

let json_of_exn e =
  (* if (ouptut_as_json) then *)
  match e with
  | Parse_mini_rule.InvalidRuleException (pattern_id, msg)     ->
      J.Object [ "pattern_id", J.String pattern_id;
                 "error", J.String "invalid rule";
                 "message", J.String msg; ]
  | Parse_mini_rule.InvalidLanguageException (pattern_id, language) ->
      J.Object [ "pattern_id", J.String pattern_id;
                 "error", J.String "invalid language";
                 "language", J.String language; ]
  | Parse_mini_rule.InvalidPatternException (pattern_id, pattern, lang, message) ->
      J.Object [ "pattern_id", J.String pattern_id;
                 "error", J.String "invalid pattern";
                 "pattern", J.String pattern;
                 "language", J.String lang;
                 "message", J.String message; ]
  | Parse_mini_rule.UnparsableYamlException msg ->
      J.Object [  "error", J.String "unparsable yaml"; "message", J.String msg; ]
  | Parse_mini_rule.InvalidYamlException msg ->
      J.Object [  "error", J.String "invalid yaml"; "message", J.String msg; ]
  | exn ->
      J.Object [  "error", J.String "unknown exception"; "message", J.String (Common.exn_to_s exn); ]


(*****************************************************************************)
(* Error *)
(*****************************************************************************)
(*s: function [[JSON_report.error]] *)
(* this is used only in the testing code, to reuse the
 * Error_code.compare_actual_to_expected
*)
let error loc (rule: Pattern_match.rule_id) =
  E.error_loc loc (E.SemgrepMatchFound (rule.id, rule.message))
(*e: function [[JSON_report.error]] *)

(*s: function [[JSON_report.match_to_error]] *)
let match_to_error x =
  let min_loc, _max_loc = x.range_loc in
  error min_loc x.rule_id
(*e: function [[JSON_report.match_to_error]] *)
(*e: semgrep/reporting/JSON_report.ml *)
