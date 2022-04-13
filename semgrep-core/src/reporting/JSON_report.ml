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
open Common
module StrSet = Common2.StringSet
open AST_generic
module V = Visitor_AST
module PI = Parse_info
module E = Semgrep_error_code
module J = JSON
module MV = Metavariable
module RP = Report
open Pattern_match
module ST = Output_from_core_t (* atdgen definitions *)
module SJ = Output_from_core_j (* JSON conversions *)

(*****************************************************************************)
(* Unique ID *)
(*****************************************************************************)

(* Returning scoping-aware information about a metavariable, so that
 * the callers of semgrep (semgrep python) can check if multiple metavariables
 * reference the same entity, or reference exactly the same code.
 * See Naming_AST.ml for more information.
 * TODO: can we delete now that the boolean logic is done in semgrep-core?
 *)
let unique_id any =
  match any with
  | E { e = N (Id (_, { id_resolved = { contents = Some (_, sid) }; _ })); _ }
    ->
      { ST.type_ = `ID; md5sum = None; sid = Some sid }
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
      { ST.type_ = `AST; md5sum = Some (Digest.to_hex md5); sid = None }

(*****************************************************************************)
(* JSON *)
(*****************************************************************************)

(* pfff (and Emacs) have the first column at index 0, but not r2c *)
let adjust_column x = x + 1

let position_of_token_location loc =
  {
    ST.line = loc.PI.line;
    col = adjust_column loc.PI.column;
    offset = loc.PI.charpos;
  }

let position_range min_loc max_loc =
  let len_max = String.length max_loc.PI.str in
  (* alt: could call position_of_token_location but more symetric like that*)
  ( {
      ST.line = min_loc.PI.line;
      col = adjust_column min_loc.PI.column;
      offset = min_loc.PI.charpos;
    },
    {
      ST.line = max_loc.PI.line;
      col = adjust_column (max_loc.PI.column + len_max);
      offset = max_loc.PI.charpos + len_max;
    } )

let range_of_any_opt startp_of_match_range any =
  let empty_range = (startp_of_match_range, startp_of_match_range) in
  match any with
  (* those are ok and we don't want to generate a NoTokenLocation for those.
   * alt: change Semgrep.atd to make optional startp/endp for metavar_value.
   *)
  | Ss []
  | Params []
  | Args []
  | Xmls [] ->
      Some empty_range
  | _ ->
      let ( let* ) = Common.( >>= ) in
      let* min_loc, max_loc = V.range_of_any_opt any in
      let startp, endp = position_range min_loc max_loc in
      Some (startp, endp)

let metavar_string_of_any any =
  any |> V.ii_of_any
  |> List.filter PI.is_origintok
  |> List.sort Parse_info.compare_pos
  |> Common.map PI.str_of_info |> Matching_report.join_with_space_if_needed

let get_propagated_value default_start metavar =
  let any_to_svalue_value any =
    match range_of_any_opt default_start any with
    | Some (start, end_) ->
        Some
          {
            ST.svalue_start = start;
            svalue_end = end_;
            svalue_abstract_content = metavar_string_of_any any;
          }
    | None -> None
  in
  match metavar with
  | E { e = N (Id (_, id_info)); _ } -> (
      match !(id_info.id_svalue) with
      | Some (Lit x) ->
          let any = E (L x |> e) in
          any_to_svalue_value any
      | Some (Sym x) -> any_to_svalue_value (E x)
      | Some (Cst _) -> None
      | Some NotCst -> None
      | None -> None)
  | _ -> None

let metavars startp_of_match_range (s, mval) =
  let any = MV.mvalue_to_any mval in
  pr2 (show_any any);
  match range_of_any_opt startp_of_match_range any with
  | None ->
      raise
        (Parse_info.NoTokenLocation
           (spf "NoTokenLocation with metavar %s, close location = %s" s
              (SJ.string_of_position startp_of_match_range)))
  | Some (startp, endp) ->
      ( s,
        {
          ST.start = startp;
          end_ = endp;
          abstract_content = metavar_string_of_any any;
          propagated_value = get_propagated_value startp_of_match_range any;
          unique_id = unique_id any;
        } )

let match_to_match x =
  try
    let min_loc, max_loc = x.range_loc in
    let startp, endp = position_range min_loc max_loc in
    Left
      ({
         ST.rule_id = x.rule_id.id;
         location =
           {
             path = x.file;
             start = startp;
             end_ = endp;
             lines = [] (* ?? spacegrep? *);
           };
         extra =
           {
             message = Some x.rule_id.message;
             metavars = x.env |> Common.map (metavars startp);
           };
       }
        : ST.match_)
    (* raised by min_max_ii_by_pos in range_of_any when the AST of the
     * pattern in x.code or the metavar does not contain any token
     *)
  with
  | Parse_info.NoTokenLocation s ->
      let loc = Parse_info.first_loc_of_file x.file in
      let s =
        spf "NoTokenLocation with pattern %s, %s" x.rule_id.pattern_string s
      in
      let err = E.mk_error ~rule_id:(Some x.rule_id.id) loc s E.MatchingError in
      Right err
  [@@profiling]

(* was in pfff/h_program-lang/R2c.ml becore *)
let hcache = Hashtbl.create 101

let lines_of_file (file : Common.filename) : string array =
  Common.memoized hcache file (fun () ->
      try Common.cat file |> Array.of_list with
      | _ -> [| "EMPTY FILE" |])

let error_to_error err =
  let severity_of_severity = function
    | E.Error -> SJ.Error
    | E.Warning -> SJ.Warning
  in
  let file = err.E.loc.PI.file in
  let lines = lines_of_file file in
  let startp, endp = position_range err.E.loc err.E.loc in
  let line = err.E.loc.PI.line in
  let rule_id = err.E.rule_id in
  let error_type = E.string_of_error_kind err.E.typ in
  let severity = severity_of_severity (E.severity_of_error err.E.typ) in
  let message = err.E.msg in
  let details = err.E.details in
  let yaml_path = err.E.yaml_path in
  {
    ST.error_type;
    rule_id;
    severity;
    location =
      {
        path = file;
        start = startp;
        end_ = endp;
        lines =
          (try [ lines.(line - 1) ] with
          | _ -> [ "NO LINE" ]);
      };
    message;
    details;
    yaml_path;
  }

let json_time_of_profiling_data profiling_data =
  let json_time_of_rule_times rule_times =
    rule_times
    |> Common.map (fun { RP.rule_id; parse_time; match_time } ->
           { ST.rule_id; parse_time; match_time })
  in
  {
    ST.targets =
      profiling_data.RP.file_times
      |> Common.map (fun { RP.file = target; rule_times; run_time } ->
             {
               ST.path = target;
               rule_times = json_time_of_rule_times rule_times;
               run_time;
             });
    rules = Common.map (fun rule -> fst rule.Rule.id) profiling_data.RP.rules;
    rules_parse_time = Some profiling_data.RP.rules_parse_time;
  }

let match_results_of_matches_and_errors files res =
  let matches, new_errs =
    Common.partition_either match_to_match res.RP.matches
  in
  let errs = !E.g_errors @ new_errs @ res.RP.errors in
  let files_with_errors =
    List.fold_left
      (fun acc err -> StrSet.add err.E.loc.file acc)
      StrSet.empty errs
  in
  let count_errors = StrSet.cardinal files_with_errors in
  let count_ok = List.length files - count_errors in
  {
    ST.matches;
    errors = errs |> Common.map error_to_error;
    skipped_targets = res.RP.skipped_targets;
    skipped_rules =
      (match res.RP.skipped_rules with
      | [] -> None
      | xs ->
          Some
            (xs
            |> Common.map (fun (kind, rule_id, tk) ->
                   let loc = PI.unsafe_token_location_of_info tk in
                   {
                     ST.rule_id;
                     details = Rule.string_of_invalid_rule_error_kind kind;
                     position = position_of_token_location loc;
                   })));
    stats = { okfiles = count_ok; errorfiles = count_errors };
    time = res.RP.final_profiling |> Option.map json_time_of_profiling_data;
  }
  |> Output_from_core_util.sort_match_results
  [@@profiling]

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

(* this is used only in the testing code, to reuse the
 * Semgrep_error_code.compare_actual_to_expected
 *)
let error loc (rule : Pattern_match.rule_id) =
  E.error rule.id loc rule.message (E.SemgrepMatchFound rule.id)

let match_to_error x =
  let min_loc, _max_loc = x.range_loc in
  error min_loc x.rule_id
