(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
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
module StrSet = Common2.StringSet
open AST_generic
module V = Visitor_AST
module PI = Parse_info
module E = Semgrep_error_code
module J = JSON
module MV = Metavariable
module RP = Report
open Pattern_match
module SJ = Output_from_core_j (* JSON conversions *)
module Out = Output_from_core_t (* atdgen definitions *)
module OutH = Output_from_core_util

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* This is to avoid circular dependencies. We can't call
 * Autofix.render_fix in this library, so we need to pass it
 * as a function argument
 *)
type render_fix = Pattern_match.t -> Textedit.t option

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* TODO: should use Common? *)

let rec last hd = function
  | [] -> hd
  | hd :: tl -> last hd tl

let first_and_last = function
  | [] -> None
  | hd :: tl -> Some (hd, last hd tl)

let convert_engine_kind ek =
  match ek with
  | OSS -> `OSSMatch
  | Pro -> `ProMatch

(*****************************************************************************)
(* JSON *)
(*****************************************************************************)

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
  (* TODO? Flds [] ? Pr []? *)
  | Ss _
  | Params _
  | Args _
  | Xmls _
  | E _
  | S _
  | T _
  | P _
  | At _
  | Fld _
  | Flds _
  | Partial _
  | Name _
  | I _
  | Str _
  | Def _
  | Dir _
  | Pr _
  | Tk _
  | TodoK _
  | Ar _
  | Pa _
  | Tp _
  | Ta _
  | Modn _
  | Ce _
  | Cs _
  | ForOrIfComp _
  | ModDk _
  | En _
  | Dk _
  | Di _
  | Lbli _
  | Anys _ ->
      let* min_loc, max_loc = V.range_of_any_opt any in
      let startp, endp = OutH.position_range min_loc max_loc in
      Some (startp, endp)

let metavar_string_of_any any =
  (* TODO: metavar_string_of_any is used in get_propagated_value
      to get the string for propagated values. Not all propagated
      values will have origintoks. For example, in
          x = 1; y = x + 1; ...
     we have y = 2 but there is no source location for 2.
     Handle such cases *)
  any |> V.ii_of_any
  |> List.filter PI.is_origintok
  |> List.sort Parse_info.compare_pos
  |> Common.map PI.str_of_info |> Matching_report.join_with_space_if_needed

let get_propagated_value default_start mvalue =
  let any_to_svalue_value any =
    match range_of_any_opt default_start any with
    | Some (start, end_) ->
        Some
          {
            Out.svalue_start = Some start;
            svalue_end = Some end_;
            svalue_abstract_content = metavar_string_of_any any;
          }
    | None ->
        Some
          {
            Out.svalue_start = None;
            svalue_end = None;
            svalue_abstract_content = metavar_string_of_any any;
          }
  in
  match mvalue with
  | E { e = N (Id (_, id_info)); _ } -> (
      match !(id_info.id_svalue) with
      | Some (Lit x) ->
          let any = E (L x |> e) in
          any_to_svalue_value any
      | Some (Sym x) -> any_to_svalue_value (E x)
      | Some (Cst _) -> None
      | Some NotCst -> None
      | None -> None)
  | __else__ -> None

let metavars startp_of_match_range (s, mval) =
  let any = MV.mvalue_to_any mval in
  match range_of_any_opt startp_of_match_range any with
  | None ->
      raise
        (Parse_info.NoTokenLocation
           (spf "NoTokenLocation with metavar %s, close location = %s" s
              (SJ.string_of_position startp_of_match_range)))
  | Some (startp, endp) ->
      ( s,
        {
          Out.start = startp;
          end_ = endp;
          abstract_content = metavar_string_of_any any;
          propagated_value = get_propagated_value startp_of_match_range any;
        } )

(* None if pi has no location information. Fake tokens should have been filtered
 * out earlier, but in case one slipped through we handle this case. *)
let parse_info_to_location pi =
  PI.token_location_of_info pi
  |> Result.to_option
  |> Option.map (fun token_location ->
         OutH.location_of_token_location token_location)

let tokens_to_locations toks = List.filter_map parse_info_to_location toks

let tokens_to_single_loc toks =
  (* toks should be nonempty and should contain only origintoks, but since we
   * can't prove that by construction we have to filter and handle the empty
   * case here. In theory this could lead to, e.g. a missing taint source for a
   * taint rule finding but it shouldn't happen in practice. *)
  let locations =
    tokens_to_locations
      (List.filter PI.is_origintok toks |> List.sort PI.compare_pos)
  in
  let* first_loc, last_loc = first_and_last locations in
  Some
    { Out.path = first_loc.path; start = first_loc.start; end_ = last_loc.end_ }

let token_to_intermediate_var token =
  let* location = tokens_to_single_loc [ token ] in
  Some { Out.location }

let tokens_to_intermediate_vars tokens =
  List.filter_map token_to_intermediate_var tokens

let rec taint_call_trace = function
  | Toks toks ->
      let* loc = tokens_to_single_loc toks in
      Some (Out.CoreLoc loc)
  | Call { call_trace; intermediate_vars; call_toks } ->
      let* location = tokens_to_single_loc call_toks in
      let intermediate_vars = tokens_to_intermediate_vars intermediate_vars in
      let* call_trace = taint_call_trace call_trace in
      Some (Out.CoreCall (location, intermediate_vars, call_trace))

let taint_trace_to_dataflow_trace { source; tokens; sink } :
    Out.core_match_dataflow_trace =
  {
    Out.taint_source = taint_call_trace source;
    intermediate_vars = Some (tokens_to_intermediate_vars tokens);
    taint_sink = taint_call_trace sink;
  }

let unsafe_match_to_match render_fix_opt (x : Pattern_match.t) : Out.core_match
    =
  let min_loc, max_loc = x.range_loc in
  let startp, endp = OutH.position_range min_loc max_loc in
  let dataflow_trace =
    Option.map
      (function
        | (lazy trace) -> taint_trace_to_dataflow_trace trace)
      x.taint_trace
  in
  let rendered_fix =
    let* render_fix = render_fix_opt in
    let* edit = render_fix x in
    Some edit.Textedit.replacement_text
  in
  (* We need to do this, because in Terraform, we may end up with a `file` which
     does not correspond to the actual location of the tokens. This `file` is
     erroneous, and should be replaced by the location of the code of the match,
     if possible. Not if it's fake, though.
     In other languages, this should hopefully not happen.
  *)
  let file =
    if
      (x.file <> min_loc.file || x.file <> max_loc.file)
      && min_loc.file <> "FAKE TOKEN LOCATION"
    then min_loc.file
    else x.file
  in
  {
    Out.rule_id = x.rule_id.id;
    location = { path = file; start = startp; end_ = endp };
    extra =
      {
        message = Some x.rule_id.message;
        metavars = x.env |> Common.map (metavars startp);
        dataflow_trace;
        rendered_fix;
        engine_kind = convert_engine_kind x.engine_kind;
      };
  }

let match_to_match render_fix (x : Pattern_match.t) :
    (Out.core_match, Semgrep_error_code.error) Common.either =
  try
    Left (unsafe_match_to_match render_fix x)
    (* raised by min_max_ii_by_pos in range_of_any when the AST of the
     * pattern in x.code or the metavar does not contain any token
     *)
  with
  | Parse_info.NoTokenLocation s ->
      let loc = Parse_info.first_loc_of_file x.file in
      let s =
        spf "NoTokenLocation with pattern %s, %s" x.rule_id.pattern_string s
      in
      let err =
        E.mk_error ~rule_id:(Some x.rule_id.id) loc s Out.MatchingError
      in
      Right err
  [@@profiling]

(* less: Semgrep_error_code should be defined fully Output_from_core.atd
 * so we would not need those conversions
 *)
let error_to_error err =
  let file = err.E.loc.PI.file in
  let startp, endp = OutH.position_range err.E.loc err.E.loc in
  let rule_id = err.E.rule_id in
  let error_type = err.E.typ in
  let severity = E.severity_of_error err.E.typ in
  let message = err.E.msg in
  let details = err.E.details in
  {
    Out.error_type;
    rule_id;
    severity;
    location = { path = file; start = startp; end_ = endp };
    message;
    details;
  }

let rec explanation_to_explanation (exp : Matching_explanation.t) :
    Out.matching_explanation =
  let { Matching_explanation.op; matches; pos; children } = exp in
  let tloc = PI.unsafe_token_location_of_info pos in
  {
    Out.op;
    children = children |> Common.map explanation_to_explanation;
    matches = matches |> Common.map (unsafe_match_to_match None);
    loc = OutH.location_of_token_location tloc;
  }

let json_time_of_profiling_data profiling_data =
  let json_time_of_rule_times rule_times =
    rule_times
    |> Common.map (fun { RP.rule_id; parse_time; match_time } ->
           { Out.rule_id; parse_time; match_time })
  in
  {
    Out.targets =
      profiling_data.RP.file_times
      |> Common.map (fun { RP.file = target; rule_times; run_time } ->
             {
               Out.path = target;
               rule_times = json_time_of_rule_times rule_times;
               run_time;
             });
    rules = Common.map (fun rule -> fst rule.Rule.id) profiling_data.RP.rules;
    rules_parse_time = Some profiling_data.RP.rules_parse_time;
    max_memory_bytes = profiling_data.max_memory_bytes;
  }

let match_results_of_matches_and_errors render_fix nfiles res =
  let matches, new_errs =
    Common.partition_either (match_to_match render_fix) res.RP.matches
  in
  let errs = !E.g_errors @ new_errs @ res.RP.errors in
  let files_with_errors =
    errs
    |> List.fold_left
         (fun acc err -> StrSet.add err.E.loc.file acc)
         StrSet.empty
  in
  let count_errors = StrSet.cardinal files_with_errors in
  let count_ok = nfiles - count_errors in
  let skipped_targets, profiling =
    match res.extra with
    | RP.Debug { skipped_targets; profiling } ->
        (Some skipped_targets, Some profiling)
    | RP.Time { profiling } -> (None, Some profiling)
    | RP.No_info -> (None, None)
  in
  {
    Out.matches;
    errors = errs |> Common.map error_to_error;
    skipped_targets;
    skipped_rules =
      (match res.RP.skipped_rules with
      | [] -> None
      | xs ->
          Some
            (xs
            |> Common.map (fun (kind, rule_id, tk) ->
                   let loc = PI.unsafe_token_location_of_info tk in
                   {
                     Out.rule_id;
                     details = Rule.string_of_invalid_rule_error_kind kind;
                     position = OutH.position_of_token_location loc;
                   })));
    stats = { okfiles = count_ok; errorfiles = 555555 };
    time = profiling |> Option.map json_time_of_profiling_data;
    explanations =
      (match res.RP.explanations with
      | [] -> None
      | xs -> Some (xs |> Common.map explanation_to_explanation));
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
  E.error rule.id loc rule.message Out.SemgrepMatchFound

let match_to_error x =
  let min_loc, _max_loc = x.range_loc in
  error min_loc x.rule_id
