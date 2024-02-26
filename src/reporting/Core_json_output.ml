(* Yoann Padioleau
 *
 * Copyright (C) 2019-2023 Semgrep Inc.
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
open Fpath_.Operators
open AST_generic
module E = Core_error
module J = JSON
module MV = Metavariable
module RP = Core_result
module PM = Pattern_match
open Pattern_match
module OutJ = Semgrep_output_v1_j
module OutUtils = Semgrep_output_utils

(*****************************************************************************)
(* Helpers *)
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
  | XmlAt _
  | Fld _
  | Flds _
  | Partial _
  | Name _
  | Raw _
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
      let* min_loc, max_loc = AST_generic_helpers.range_of_any_opt any in
      let startp, endp = OutUtils.position_range min_loc max_loc in
      Some (startp, endp)

(*****************************************************************************)
(* Converters *)
(*****************************************************************************)

let metavar_string_of_any any =
  (* TODO: metavar_string_of_any is used in get_propagated_value
      to get the string for propagated values. Not all propagated
      values will have origintoks. For example, in
          x = 1; y = x + 1; ...
     we have y = 2 but there is no source location for 2.
     Handle such cases *)
  any |> AST_generic_helpers.ii_of_any
  |> List.filter Tok.is_origintok
  |> List.sort Tok.compare_pos
  |> List_.map Tok.content_of_tok
  |> Core_text_output.join_with_space_if_needed

let get_propagated_value default_start mvalue =
  let any_to_svalue_value any =
    match range_of_any_opt default_start any with
    | Some (start, end_) ->
        Some
          OutJ.
            {
              svalue_start = Some start;
              svalue_end = Some end_;
              svalue_abstract_content = metavar_string_of_any any;
            }
    | None ->
        Some
          OutJ.
            {
              svalue_start = None;
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
        (Tok.NoTokenLocation
           (spf "NoTokenLocation with metavar %s, close location = %s" s
              (OutJ.string_of_position startp_of_match_range)))
  | Some (startp, endp) ->
      ( s,
        OutJ.
          {
            start = startp;
            end_ = endp;
            abstract_content = metavar_string_of_any any;
            propagated_value = get_propagated_value startp_of_match_range any;
          } )

(* TODO! semgrep-core used to have its own format for taint traces
 * (called core_match_call_trace), but with osemgrep we want to merge
 * things gradually, but the cli_match_dataflow_trace has those
 * strings attached to location that we ultimately need to generate
 * directly from semgrep-core (to avoid some boilerplate code in
 * pysemgrep).
 *)
let content_of_loc (loc : OutJ.location) : string =
  OutUtils.content_of_file_at_range (loc.start, loc.end_) loc.path

let token_to_intermediate_var token : OutJ.match_intermediate_var option =
  let* location = OutUtils.tokens_to_single_loc [ token ] in
  Some
    (OutJ.{ location; content = content_of_loc location }
      : OutJ.match_intermediate_var)

let tokens_to_intermediate_vars tokens =
  List_.map_filter token_to_intermediate_var tokens

let rec taint_call_trace (trace : PM.taint_call_trace) :
    OutJ.match_call_trace option =
  match trace with
  | Toks toks ->
      let* loc = OutUtils.tokens_to_single_loc toks in
      Some (OutJ.CliLoc (loc, content_of_loc loc))
  | Call { call_trace; intermediate_vars; call_toks } ->
      let* loc = OutUtils.tokens_to_single_loc call_toks in
      let intermediate_vars = tokens_to_intermediate_vars intermediate_vars in
      let* call_trace = taint_call_trace call_trace in
      Some
        (OutJ.CliCall ((loc, content_of_loc loc), intermediate_vars, call_trace))

let taint_trace_to_dataflow_trace (traces : PM.taint_trace_item list) :
    OutJ.match_dataflow_trace =
  (* Here, we ignore all but the first taint trace, for source or sink.
     This is because we added support for multiple sources/sinks in a single
     trace, but only internally to semgrep-core. Externally, our CLI dataflow
     trace datatype still has only one trace per finding. To fit into that
     type, we have to pick one arbitrarily.

     This is fine to do, because we previously only emitted one finding per
     taint sink, due to deduplication, so we shouldn't get more or less
     findings. It's possible that this could change the dataflow trace of
     an existing finding though.
  *)
  let source_call_trace, tokens, sink_call_trace =
    match traces with
    | [] -> raise Common.Impossible
    | { Pattern_match.source_trace; tokens; sink_trace } :: _ ->
        (source_trace, tokens, sink_trace)
  in
  OutJ.
    {
      taint_source = taint_call_trace source_call_trace;
      intermediate_vars = Some (tokens_to_intermediate_vars tokens);
      taint_sink = taint_call_trace sink_call_trace;
    }

let unsafe_match_to_match
    ({ pm = x; is_ignored; autofix_edit } : Core_result.processed_match) :
    OutJ.core_match =
  let min_loc, max_loc = x.range_loc in
  let startp, endp = OutUtils.position_range min_loc max_loc in
  let dataflow_trace =
    Option.map
      (function
        | (lazy trace) -> taint_trace_to_dataflow_trace trace)
      x.taint_trace
  in
  let metavars = x.env |> List_.map (metavars startp) in
  (* message where the metavars have been interpolated *)
  (* TODO(secrets): apply masking logic here *)
  let message =
    Metavar_replacement.interpolate_metavars x.rule_id.message
      (Metavar_replacement.of_bindings x.env)
  in
  let path, historical_info =
    match x.path.origin with
    (* We need to do this, because in Terraform, we may end up with a `file` which
       does not correspond to the actual location of the tokens. This `file` is
       erroneous, and should be replaced by the location of the code of the match,
       if possible. Not if it's fake, though.
       In other languages, this should hopefully not happen.
    *)
    | File path ->
        if
          (!!path <> min_loc.pos.file || !!path <> max_loc.pos.file)
          && min_loc.pos.file <> "FAKE TOKEN LOCATION"
        then (Fpath.v min_loc.pos.file, None)
        else (path, None)
    (* TODO(cooper): if we can have a uri or something more general than a
     * file path here then we can stop doing this hack. *)
    | GitBlob { sha = blob_sha; paths } -> (
        match paths with
        | [] -> (x.path.internal_path_to_content (* no better path *), None)
        | (commit_sha, path) :: _ ->
            let git_blob =
              Some (Digestif.SHA1.of_hex (Git_wrapper.show_sha blob_sha))
            in
            let git_commit =
              Digestif.SHA1.of_hex (Git_wrapper.show_sha commit_sha)
            in
            ( path,
              Some
                ({
                   git_commit;
                   git_blob;
                   git_commit_timestamp =
                     (* TODO: CACHE THIS *)
                     (match Git_wrapper.commit_timestamp commit with
                     | Some x -> x
                     | None ->
                         Logs.warn (fun m ->
                             m
                               "Issue getting timestamp for commit %a. \
                                Reporting current time."
                               Git_wrapper.pp_sha sha);
                         Timedesc.Timestamp.now ());
                 }
                  : OutJ.historical_info) ))
  in
  {
    check_id = x.rule_id.id;
    (* inherited location *)
    path;
    start = startp;
    end_ = endp;
    (* end inherited location *)
    extra =
      {
        message = Some message;
        severity = x.severity_override;
        metadata = Option.map JSON.to_yojson x.metadata_override;
        metavars;
        dataflow_trace;
        fix =
          Option.map (fun edit -> edit.Textedit.replacement_text) autofix_edit;
        is_ignored;
        (* TODO *)
        engine_kind = x.engine_kind;
        validation_state = Some x.validation_state;
        historical_info;
        extra_extra = None;
      };
  }

let match_to_match (x : Core_result.processed_match) :
    (OutJ.core_match, Core_error.t) Either.t =
  try
    Left (unsafe_match_to_match x)
    (* raised by min_max_ii_by_pos in range_of_any when the AST of the
     * pattern in x.code or the metavar does not contain any token
     *)
  with
  | Tok.NoTokenLocation s ->
      let loc = Tok.first_loc_of_file !!(x.pm.path.internal_path_to_content) in
      let s =
        spf "NoTokenLocation with pattern %s, %s" x.pm.rule_id.pattern_string s
      in
      let err = E.mk_error (Some x.pm.rule_id.id) loc s OutJ.MatchingError in
      Right err
[@@profiling]

(* less: Semgrep_error_code should be defined fully Output_from_core.atd
 * so we would not need those conversions
 *)
let error_to_error (err : Core_error.t) : OutJ.core_error =
  let file = err.loc.pos.file in
  let startp, endp = OutUtils.position_range err.loc err.loc in
  let rule_id = err.rule_id in
  let error_type = err.typ in
  let severity = E.severity_of_error err.typ in
  let message = err.msg in
  let details = err.details in
  {
    error_type;
    rule_id;
    severity;
    location = { path = Fpath.v file; start = startp; end_ = endp };
    message;
    details;
  }

let rec explanation_to_explanation (exp : Matching_explanation.t) :
    OutJ.matching_explanation =
  let { Matching_explanation.op; matches; pos; children } = exp in
  let tloc = Tok.unsafe_loc_of_tok pos in
  {
    op;
    children = children |> List_.map explanation_to_explanation;
    matches =
      matches
      |> List_.map (fun pm ->
             unsafe_match_to_match (Core_result.mk_processed_match pm));
    loc = OutUtils.location_of_token_location tloc;
  }

let profiling_to_profiling (profiling_data : Core_profiling.t) : OutJ.profile =
  let rule_ids : Rule_ID.t list =
    profiling_data.rules |> List_.map (fun (rule : Rule.t) -> fst rule.id)
  in
  {
    targets =
      profiling_data.file_times
      |> List_.map
           (fun { Core_profiling.file = target; rule_times; run_time } ->
             let (rule_id_to_rule_prof
                   : (Rule_ID.t, Core_profiling.rule_profiling) Hashtbl.t) =
               rule_times
               |> List_.map (fun (rp : Core_profiling.rule_profiling) ->
                      (rp.rule_id, rp))
               |> Hashtbl_.hash_of_list
             in

             OutJ.
               {
                 path = target;
                 match_times =
                   rule_ids
                   |> List_.map (fun rule_id ->
                          try
                            let rprof : Core_profiling.rule_profiling =
                              Hashtbl.find rule_id_to_rule_prof rule_id
                            in
                            rprof.match_time
                          with
                          | Not_found -> 0.);
                 (* TODO: we could probably just aggregate in a single
                  * float instead of returning those list of parse_time
                  * which don't really make sense; we just parse once a file.
                  *)
                 parse_times =
                   rule_ids
                   |> List_.map (fun rule_id ->
                          try
                            let rprof : Core_profiling.rule_profiling =
                              Hashtbl.find rule_id_to_rule_prof rule_id
                            in
                            rprof.parse_time
                          with
                          | Not_found -> 0.);
                 num_bytes = UFile.filesize target;
                 run_time;
               });
    rules = rule_ids;
    rules_parse_time = profiling_data.rules_parse_time;
    max_memory_bytes = Some profiling_data.max_memory_bytes;
    (* TODO: does it cover all targets or just the relevant target we actually
     * parsed for matching?
     *)
    total_bytes =
      profiling_data.file_times
      |> List_.map (fun { Core_profiling.file = target; _ } ->
             UFile.filesize target)
      |> Common2.sum_int;
    (* those are filled later in pysemgrep from the Profiler class *)
    profiling_times = [];
  }

(* TODO: We used to return some stats, should we generalize
   that and return what is currently in parsing_data.py instead?
   nfiles below was probably redundant anyway and could be
   set to List.length res.scanned

   old code:
    module StrSet = Common2.StringSet
    let core_output_of_matches_and_errors render_fix nfiles res =
      ...
     let files_with_errors =
       errs
       |> List.fold_left
            (fun acc err -> StrSet.add err.E.loc.pos.file acc)
            StrSet.empty
     in
       let count_errors = StrSet.cardinal files_with_errors in
       let count_ok = nfiles - count_errors in

       stats = { okfiles = count_ok; errorfiles = count_errors };
*)

(*****************************************************************************)
(* Final semgrep-core output *)
(*****************************************************************************)

let core_output_of_matches_and_errors (res : Core_result.t) : OutJ.core_output =
  let matches, new_errs =
    Either_.partition_either match_to_match res.processed_matches
  in
  let errs = !E.g_errors @ new_errs @ res.errors in
  E.g_errors := [];
  let skipped_targets, profiling =
    match res.extra with
    | Core_profiling.Debug { skipped_targets; profiling } ->
        (Some skipped_targets, Some profiling)
    | Core_profiling.Time { profiling } -> (None, Some profiling)
    | Core_profiling.No_info -> (None, None)
  in
  {
    results = matches |> OutUtils.sort_core_matches;
    errors = errs |> List_.map error_to_error;
    paths =
      {
        skipped = skipped_targets;
        (* TODO: those are set later in Cli_json_output.ml,
         * but should we compute scanned here instead?
         *)
        scanned = [];
      };
    skipped_rules =
      res.skipped_rules
      |> List_.map (fun ((kind, rule_id, tk) : Rule.invalid_rule_error) ->
             let loc = Tok.unsafe_loc_of_tok tk in
             OutJ.
               {
                 rule_id;
                 details = Rule.string_of_invalid_rule_error_kind kind;
                 position = OutUtils.position_of_token_location loc;
               });
    time = profiling |> Option.map profiling_to_profiling;
    explanations =
      res.explanations |> Option.map (List_.map explanation_to_explanation);
    rules_by_engine = Some res.rules_by_engine;
    interfile_languages_used =
      Some (List_.map (fun l -> Xlang.to_string l) res.interfile_languages_used);
    engine_requested = Some `OSS;
    version = Some Version.version;
  }
[@@profiling]

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

(* this is used only in the testing code, to reuse the
 * Semgrep_error_code.compare_actual_to_expected
 *)
let push_error loc (rule : Pattern_match.rule_id) =
  E.push_error rule.id loc rule.message OutJ.SemgrepMatchFound

let match_to_push_error x =
  let min_loc, _max_loc = x.range_loc in
  push_error min_loc x.rule_id
