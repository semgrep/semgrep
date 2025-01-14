(* Yoann Padioleau
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
open Common
open Fpath_.Operators
open AST_generic
module J = JSON
module E = Core_error
module MV = Metavariable
module Out = Semgrep_output_v1_j
module OutUtils = Semgrep_output_utils
module Log = Log_reporting.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* semgrep-core -json output.
 *
 * See semgrep_output_v1.atd and its [core_output] for the JSON spec of this
 * output. This "core" output is then read by pysemgrep in core_runner.py
 * (or by osemgrep in Core_runner.ml) and transform in a "CLI" output
 * (see [cli_output] in semgrep_output_v1.atd) which is then finally printed
 * out to the user (see Cli_json_output.ml and Output.ml).
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let max_truncated_length = 2048

let truncate_for_output =
  String_.truncate_with_message max_truncated_length
    (* nosemgrep *)
    (spf "%s... (truncated %d more characters)")

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
(* Deduplication *)
(*****************************************************************************)

(* The actual type here isn't super important. We just feed it into Hashtbl
 * which uses polymorphic hash and equals. It just needs to have a consistent
 * representation and include anything relevant for deduplication. *)
type key = string * string * int * int * string option * string option
[@@deriving show]

(* This is a port of the original pysemgrep cli_unique_key. This used to be in the CLI,
   but has since been moved to core.
*)
let core_unique_key (c : Out.core_match) : key =
  let name = Rule_ID.to_string c.check_id in
  let path =
    match c.extra.historical_info with
    | Some { git_blob = Some sha; _ } -> ATD_string_wrap.Sha1.unwrap sha
    | _ -> Fpath.to_string c.path
  in
  ( name,
    path,
    c.start.offset,
    c.end_.offset,
    c.extra.message,
    (* TODO: Bring this back.
       This is necessary so we don't deduplicate taint findings which
       have different sources.

       self.match.extra.dataflow_trace.to_json_string
       if self.match.extra.dataflow_trace
       else None,
    *)
    None
    (* NOTE: previously, we considered self.match.extra.validation_state
       here, but since in some cases (e.g., with `anywhere`) we generate
       many matches in certain cases, we want to consider secrets
       matches unique under the above set of things, but with a priority
       associated with the validation state; i.e., a match with a
       confirmed valid state should replace all matches equal under the
       above key. We can't do that just by not considering validation
       state since we would pick one arbitrarily, and if we added it
       below then we would report _both_ valid and invalid (but we only
       want to report valid, if a valid one is present and unique per
       above fields). See also `should_report_instead`.
    *) )

(*
 # Sort results so as to guarantee the same results across different
 # runs. Results may arrive in a different order due to parallelism
 # (-j option).
*)
let dedup_and_sort (xs : Out.core_match list) : Out.core_match list =
  (* Whether we prefer to report match x over match y.
     This is currently only used for Secrets findings, which prefer a
     finding with a confirmed validation status.
  *)
  let should_report_instead ((x : Out.core_match), (y : Out.core_match)) =
    match (x, y) with
    | { Out.extra = { validation_state = None; _ }; _ }, _ -> false
    | _, { Out.extra = { validation_state = None; _ }; _ } -> true
    | { Out.extra = { validation_state = Some `Confirmed_valid; _ }; _ }, _ -> (
        match y with
        | { Out.extra = { validation_state = Some `Confirmed_valid; _ }; _ } ->
            false
        | _ -> true)
    | _ -> false
  in
  let seen = Hashtbl.create 101 in
  xs |> OutUtils.sort_core_matches
  (* This deduplication logic used to live in Pysemgrep, which would assume that
     the matches had already been sorted via sort_core_matches.
     If you run through this deduplication logic without that assumption, you'll
     keep undesirable matches, such as those with less metavariables.
  *)
  |> List.iter (fun x ->
         let key = core_unique_key x in
         match Hashtbl.find_opt seen key with
         | None -> Hashtbl.add seen key x
         | Some y when should_report_instead (x, y) ->
             Hashtbl.replace seen key x
         | _ -> ());
  (* Here, we must sort again, though.
     This is because we yet again need to enforce that when Pysemgrep receives these
     matches, that they are sorted via sort_core_matches.
     If we don't do this, then stuff like test_baseline will start breaking.
     So we end up sorting twice. Such is life.
     LATER: Can optimize if necessary
  *)
  Hashtbl.to_seq_values seen |> List.of_seq |> OutUtils.sort_core_matches

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
          Out.
            {
              svalue_start = Some start;
              svalue_end = Some end_;
              svalue_abstract_content = metavar_string_of_any any;
            }
    | None ->
        Some
          Out.
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
              (Out.string_of_position startp_of_match_range)))
  | Some (startp, endp) ->
      ( s,
        Out.
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
let content_of_loc (loc : Out.location) : string =
  OutUtils.content_of_file_at_range (loc.start, loc.end_) loc.path

let token_to_intermediate_var token : Out.match_intermediate_var option =
  let* location = OutUtils.tokens_to_single_loc [ token ] in
  Some
    (Out.{ location; content = content_of_loc location }
      : Out.match_intermediate_var)

let tokens_to_intermediate_vars tokens =
  List_.filter_map token_to_intermediate_var tokens

let rec taint_call_trace (trace : Taint_trace.call_trace) :
    Out.match_call_trace option =
  match trace with
  | Toks toks ->
      let* loc = OutUtils.tokens_to_single_loc toks in
      Some (Out.CliLoc (loc, content_of_loc loc))
  | Call { call_trace; intermediate_vars; call_toks } ->
      let* loc = OutUtils.tokens_to_single_loc call_toks in
      let intermediate_vars = tokens_to_intermediate_vars intermediate_vars in
      let* call_trace = taint_call_trace call_trace in
      Some
        (Out.CliCall ((loc, content_of_loc loc), intermediate_vars, call_trace))

let taint_trace_to_dataflow_trace (traces : Taint_trace.item list) :
    Out.match_dataflow_trace =
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
    | { Taint_trace.source_trace; tokens; sink_trace } :: _ ->
        (source_trace, tokens, sink_trace)
  in
  Out.
    {
      taint_source = taint_call_trace source_call_trace;
      intermediate_vars = Some (tokens_to_intermediate_vars tokens);
      taint_sink = taint_call_trace sink_call_trace;
    }

let path_and_historical (path : Target.path) ~(min_loc : Tok.location)
    ~(max_loc : Tok.location) : Fpath.t * Out.historical_info option =
  match path.origin with
  (* We need to do this, because in Terraform, we may end up with a `file` which
     does not correspond to the actual location of the tokens. This `file` is
     erroneous, and should be replaced by the location of the code of the match,
     if possible. Not if it's fake, though.
     In other languages, this should hopefully not happen.
  *)
  | File path ->
      if
        (path <> min_loc.pos.file || path <> max_loc.pos.file)
        && not (Fpath_.is_fake_file min_loc.pos.file)
      then (min_loc.pos.file, None)
      else (path, None)
  (* TODO(cooper): if we can have a uri or something more general than a
   * file path here then we can stop doing this hack. *)
  | GitBlob { sha; paths } -> (
      match paths with
      | [] -> (path.internal_path_to_content (* no better path *), None)
      | (commit, path) :: _ ->
          let git_commit = Git_wrapper.commit_digest commit in
          let timestamp, offset = (Git_wrapper.commit_author commit).date in
          let offset =
            Option.value offset
              ~default:{ sign = `Plus; hours = 0; minutes = 0 }
          in
          ( path,
            Some
              ({
                 git_commit;
                 git_blob = Some sha;
                 git_commit_timestamp =
                   Datetime_.of_unix_int_time timestamp offset.sign offset.hours
                     offset.minutes;
               }
                : Out.historical_info) ))

let sca_pattern_to_sca_pattern (pat : SCA_pattern.t) : Out.sca_pattern =
  let SCA_pattern.{ ecosystem; package_name = package; version_constraints } =
    pat
  in
  let semver_range =
    SCA_pattern.version_constraints_to_string version_constraints
  in
  Out.{ ecosystem; package; semver_range }

let sca_dependency_to_found_dependency (dep : SCA_dependency.t) :
    Out.found_dependency =
  let SCA_dependency.
        {
          package_name;
          package_version = _;
          package_version_string;
          ecosystem;
          transitivity;
          url;
          loc = _;
          tokens = _;
        } =
    dep
  in
  Out.
    {
      package = package_name;
      version = package_version_string;
      ecosystem;
      (* ?? *)
      allowed_hashes = [];
      resolved_url = url |> Option.map Uri.to_string;
      (* TODO *)
      transitivity;
      manifest_path = None;
      lockfile_path = None;
      line_number = None;
      children = None;
      git_ref = None;
    }

(* TODO: this is a v0, need to port pysemgrep code *)
let sca_to_sca (m : SCA_match.t) : Out.sca_match =
  let SCA_match.{ dep; pat; kind } = m in
  let reachable =
    match kind with
    | CodeAndLockfileMatch -> true
    | LockfileOnlyMatch -> false
  in
  (* TODO: need to look at the rule! we might have LockfileOnlyMatch above
   * with a reachability rune
   *)
  let reachability_rule = reachable in
  let dependency_match : Out.dependency_match =
    let dependency_pattern : Out.sca_pattern = sca_pattern_to_sca_pattern pat in
    let found_dependency : Out.found_dependency =
      sca_dependency_to_found_dependency dep
    in
    let lockfile =
      let loc, _ = dep.loc in
      loc.pos.file
    in
    Out.{ dependency_pattern; found_dependency; lockfile }
  in
  Out.
    {
      reachable;
      reachability_rule;
      (* coupling: dependency_aware_rule.py:SCA_FINDING_SCHEMA *)
      sca_finding_schema = 20220913;
      dependency_match;
    }

(* "unsafe" because can raise NoTokenLocation which is captured in
 * the safe match_to_match version further below
 *)
let unsafe_match_to_match
    ({ pm; is_ignored; autofix_edit } : Core_result.processed_match) :
    Out.core_match =
  let min_loc, max_loc = pm.range_loc in
  let startp, endp = OutUtils.position_range min_loc max_loc in
  let dataflow_trace =
    pm.taint_trace
    |> Option.map (function (lazy trace) -> taint_trace_to_dataflow_trace trace)
  in

  let metavars = pm.env |> List_.map (metavars startp) in
  let metadata =
    let* json = pm.rule_id.metadata in
    let rule_metadata = JSON.to_yojson json in
    match pm.metadata_override with
    | Some metadata_override ->
        Some (JSON.update rule_metadata (JSON.to_yojson metadata_override))
    | None -> Some rule_metadata
  in
  (* message where the metavars have been interpolated *)
  (* TODO(secrets): apply masking logic here *)
  let message =
    Metavar_replacement.interpolate_metavars ~fmt:truncate_for_output
      pm.rule_id.message
      (Metavar_replacement.of_bindings pm.env)
  in
  let path, historical_info = path_and_historical pm.path ~min_loc ~max_loc in
  {
    check_id = pm.rule_id.id;
    (* inherited location *)
    path;
    start = startp;
    end_ = endp;
    (* end inherited location *)
    extra =
      {
        message = Some message;
        severity = pm.severity_override;
        metadata;
        metavars;
        dataflow_trace;
        fix =
          Option.map (fun edit -> edit.Textedit.replacement_text) autofix_edit;
        is_ignored;
        engine_kind = pm.engine_of_match;
        sca_match = Option.map sca_to_sca pm.sca_match;
        validation_state = Some pm.validation_state;
        historical_info;
        extra_extra = None;
      };
  }

let match_to_match (x : Core_result.processed_match) :
    (Out.core_match, Core_error.t) result =
  try
    Ok
      (Logs_.with_debug_trace ~__FUNCTION__ ~src:Log_reporting.src
         ~pp_input:(fun _ ->
           "target: "
           ^ !!(x.pm.path.internal_path_to_content)
           ^ "\nruleid: "
           ^ (x.pm.rule_id.id |> Rule_ID.to_string))
         (fun () -> unsafe_match_to_match x))
    (* raised by min_max_ii_by_pos in range_of_any when the AST of the
     * pattern in x.code or the metavar does not contain any token
     *)
  with
  | Tok.NoTokenLocation s ->
      let loc = Tok.first_loc_of_file x.pm.path.internal_path_to_content in
      let s =
        spf "NoTokenLocation with pattern %s, %s" x.pm.rule_id.pattern_string s
      in
      let err =
        E.mk_error ~rule_id:x.pm.rule_id.id ~msg:s ~loc Out.MatchingError
      in
      Error err
[@@profiling]

(* less: Semgrep_error_code should be defined fully Output_from_core.atd
 * so we would not need those conversions
 *)
let error_to_error (err : Core_error.t) : Out.core_error =
  let location =
    let* loc = err.loc in
    let file = loc.pos.file in
    let startp, endp = OutUtils.position_range loc loc in
    Some { Out.path = file; start = startp; end_ = endp }
  in
  let rule_id = err.rule_id in
  let error_type = err.typ in
  let severity = E.severity_of_error err.typ in
  (* A lot of times if a file is minified/1 or 2 lines, we'll not be able to
     parse it, and raise a syntax error, and that error message contains the
     content of the line it can't parse. this results in essentially including
     the whole file in the json blob which is not good for security or perf
     reasons. We've seen upwards of 4mb of text in these messages. So let's
     truncate it *)
  let message = truncate_for_output err.msg in
  let details = err.details in
  { error_type; rule_id; severity; location; message; details }

let extra_to_extra (extra : Matching_explanation.extra) :
    Out.matching_explanation_extra =
  {
    before_negation_matches =
      extra.before_negation_matches
      |> Option.map
           (List_.map (fun pm ->
                unsafe_match_to_match (Core_result.mk_processed_match pm)));
    before_filter_matches =
      extra.before_filter_matches
      |> Option.map
           (List_.map (fun pm ->
                unsafe_match_to_match (Core_result.mk_processed_match pm)));
  }

let rec explanation_to_explanation (exp : Matching_explanation.t) :
    Out.matching_explanation =
  let { Matching_explanation.op; matches; pos; children; extra } = exp in
  let tloc = Tok.unsafe_loc_of_tok pos in
  {
    op;
    children = children |> List_.map explanation_to_explanation;
    matches =
      matches
      |> List_.map (fun pm ->
             unsafe_match_to_match (Core_result.mk_processed_match pm));
    loc = OutUtils.location_of_token_location tloc;
    extra = Option.map extra_to_extra extra;
  }

let profiling_to_profiling (profiling_data : Core_profiling.t) : Out.profile =
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

             Out.
               {
                 path = target;
                 match_times =
                   rule_ids
                   |> List_.map (fun rule_id ->
                          try
                            let rprof : Core_profiling.rule_profiling =
                              Hashtbl.find rule_id_to_rule_prof rule_id
                            in
                            rprof.rule_match_time
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
                            rprof.rule_parse_time
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

let core_output_of_matches_and_errors (res : Core_result.t) : Out.core_output =
  let matches, new_errs =
    Result_.partition match_to_match res.processed_matches
  in
  let errs = new_errs @ res.errors in
  {
    results = matches |> dedup_and_sort;
    errors = errs |> List_.map error_to_error;
    paths =
      {
        (* It seems that we have two separate paths, one for osemgrep
            (Cli_json_output.ml) and another for pysemgrep here. We
            should update this section to output the results
            specifically for pysemgrep. *)
        skipped = None;
        scanned = res.scanned |> List_.map Target.internal_path;
      };
    skipped_rules =
      res.skipped_rules
      |> List_.map (fun ((kind, rule_id, tk) : Rule_error.invalid_rule) ->
             let loc = Tok.unsafe_loc_of_tok tk in
             Out.
               {
                 rule_id;
                 details = Rule_error.string_of_invalid_rule_kind kind;
                 position = OutUtils.position_of_token_location loc;
               });
    time = res.profiling |> Option.map profiling_to_profiling;
    explanations =
      res.explanations |> Option.map (List_.map explanation_to_explanation);
    rules_by_engine = Some res.rules_by_engine;
    interfile_languages_used =
      Some
        (List_.map (fun l -> Analyzer.to_string l) res.interfile_languages_used);
    engine_requested = Some `OSS;
    version = Version.version;
  }
[@@profiling]

(******************************************************************************)
(* Exposed for testing *)
(******************************************************************************)

let test_core_unique_key = core_unique_key
