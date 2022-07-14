(* Yoann Padioleau
 *
 * Copyright (C) 2020-2022 r2c
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
open Runner_config
module PI = Parse_info
module E = Semgrep_error_code
module MR = Mini_rule
module R = Rule
module RP = Report
module In = Input_to_core_j
module Out = Output_from_core_j

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Entry points to the semgrep engine with its command-line configuration.
 *
 * This used to be in Main.ml, but Main.ml started to become really big,
 * and we also need a way to run the semgrep engine from semgrep-core
 * variants, hence this callable library.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*
   If the target is a named pipe, copy it into a regular file and return
   that. This allows multiple reads on the file.

   This is intended to support one or a small number of targets created
   manually on the command line with e.g. <(echo 'eval(x)') which the
   shell replaces by a named pipe like '/dev/fd/63'.

   update: This can be used also to fetch rules from the network!
   e.g., semgrep-core -rules <(curl https://semgrep.dev/c/p/ocaml) ...

   coupling: this functionality is implemented also in semgrep-python.
*)
let replace_named_pipe_by_regular_file path =
  match (Unix.stat path).st_kind with
  | Unix.S_FIFO ->
      let data = Common.read_file path in
      let prefix = spf "semgrep-core-" in
      let suffix = spf "-%s" (Filename.basename path) in
      let tmp_path, oc =
        Filename.open_temp_file
          ~mode:[ Open_creat; Open_excl; Open_wronly; Open_binary ]
          prefix suffix
      in
      let remove () = if Sys.file_exists tmp_path then Sys.remove tmp_path in
      (* Try to remove temporary file when program exits. *)
      at_exit remove;
      Fun.protect
        ~finally:(fun () -> close_out_noerr oc)
        (fun () -> output_string oc data);
      tmp_path
  | _ -> path

(* for -gen_layer, see Experiments.ml *)
let _matching_tokens = ref []

let string_of_toks toks =
  String.concat ", " (Common.map (fun tok -> PI.str_of_info tok) toks)

let rec print_taint_call_trace ~format ~spaces = function
  | Pattern_match.Toks toks -> Matching_report.print_match ~format ~spaces toks
  | Call { call_toks; intermediate_vars; call_trace } ->
      let spaces_string = String.init spaces (fun _ -> ' ') in
      pr (spaces_string ^ "call to");
      Matching_report.print_match ~format ~spaces call_toks;
      if intermediate_vars <> [] then
        pr
          (spf "%sthese intermediate values are tainted: %s" spaces_string
             (string_of_toks intermediate_vars));
      pr (spaces_string ^ "then");
      print_taint_call_trace ~format ~spaces:(spaces + 2) call_trace

let print_taint_trace ~format taint_trace =
  if format <> Matching_report.Normal then ()
  else
    let (lazy { Pattern_match.source; tokens; sink }) = taint_trace in
    pr "  * Taint comes from:";
    print_taint_call_trace ~format ~spaces:4 source;
    if tokens <> [] then
      pr
        (spf "  * These intermediate values are tainted: %s"
           (string_of_toks tokens));
    match sink with
    | Pattern_match.Toks _ -> ()
    | Call _ ->
        pr "  * This is how taint reaches the sink:";
        print_taint_call_trace ~format ~spaces:4 sink

let print_match ?str config match_ ii_of_any =
  (* there are a few fake tokens in the generic ASTs now (e.g.,
   * for DotAccess generated outside the grammar) *)
  let { match_format; mvars; _ } = config in
  let { Pattern_match.env; tokens = (lazy tokens_matched_code); taint_trace; _ }
      =
    match_
  in
  let toks = tokens_matched_code |> List.filter PI.is_origintok in
  (if mvars = [] then Matching_report.print_match ?str ~format:match_format toks
  else
    (* similar to the code of Lib_matcher.print_match, maybe could
     * factorize code a bit.
     *)
    let mini, _maxi = PI.min_max_ii_by_pos toks in
    let file, line = (PI.file_of_info mini, PI.line_of_info mini) in

    let strings_metavars =
      mvars
      |> Common.map (fun x ->
             match Common2.assoc_opt x env with
             | Some any ->
                 any |> ii_of_any
                 |> List.filter PI.is_origintok
                 |> Common.map PI.str_of_info
                 |> Matching_report.join_with_space_if_needed
             | None -> failwith (spf "the metavariable '%s' was not binded" x))
    in
    pr (spf "%s:%d: %s" file line (Common.join ":" strings_metavars));
    ());
  Option.iter (print_taint_trace ~format:match_format) taint_trace;
  toks |> List.iter (fun x -> Common.push x _matching_tokens)

let timeout_function file timeout f =
  let timeout = if timeout <= 0. then None else Some timeout in
  match
    Common.set_timeout_opt ~name:"Run_semgrep.timeout_function" timeout f
  with
  | Some res -> res
  | None ->
      let loc = PI.first_loc_of_file file in
      let err = E.mk_error loc "" Out.Timeout in
      Common.push err E.g_errors

(*****************************************************************************)
(* Parallelism *)
(*****************************************************************************)

(*
   Run jobs in parallel, using number of cores specified with -j.
*)
let map_targets ncores f (targets : In.target list) =
  (*
     Sorting the targets by decreasing size is based on the assumption
     that larger targets will take more time to process. Starting with
     the longer jobs allows parmap to feed the workers with shorter and
     shorter jobs, as a way of maximizing CPU usage.
     This is a kind of greedy algorithm, which is in general not optimal
     but hopefully good enough in practice.

     This is needed only when ncores > 1, but to reduce discrepancy between
     the two modes, we always sort the target queue in the same way.
  *)
  let targets = Find_target.sort_targets_by_decreasing_size targets in
  if ncores <= 1 then Common.map f targets
  else (
    (*
       Parmap creates ncores children processes which listen for
       chunks of input. When a chunk size is specified, parmap feeds
       the ncores processes in small chunks of the specified size
       instead of just dividing the input list into exactly ncores chunks.

       Since our jobs are relatively big compared to the serialization
       and communication overhead, setting the chunk size to 1 works
       fine.  We don't want to have two giant target files in the same
       chunk, so this setting takes care of it.
    *)
    (* Quoting Parmap's README:
     * > To obtain maximum speed, Parmap tries to pin the worker processes to a CPU
     * Unfortunately, on the new Apple M1, and depending on the number of workers,
     * Parmap will enter an infinite loop trying (but failing) to pin a worker to
     * CPU 0. This only happens with HomeBrew installs, presumably because under
     * HomeBrew's build environment HAVE_MACH_THREAD_POLICY_H is set
     * (https://github.com/rdicosmo/parmap/blob/1.2.3/src/setcore_stubs.c#L47).
     * So, despite it may hurt perf a bit, we disable core pinning to work around
     * this issue until this is fixed in a future version of Parmap.
     *)
    Parmap.disable_core_pinning ();
    assert (ncores > 0);
    let init _ = Logging.add_PID_tag () in
    Parmap.parmap ~init ~ncores ~chunksize:1 f (Parmap.L targets))

(*****************************************************************************)
(* Timeout *)
(*****************************************************************************)

(* Certain patterns may be too general and match too many times on big files.
 * This does not cause a Timeout during parsing or matching, but returning
 * a huge number of matches can stress print_matches_and_errors_json
 * and anyway is probably a sign that the pattern should be rewritten.
 * This puts also lots of stress on the semgrep Python wrapper which has
 * to do lots of range intersections with all those matches.
 *)
let filter_files_with_too_many_matches_and_transform_as_timeout
    max_match_per_file matches =
  let per_files =
    matches
    |> Common.map (fun m -> (m.Pattern_match.file, m))
    |> Common.group_assoc_bykey_eff
  in
  let offending_file_list =
    per_files
    |> List.filter_map (fun (file, xs) ->
           if List.length xs > max_match_per_file then Some file else None)
  in
  let offending_files = Common.hashset_of_list offending_file_list in
  let new_matches =
    matches
    |> Common.exclude (fun m ->
           Hashtbl.mem offending_files m.Pattern_match.file)
  in
  let new_errors, new_skipped =
    offending_file_list
    |> Common.map (fun file ->
           (* logging useful info for rule writers *)
           logger#info "too many matches on %s, generating exn for it" file;
           let sorted_offending_rules =
             let matches = List.assoc file per_files in
             matches
             |> Common.map (fun m ->
                    let rule_id = m.Pattern_match.rule_id in
                    ( ( rule_id.Pattern_match.id,
                        rule_id.Pattern_match.pattern_string ),
                      m ))
             |> Common.group_assoc_bykey_eff
             |> Common.map (fun (k, xs) -> (k, List.length xs))
             |> Common.sort_by_val_highfirst
             (* nosemgrep *)
           in
           let offending_rules = List.length sorted_offending_rules in
           let biggest_offending_rule =
             match sorted_offending_rules with
             | x :: _ -> x
             | _ -> assert false
           in
           let (id, pat), cnt = biggest_offending_rule in
           logger#info
             "most offending rule: id = %s, matches = %d, pattern = %s" id cnt
             pat;

           (* todo: we should maybe use a new error: TooManyMatches of int * string*)
           let loc = Parse_info.first_loc_of_file file in
           let error =
             E.mk_error ~rule_id:(Some id) loc
               (spf
                  "%d rules result in too many matches, most offending rule \
                   has %d: %s"
                  offending_rules cnt pat)
               Out.TooManyMatches
           in
           let skipped =
             sorted_offending_rules
             |> Common.map (fun ((rule_id, _pat), n) ->
                    let details =
                      spf
                        "found %i matches for rule %s, which exceeds the \
                         maximum of %i matches."
                        n rule_id max_match_per_file
                    in
                    {
                      Output_from_core_t.path = file;
                      reason = Too_many_matches;
                      details;
                      rule_id = Some rule_id;
                    })
           in
           (error, skipped))
    |> List.split
  in
  (new_matches, new_errors, List.flatten new_skipped)
  [@@profiling "Run_semgrep.filter_too_many_matches"]

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)
(* Small wrapper over Semgrep_error_code.exn_to_error to handle also semgrep-specific
 * exns that have a position.
 *
 * See also JSON_report.json_of_exn for non-target related exn handling.
 *
 * invariant: every target-related semgrep-specific exn that has a
 * Parse_info.t should be captured here for precise location in error
 * reporting.
 *  - TODO: naming exns?
 *)
let exn_to_error file (e : Exception.t) =
  match Exception.get_exn e with
  | AST_generic.Error (s, tok) ->
      let loc = PI.unsafe_token_location_of_info tok in
      E.mk_error loc s AstBuilderError
  | _ -> E.exn_to_error file e

(*****************************************************************************)
(* Parsing (non-cached) *)
(*****************************************************************************)

(* Experiment *)
let process_metatypes metatypes_file =
  let metatypes = Parse_rule.parse_metatypes metatypes_file in
  Hooks.metatypes := Some metatypes

(* TODO? this is currently deprecated, but pad still has hope the
 * feature can be resurrected.
 *)
let parse_equivalences equivalences_file =
  match equivalences_file with
  | "" -> []
  | file -> Parse_equivalences.parse file
  [@@profiling]

let parse_pattern lang_pattern str =
  try Parse_pattern.parse_pattern lang_pattern ~print_errors:false str with
  | exn ->
      logger#error "parse_pattern: exn = %s" (Common.exn_to_s exn);
      raise
        (Rule.InvalidRule
           ( Rule.InvalidPattern
               (str, Xlang.of_lang lang_pattern, Common.exn_to_s exn, []),
             "no-id",
             Parse_info.unsafe_fake_info "no loc" ))
  [@@profiling]

(*****************************************************************************)
(* Iteration helpers *)
(*****************************************************************************)

let iter_targets_and_get_matches_and_exn_to_errors config f targets =
  targets
  |> map_targets config.ncores (fun target ->
         let file = target.In.path in
         logger#info "Analyzing %s" file;
         let res, run_time =
           Common.with_time (fun () ->
               try
                 let get_context () =
                   match !Rule.last_matched_rule with
                   | None -> file
                   | Some rule_id -> spf "%s on %s" rule_id file
                 in
                 Memory_limit.run_with_memory_limit ~get_context
                   ~mem_limit_mb:config.max_memory_mb (fun () ->
                     (* we used to call timeout_function() here, but this
                      * is now done in Match_rules because we now
                      * timeout per rule, not per file since semgrep-python
                      * pass all the rules to semgrep-core.
                      *
                      * old: timeout_function file config.timeout ...
                      *)
                     f target |> fun v ->
                     (* This is just to test -max_memory, to give a chance
                      * to Gc.create_alarm to run even if the program does
                      * not even need to run the Gc. However, this has a
                      * slow perf penality on small programs, which is why
                      * it's better to keep guarded when you're
                      * not testing -max_memory.
                      *)
                     if config.test then Gc.full_major ();
                     logger#trace "done with %s" file;
                     v)
               with
               (* note that Semgrep_error_code.exn_to_error already handles
                * Timeout and would generate a TimeoutError code for it,
                * but we intercept Timeout here to give a better diagnostic.
                *)
               | (Match_rules.File_timeout | Out_of_memory) as exn ->
                   (match !Match_patterns.last_matched_rule with
                   | None -> ()
                   | Some rule ->
                       logger#info "critical exn while matching ruleid %s"
                         rule.MR.id;
                       logger#info "full pattern is: %s" rule.MR.pattern_string);
                   let loc = Parse_info.first_loc_of_file file in
                   let errors =
                     [
                       E.mk_error ~rule_id:!Rule.last_matched_rule loc ""
                         (match exn with
                         | Match_rules.File_timeout ->
                             logger#info "Timeout on %s" file;
                             Out.Timeout
                         | Out_of_memory ->
                             logger#info "OutOfMemory on %s" file;
                             Out.OutOfMemory
                         | _ -> raise Impossible);
                     ]
                   in
                   RP.make_match_result [] errors
                     (RP.empty_partial_profiling file)
               (* those were converted in Main_timeout in timeout_function()*)
               | Timeout _ -> assert false
               | exn when not !Flag_semgrep.fail_fast ->
                   let e = Exception.catch exn in
                   let errors = [ exn_to_error file e ] in
                   RP.make_match_result [] errors
                     (RP.empty_partial_profiling file))
         in
         RP.add_run_time run_time res)

(*****************************************************************************)
(* File targeting and rule filtering *)
(*****************************************************************************)

let rules_for_xlang xlang rules =
  rules
  |> List.filter (fun r ->
         match (xlang, r.R.languages) with
         | Xlang.LRegex, Xlang.LRegex
         | Xlang.LGeneric, Xlang.LGeneric ->
             true
         | Xlang.L (x, _empty), Xlang.L (y, ys) -> List.mem x (y :: ys)
         | (Xlang.LRegex | Xlang.LGeneric | Xlang.L _), _ -> false)

let mk_rule_table rules =
  let rule_pairs = Common.map (fun r -> (fst r.R.id, r)) rules in
  Common.hash_of_list rule_pairs

let xtarget_of_file config xlang file =
  let lazy_ast_and_errors =
    match xlang with
    | Xlang.L (lang, other_langs) ->
        (* xlang from the language field in -target, which should be unique *)
        assert (other_langs = []);
        lazy
          (Parse_with_caching.parse_and_resolve_name
             ~parsing_cache_dir:config.parsing_cache_dir
             (* alt: could define a AST_generic.version *)
             config.version lang file)
    | _ -> lazy (failwith "requesting generic AST for LRegex|LGeneric")
  in

  {
    Xtarget.file;
    xlang;
    lazy_content = lazy (Common.read_file file);
    lazy_ast_and_errors;
  }

let targets_of_config (config : Runner_config.t)
    (all_rule_ids_when_no_target_file : Rule.rule_id list) :
    In.targets * Out.skipped_target list =
  match (config.target_file, config.roots, config.lang) with
  (* We usually let semgrep-python computes the list of targets (and pass it
   * via -target), but it's convenient to also run semgrep-core without
   * semgrep-python and to recursively get a list of targets.
   * We just have a poor's man file targeting/filtering here, just enough
   * to run semgrep-core independently of semgrep-python to test things.
   *)
  | "", roots, Some xlang ->
      (* less: could also apply Common.fullpath? *)
      let roots = roots |> Common.map replace_named_pipe_by_regular_file in
      let lang_opt =
        match xlang with
        | Xlang.LRegex
        | Xlang.LGeneric ->
            None (* we will get all the files *)
        | Xlang.L (lang, []) -> Some lang
        (* config.lang comes from Xlang.of_string which returns just a lang *)
        | Xlang.L (_, _) -> assert false
      in
      let files, skipped = Find_target.files_of_dirs_or_files lang_opt roots in
      let targets =
        files
        |> Common.map (fun file ->
               {
                 In.path = file;
                 language = Xlang.to_string xlang;
                 rule_ids = all_rule_ids_when_no_target_file;
               })
      in
      (targets, skipped)
  | "", _, None -> failwith "you need to specify a language with -lang"
  (* main code path for semgrep python, with targets specified by -target *)
  | target_file, roots, lang_opt ->
      let str = Common.read_file target_file in
      let targets = In.targets_of_string str in
      let skipped = [] in
      (* in deep mode we actually have a single root dir passed *)
      if roots <> [] then
        logger#error "if you use -targets, you should not specify files";
      (* TODO: ugly, this is because the code path for -e/-f requires
       * a language, even with a -target, see test_target_file.py
       *)
      if lang_opt <> None && config.rules_file <> "" then
        failwith "if you use -targets and -rules, you should not specify a lang";
      (targets, skipped)

(**
   Generate the list of targets to run extract rules against given a config,
   the ids for rules to run against the generated targets and a set of extract
   rules used to perform the extraction.
*)
let extract_targets_of_config config rule_ids extractors =
  let erule_ids = Common.map (fun r -> fst r.R.id) extractors in
  let extract_targets, extract_skipped = targets_of_config config erule_ids in
  let match_hook str match_ =
    if config.output_format = Text then (
      print_string "extracted content from ";
      print_match ~str config match_ Metavariable.ii_of_mval)
  in
  logger#info "extracting nested content from %d files, skipping %d files"
    (List.length extract_targets)
    (List.length extract_skipped);
  List.concat_map
    (fun t ->
      (* TODO: addt'l filtering required for rule_ids when targets are
         passed explicitly? *)
      let file = t.In.path in
      let xlang = Xlang.of_string t.In.language in
      let xtarget = xtarget_of_file config xlang file in
      let extract_targets =
        Extract.extract_nested_lang ~match_hook ~timeout:config.timeout
          ~timeout_threshold:config.timeout_threshold extractors xtarget
          rule_ids
      in
      (* Print number of extra targets so Python knows *)
      if config.output_format = Json && extract_targets <> [] then
        pr (string_of_int (List.length extract_targets));
      extract_targets)
    extract_targets
  |> fun extracted_ranges ->
  (List.fold_right (fun (t, fn) (ts, fn_tbl) ->
       Hashtbl.add fn_tbl t.In.path fn;
       (t :: ts, fn_tbl)))
    extracted_ranges
    ([], Hashtbl.create (List.length extract_targets))

(*****************************************************************************)
(* Semgrep -config *)
(*****************************************************************************)

(* This is the main function used by the semgrep python wrapper right now.
 * It takes a set of rules and a set of targets (targets derived from config,
 * and potentially also extract rules) and recursively process those targets.
 *)
let semgrep_with_rules config ((rules, invalid_rules), rules_parse_time) =
  (* Return an exception
     - always, if there are no rules but just invalid rules
     - when users want to fail fast, if there are valid and invalid rules *)
  (match (rules, invalid_rules) with
  | [], [] -> ()
  | [], err :: _ -> raise (Rule.InvalidRule err)
  | _, err :: _ (* TODO fail fast when only when strict? *) ->
      raise (Rule.InvalidRule err)
  | _, [] -> ());

  let rules, extract_rules =
    Common.partition_either
      (fun r ->
        match r.Rule.mode with
        | `Extract _ as e -> Right { r with mode = e }
        | mode -> Left { r with mode })
      rules
  in

  let rule_ids = Common.map (fun r -> fst r.R.id) rules in

  let extracted_targets, extract_result_map =
    extract_targets_of_config config rule_ids extract_rules
  in
  (* TODO: possibly extract (recursively) from generated stuff *)
  if not (config.metatypes_file = "") then
    process_metatypes config.metatypes_file;
  let rule_table = mk_rule_table rules in
  let targets, skipped = targets_of_config config rule_ids in
  logger#info "processing %d files, skipping %d files" (List.length targets)
    (List.length skipped);
  let file_results =
    targets @ extracted_targets
    |> iter_targets_and_get_matches_and_exn_to_errors config (fun target ->
           let file = target.In.path in
           let xlang = Xlang.of_string target.In.language in
           let rules =
             (* Assumption: find_opt will return None iff a r_id
                 is in skipped_rules *)
             List.filter_map
               (fun r_id -> Hashtbl.find_opt rule_table r_id)
               target.In.rule_ids
           in

           let xtarget = xtarget_of_file config xlang file in
           let match_hook str match_ =
             if config.output_format = Text then
               print_match ~str config match_ Metavariable.ii_of_mval
           in
           let res =
             Match_rules.check ~match_hook ~timeout:config.timeout
               ~timeout_threshold:config.timeout_threshold
               ( Config_semgrep.default_config,
                 parse_equivalences config.equivalences_file )
               rules xtarget
           in
           if config.output_format = Json then pr ".";
           (* Print when each file is done so Python knows *)
           Hashtbl.find_opt extract_result_map file
           |> Option.fold ~some:(fun f -> f res) ~none:res)
  in
  let res = RP.make_final_result file_results rules ~rules_parse_time in
  logger#info "found %d matches, %d errors" (List.length res.matches)
    (List.length res.errors);

  let matches, new_errors, new_skipped =
    filter_files_with_too_many_matches_and_transform_as_timeout
      config.max_match_per_file res.matches
  in

  (* note: uncomment the following and use semgrep-core -stat_matches
   * to debug too-many-matches issues.
   * Common2.write_value matches "/tmp/debug_matches";
   *)

  (* Concatenate all the skipped targets *)
  let extra =
    match res.extra with
    | RP.Debug { skipped_targets; profiling } ->
        let skipped_targets = skipped @ new_skipped @ skipped_targets in
        logger#info "there were %d skipped targets"
          (List.length skipped_targets);
        RP.Debug { skipped_targets; profiling }
    | RP.Time profiling -> RP.Time profiling
    | RP.No_info -> RP.No_info
  in
  let errors = new_errors @ res.errors in
  ( { RP.matches; errors; skipped_rules = invalid_rules; extra },
    targets |> Common.map (fun x -> x.In.path) )

let semgrep_with_raw_results_and_exn_handler config =
  let rules_file = config.rules_file in
  (* useful when using process substitution, e.g.
   * semgrep-core -rules <(curl https://semgrep.dev/c/p/ocaml) ...
   *)
  let rules_file = replace_named_pipe_by_regular_file rules_file in
  try
    logger#linfo
      (lazy (spf "Parsing %s:\n%s" rules_file (read_file rules_file)));
    let timed_rules =
      Common.with_time (fun () ->
          Parse_rule.parse_and_filter_invalid_rules rules_file)
    in
    let res, files = semgrep_with_rules config timed_rules in
    (None, res, files)
  with
  | exn when not !Flag_semgrep.fail_fast ->
      let e = Exception.catch exn in
      logger#info "Uncaught exception: %s" (Exception.to_string e);
      let res =
        { RP.empty_final_result with errors = [ E.exn_to_error "" e ] }
      in
      (Some e, res, [])

let semgrep_with_rules_and_formatted_output config =
  let exn, res, files = semgrep_with_raw_results_and_exn_handler config in
  (* note: uncomment the following and use semgrep-core -stat_matches
   * to debug too-many-matches issues.
   * Common2.write_value matches "/tmp/debug_matches";
   *)
  match config.output_format with
  | Json -> (
      let res = JSON_report.match_results_of_matches_and_errors files res in
      (*
        Not pretty-printing the json output (Yojson.Safe.prettify)
        because it kills performance, adding an extra 50% time on our
        calculate_ci_perf.py benchmarks.
        User should use an external tool like jq or ydump (latter comes with
        yojson) for pretty-printing json.
      *)
      let s = Out.string_of_core_match_results res in
      logger#info "size of returned JSON string: %d" (String.length s);
      pr s;
      match exn with
      | Some e -> Runner_exit.exit_semgrep (Unknown_exception e)
      | None -> ())
  | Text ->
      (* the match has already been printed above. We just print errors here *)
      if not (null res.errors) then (
        pr "WARNING: some files were skipped on only partially analyzed:";
        res.errors |> List.iter (fun err -> pr (E.string_of_error err)))

(*****************************************************************************)
(* Semgrep -e/-f *)
(*****************************************************************************)

let minirule_of_pattern lang pattern_string pattern =
  {
    MR.id = "-e/-f";
    pattern_string;
    pattern;
    inside = false;
    message = "";
    severity = R.Error;
    languages = [ lang ];
  }

let rule_of_pattern lang pattern_string pattern =
  let fk = PI.unsafe_fake_info "" in
  let xlang = Xlang.L (lang, []) in
  let xpat =
    Xpattern.mk_xpat (Xpattern.Sem (pattern, lang)) (pattern_string, fk)
  in
  {
    R.id = ("-e/-f", fk);
    mode = `Search (R.New (R.P (xpat, None)));
    message = "";
    severity = R.Error;
    languages = xlang;
    options = None;
    equivalences = None;
    fix = None;
    fix_regexp = None;
    paths = None;
    metadata = None;
  }

(* less: could be nice to generalize to rule_of_config, but we sometimes
 * need to generate a rule, sometimes a minirule
 *)
let pattern_of_config lang config =
  match (config.pattern_file, config.pattern_string) with
  | "", "" -> failwith "I need a pattern; use -f or -e"
  | s1, s2 when s1 <> "" && s2 <> "" ->
      failwith "I need just one pattern; use -f OR -e (not both)"
  | file, _ when file <> "" ->
      let s = Common.read_file file in
      (parse_pattern lang s, s)
  (* this is for Emma, who often confuses -e with -f :) *)
  | _, s when s =~ ".*\\.sgrep$" ->
      failwith "you probably want -f with a .sgrep file, not -e"
  | _, s when s <> "" -> (parse_pattern lang s, s)
  | _ -> raise Impossible

(* simpler code path compared to semgrep_with_rules *)
(* FIXME: don't use a different processing logic depending on the output
   format:
   - Pass a hook to semgrep_with_patterns for printing matches incrementally.
   - Have semgrep_with_patterns return the results and errors.
   - Print the final results (json or text) using dedicated functions.
*)
let semgrep_with_one_pattern config =
  assert (config.rules_file = "");

  (* TODO: support generic and regex patterns as well? See code in Deep. *)
  let lang = Xlang.lang_of_opt_xlang config.lang in
  let pattern, pattern_string = pattern_of_config lang config in

  match config.output_format with
  | Json ->
      let rule, rules_parse_time =
        Common.with_time (fun () -> rule_of_pattern lang pattern_string pattern)
      in
      let res, files =
        semgrep_with_rules config (([ rule ], []), rules_parse_time)
      in
      let json = JSON_report.match_results_of_matches_and_errors files res in
      let s = Out.string_of_core_match_results json in
      pr s
  | Text ->
      let minirule, _rules_parse_time =
        Common.with_time (fun () ->
            [ minirule_of_pattern lang pattern_string pattern ])
      in
      (* simpler code path than in semgrep_with_rules *)
      let targets, _skipped =
        targets_of_config config (Common.map (fun r -> r.MR.id) minirule)
      in
      let files = targets |> Common.map (fun t -> t.In.path) in
      files
      |> List.iter (fun file ->
             logger#info "processing: %s" file;
             let process file =
               timeout_function file config.timeout (fun () ->
                   let ast =
                     Parse_target.parse_and_resolve_name_warn_if_partial lang
                       file
                   in
                   Match_patterns.check
                     ~hook:(fun match_ ->
                       print_match config match_ Metavariable.ii_of_mval)
                     ( Config_semgrep.default_config,
                       parse_equivalences config.equivalences_file )
                     minirule (file, lang, ast)
                   |> ignore)
             in

             if not config.error_recovery then
               E.try_with_print_exn_and_reraise file (fun () -> process file)
             else E.try_with_exn_to_error file (fun () -> process file));

      let n = List.length !E.g_errors in
      if n > 0 then pr2 (spf "error count: %d" n);
      (* This is for pad's codemap visualizer *)
      Experiments.gen_layer_maybe _matching_tokens pattern_string files

(*****************************************************************************)
(* Semgrep dispatch *)
(*****************************************************************************)
let semgrep_dispatch config =
  if config.rules_file <> "" then semgrep_with_rules_and_formatted_output config
  else semgrep_with_one_pattern config
