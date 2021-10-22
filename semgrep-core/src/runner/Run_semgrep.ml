open Common
open Runner_common
module PI = Parse_info
module E = Semgrep_error_code
module MR = Mini_rule
module R = Rule
module SJ = Semgrep_core_response_j
module RP = Report
module P = Parse_with_caching

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(* All the entry points to run semgrep *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let logger = Logging.get_logger [ __MODULE__ ]

(*
   If the target is a named pipe, copy it into a regular file and return
   that. This allows multiple reads on the file.

   This is intended to support one or a small number of targets created
   manually on the command line with e.g. <(echo 'eval(x)') which the
   shell replaces by a named pipe like '/dev/fd/63'.
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

let print_match ?str match_format mvars mvar_binding ii_of_any
    tokens_matched_code =
  (* there are a few fake tokens in the generic ASTs now (e.g.,
   * for DotAccess generated outside the grammar) *)
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
      |> List.map (fun x ->
             match Common2.assoc_opt x mvar_binding with
             | Some any ->
                 any |> ii_of_any
                 |> List.filter PI.is_origintok
                 |> List.map PI.str_of_info
                 |> Matching_report.join_with_space_if_needed
             | None -> failwith (spf "the metavariable '%s' was not binded" x))
    in
    pr (spf "%s:%d: %s" file line (Common.join ":" strings_metavars));
    ());
  toks |> List.iter (fun x -> Common.push x _matching_tokens)

(* when called from semgrep-python, error messages in semgrep-core or
 * certain profiling statistics may refer to rule id that are generated
 * by semgrep-python, making it hard to know what the problem is.
 * At least we can save this generated rule file to help debugging.
 *)
let save_rules_file_in_tmp rules_file =
  let tmp = Filename.temp_file "semgrep_core_rule-" ".yaml" in
  pr2 (spf "saving rules file for debugging in: %s" tmp);
  Common.write_file ~file:tmp (Common.read_file rules_file)

(* TODO duplicated in main *)
let lang_of_string s =
  match Lang.lang_of_string_opt s with
  | Some x -> x
  | None -> failwith (Lang.unsupported_language_message s)

(*****************************************************************************)
(* Parallelism *)
(*****************************************************************************)

(*
   Run jobs in parallel, using number of cores specified with -j.
*)
let map_targets ncores f (targets : Common.filename list) =
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
  let targets = Find_target.sort_by_decreasing_size targets in
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
    Parmap.parmap ~ncores ~chunksize:1 f (Parmap.L targets))

(*****************************************************************************)
(* Timeout *)
(*****************************************************************************)

let timeout_function file timeout f =
  let saved_busy_with_equal = !AST_utils.busy_with_equal in
  let timeout = if timeout <= 0. then None else Some timeout in
  match
    Common.set_timeout_opt ~verbose:false ~name:"Run_semgrep.timeout_function"
      timeout f
  with
  | Some res -> res
  | None ->
      (* Note that we could timeout while testing the equality of two ASTs and
       * `busy_with_equal` will then erroneously have a `<> Not_busy` value. *)
      AST_utils.busy_with_equal := saved_busy_with_equal;
      logger#info "Run_semgrep: timeout for file %s" file;
      raise (Main_timeout file)

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
    |> List.map (fun file ->
           (* logging useful info for rule writers *)
           logger#info "too many matches on %s, generating exn for it" file;
           let sorted_offending_rules =
             let matches = List.assoc file per_files in
             matches
             |> List.map (fun m ->
                    let rule_id = m.Pattern_match.rule_id in
                    ( ( rule_id.Pattern_match.id,
                        rule_id.Pattern_match.pattern_string ),
                      m ))
             |> Common.group_assoc_bykey_eff
             |> List.map (fun (k, xs) -> (k, List.length xs))
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
               E.TooManyMatches
           in
           let skipped =
             sorted_offending_rules
             |> List.map (fun ((rule_id, _pat), n) ->
                    let details =
                      spf
                        "found %i matches for rule %s, which exceeds the \
                         maximum of %i matches."
                        n rule_id max_match_per_file
                    in
                    {
                      Semgrep_core_response_t.path = file;
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
let exn_to_error file exn =
  match exn with
  | AST_generic.Error (s, tok) ->
      let loc = PI.unsafe_token_location_of_info tok in
      E.mk_error loc s AstBuilderError
  | _ -> E.exn_to_error file exn

(*****************************************************************************)
(* Iteration helpers *)
(*****************************************************************************)

let iter_files_and_get_matches_and_exn_to_errors config f files =
  files
  |> map_targets config.ncores (fun file ->
         logger#info "Analyzing %s" file;
         let res, run_time =
           Common.with_time (fun () ->
               try
                 Memory_limit.run_with_memory_limit
                   ~mem_limit_mb:config.max_memory_mb (fun () ->
                     timeout_function file config.timeout (fun () ->
                         f file |> fun v ->
                         (* This is just to test -max_memory, to give a chance
                          * to Gc.create_alarm to run even if the program does
                          * not even need to run the Gc. However, this has a slow
                          * perf penality on small programs, which is why it's
                          * better to keep guarded when you're
                          * not testing -max_memory.
                          *)
                         if config.test then Gc.full_major ();
                         logger#info "done with %s" file;
                         v))
               with
               (* note that Semgrep_error_code.exn_to_error already handles Timeout
                * and would generate a TimeoutError code for it, but we intercept
                * Timeout here to give a better diagnostic.
                *)
               | (Main_timeout _ | Out_of_memory) as exn ->
                   (match !Match_patterns.last_matched_rule with
                   | None -> ()
                   | Some rule ->
                       logger#info "critical exn while matching ruleid %s"
                         rule.MR.id;
                       logger#info "full pattern is: %s" rule.MR.pattern_string);
                   let loc = Parse_info.first_loc_of_file file in
                   {
                     RP.matches = [];
                     errors =
                       [
                         E.mk_error loc ""
                           (match exn with
                           | Main_timeout file ->
                               logger#info "Timeout on %s" file;
                               E.Timeout
                           | Out_of_memory ->
                               logger#info "OutOfMemory on %s" file;
                               E.OutOfMemory
                           | _ -> raise Impossible);
                       ];
                     skipped = [];
                     profiling = RP.empty_partial_profiling file;
                   }
               | exn when not config.fail_fast ->
                   {
                     RP.matches = [];
                     errors = [ exn_to_error file exn ];
                     skipped = [];
                     profiling = RP.empty_partial_profiling file;
                   })
         in
         RP.add_run_time run_time res)

let xlang_files_of_dirs_or_files xlang files_or_dirs =
  match xlang with
  | R.LRegex
  | R.LGeneric ->
      (* TODO: assert is_file ? spacegrep filter files?
       * Anyway right now the Semgrep python wrapper is
       * calling -config with an explicit list of files.
       *)
      (files_or_dirs, [])
  | R.L (lang, _) -> Find_target.files_of_dirs_or_files lang files_or_dirs

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(*****************************************************************************)
(* Semgrep -rules_file *)
(*****************************************************************************)
(* This is the main function used by the semgrep python wrapper right now.
 * It takes a language, a set of mini rules (rules with a single pattern,
 * no formula) and a set of files or dirs and recursively process those
 * files or dirs.
 *)
let semgrep_with_patterns config (rules, rule_parse_time) files skipped =
  logger#info "processing %d files" (List.length files);
  let lang = lang_of_string config.lang in
  let file_results =
    files
    |> iter_files_and_get_matches_and_exn_to_errors config (fun file ->
           let (ast, errors), parse_time =
             Common.with_time (fun () ->
                 P.parse_generic config.use_parsing_cache config.version lang
                   file)
           in
           let (matches, errors), match_time =
             Common.with_time (fun () ->
                 let rules =
                   rules |> List.filter (fun r -> List.mem lang r.MR.languages)
                 in
                 ( Match_patterns.check
                     ~hook:(fun _ _ -> ())
                     Config_semgrep.default_config rules
                     (P.parse_equivalences config.equivalences_file)
                     (file, lang, ast),
                   errors ))
           in
           {
             RP.matches;
             errors;
             skipped = [];
             profiling = { file; parse_time; match_time };
           })
  in
  let res =
    RP.make_rule_result file_results config.report_time rule_parse_time
  in
  let res = { res with skipped = skipped @ res.skipped } in
  logger#info "found %d matches, %d errors, %d skipped targets"
    (List.length res.RP.matches)
    (List.length res.RP.errors)
    (List.length res.RP.skipped);
  let matches, new_errors, new_skipped =
    filter_files_with_too_many_matches_and_transform_as_timeout
      config.max_match_per_file res.RP.matches
  in
  let errors = new_errors @ res.RP.errors in
  let skipped = new_skipped @ res.RP.skipped in
  let res =
    { RP.matches; errors; skipped; rule_profiling = res.RP.rule_profiling }
  in
  (* note: uncomment the following and use semgrep-core -stat_matches
   * to debug too-many-matches issues.
   * Common2.write_value matches "/tmp/debug_matches";
   *)
  let res = JSON_report.match_results_of_matches_and_errors files res in
  (* TODO need change type match_results.time to a choice
     let res =
       if !profile then (
         let json = JSON_report.json_of_profile_info !profile_start in
         (* so we don't get also the profile output of Common.main_boilerplate*)
         Common.profile := Common.ProfNone;
         flds @ [ ("profiling", json) ] )
       else flds
     in
  *)
  (*
     Not pretty-printing the json output (Yojson.Safe.prettify)
     because it kills performance, adding an extra 50% time on our
     calculate_ci_perf.py benchmarks.
     User should use an external tool like jq or ydump (latter comes with
     yojson) for pretty-printing json.
  *)
  let s = SJ.string_of_match_results res in
  logger#info "size of returned JSON string: %d" (String.length s);
  pr s

let semgrep_with_patterns_file config roots =
  let lang = lang_of_string config.lang in
  let rules_file = config.rules_file in
  let targets, skipped = Find_target.files_of_dirs_or_files lang roots in
  try
    logger#info "Parsing %s" rules_file;
    let timed_rules =
      Common.with_time (fun () -> Parse_mini_rule.parse rules_file)
    in
    semgrep_with_patterns config timed_rules targets skipped;
    if config.profile then save_rules_file_in_tmp rules_file
  with exn ->
    logger#debug "exn before exit %s" (Common.exn_to_s exn);
    (* if !Flag.debug then save_rules_file_in_tmp (); *)
    let res =
      {
        RP.matches = [];
        errors = [ E.exn_to_error "" exn ];
        skipped = [];
        rule_profiling = None;
      }
    in
    let json = JSON_report.match_results_of_matches_and_errors [] res in
    let s = SJ.string_of_match_results json in
    pr s;
    exit 2

(*****************************************************************************)
(* Semgrep -config *)
(*****************************************************************************)

let semgrep_with_rules config (rules, rule_parse_time) files_or_dirs =
  (* todo: at some point we should infer the lang from the rules and
   * apply different rules with different languages and different files
   * automatically, like the semgrep python wrapper.
   *
   * For now python wrapper passes down all files that should be scanned
   *)
  let xlang = R.xlang_of_string config.lang in
  let files, skipped = xlang_files_of_dirs_or_files xlang files_or_dirs in
  logger#info "processing %d files, skipping %d files" (List.length files)
    (List.length skipped);

  let file_results =
    files
    |> iter_files_and_get_matches_and_exn_to_errors config (fun file ->
           let rules =
             rules
             |> List.filter (fun r ->
                    match (r.R.languages, xlang) with
                    | R.L (x, xs), R.L (lang, _) -> List.mem lang (x :: xs)
                    | R.LRegex, R.LRegex
                    | R.LGeneric, R.LGeneric ->
                        true
                    | _ -> false)
           in
           let hook str env matched_tokens =
             if config.output_format = Text then
               let xs = Lazy.force matched_tokens in
               print_match ~str config.match_format config.mvars env
                 Metavariable.ii_of_mval xs
           in
           let lazy_ast_and_errors =
             lazy
               (match xlang with
               | R.L (lang, _) ->
                   P.parse_generic config.use_parsing_cache config.version lang
                     file
               | R.LRegex
               | R.LGeneric ->
                   failwith "requesting generic AST for LRegex|LGeneric")
           in
           let file_and_more =
             {
               File_and_more.file;
               xlang;
               lazy_content = lazy (Common.read_file file);
               lazy_ast_and_errors;
             }
           in
           let res =
             Run_rules.check hook Config_semgrep.default_config rules
               (P.parse_equivalences config.equivalences_file)
               file_and_more
           in
           RP.add_file file res)
  in
  let res =
    RP.make_rule_result file_results config.report_time rule_parse_time
  in
  let res = { res with skipped = skipped @ res.skipped } in
  logger#info "found %d matches, %d errors, %d skipped targets"
    (List.length res.matches) (List.length res.errors) (List.length res.skipped);
  let matches, new_errors, new_skipped =
    filter_files_with_too_many_matches_and_transform_as_timeout
      config.max_match_per_file res.matches
  in
  let skipped = new_skipped @ res.skipped in
  let errors = new_errors @ res.errors in
  let res =
    { RP.matches; errors; skipped; rule_profiling = res.RP.rule_profiling }
  in
  (* note: uncomment the following and use semgrep-core -stat_matches
   * to debug too-many-matches issues.
   * Common2.write_value matches "/tmp/debug_matches";
   *)
  match config.output_format with
  | Json ->
      let res = JSON_report.match_results_of_matches_and_errors files res in
      let s = SJ.string_of_match_results res in
      logger#info "size of returned JSON string: %d" (String.length s);
      pr s
  | Text ->
      (* the match has already been printed above. We just print errors here *)
      (* pr (spf "number of errors: %d" (List.length errs)); *)
      errors |> List.iter (fun err -> pr (E.string_of_error err))

let semgrep_with_rules_file config files_or_dirs =
  let rules_file = config.config_file in
  try
    logger#info "Parsing %s" rules_file;
    let timed_rules =
      Common.with_time (fun () -> Parse_rule.parse rules_file)
    in
    semgrep_with_rules config timed_rules files_or_dirs
  with exn when config.output_format = Json ->
    logger#debug "exn before exit %s" (Common.exn_to_s exn);
    let res =
      {
        RP.matches = [];
        errors = [ E.exn_to_error "" exn ];
        skipped = [];
        rule_profiling = None;
      }
    in
    let json = JSON_report.match_results_of_matches_and_errors [] res in
    let s = SJ.string_of_match_results json in
    pr s;
    exit 2

(*****************************************************************************)
(* Semgrep -e/-f *)
(*****************************************************************************)

let rule_of_pattern lang pattern_string pattern =
  {
    MR.id = "-e/-f";
    pattern_string;
    pattern;
    inside = false;
    message = "";
    severity = R.Error;
    languages = [ lang ];
  }

(* simpler code path compared to semgrep_with_rules *)
(* FIXME: don't use a different processing logic depending on the output
   format:
   - Pass a hook to semgrep_with_patterns for printing matches incrementally.
   - Have semgrep_with_patterns return the results and errors.
   - Print the final results (json or text) using dedicated functions.
*)
let semgrep_with_one_pattern config roots =
  (* old: let xs = List.map Common.fullpath xs in
   * better no fullpath here, not our responsability.
   *)
  let lang = lang_of_string config.lang in
  let pattern, pattern_string =
    match (config.pattern_file, config.pattern_string) with
    | "", "" -> failwith "I need a pattern; use -f or -e"
    | s1, s2 when s1 <> "" && s2 <> "" ->
        failwith "I need just one pattern; use -f OR -e (not both)"
    | file, _ when file <> "" ->
        let s = Common.read_file file in
        (P.parse_pattern lang s, s)
    (* this is for Emma, who often confuses -e with -f :) *)
    | _, s when s =~ ".*\\.sgrep$" ->
        failwith "you probably want -f with a .sgrep file, not -e"
    | _, s when s <> "" -> (P.parse_pattern lang s, s)
    | _ -> raise Impossible
  in
  let rule, rule_parse_time =
    Common.with_time (fun () -> [ rule_of_pattern lang pattern_string pattern ])
  in

  let targets, skipped = Find_target.files_of_dirs_or_files lang roots in
  let targets = Common.map replace_named_pipe_by_regular_file targets in
  match config.output_format with
  | Json ->
      (* closer to -rules_file, but no incremental match output *)
      semgrep_with_patterns config (rule, rule_parse_time) targets skipped
  | Text ->
      (* simpler code path than in semgrep_with_rules *)
      targets
      |> List.iter (fun file ->
             logger#info "processing: %s" file;
             let process file =
               timeout_function file config.timeout (fun () ->
                   let ast, errors =
                     P.parse_generic config.use_parsing_cache config.version
                       lang file
                   in
                   if errors <> [] then
                     pr2 (spf "WARNING: fail to fully parse %s" file);
                   Match_patterns.check
                     ~hook:(fun env matched_tokens ->
                       let xs = Lazy.force matched_tokens in
                       print_match config.match_format config.mvars env
                         Metavariable.ii_of_mval xs)
                     Config_semgrep.default_config rule
                     (P.parse_equivalences config.equivalences_file)
                     (file, lang, ast)
                   |> ignore)
             in

             if not config.error_recovery then
               E.try_with_print_exn_and_reraise file (fun () -> process file)
             else E.try_with_exn_to_error file (fun () -> process file));

      let n = List.length !E.g_errors in
      if n > 0 then pr2 (spf "error count: %d" n);
      (* TODO: what's that? *)
      Experiments.gen_layer_maybe _matching_tokens pattern_string targets
