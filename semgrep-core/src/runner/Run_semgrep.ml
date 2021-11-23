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
(* Parsing (non-cached) *)
(*****************************************************************************)

let parse_equivalences equivalences_file =
  match equivalences_file with
  | "" -> []
  | file -> Parse_equivalences.parse file
  [@@profiling]

let parse_pattern lang_pattern str =
  try
    Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
        let res =
          Parse_pattern.parse_pattern lang_pattern ~print_errors:false str
        in
        res)
  with exn ->
    raise
      (Rule.InvalidPattern
         ( "no-id",
           str,
           Xlang.of_lang lang_pattern,
           Common.exn_to_s exn,
           Parse_info.unsafe_fake_info "no loc",
           [] ))
  [@@profiling]

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

let xlang_files_of_dirs_or_files (xlang : Xlang.t) files_or_dirs =
  let opt_lang =
    match xlang with
    | LRegex
    | LGeneric ->
        None
    | L (lang, _other_langs) ->
        (* FIXME: we should include other_langs if there are any! *)
        Some lang
  in
  Find_target.files_of_dirs_or_files opt_lang files_or_dirs

(*****************************************************************************)
(* Semgrep -config *)
(*****************************************************************************)
(* This is the main function used by the semgrep python wrapper right now.
 * It takes a language, a set of rules and a set of files or dirs and
 * recursively process those files or dirs.
 *)

let semgrep_with_rules config (rules, rule_parse_time) files_or_dirs =
  (* todo: at some point we should infer the lang from the rules and
   * apply different rules with different languages and different files
   * automatically, like the semgrep python wrapper.
   *
   * For now python wrapper passes down all files that should be scanned
   *)
  let xlang = Xlang.of_opt_xlang config.lang in
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
                    | L (x, xs), L (lang, _) -> List.mem lang (x :: xs)
                    | LRegex, LRegex
                    | LGeneric, LGeneric ->
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
               | L (lang, _) ->
                   P.parse_generic config.use_parsing_cache config.version lang
                     file
               | LRegex
               | LGeneric ->
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
               (parse_equivalences config.equivalences_file)
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
  (* note: uncomment the following and use semgrep-core -stat_matches
   * to debug too-many-matches issues.
   * Common2.write_value matches "/tmp/debug_matches";
   *)
  let skipped = new_skipped @ res.skipped in
  let errors = new_errors @ res.errors in
  ( { RP.matches; errors; skipped; rule_profiling = res.RP.rule_profiling },
    files )

let semgrep_with_raw_results_and_exn_handler config files_or_dirs =
  let rules_file = config.config_file in
  try
    logger#info "Parsing %s" rules_file;
    let timed_rules =
      Common.with_time (fun () -> Parse_rule.parse rules_file)
    in
    let res, files = semgrep_with_rules config timed_rules files_or_dirs in
    (None, res, files)
  with exn ->
    let trace = Printexc.get_backtrace () in
    logger#info "Uncaught exception: %s\n%s" (Common.exn_to_s exn) trace;
    let res =
      {
        RP.matches = [];
        errors = [ E.exn_to_error "" exn ];
        skipped = [];
        rule_profiling = None;
      }
    in
    (Some exn, res, [])

let semgrep_with_formatted_output config files_or_dirs =
  let exn, res, files =
    semgrep_with_raw_results_and_exn_handler config files_or_dirs
  in
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
      let s = SJ.string_of_match_results res in
      logger#info "size of returned JSON string: %d" (String.length s);
      pr s;
      match exn with
      | Some e -> Runner_exit.exit_semgrep (Unknown_exception e)
      | None -> ())
  | Text ->
      (* the match has already been printed above. We just print errors here *)
      (* pr (spf "number of errors: %d" (List.length errs)); *)
      res.errors |> List.iter (fun err -> pr (E.string_of_error err))

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
  (* TODO: support generic and regex patterns as well? *)
  let lang = Xlang.lang_of_opt_xlang config.lang in
  let pattern, pattern_string =
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
  in
  let rule, _rule_parse_time =
    Common.with_time (fun () -> [ rule_of_pattern lang pattern_string pattern ])
  in

  let targets, _skipped =
    Find_target.files_of_dirs_or_files (Some lang) roots
  in
  let targets = Common.map replace_named_pipe_by_regular_file targets in
  match config.output_format with
  | Json ->
      (* closer to -rules_file, but no incremental match output *)
      (*      semgrep_with_patterns config (rule, rule_parse_time) targets skipped *)
      failwith "TODO: -e/-f and JSON"
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
                     (parse_equivalences config.equivalences_file)
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
