(* Yoann Padioleau
 *
 * Copyright (C) 2020-2023 r2c
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
open File.Operators
open Core_scan_config
module PM = Pattern_match
module E = Core_error
module MR = Mini_rule
module R = Rule
module RP = Core_result
module In = Input_to_core_j
module Out = Semgrep_output_v1_j

let logger = Logging.get_logger [ __MODULE__ ]
let debug_extract_mode = ref false

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* A "Core" scan.
 *
 * # Notes
 *
 * ## pysemgrep vs semgrep core
 *
 * Officially, `semgrep-core` is never run on its own. External users run
 * `semgrep`, which invokes `semgrep-core` with the appropriate rules and targets.
 * However, for development purposes it can be convenient to skip the wrapper.
 * Therefore, we also maintain some code paths that allow `semgrep-core` to take
 * in rules or patterns and perform its own file targeting. These will not always
 * return the same results as the equivalent `semgrep` run. To see valid inputs to
 * `semgrep-core`, see `semgrep-core --help`.
 *
 * When invoked by `semgrep`, `semgrep-core` will always be passed `-rules` and
 * `-targets`. All the code relevant to `semgrep` runs will be found in branches
 * where the rules file and the targets file are not `""`.
 *
 * While the `rules` file is just the collection of rules, the `targets` file
 * describes the mapping of targets to rules. See `targets` in `Input_to_core.atd`
 * for a description of its schema. `semgrep-core` follows the target-to-rule
 * mappings without validation or filtering.
 *
 * ## Performance
 *
 * The main components of performance can generally be broken down into:
 *
 * - rule parse time
 * - target parse time
 * - match time
 *   - pattern match time
 *   - formula evaluation time
 *
 * The `-json_time` output includes timings for the three broad components. In
 * general (at least at the time this was written), target parsing takes the
 * longest. Matching does not usually take long, though it can when there are
 * many general patterns (e.g. `$X`) that require going deep (e.g. multiple `...`
 * or `<... $X ...>`). Also, getting the range of a large sub-AST can be slow
 * because it requires collecting all the tokens in that sub-AST. Most of the
 * rules that take longest are generic rules, however, because they run on
 * every file.
 *
 * ## Memory usage
 *
 * Semgrep uses memory liberally. The main components of memory usage can
 * generally be broken down into:
 *
 * - rule parsing
 * - targets file parsing
 *   - in addition to saving the targets, we seem to keep around the
 *     buffer used to parse it
 * - running each target (this should not accumulate)
 *   - parsing the target
 *   - saving the pattern results
 *   - collecting the output for each target
 *   - other memory used while matching
 * - saving the output for each target
 * - some unknown accumulation that happens while running targets but goes
 *   away if we run `Gc.full_major` after each target
 * - creating the final output
 *
 * Of these, the memory used by rule parsing and targets file parsing has an
 * outsized impact on the memory used by multi-process runs because those two
 * structures are used by every process. (They are only read from, but they
 * end up getting copied into the process during garbage collection.) In
 * particular, the targets file is O(num_rules \* num_targets).
 *
 * If we need to reduce memory further, some options include:
 *
 * - Reduce memory used by rule parsing? (Have not investigated but from my
 *   personal profiling this seems too high)
 * - Further streamline the targets file and fix the problem with the buffer
 * - Investigate the unknown accumulation
 * - Compress non-necessary output (namely, `-json_time` is passed by default)
 * - Stream the outputs instead of collecting them and outputting them at the
 *   end
 *
 * For profiling, a useful module is `mem_usage`. See physical memory used in
 * total and by each object with:
 *
 * ```ocaml
 * let phys_mem () = Mem_usage.prettify_bytes (Mem_usage.((info ()).process_physical_memory))
 * let obj_size o = Mem_usage.prettify_bytes (Obj.reachable_words (Obj.repr o) * 8)
 * ```
 *
 * Note that `obj_size` may inadvertently prevent an object from being freed.
 *
 * Additionally, OCaml provides `Spacetime` for certain compilers. It works best
 * on Linux machines.
 *
 * We also have had stack overflows. OCaml <=4.14.0, we avoided this using
 * `Common.map`, which is tail-recursive, instead of `List.map`.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* The type of the semgrep core scan. We define it here so that
   semgrep and semgrep-proprietary use the same definition *)
type core_scan_func = Core_scan_config.t -> Core_result.result_or_exn

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
  if !Common.jsoo then path
    (* don't bother supporting exotic things like fds if running in JS *)
  else
    match (Unix.stat !!path).st_kind with
    | Unix.S_FIFO ->
        let data = File.read_file path in
        let prefix = spf "semgrep-core-" in
        let suffix = spf "-%s" (Fpath.basename path) in
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
        Fpath.v tmp_path
    | _ -> path

let update_cli_progress config =
  (* Print when each file is done so the Python progress bar knows *)
  match config.output_format with
  | Json true -> pr "."
  | _ -> ()

(*
   Sort targets by decreasing size. This is meant for optimizing
   CPU usage when processing targets in parallel on a fixed number of cores.
*)
let sort_targets_by_decreasing_size (targets : In.target list) : In.target list
    =
  targets
  |> Common.map (fun target -> (target, Common2.filesize target.In.path))
  |> List.sort (fun (_, (a : int)) (_, b) -> compare b a)
  |> Common.map fst

(*****************************************************************************)
(* Printing matches *)
(*****************************************************************************)

let string_of_toks toks =
  String.concat ", " (Common.map (fun tok -> Tok.content_of_tok tok) toks)

let rec print_taint_call_trace ~format ~spaces = function
  | Pattern_match.Toks toks -> Core_text_output.print_match ~format ~spaces toks
  | Call { call_toks; intermediate_vars; call_trace } ->
      let spaces_string = String.init spaces (fun _ -> ' ') in
      pr (spaces_string ^ "call to");
      Core_text_output.print_match ~format ~spaces call_toks;
      if intermediate_vars <> [] then
        pr
          (spf "%sthese intermediate values are tainted: %s" spaces_string
             (string_of_toks intermediate_vars));
      pr (spaces_string ^ "then");
      print_taint_call_trace ~format ~spaces:(spaces + 2) call_trace

let print_taint_trace ~format taint_trace =
  if format =*= Core_text_output.Normal then
    taint_trace |> Lazy.force
    |> List.iteri (fun idx { PM.source_trace; tokens; sink_trace } ->
           if idx =*= 0 then pr "  * Taint may come from this source:"
           else pr "  * Taint may also come from this source:";
           print_taint_call_trace ~format ~spaces:4 source_trace;
           if tokens <> [] then
             pr
               (spf "  * These intermediate values are tainted: %s"
                  (string_of_toks tokens));
           pr "  * This is how taint reaches the sink:";
           print_taint_call_trace ~format ~spaces:4 sink_trace)

let print_match ?str config match_ ii_of_any =
  (* there are a few fake tokens in the generic ASTs now (e.g.,
   * for DotAccess generated outside the grammar) *)
  let { match_format; mvars; _ } = config in
  let { Pattern_match.env; tokens = (lazy tokens_matched_code); taint_trace; _ }
      =
    match_
  in
  let toks = tokens_matched_code |> List.filter Tok.is_origintok in
  (if mvars =*= [] then
   Core_text_output.print_match ?str ~format:match_format toks
  else
    (* similar to the code of Lib_matcher.print_match, maybe could
     * factorize code a bit.
     *)
    let mini, _maxi = Tok_range.min_max_toks_by_pos toks in
    let file, line = (Tok.file_of_tok mini, Tok.line_of_tok mini) in

    let strings_metavars =
      mvars
      |> Common.map (fun x ->
             match Common2.assoc_opt x env with
             | Some any ->
                 any |> ii_of_any
                 |> List.filter Tok.is_origintok
                 |> Common.map Tok.content_of_tok
                 |> Core_text_output.join_with_space_if_needed
             | None -> failwith (spf "the metavariable '%s' was not bound" x))
    in
    pr (spf "%s:%d: %s" file line (Common.join ":" strings_metavars));
    ());
  Option.iter (print_taint_trace ~format:match_format) taint_trace

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
  let targets = sort_targets_by_decreasing_size targets in
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
    |> Common.map_filter (fun (file, xs) ->
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
             "most offending rule: id = %s, matches = %d, pattern = %s"
             (id :> string)
             cnt pat;

           (* todo: we should maybe use a new error: TooManyMatches of int * string*)
           let loc = Tok.first_loc_of_file file in
           let error =
             E.mk_error (Some id) loc
               (spf
                  "%d rules result in too many matches, most offending rule \
                   has %d: %s"
                  offending_rules cnt pat)
               Out.TooManyMatches
           in
           let skipped =
             sorted_offending_rules
             |> Common.map (fun (((rule_id : Rule_ID.t), _pat), n) ->
                    let details =
                      Some
                        (spf
                           "found %i matches for rule %s, which exceeds the \
                            maximum of %i matches."
                           n
                           (rule_id :> string)
                           max_match_per_file)
                    in
                    {
                      Semgrep_output_v1_t.path = file;
                      reason = Too_many_matches;
                      details;
                      rule_id = Some (rule_id :> string);
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
      let loc = Tok.unsafe_loc_of_tok tok in
      E.mk_error None loc s AstBuilderError
  | _ -> E.exn_to_error None file e

(* Convert invalid rules to errors to be reported at the end.
   This used to raise an exception causing an early abort.

   TODO: restore early abort but only in strict mode?
   TODO: report an error or not depending on the kind of problem?
*)
let errors_of_invalid_rule_errors (invalid_rules : Rule.invalid_rule_error list)
    =
  Common.map E.error_of_invalid_rule_error invalid_rules

let sanity_check_invalid_patterns (res : Core_result.t) :
    Core_result.result_or_exn =
  match
    res.errors
    |> List.find_opt (function
         | { Core_error.typ = Out.PatternParseError _; _ } -> true
         | _else_ -> false)
  with
  | None -> Ok res
  | Some err ->
      let e = Exception.catch (Failure "Pattern parse error") in
      Error (e, Some err)

(*****************************************************************************)
(* Parsing (non-cached) *)
(*****************************************************************************)

(* for -rules *)
let rules_from_rule_source config =
  let rule_source =
    match config.rule_source with
    | Some (Rule_file file) ->
        (* useful when using process substitution, e.g.
         * semgrep-core -rules <(curl https://semgrep.dev/c/p/ocaml) ...
         *)
        Some (Rule_file (replace_named_pipe_by_regular_file file))
    | other -> other
  in
  match rule_source with
  | Some (Rule_file file) ->
      logger#linfo (lazy (spf "Parsing %s:\n%s" !!file (File.read_file file)));
      Parse_rule.parse_and_filter_invalid_rules file
  | Some (Rules rules) -> (rules, [])
  | None ->
      (* TODO: ensure that this doesn't happen *)
      failwith "missing rules"

(* TODO? this is currently deprecated, but pad still has hope the
 * feature can be resurrected.
 *)
let parse_equivalences equivalences_file =
  match equivalences_file with
  | None -> []
  | Some file -> Parse_equivalences.parse file
  [@@profiling]

(*****************************************************************************)
(* Iteration helpers *)
(*****************************************************************************)

let iter_targets_and_get_matches_and_exn_to_errors config f targets =
  targets
  |> map_targets config.ncores (fun (target : In.target) ->
         let file = Fpath.v target.path in
         logger#info "Analyzing %s" !!file;
         let res, run_time =
           Common.with_time (fun () ->
               try
                 let get_context () =
                   match !Rule.last_matched_rule with
                   | None -> !!file
                   | Some rule_id -> spf "%s on %s" (rule_id :> string) !!file
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
                     logger#trace "done with %s" !!file;
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
                         (rule.MR.id :> string);
                       logger#info "full pattern is: %s" rule.MR.pattern_string);
                   let loc = Tok.first_loc_of_file !!file in
                   let errors =
                     Core_error.ErrorSet.singleton
                       (E.mk_error !Rule.last_matched_rule loc ""
                          (match exn with
                          | Match_rules.File_timeout ->
                              logger#info "Timeout on %s" !!file;
                              Out.Timeout
                          | Out_of_memory ->
                              logger#info "OutOfMemory on %s" !!file;
                              Out.OutOfMemory
                          | _ -> raise Impossible))
                   in
                   Core_result.make_match_result [] errors
                     (Core_profiling.empty_partial_profiling file)
               (* those were converted in Main_timeout in timeout_function()*)
               | Time_limit.Timeout _ -> assert false
               (* It would be nice to detect 'R.Err (R.InvalidRule _)' here
                * for errors while parsing patterns. This exn used to be raised earlier
                * in sanity_check_rules_and_invalid_rules(), but after
                * the lazy parsing of patterns, those errors are raised
                * later. Unfortunately, we can't catch and reraise here, because
                * with -j 2, Parmap will just abort the whole thing and return
                * a different kind of exception to the caller. Instead, we
                * we need to convert all exns in errors (see the code further below),
                * and only in sanity_check_invalid_patterns() we can detect if one
                * of those errors was a PatternParseError.
                * does-not-work:
                * | R.Err (R.InvalidRule _) as exn when false ->
                *   Exception.catch_and_reraise exn
                *)
               (* convert all other exns (e.g., a parse error in a target file,
                * a parse error in a pattern), in an empty match result with errors,
                * so that one error in one target file or rule does not abort the whole
                * semgrep-core process.
                *)
               | exn when not !Flag_semgrep.fail_fast ->
                   let e = Exception.catch exn in
                   let errors =
                     Core_error.ErrorSet.singleton (exn_to_error !!file e)
                   in
                   Core_result.make_match_result [] errors
                     (Core_profiling.empty_partial_profiling file))
         in
         Core_result.add_run_time run_time res)

(*****************************************************************************)
(* File targeting and rule filtering *)
(*****************************************************************************)

let rules_for_xlang (xlang : Xlang.t) (rules : Rule.t list) : Rule.t list =
  rules
  |> List.filter (fun r ->
         match (xlang, r.R.languages.target_analyzer) with
         | LRegex, LRegex
         | LSpacegrep, LSpacegrep
         | LAliengrep, LAliengrep ->
             true
         | ( L
               ( x,
                 _empty
                 (* FIXME: why should '_empty' be empty? Use [] and 'assert' *)
               ),
             L (y, ys) ) ->
             List.mem x (y :: ys)
         | (LRegex | LSpacegrep | LAliengrep | L _), _ -> false)

(* Creates a table mapping rule id indicies to rules. In the case that a rule
 * id is present and there is no correpsonding rule, that rule is simply
 * omitted from the final table.
 * TODO: This is needed because?
 *)
let mk_rule_table (rules : Rule.t list) (list_of_rule_ids : string list) :
    (int, Rule.t) Hashtbl.t =
  let rule_table =
    rules |> Common.map (fun r -> (fst r.R.id, r)) |> Common.hash_of_list
  in
  let id_pairs =
    list_of_rule_ids
    |> Common.mapi (fun i x -> (i, Rule_ID.of_string x))
    (* We filter out rules here if they don't exist, because we might have a
     * rule_id for an extract mode rule, but extract mode rules won't appear in
     * rule pairs, because they won't be in the table we make for search
     * because we don't want to run them at this stage.
     *)
    |> Common.map_filter (fun (i, rule_id) ->
           let* x = Hashtbl.find_opt rule_table rule_id in
           Some (i, x))
  in
  Common.hash_of_list id_pairs

(* TODO: use Fpath.t for file *)
let xtarget_of_file (config : Core_scan_config.t) (xlang : Xlang.t)
    (file : Fpath.t) : Xtarget.t =
  let lazy_ast_and_errors =
    lazy
      (let lang =
         (* ew. We fail tests if this gets pulled out of the lazy block. *)
         match xlang with
         | L (lang, []) -> lang
         | L (_lang, _ :: _) ->
             (* xlang from the language field in -target should be unique *)
             assert false
         | _ ->
             (* alt: could return an empty program, but better to be defensive*)
             failwith
               "requesting generic AST for an unspecified target language"
       in
       Parse_with_caching.parse_and_resolve_name
         ~parsing_cache_dir:config.parsing_cache_dir AST_generic.version lang
         file)
  in
  {
    Xtarget.file;
    xlang;
    lazy_content = lazy (File.read_file file);
    lazy_ast_and_errors;
  }

(* Compute the set of targets, either by reading what was passed
 * in -target, or by using our poor's man file targeting with
 * Find_target.files_of_dirs_or_files.
 *
 * The rule ids argument is useful only when -target is not specified.
 * The In.target type now requires the list of rule ids to use for
 * a target, which gives flexibility to the caller (e.g., filter
 * certain rules for certain targets in the semgrep-cli wrapper
 * by using the include/exclude fields.).
 *)
let targets_of_config (config : Core_scan_config.t)
    (all_rule_ids_when_no_target_file : Rule_ID.t list) :
    In.targets * Out.skipped_target list =
  match (config.target_source, config.roots, config.lang) with
  (* We usually let semgrep-python computes the list of targets (and pass it
   * via -target), but it's convenient to also run semgrep-core without
   * semgrep-python and to recursively get a list of targets.
   * We just have a poor's man file targeting/filtering here, just enough
   * to run semgrep-core independently of semgrep-python to test things.
   *)
  | None, roots, Some xlang ->
      (* less: could also apply Common.fullpath? *)
      let roots = roots |> Common.map replace_named_pipe_by_regular_file in
      let lang_opt =
        match xlang with
        | Xlang.LRegex
        | Xlang.LSpacegrep
        | Xlang.LAliengrep ->
            None (* we will get all the files *)
        | Xlang.L (lang, []) -> Some lang
        (* config.lang comes from Xlang.of_string which returns just a lang *)
        | Xlang.L (_, _) -> assert false
      in
      let files, skipped =
        Find_targets_old.files_of_dirs_or_files lang_opt roots
      in
      let rule_ids = all_rule_ids_when_no_target_file in
      let target_mappings =
        files
        |> Common.map (fun file ->
               {
                 In.path = Fpath.to_string file;
                 language = xlang;
                 rule_nums = Common.mapi (fun i _ -> i) rule_ids;
               })
      in
      ({ target_mappings; rule_ids = (rule_ids :> string list) }, skipped)
  | None, _, None -> failwith "you need to specify a language with -lang"
  (* main code path for semgrep python, with targets specified by -target *)
  | Some target_source, roots, lang_opt ->
      let targets =
        match target_source with
        | Targets x -> x
        | Target_file target_file ->
            File.read_file target_file |> In.targets_of_string
      in
      let skipped = [] in
      (* in deep mode we actually have a single root dir passed *)
      if roots <> [] then
        logger#error "if you use -targets, you should not specify files";
      (* TODO: ugly, this is because the code path for -e/-f requires
       * a language, even with a -target, see test_target_file.py
       *)
      if lang_opt <> None && config.rule_source <> None then
        failwith "if you use -targets and -rules, you should not specify a lang";
      (targets, skipped)

(*****************************************************************************)
(* Extract-mode helpers *)
(*****************************************************************************)

(* Extract new targets using the extractors. The rule ids correspond
 * to the rules to run against those new extracted targets (which should be
 * the original rules passed via -rules, without the extract-mode rules).
 *)
let extracted_targets_of_config (config : Core_scan_config.t)
    (all_rules : Rule.t list) :
    In.target list
    * ( Common.filename,
        Match_extract_mode.match_result_location_adjuster )
      Hashtbl.t =
  let extractors =
    Common.map_filter
      (fun (r : Rule.t) ->
        match r.mode with
        | `Extract _ as e -> Some ({ r with mode = e } : Rule.extract_rule)
        | `Search _
        | `Taint _
        | `Steps _
        | `Secrets _ ->
            None)
      all_rules
  in
  let erule_ids = Common.map (fun r -> fst r.R.id) extractors in
  (* TODO? do we need the erule_ids here? can we just pass []? *)
  let basic_targets_info, _skipped = targets_of_config config erule_ids in
  let basic_targets = basic_targets_info.target_mappings in
  logger#info "extracting nested content from %d files"
    (List.length basic_targets);
  let match_hook str match_ =
    if !debug_extract_mode && config.output_format =*= Text then (
      pr2 "extracted content from ";
      print_match ~str config match_ Metavariable.ii_of_mval)
  in
  let extracted_ranges =
    basic_targets
    |> List.concat_map (fun (t : In.target) ->
           (* TODO: addt'l filtering required for rule_ids when targets are
              passed explicitly? *)
           let file = t.path in
           let xlang = t.language in
           let xtarget = xtarget_of_file config xlang (Fpath.v file) in
           let extracted_targets =
             Match_extract_mode.extract_nested_lang ~match_hook
               ~timeout:config.timeout
               ~timeout_threshold:config.timeout_threshold
               ~all_rules:(all_rules :> Rule.t list)
               extractors xtarget
           in
           (* Print number of extra targets so Python knows *)
           (match config.output_format with
           | Json true when extracted_targets <> [] ->
               pr (string_of_int (List.length extracted_targets))
           | _ -> ());
           extracted_targets)
  in
  List.fold_right
    (fun (t, fn) (ts, fn_tbl) ->
      Hashtbl.add fn_tbl t.In.path fn;
      (t :: ts, fn_tbl))
    extracted_ranges
    ([], Hashtbl.create (List.length basic_targets))

(*****************************************************************************)
(* a "core" scan *)
(*****************************************************************************)

(* This is the main function used by pysemgrep right now.
 * This is also called now from osemgrep.
 * It takes a set of rules and a set of targets (targets derived from config,
 * and potentially also extract rules) and iteratively process those targets.
 *)
let scan ?match_hook config ((rules, invalid_rules), rules_parse_time) :
    Core_result.t =
  let rule_errors = errors_of_invalid_rule_errors invalid_rules in
  let rule_ids = rules |> Common.map (fun r -> fst r.R.id) in

  (* The basic targets.
   * TODO: possibly extract (recursively) from generated stuff? *)
  let targets_info, skipped = targets_of_config config rule_ids in
  let targets =
    (* Optimization: no valid rule => no findings.
       This solution avoids using an exception which would be a little harder
       to track.
       Use case: a user is creating a rule and testing it on their project
       but the rule is invalid. *)
    match rules with
    | [] -> []
    | _some_rules -> targets_info.target_mappings
  in

  (* The "extracted" targets we generate on the fly by calling
   * our extractors (extract mode rules) on the relevant basic targets.
   *)
  let new_extracted_targets, extract_result_map =
    extracted_targets_of_config config rules
  in

  let all_targets = targets @ new_extracted_targets in

  (* Note that 'rules' here contains only search/taint rules from the above
   * partition; i.e., it doesn't contain any extract mode rules.
   *
   * However,
   * target_info.rule_ids might include extract mode rules previously used on
   * this target. mk_rule_table resolves this by ignoring any rule id it can't
   * find in the rules list.
   *)
  let rule_table = mk_rule_table rules targets_info.rule_ids in

  (* Let's go! *)
  logger#info "processing %d files, skipping %d files" (List.length all_targets)
    (List.length skipped);
  let file_results =
    all_targets
    |> iter_targets_and_get_matches_and_exn_to_errors config
         (fun (target : In.target) ->
           let file = Fpath.v target.path in
           let xlang = target.language in
           let rules =
             (* Assumption: find_opt will return None iff a r_id
                 is in skipped_rules *)
             target.In.rule_nums
             |> Common.map_filter (fun r_num ->
                    Hashtbl.find_opt rule_table r_num)
             (* Don't run the extract rules
                Note: we can't filter this out earlier because the rule indexes need to be stable *)
             |> List.filter (fun r ->
                    match r.R.mode with
                    | `Extract _ -> false
                    (* TODO We are running Secrets rules now, but they just
                       get turned into search rules inside matching.
                       Unify Secrets and Search rules. *)
                    | `Secrets _
                    | `Search _
                    | `Taint _
                    | `Steps _ ->
                        true)
             |> List.filter (fun r ->
                    (* TODO: some of this is already done in pysemgrep, so maybe
                     * we should guard with a flag that only osemgrep set
                     * like Core_scan_config.paths_processing: bool?
                     *)
                    match r.R.paths with
                    | None -> true
                    | Some paths -> Filter_target.filter_paths paths file)
           in
           let xtarget = xtarget_of_file config xlang file in
           let default_match_hook str match_ =
             if config.output_format =*= Text then
               print_match ~str config match_ Metavariable.ii_of_mval
           in
           let match_hook =
             Option.value match_hook ~default:default_match_hook
           in
           let xconf =
             {
               Match_env.config = Rule_options.default_config;
               equivs = parse_equivalences config.equivalences_file;
               nested_formula = false;
               matching_explanations = config.matching_explanations;
               filter_irrelevant_rules = config.filter_irrelevant_rules;
             }
           in
           let matches =
             let matches =
               Match_rules.check ~match_hook ~timeout:config.timeout
                 ~timeout_threshold:config.timeout_threshold xconf rules xtarget
             in
             (* If our target is a proprietary language, or we've been using the proprietary
              * engine, then label all the resulting matches with the Pro engine kind.
              * This can't really be done any later, because we need the language that
              * we're running on.
              *)
             (* If these hooks are set, it's probably a pretty good indication that we're
                using Pro features.
             *)
             if
               Option.is_some
                 !Match_tainting_mode.hook_setup_hook_function_taint_signature
               || Option.is_some
                    !Dataflow_tainting.hook_function_taint_signature
               || Xlang.is_proprietary xtarget.xlang
             then
               {
                 matches with
                 Core_result.matches =
                   Common.map PM.to_proprietary matches.matches;
               }
             else matches
           in
           (* So we can display matches incrementally in osemgrep!
            * Note that this is run in a child process of Parmap, so
            * the hook should not rely on shared memory.
            *)
           config.file_match_results_hook
           |> Option.iter (fun hook -> hook file matches);

           update_cli_progress config;

           (* adjust the match location for extracted targets *)
           match Hashtbl.find_opt extract_result_map !!file with
           | Some f -> f matches
           | None -> matches)
  in
  let scanned =
    (* we do not use all_targets here, because we don't count
     * the extracted targets
     *)
    targets |> Common.map (fun x -> Fpath.v x.In.path)
  in
  let res =
    RP.make_final_result file_results
      (Common.map (fun r -> (r, `OSS)) rules)
      invalid_rules scanned ~rules_parse_time
  in
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

  (* concatenate all errors *)
  let errors = rule_errors @ new_errors @ res.errors in

  (* Concatenate all the skipped targets
   * TODO: maybe we should move skipped_target out of Debug and always
   * do it?
   *)
  let extra =
    match res.extra with
    | Core_profiling.Debug { skipped_targets; profiling } ->
        let skipped_targets = skipped @ new_skipped @ skipped_targets in
        logger#info "there were %d skipped targets"
          (List.length skipped_targets);
        Core_profiling.Debug { skipped_targets; profiling }
    | (Core_profiling.Time _ | Core_profiling.No_info) as x -> x
  in
  { res with matches; errors; extra }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let scan_with_exn_handler (config : Core_scan_config.t) :
    Core_result.result_or_exn =
  try
    let timed_rules =
      Common.with_time (fun () -> rules_from_rule_source config)
    in
    (* The pre and post processors hook here is currently just used
       for the secrets post processor, but it should now be trivial to
       hook any post processing step that needs to look at rules and
       results. *)
    let res =
      Pre_post_core_scan.call_with_pre_and_post_processor (scan config)
        timed_rules
    in
    sanity_check_invalid_patterns res
  with
  | exn when not !Flag_semgrep.fail_fast ->
      let e = Exception.catch exn in
      logger#info "Uncaught exception: %s" (Exception.to_string e);
      Error (e, None)
