(* Yoann Padioleau
 *
 * Copyright (C) 2020-2023 Semgrep Inc.
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
module PM = Pattern_match
module E = Core_error
module MR = Mini_rule
module R = Rule
module RP = Core_result
module In = Input_to_core_j
module OutJ = Semgrep_output_v1_j

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* A "Core" scan.
 *
 * ## pysemgrep vs semgrep-core
 *
 * Officially, `semgrep-core` is never run on its own. External users run
 * `semgrep`, which invokes `semgrep-core` with the appropriate rules and
 * targets. However, for development purposes it can be convenient to skip
 * the wrapper. Therefore, we also maintain some code paths that allow
 * `semgrep-core` to take in rules or patterns and perform its own file
 * targeting. These will not always return the same results as the equivalent
 * `semgrep` run.
 *
 * When invoked by `pysemgrep`, `semgrep-core` will always be passed
 * `-rules` and `-targets`. All the code relevant to `pysemgrep` runs will be
 * found in code paths where the rules file and the targets file are not `""`.
 *
 * While the `rules` file is just the collection of rules, the `targets` file
 * describes the mapping of targets to rules. See `Input_to_core.atd` target
 * type. `semgrep-core` follows the target-to-rulemappings without validation
 * or filtering.
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
 * many general patterns (e.g. `$X`) that require going deep
 *(e.g. multiple `...` or `<... $X ...>`). Also, getting the range of a
 * large sub-AST can be slow because it requires collecting all the tokens in
 * that sub-AST. Most of the rules that take longest are generic rules,
 * however, because they run on every file.
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
 * Additionally, OCaml provides `Spacetime` for certain compilers.
 *It works best on Linux machines.
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

(* A target is [Not_scanned] when semgrep didn't find any applicable rules.
 * The information is useful to return to pysemgrep/osemgrep to
 * display statistics.
 *
 * The [original_target] below is useful to deal with extract rules. Indeed,
 * the file processed in [iter_targets_and_get_matches_and_exn_to_errors]
 * may be a temporary "extracted" file that is not the file we want to report
 * as scanned; we want to report as scanned the original target file.
 *)
type was_scanned = Scanned of Extract.original_target | Not_scanned

(* Type of the iter_targets_and_get_matches_and_exn_to_errors callback.

   A target handler returns (matches, was_scanned) where was_scanned indicates
   whether at least one rule applied to the target since the target could
   be excluded by all the rules via per-rule include/exclude patterns.
   alt: baking this flag into match_result type would lead to even worse
   complexity

   Remember that a target handler runs in another process (via Parmap).
*)
type target_handler = In.target -> RP.matches_single_file * was_scanned

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
  UFile.replace_named_pipe_by_regular_file_if_needed ~prefix:"semgrep-core-"
    path

(*
   Sort targets by decreasing size. This is meant for optimizing
   CPU usage when processing targets in parallel on a fixed number of cores.
*)
let sort_targets_by_decreasing_size (targets : In.target list) : In.target list
    =
  targets
  |> List_.map (fun target -> (target, UFile.filesize (Fpath.v target.In.path)))
  |> List.sort (fun (_, (a : int)) (_, b) -> compare b a)
  |> List_.map fst

(* In some context, a target passed in might have disappeared, or have been
 * encoded in the wrong way in the Inputs_to_core.atd (for example
 * in the case of filenames with special unicode bytes in it), in which case
 * Common2.filesize above would fail and crash the whole scan as the
 * raised exn is outside the iter_targets_and_get_matches_and_exn_to_errors
 * big try. This is why it's better to filter those problematic targets
 * early on.
 *)
let filter_existing_targets (targets : In.target list) :
    In.target list * OutJ.skipped_target list =
  targets
  |> Either_.partition_either (fun (target : In.target) ->
         let file = target.In.path in
         if Sys.file_exists file then Left target
         else
           Right
             {
               Semgrep_output_v1_t.path = Fpath.v file;
               reason = Nonexistent_file;
               details = Some "File does not exist";
               rule_id = None;
             })

let set_matches_to_proprietary_origin_if_needed (xtarget : Xtarget.t)
    (matches : RP.matches_single_file) : RP.matches_single_file =
  (* If our target is a proprietary language, or we've been using the
   * proprietary engine, then label all the resulting matches with the Pro
   * engine kind. This can't really be done any later, because we need the
   * language that we're running on.
   *
   * If those hooks are set, it's probably a pretty good indication that
   * we're using Pro features.
   *)
  if
    Option.is_some !Match_tainting_mode.hook_setup_hook_function_taint_signature
    || Option.is_some !Dataflow_tainting.hook_function_taint_signature
    || Xlang.is_proprietary xtarget.xlang
  then
    {
      matches with
      Core_result.matches = List_.map PM.to_proprietary matches.matches;
    }
  else matches

(*****************************************************************************)
(* Pysemgrep progress bar *)
(*****************************************************************************)

(* Print additional target count so the Python progress bar knows *)
let print_cli_additional_targets (config : Core_scan_config.t) (n : int) : unit
    =
  match config.output_format with
  | Json true -> Out.put (string_of_int n)
  | _ -> ()

(* TODO: suspicious: this runs in a child process. Please explain how it's
   safe to write a dot on stdout in a child process and why it's mixed with
   JSON output.
*)
let print_cli_progress (config : Core_scan_config.t) : unit =
  (* Print when each file is done so the Python progress bar knows *)
  match config.output_format with
  | Json true -> Out.put "."
  | _ -> ()

(*****************************************************************************)
(* Printing matches *)
(*****************************************************************************)

let string_of_toks toks =
  String.concat ", " (List_.map (fun tok -> Tok.content_of_tok tok) toks)

(* TODO: use Logs.app instead of those Out.put? *)
let rec print_taint_call_trace ~format ~spaces = function
  | Pattern_match.Toks toks -> Core_text_output.print_match ~format ~spaces toks
  | Call { call_toks; intermediate_vars; call_trace } ->
      let spaces_string = String.init spaces (fun _ -> ' ') in
      Out.put (spaces_string ^ "call to");
      Core_text_output.print_match ~format ~spaces call_toks;
      if intermediate_vars <> [] then
        Out.put
          (spf "%sthese intermediate values are tainted: %s" spaces_string
             (string_of_toks intermediate_vars));
      Out.put (spaces_string ^ "then");
      print_taint_call_trace ~format ~spaces:(spaces + 2) call_trace

let print_taint_trace ~format taint_trace =
  if format =*= Core_text_output.Normal then
    taint_trace |> Lazy.force
    |> List.iteri (fun idx { PM.source_trace; tokens; sink_trace } ->
           if idx =*= 0 then Out.put "  * Taint may come from this source:"
           else Out.put "  * Taint may also come from this source:";
           print_taint_call_trace ~format ~spaces:4 source_trace;
           if tokens <> [] then
             Out.put
               (spf "  * These intermediate values are tainted: %s"
                  (string_of_toks tokens));
           Out.put "  * This is how taint reaches the sink:";
           print_taint_call_trace ~format ~spaces:4 sink_trace)

let print_match ?str (config : Core_scan_config.t) match_ ii_of_any =
  (* there are a few fake tokens in the generic ASTs now (e.g.,
   * for DotAccess generated outside the grammar) *)
  let { Pattern_match.env; tokens = (lazy tokens_matched_code); taint_trace; _ }
      =
    match_
  in
  let toks = tokens_matched_code |> List.filter Tok.is_origintok in
  (if config.mvars =*= [] then
     Core_text_output.print_match ?str ~format:config.match_format toks
   else
     (* similar to the code of Lib_matcher.print_match, maybe could
      * factorize code a bit.
      *)
     let mini, _maxi = Tok_range.min_max_toks_by_pos toks in
     let file, line = (Tok.file_of_tok mini, Tok.line_of_tok mini) in

     let strings_metavars =
       config.mvars
       |> List_.map (fun x ->
              match Common2.assoc_opt x env with
              | Some any ->
                  any |> ii_of_any
                  |> List.filter Tok.is_origintok
                  |> List_.map Tok.content_of_tok
                  |> Core_text_output.join_with_space_if_needed
              | None -> failwith (spf "the metavariable '%s' was not bound" x))
     in
     Out.put (spf "%s:%d: %s" file line (String.concat ":" strings_metavars));
     ());
  Option.iter (print_taint_trace ~format:config.match_format) taint_trace

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
  if ncores <= 1 then List_.map f targets
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
    |> List_.map (fun ({ pm; _ } : Core_result.processed_match) ->
           (!!(pm.file), pm))
    |> Assoc.group_assoc_bykey_eff
  in

  let offending_file_list =
    per_files
    |> List_.map_filter (fun (file, xs) ->
           if List.length xs > max_match_per_file then Some file else None)
  in
  let offending_files = Hashtbl_.hashset_of_list offending_file_list in
  let new_matches =
    matches
    |> List_.exclude (fun ({ pm; _ } : Core_result.processed_match) ->
           Hashtbl.mem offending_files !!(pm.file))
  in
  let new_errors, new_skipped =
    offending_file_list
    |> List_.map (fun file ->
           (* logging useful info for rule writers *)
           logger#info "too many matches on %s, generating exn for it" file;
           let sorted_offending_rules =
             let matches = List.assoc file per_files in
             matches
             |> List_.map (fun m ->
                    let rule_id = m.Pattern_match.rule_id in
                    ( ( rule_id.Pattern_match.id,
                        rule_id.Pattern_match.pattern_string ),
                      m ))
             |> Assoc.group_assoc_bykey_eff
             |> List_.map (fun (k, xs) -> (k, List.length xs))
             |> Assoc.sort_by_val_highfirst
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
             (Rule_ID.to_string id) cnt pat;

           (* todo: we should maybe use a new error: TooManyMatches of int * string*)
           let loc = Tok.first_loc_of_file file in
           let error =
             E.mk_error (Some id) loc
               (spf
                  "%d rules result in too many matches, most offending rule \
                   has %d: %s"
                  offending_rules cnt pat)
               OutJ.TooManyMatches
           in
           let skipped =
             sorted_offending_rules
             |> List_.map (fun (((rule_id : Rule_ID.t), _pat), n) ->
                    let details =
                      Some
                        (spf
                           "found %i matches for rule %s, which exceeds the \
                            maximum of %i matches."
                           n
                           (Rule_ID.to_string rule_id)
                           max_match_per_file)
                    in
                    {
                      Semgrep_output_v1_t.path = Fpath.v file;
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

(* Convert invalid rules to errors to be reported at the end.
   This used to raise an exception causing an early abort.

   TODO: restore early abort but only in strict mode?
   TODO: report an error or not depending on the kind of problem?
*)
let errors_of_invalid_rule_errors (invalid_rules : Rule.invalid_rule_error list)
    =
  List_.map E.error_of_invalid_rule_error invalid_rules

let sanity_check_invalid_patterns (res : Core_result.t) :
    Core_result.result_or_exn =
  match
    res.errors
    |> List.find_opt (function
         | { Core_error.typ = OutJ.PatternParseError _; _ } -> true
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
let rules_from_rule_source (config : Core_scan_config.t) :
    Rule.t list * Rule.invalid_rule_error list =
  let rule_source =
    match config.rule_source with
    | Some (Core_scan_config.Rule_file file) ->
        (* useful when using process substitution, e.g.
         * semgrep-core -rules <(curl https://semgrep.dev/c/p/ocaml) ...
         *)
        Some
          (Core_scan_config.Rule_file (replace_named_pipe_by_regular_file file))
    | other -> other
  in
  match rule_source with
  | Some (Core_scan_config.Rule_file file) ->
      logger#linfo (lazy (spf "Parsing %s:\n%s" !!file (UFile.read_file file)));
      Parse_rule.parse_and_filter_invalid_rules ~rewrite_rule_ids:None file
  | Some (Core_scan_config.Rules rules) -> (rules, [])
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

(*
   Returns a list of match results and a separate list of scanned targets.
*)
let iter_targets_and_get_matches_and_exn_to_errors (config : Core_scan_config.t)
    (handle_target : target_handler) (targets : In.target list) :
    Core_profiling.file_profiling RP.match_result list * Fpath.t list =
  (* The path in match_and_path_list is None when the file was not scanned *)
  let (match_and_path_list
        : (Core_profiling.file_profiling RP.match_result * Fpath.t option) list)
      =
    targets
    |> map_targets config.ncores (fun (target : In.target) ->
           let file = Fpath.v target.path in
           logger#info "Analyzing %s" !!file;
           let (res, was_scanned), run_time =
             Common.with_time (fun () ->
                 try
                   (* this is used to generate warnings in the logs
                    * when we exceed or are close to exceed the memory limit
                    *)
                   let get_context () =
                     match !Rule.last_matched_rule with
                     | None -> !!file
                     | Some rule_id ->
                         spf "%s on %s" (Rule_ID.to_string rule_id) !!file
                   in
                   Memory_limit.run_with_memory_limit ~get_context
                     ~mem_limit_mb:config.max_memory_mb (fun () ->
                       (* we used to call timeout_function() here, but this
                        * is now done in Match_rules because we now
                        * timeout per rule, not per file since pysemgrep
                        * passed all the rules to semgrep-core.
                        *
                        * old: timeout_function file config.timeout ...
                        *)
                       let res, was_scanned = handle_target target in

                       (* This is just to test -max_memory, to give a chance
                        * to Gc.create_alarm to run even if the program does
                        * not even need to run the Gc. However, this has a
                        * slow perf penality on small programs, which is why
                        * it's better to keep guarded when you're
                        * not testing -max_memory.
                        *)
                       if config.test then Gc.full_major ();
                       logger#trace "done with %s" !!file;

                       (res, was_scanned))
                 with
                 (* note that Semgrep_error_code.exn_to_error already handles
                  * Timeout and would generate a TimeoutError code for it,
                  * but we intercept Timeout here to give a better diagnostic.
                  *)
                 | (Match_rules.File_timeout _ | Out_of_memory) as exn ->
                     (* TODO? why we use Match_patters.last_matched_rule here
                      * and below Rule.last_matched_rule?
                      *)
                     (match !Match_patterns.last_matched_rule with
                     | None -> ()
                     | Some rule ->
                         logger#info "critical exn while matching ruleid %s"
                           (Rule_ID.to_string rule.id);
                         logger#info "full pattern is: %s"
                           rule.MR.pattern_string);
                     let loc = Tok.first_loc_of_file !!file in
                     let errors =
                       match exn with
                       | Match_rules.File_timeout rule_ids ->
                           logger#info "Timeout on %s" !!file;
                           (* TODO what happened here is several rules
                              timed out while trying to scan a file.
                              Which heuristically indicates that the
                              file is probably the problem. Once we get
                              rid of the python wrapper we should
                              improve the error message displayed to
                              clearly state that someone investigating
                              should assume the timeout is due to the
                              file *)
                           rule_ids
                           |> List_.map (fun error_rule_id ->
                                  E.mk_error (Some error_rule_id) loc ""
                                    OutJ.Timeout)
                           |> E.ErrorSet.of_list
                       | Out_of_memory ->
                           logger#info "OutOfMemory on %s" !!file;
                           E.ErrorSet.singleton
                             (E.mk_error !Rule.last_matched_rule loc ""
                                OutJ.OutOfMemory)
                       | _ -> raise Impossible
                     in
                     ( Core_result.make_match_result [] errors
                         (Core_profiling.empty_partial_profiling file),
                       Scanned (Original file) )
                     (* those were converted in Main_timeout in timeout_function()*)
                     (* FIXME:
                        Actually, I managed to get this assert to trigger by running
                        semgrep -c p/default-v2 on elasticsearch with -timeout 0.01 ! *)
                 | Time_limit.Timeout _ ->
                     failwith
                       "Time limit exceeded (this shouldn't happen, FIXME)"
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
                       Core_error.ErrorSet.singleton
                         (E.exn_to_error None !!file e)
                     in
                     ( Core_result.make_match_result [] errors
                         (Core_profiling.empty_partial_profiling file),
                       Scanned (Original file) ))
           in
           let scanned_path =
             match was_scanned with
             | Scanned (Original target) -> Some target
             | Not_scanned -> None
           in
           (Core_result.add_run_time run_time res, scanned_path))
  in
  let matches, opt_paths = List.split match_and_path_list in
  let scanned =
    opt_paths |> List_.map_filter Fun.id
    (* It's necessary to remove duplicates because extracted targets are
       mapped back to their original target, and you can have multiple
       extracted targets for a single file. Might as well sort too *)
    |> List.sort_uniq Fpath.compare
  in
  (matches, scanned)

(*****************************************************************************)
(* File targeting and rule filtering *)
(*****************************************************************************)

let xtarget_of_file ~parsing_cache_dir (xlang : Xlang.t) (file : Fpath.t) :
    Xtarget.t =
  let lazy_ast_and_errors =
    lazy
      (let lang =
         (* ew. We fail tests if this gets pulled out of the lazy block. *)
         match xlang with
         | L (lang, []) -> lang
         | L (_lang, _ :: _) ->
             failwith
               "xlang from the language field in -target should be unique \
                (this shouldn't happen FIXME)"
         | _ ->
             (* alt: could return an empty program, but better to be defensive*)
             failwith
               "requesting generic AST for an unspecified target language"
       in
       Parse_with_caching.parse_and_resolve_name ~parsing_cache_dir
         AST_generic.version lang file)
  in
  {
    Xtarget.file;
    xlang;
    lazy_content = lazy (UFile.read_file file);
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
let targets_of_config (config : Core_scan_config.t) :
    In.targets * OutJ.skipped_target list =
  match (config.target_source, config.roots, config.lang) with
  (* We usually let semgrep-python computes the list of targets (and pass it
   * via -target), but it's convenient to also run semgrep-core without
   * semgrep-python and to recursively get a list of targets.
   * We just have a poor's man file targeting/filtering here, just enough
   * to run semgrep-core independently of semgrep-python to test things.
   *)
  | None, roots, Some xlang ->
      (* less: could also apply Common.fullpath? *)
      let roots = roots |> List_.map replace_named_pipe_by_regular_file in
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
      let target_mappings =
        files
        |> List_.map (fun file ->
               {
                 In.path = Fpath.to_string file;
                 analyzer = xlang;
                 products = Product.all;
               })
      in
      (target_mappings, skipped)
  | None, _, None -> failwith "you need to specify a language with -lang"
  (* main code path for semgrep python, with targets specified by -target *)
  | Some target_source, roots, lang_opt ->
      (* sanity checking *)
      (* in deep mode we actually have a single root dir passed *)
      if roots <> [] then
        logger#error "if you use -targets, you should not specify files";
      (* TODO: ugly, this is because the code path for -e/-f requires
       * a language, even with a -target, see test_target_file.py
       *)
      if lang_opt <> None && config.rule_source <> None then
        failwith "if you use -targets and -rules, you should not specify a lang";
      let targets =
        match target_source with
        | Targets x -> x
        | Target_file target_file ->
            UFile.read_file target_file |> In.targets_of_string
      in
      filter_existing_targets targets

(*****************************************************************************)
(* Extract-mode helpers *)
(*****************************************************************************)

(* Extract new targets using the extractors *)
let extracted_targets_of_config (config : Core_scan_config.t)
    (extract_rules : Rule.extract_rule list) (basic_targets : In.target list) :
    In.target list * Extract.adjusters =
  logger#info "extracting nested content from %d files"
    (List.length basic_targets);
  let match_hook str match_ =
    if !Extract.debug_extract_mode && config.output_format =*= Text then (
      UCommon.pr2 "extracted content from ";
      print_match ~str config match_ Metavariable.ii_of_mval)
  in
  let (extracted_targets : Extract.extracted_target_and_adjuster list) =
    basic_targets
    |> List.concat_map (fun (t : In.target) ->
           (* TODO: addt'l filtering required for rule_ids when targets are
              passed explicitly? *)
           let file = t.path in
           let xtarget =
             xtarget_of_file ~parsing_cache_dir:config.parsing_cache_dir
               t.analyzer (Fpath.v file)
           in
           let extracted_targets =
             Match_extract_mode.extract ~match_hook ~timeout:config.timeout
               ~timeout_threshold:config.timeout_threshold extract_rules xtarget
           in
           (* Print number of extra targets so pysemgrep knows *)
           if not (List_.null extracted_targets) then
             print_cli_additional_targets config (List.length extracted_targets);
           extracted_targets)
  in
  let adjusters = Extract.adjusters_of_extracted_targets extracted_targets in
  let in_targets : In.target list =
    extracted_targets
    |> List_.map (fun Extract.{ extracted = Extracted path; analyzer; _ } ->
           (* Extract mode targets work with any product? *)
           { In.path = !!path; analyzer; products = Product.all })
  in
  (in_targets, adjusters)

(*****************************************************************************)
(* a "core" scan *)
(*****************************************************************************)

(* This is used by semgrep-proprietary. *)
let select_applicable_rules_for_analyzer ~analyzer rules =
  rules
  |> List.filter (fun (r : Rule.t) ->
         (* Don't run a Python rule on a JavaScript target *)
         Xlang.is_compatible ~require:analyzer ~provide:r.target_analyzer)
  (* Don't run the extract rules
     Note: we can't filter this out earlier because the rule
     indexes need to be stable.
     TODO: The above may no longer apply since we got rid of
     the numeric indices mapping to rule IDs/names. Do something?
  *)
  |> List_.exclude Extract.is_extract_rule

(* This is also used by semgrep-proprietary. *)
(* TODO: reduce memory allocation by using only one call to List.filter?
   or something even better to reduce the time spent on each target in
   case we have a high number of rules and a high fraction of irrelevant
   rules? *)
let select_applicable_rules_for_target ~analyzer ~products ~path
    ~respect_rule_paths rules =
  select_applicable_rules_for_analyzer ~analyzer rules
  |> List.filter (fun r ->
         List.exists (Semgrep_output_v1_j.equal_product r.Rule.product) products)
  |> List.filter (fun r ->
         (* Honor per-rule include/exclude.
            * Note that this also done in pysemgrep, but we need to do it
            * again here for osemgrep which use a different file targeting
            * strategy.
         *)
         match r.R.paths with
         | Some paths when respect_rule_paths ->
             Filter_target.filter_paths paths path
         | _else -> true)

(* build the callback for iter_targets_and_get_matches_and_exn_to_errors *)
let mk_target_handler (config : Core_scan_config.t) (valid_rules : Rule.t list)
    (prefilter_cache_opt : Match_env.prefilter_config)
    (adjusters : Extract.adjusters) match_hook : target_handler =
 (* Note that this function runs in another process *)
 fun (target : In.target) ->
  let file = Fpath.v target.path in
  let analyzer = target.analyzer in
  let products = target.products in
  let applicable_rules =
    select_applicable_rules_for_target ~analyzer ~products ~path:file
      ~respect_rule_paths:config.respect_rule_paths valid_rules
  in
  let was_scanned =
    match applicable_rules with
    | [] -> Not_scanned
    | _x :: _xs ->
        (* Map back extracted targets when recording files as scanned *)
        let original_target =
          match Hashtbl.find_opt adjusters.original_target (Extracted file) with
          | None -> Extract.Original file
          | Some orig -> orig
        in
        Scanned original_target
  in
  (* TODO: can we skip all of this if there are no applicable
     rules? In particular, can we skip print_cli_progress? *)
  let xtarget =
    xtarget_of_file ~parsing_cache_dir:config.parsing_cache_dir analyzer file
  in
  let default_match_hook str match_ =
    if config.output_format =*= Text then
      print_match ~str config match_ Metavariable.ii_of_mval
  in
  let match_hook = Option.value match_hook ~default:default_match_hook in
  let xconf =
    {
      Match_env.config = Rule_options.default_config;
      equivs = parse_equivalences config.equivalences_file;
      nested_formula = false;
      matching_explanations = config.matching_explanations;
      filter_irrelevant_rules = prefilter_cache_opt;
    }
  in
  let matches =
    (* !!Calling Match_rules!! Calling the matching engine!! *)
    Match_rules.check ~match_hook ~timeout:config.timeout
      ~timeout_threshold:config.timeout_threshold xconf applicable_rules xtarget
    |> set_matches_to_proprietary_origin_if_needed xtarget
    |> Extract.adjust_location_extracted_targets_if_needed adjusters file
  in
  (* So we can display matches incrementally in osemgrep!
   * Note that this is run in a child process of Parmap, so
   * the hook should not rely on shared memory.
   *)
  config.file_match_results_hook |> Option.iter (fun hook -> hook file matches);
  print_cli_progress config;
  (matches, was_scanned)

(* This is the main function used by pysemgrep right now.
 * This is also called now from osemgrep.
 * It takes a set of rules and a set of targets and iteratively process those
 * targets.
 * coupling: If you modify this function, you probably need also to modify
 * Deep_scan.scan() in semgrep-pro which is mostly a copy-paste of this file.
 *)
let scan ?match_hook config ((valid_rules, invalid_rules), rules_parse_time) :
    Core_result.t =
  let rule_errors = errors_of_invalid_rule_errors invalid_rules in

  (* The basic targets.
   * TODO: possibly extract (recursively) from generated stuff? *)
  let basic_targets, skipped = targets_of_config config in
  let targets =
    (* Optimization: no valid rule => no findings.
       This solution avoids using an exception which would be a little harder
       to track.
       Use case: a user is creating a rule and testing it on their project
       but the rule is invalid. *)
    match valid_rules with
    | [] -> []
    | _some_rules -> basic_targets
  in

  (* The "extracted" targets we generate on the fly by calling
   * our extractors (extract mode rules) on the relevant basic targets.
   *)
  let new_extracted_targets, adjusters =
    extracted_targets_of_config config
      (Extract.filter_extract_rules valid_rules)
      basic_targets
  in

  let all_targets = targets @ new_extracted_targets in
  let prefilter_cache_opt =
    if config.filter_irrelevant_rules then
      Match_env.PrefilterWithCache (Hashtbl.create (List.length valid_rules))
    else NoPrefiltering
  in

  (* Let's go! *)
  logger#info "processing %d files, skipping %d files" (List.length all_targets)
    (List.length skipped);
  let file_results, scanned_targets =
    all_targets
    |> iter_targets_and_get_matches_and_exn_to_errors config
         (mk_target_handler config valid_rules prefilter_cache_opt adjusters
            match_hook)
  in
  let scanned_target_table =
    (* provide fast access to paths that were scanned by at least one rule;
       includes extracted targets *)
    (* TODO: create a new function: Common.hash_of_list ~get_key list ? *)
    let tbl = Hashtbl.create (List.length scanned_targets) in
    List.iter (fun x -> Hashtbl.replace tbl !!x ()) scanned_targets;
    tbl
  in
  let scanned =
    (* we do not use all_targets here, because we don't count
     * the extracted targets
     *)
    targets
    |> List.filter (fun (x : In.target) ->
           Hashtbl.mem scanned_target_table x.path)
    |> List_.map (fun x -> Fpath.v x.In.path)
  in
  (* Since the OSS engine was invoked, there were no interfile languages
     requested *)
  let interfile_languages_used = [] in
  let res =
    RP.make_final_result file_results
      (List_.map (fun r -> (r, `OSS)) valid_rules)
      invalid_rules scanned interfile_languages_used ~rules_parse_time
  in
  logger#info "found %d matches, %d errors"
    (List.length res.processed_matches)
    (List.length res.errors);

  let processed_matches, new_errors, new_skipped =
    filter_files_with_too_many_matches_and_transform_as_timeout
      config.max_match_per_file res.processed_matches
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
  { res with processed_matches; errors; extra }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let get_rules config =
  Common.with_time (fun () -> rules_from_rule_source config)
[@@trace]

let scan_with_exn_handler (config : Core_scan_config.t) :
    Core_result.result_or_exn =
  try
    Tracing.initial_configuration ();
    let timed_rules = Tracing.with_setup (fun () -> get_rules config) in
    (* The pre and post processors hook here is currently just used
       for the secrets post processor, but it should now be trivial to
       hook any post processing step that needs to look at rules and
       results. *)
    let res =
      Pre_post_core_scan.call_with_pre_and_post_processor Fun.id scan config
        timed_rules
    in
    sanity_check_invalid_patterns res
  with
  | exn when not !Flag_semgrep.fail_fast ->
      let e = Exception.catch exn in
      logger#info "Uncaught exception: %s" (Exception.to_string e);
      Error (e, None)
