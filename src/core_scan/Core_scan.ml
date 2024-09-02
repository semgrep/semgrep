(* Yoann Padioleau
 *
 * Copyright (C) 2020-2024 Semgrep Inc.
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
module ESet = Core_error.ErrorSet
module MR = Mini_rule
module R = Rule
module In = Input_to_core_j
module Out = Semgrep_output_v1_j

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* A "Core" scan.
 *
 * ## pysemgrep vs semgrep-core
 *
 * When invoked by `pysemgrep`, `semgrep-core` will always be passed
 * `-rules` and `-targets`.
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
 * `List_.map`, which is tail-recursive, instead of `List.map`.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* The type of the semgrep core scan. We define it here so that
   semgrep and semgrep-proprietary use the same definition *)
type func = Core_scan_config.t -> Core_result.result_or_exn

(* TODO: far more: alarm, stdout (sometimes), ... *)
type caps = < Cap.fork >

(* A target is [Not_scanned] when semgrep didn't find any applicable rules.
 * The information is useful to return to pysemgrep/osemgrep to
 * display statistics.
 * TODO? do we need this type? switch to a simple bool?
 *)
type was_scanned = Scanned of Fpath.t | Not_scanned

(* Type of the iter_targets_and_get_matches_and_exn_to_errors callback.

   A target handler returns (matches, was_scanned) where was_scanned indicates
   whether at least one rule applied to the target since the target could
   be excluded by all the rules via per-rule include/exclude patterns.
   alt: baking this flag into match_result type would lead to even worse
   complexity

   Remember that a target handler runs in another process (via Parmap).
*)
type target_handler = Target.t -> Core_result.matches_single_file * was_scanned

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let set_matches_to_proprietary_origin_if_needed (xtarget : Xtarget.t)
    (matches : Core_result.matches_single_file) :
    Core_result.matches_single_file =
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
  then Report_pro_findings.annotate_pro_findings xtarget matches
  else matches

(*****************************************************************************)
(* Scanned Helpers *)
(*****************************************************************************)

let scanned_unless_empty_rules rules (path : Fpath.t) : was_scanned =
  match rules with
  | [] -> Not_scanned
  | _ -> Scanned path

let scanned_of_targets ~targets ~scanned_targets =
  (* TODO: Delete any lockfile-only findings whose rule produced a code+lockfile
     finding in that lockfile *)
  let scanned_target_table =
    (* provide fast access to paths that were scanned by at least one rule;
       includes extracted targets *)
    (* TODO: create a new function: Common.hash_of_list ~get_key list ? *)
    let tbl = Hashtbl.create (List.length scanned_targets) in
    List.iter (fun x -> Hashtbl.replace tbl !!x ()) scanned_targets;
    tbl
  in
  (* old: we were using all_targets and targets to not count extracted one *)
  targets
  |> List.filter (fun (x : Target.t) ->
         let internal_path = Target.internal_path x in
         Hashtbl.mem scanned_target_table !!internal_path)

(*****************************************************************************)
(* Pysemgrep progress bar *)
(*****************************************************************************)
(* LATER: remove once osegmrep is fully done *)

(* Print additional target count so the Python progress bar knows *)
let print_cli_additional_targets (config : Core_scan_config.t) (n : int) : unit
    =
  match config.output_format with
  | Json true -> UConsole.print (string_of_int n)
  | _ -> ()

(* TODO: suspicious: this runs in a child process. Please explain how it's
   safe to write a dot on stdout in a child process and why it's mixed with
   JSON output.
*)
let print_cli_progress (config : Core_scan_config.t) : unit =
  (* Print when each file is done so the Python progress bar knows *)
  match config.output_format with
  | Json true -> UConsole.print "."
  | _ -> ()

(*****************************************************************************)
(* Semgrep-core incremental matches *)
(*****************************************************************************)

(* LATER: remove once osegmrep is fully done *)
let print_incremental_matches_when_text_mode (config : Core_scan_config.t)
    (match_ : Pattern_match.t) : unit =
  match config.output_format with
  | Text mvars ->
      (* alt: we could pass the stdout caps to Core_scan, but that would
           * require to change lots of callers, and ideally we don't want
           * Core_scan to display things on stdout; this is used here only
           * as a deprecated way to get matchings output, so let's use
           * the UNSAFE helper.
      *)
      let caps = Cap.stdout_caps_UNSAFE () in
      Core_text_output.print_match caps match_ mvars
  | Json _
  | NoOutput ->
      ()

(*****************************************************************************)
(* Timeout *)
(*****************************************************************************)

(* Certain patterns may be too general and match too many times on big files.
 * This does not cause a Timeout during parsing or matching, but returning
 * a huge number of matches can stress Core_json_output.
 * and anyway is probably a sign that the pattern should be rewritten.
 *)
let filter_files_with_too_many_matches_and_transform_as_timeout
    max_match_per_file matches =
  let per_files =
    matches
    |> List_.map (fun ({ pm; _ } : Core_result.processed_match) ->
           (!!(pm.path.internal_path_to_content), pm))
    |> Assoc.group_assoc_bykey_eff
  in

  let offending_file_list =
    per_files
    |> List_.filter_map (fun (file, xs) ->
           if List.length xs > max_match_per_file then Some file else None)
  in
  let offending_files = Hashtbl_.hashset_of_list offending_file_list in
  let new_matches =
    matches
    |> List_.exclude (fun ({ pm; _ } : Core_result.processed_match) ->
           Hashtbl.mem offending_files !!(pm.path.internal_path_to_content))
  in
  let new_errors, new_skipped =
    offending_file_list
    |> List_.map (fun file ->
           (* logging useful info for rule writers *)
           Logs.warn (fun m ->
               m "too many matches on %s, generating exn for it" file);
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
           Logs.warn (fun m ->
               m "most offending rule: id = %s, matches = %d, pattern = %s"
                 (Rule_ID.to_string id) cnt pat);

           (* todo: we should maybe use a new error: TooManyMatches of int * string*)
           let loc = Tok.first_loc_of_file file in
           let error =
             E.mk_error ~rule_id:(Some id)
               ~msg:
                 (spf
                    "%d rules result in too many matches, most offending rule \
                     has %d: %s"
                    offending_rules cnt pat)
               loc Out.TooManyMatches
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
  (new_matches, new_errors, List_.flatten new_skipped)
[@@profiling "Run_semgrep.filter_too_many_matches"]

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)

let parse_and_resolve_name (lang : Lang.t) (fpath : Fpath.t) :
    AST_generic.program * Tok.location list =
  let { Parsing_result2.ast; skipped_tokens; _ } =
    Logs_.with_debug_trace "Core_scan.parse_and_resolve_name" (fun () ->
        Logs.debug (fun m ->
            m "Parsing (and naming) %s (with lang %s)" !!fpath (Lang.show lang));
        Parse_target.parse_and_resolve_name lang fpath)
  in
  (ast, skipped_tokens)

(* for -rules *)
let rules_from_rule_source (rule_source : Core_scan_config.rule_source) :
    Rule_error.rules_and_invalid =
  match rule_source with
  | Core_scan_config.Rule_file file -> (
      Logs.info (fun m -> m "Parsing rules in %s" !!file);
      match
        Parse_rule.parse_and_filter_invalid_rules ~rewrite_rule_ids:None file
      with
      | Ok rules -> rules
      | Error e -> failwith ("Error in parsing: " ^ Rule_error.string_of_error e)
      )
  | Core_scan_config.Rules rules -> (rules, [])
[@@trace]

(* TODO? this is currently deprecated, but pad still has hope the
 * feature can be resurrected.
 *)
let parse_equivalences equivalences_file =
  match equivalences_file with
  | None -> []
  | Some file -> Parse_equivalences.parse file
[@@profiling]

(*****************************************************************************)
(* logging/telemetry *)
(*****************************************************************************)
let handle_target_with_trace (handle_target : Target.t -> 'a) (t : Target.t) :
    'a =
  let target_name = Target.internal_path t in
  let data () =
    [
      ("filename", `String !!target_name);
      ("num_bytes", `Int (UFile.filesize target_name));
    ]
  in
  Tracing.with_span ~__FILE__ ~__LINE__ ~data "scan.handle_target" (fun _sp ->
      handle_target t)

let log_scan_inputs (config : Core_scan_config.t) ~targets ~skipped ~valid_rules
    ~invalid_rules =
  (* Add information to the trace *)
  let num_rules = List.length valid_rules in
  let num_targets = List.length targets in
  let num_skipped = List.length skipped in
  Tracing.add_data_to_opt_span config.top_level_span
    [
      ("num_rules", `Int num_rules);
      ("num_targets", `Int num_targets);
      ("num_skipped_targets", `Int num_skipped);
    ];
  Logs.info (fun m ->
      m "scan: processing %d files (skipping %d), with %d rules (skipping %d )"
        num_targets num_skipped num_rules
        (List.length invalid_rules));
  ()

let log_scan_results (config : Core_scan_config.t) (res : Core_result.t)
    ~skipped_targets =
  (* TODO: delete this comment and -stat_matches.
   * note: uncomment the following and use semgrep-core -stat_matches
   * to debug too-many-matches issues.
   * Common2.write_value matches "/tmp/debug_matches";
   *)
  let num_matches = List.length res.processed_matches in
  let num_errors = List.length res.errors in
  Tracing.add_data_to_opt_span config.top_level_span
    [ ("num_matches", `Int num_matches); ("num_errors", `Int num_errors) ];
  Logs.debug (fun m ->
      m "scan: found %d matches, %d errors" num_matches num_errors);
  Logs.debug (fun m ->
      m "scan: there were %d skipped targets" (List.length skipped_targets));
  ()

(* This is used to generate warnings in the logs
 * when we exceed or are close to exceed the memory limit.
 *)
let get_context_for_memory_limit target () =
  let origin = Target.origin target in
  match !Rule.last_matched_rule with
  | None -> Origin.to_string origin
  | Some rule_id ->
      spf "%s on %s" (Rule_ID.to_string rule_id) (Origin.to_string origin)

let log_critical_exn_and_last_rule () =
  (* TODO? why we use Match_patters.last_matched_rule here
     * and below Rule.last_matched_rule?
  *)
  match !Match_patterns.last_matched_rule with
  | None -> ()
  | Some rule ->
      Logs.warn (fun m ->
          m "critical exn while matching ruleid %s" (Rule_ID.to_string rule.id));
      Logs.debug (fun m -> m "full pattern is: %s" rule.MR.pattern_string);
      ()

let errors_of_timeout_or_memory_exn (exn : exn) (target : Target.t) : ESet.t =
  let internal_path = Target.internal_path target in
  let origin = Target.origin target in
  let loc = Tok.first_loc_of_file !!internal_path in
  match exn with
  | Match_rules.File_timeout rule_ids ->
      Logs.warn (fun m -> m "Timeout on %s" (Origin.to_string origin));
      (* TODO what happened here is several rules
         timed out while trying to scan a file.
         Which heuristically indicates that the
         file is probably the problem. Once we get
         rid of the python wrapper we should
         improve the error message displayed to
         clearly state that someone investigating
         should assume the timeout is due to the
          file
      *)
      rule_ids
      |> List_.map (fun error_rule_id ->
             E.mk_error ~rule_id:(Some error_rule_id) loc Out.Timeout)
      |> ESet.of_list
  | Out_of_memory ->
      Logs.warn (fun m -> m "OutOfMemory on %s" (Origin.to_string origin));
      ESet.singleton
        (E.mk_error ~rule_id:!Rule.last_matched_rule loc Out.OutOfMemory)
  | _ -> raise Impossible

(*****************************************************************************)
(* Iteration helpers *)
(*****************************************************************************)

(* Returns a list of match results and a separate list of scanned targets *)
let iter_targets_and_get_matches_and_exn_to_errors (caps : < Cap.fork >)
    (config : Core_scan_config.t) (handle_target : target_handler)
    (targets : Target.t list) :
    Core_profiling.file_profiling Core_result.match_result list * Fpath.t list =
  (* The path in match_and_path_list is None when the file was not scanned *)
  let (match_and_path_list
        : (Core_profiling.file_profiling Core_result.match_result
          * Fpath.t option)
          list) =
    targets
    |> Parmap_targets.map_targets__run_in_forked_process_do_not_modify_globals
         caps config.ncores (fun (target : Target.t) ->
           let internal_path = Target.internal_path target in
           let noprof = Core_profiling.empty_partial_profiling internal_path in
           Logs.debug (fun m -> m "Core_scan analyzing %s" !!internal_path);

           (* Coupling: if you update handle_target_maybe_with_trace here
            * it's very likely you'd need to update the same in Deep_scan.ml
            *
            * Sadly we need to disable tracing when we are using more than 1
            * cores.
            *
            * The reason is that parmap forks new processes, and we occasionally
            * run into a deadlock where the scan just freezes when we use
            * tracing and multiprocesses together.
            *
            * Hopefully, Ocaml5 with multithread support will resolve this issue.
            * For now, just turn off tracing when we use more than 1 core.
            *)
           let handle_target_maybe_with_trace =
             if config.ncores > 1 then handle_target
             else handle_target_with_trace handle_target
           in

           let (res, was_scanned), run_time =
             Common.with_time (fun () ->
                 try
                   Memory_limit.run_with_memory_limit
                     ~get_context:(get_context_for_memory_limit target)
                     ~mem_limit_mb:config.max_memory_mb (fun () ->
                       (* we used to call Time_limit.set_timeout() here, but
                        * this is now done in Match_rules.check() because we
                        * now timeout per rule, not per file since pysemgrep
                        * passed all the rules to semgrep-core.
                        *)
                       let res, was_scanned =
                         handle_target_maybe_with_trace target
                       in
                       (* old: This was to test -max_memory, to give a chance
                        * to Gc.create_alarm to run even if the program does
                        * not even need to run the Gc. However, this has a
                        * slow perf penality on small programs, which is why
                        * it's better to keep guarded when you're
                        * not testing -max_memory.
                        * if config.test then Gc.full_major ();
                        *)
                       (res, was_scanned))
                 with
                 (* note that exn_to_error called further below already handles
                  * Timeout and would generate a TimeoutError code for it,
                  * but we intercept Timeout here to give a better diagnostic.
                  *)
                 | (Match_rules.File_timeout _ | Out_of_memory) as exn ->
                     log_critical_exn_and_last_rule ();
                     let errors = errors_of_timeout_or_memory_exn exn target in
                     ( Core_result.mk_match_result [] errors noprof,
                       Scanned internal_path )
                 | Time_limit.Timeout _ ->
                     (* converted in Main_timeout in timeout_function() *)
                     (* FIXME:
                          Actually, I managed to get this assert to trigger by
                          running semgrep -c p/default-v2 on elasticsearch with
                          -timeout 0.01 !
                     *)
                     failwith
                       "Time limit exceeded (this shouldn't happen, FIXME)"
                 (* convert all other exns (e.g., a parse error in a target file)
                  * in an empty match result with errors, so that one error in
                  * one target file does not abort the whole scan and the
                  * semgrep-core program.
                  *)
                 | exn when not !Flag_semgrep.fail_fast ->
                     let e = Exception.catch exn in
                     let errors =
                       ESet.singleton (E.exn_to_error None internal_path e)
                     in
                     ( Core_result.mk_match_result [] errors noprof,
                       Scanned internal_path ))
           in
           let scanned_path =
             match was_scanned with
             | Scanned target -> Some target
             | Not_scanned -> None
           in
           (Core_result.add_run_time run_time res, scanned_path))
  in
  let matches, opt_paths = List.split match_and_path_list in
  let scanned =
    opt_paths |> List_.filter_map Fun.id
    (* It's necessary to remove duplicates because extracted targets are
       mapped back to their original target, and you can have multiple
       extracted targets for a single file. Might as well sort too *)
    |> List.sort_uniq Fpath.compare
  in
  (matches, scanned)

(*****************************************************************************)
(* File targeting *)
(*****************************************************************************)

(* In some context, a target passed in might have disappeared, or have been
 * encoded in the wrong way in the Inputs_to_core.atd (for example
 * in the case of filenames with special unicode bytes in it), in which case
 * Common2.filesize above would fail and crash the whole scan as the
 * raised exn is outside the iter_targets_and_get_matches_and_exn_to_errors
 * big try. This is why it's better to filter those problematic targets
 * early on.
 *)
let filter_existing_targets (targets : Target.t list) :
    Target.t list * Out.skipped_target list =
  targets
  |> Either_.partition (fun (target : Target.t) ->
         let internal_path = Target.internal_path target in
         if Sys.file_exists !!internal_path then Left target
         else
           match Target.origin target with
           | File path ->
               Right
                 {
                   Semgrep_output_v1_t.path;
                   reason = Nonexistent_file;
                   details = Some "File does not exist";
                   rule_id = None;
                 }
           | GitBlob { sha; _ } ->
               Right
                 {
                   Semgrep_output_v1_t.path = Target.internal_path target;
                   reason = Nonexistent_file;
                   details =
                     Some
                       (spf "Issue creating a target from git blob %s"
                          (Digestif.SHA1.to_hex sha));
                   rule_id = None;
                 })

(* Compute the set of targets, either by reading what was passed
 * in -targets or passed by osemgrep in Targets.
 *)
let targets_of_config (config : Core_scan_config.t) :
    Target.t list * Out.skipped_target list =
  (* sanity checking
   * LATER: we should get rid of those fields instead *)
  if config.lang <> None then
    failwith "config.lang is not empty in Core_scan.targets_of_config";
  if config.roots <> [] then
    failwith "config.roots is not empty in Core_scan.targets_of_config";
  match config.target_source with
  | None -> failwith "you need to specify targets with -targets"
  | Some target_source -> (
      match target_source with
      | Targets x -> x |> filter_existing_targets
      | Target_file target_file ->
          UFile.read_file target_file
          |> In.targets_of_string
          |> List_.map Target.target_of_input_to_core
          |> filter_existing_targets)

(* DEPRECATED
 * Compute the set of targets, either by reading what was passed
 * in -target, or by using our poor's man file targeting with
 * Find_targets_old.files_of_dirs_or_files.
 *)
let targets_of_config_DEPRECATED (config : Core_scan_config.t) :
    Target.t list * Out.skipped_target list =
  match (config.target_source, config.roots, config.lang) with
  (* We usually let semgrep-python computes the list of targets (and pass it
   * via -target), but it's convenient to also run semgrep-core without
   * semgrep-python and to recursively get a list of targets.
   * We just have a poor's man file targeting/filtering here, just enough
   * to run semgrep-core independently of semgrep-python to test things.
   *)
  | None, roots, Some xlang ->
      (* less: could also apply Common.fullpath? *)
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
        roots
        |> List_.map Scanning_root.to_fpath
        |> Find_targets_old.files_of_dirs_or_files lang_opt
      in
      let target_mappings =
        files
        |> List_.map (fun file : Target.t ->
               Regular (Target.mk_regular xlang Product.all (Origin.File file)))
      in
      (target_mappings, skipped)
  | None, _, None -> failwith "you need to specify a language with -lang"
  (* main code path for semgrep python, with targets specified by -target *)
  | Some _target_source, roots, lang_opt -> (
      (* sanity checking *)
      (* in deep mode we actually have a single root dir passed *)
      if roots <> [] then
        Logs.warn (fun m ->
            m "if you use -targets, you should not specify files");
      (* TODO: ugly, this is because the code path for -e/-f requires
       * a language, even with a -target, see test_target_file.py
       *)
      if lang_opt <> None then
        failwith "if you use -targets and -rules, you should not specify a lang";
      match config.target_source with
      | None -> failwith "you need to specify targets with -targets"
      | Some target_source -> (
          match target_source with
          | Targets x -> x |> filter_existing_targets
          | Target_file target_file ->
              UFile.read_file target_file
              |> In.targets_of_string
              |> List_.map Target.target_of_input_to_core
              |> filter_existing_targets))

(*****************************************************************************)
(* Rule selection *)
(*****************************************************************************)

(* This is used by semgrep-proprietary. *)
let rules_for_analyzer ~analyzer rules =
  rules
  |> List.filter (fun (r : Rule.t) ->
         (* Don't run a Python rule on a JavaScript target *)
         Xlang.is_compatible ~require:analyzer ~provide:r.target_analyzer)

(* Note that filtering is applied on the basis of the target's origin, not the
 * target's "file". This is because filtering should apply to the user's
 * perception of the file, not whatever we may transform it to internally.
 *
 * For instance, the "file" of a target may be a tempfile which has no meaning,
 * and is essentially randomly generated. `paths:` filtering shouldn't apply to
 * this!
 *
 * Note also that `paths:` filters are relative to the root of a project [0],
 * so if the target's file is an absolute path, we don't want to use that for
 * filtering: instead, we'd want the origin to be the desired relative path and
 * use that.
 *
 * [0]: <https://semgrep.dev/docs/writing-rules/rule-syntax/#paths>
 *)
let rules_for_origin paths (origin : Origin.t) =
  match paths with
  | Some paths -> (
      match origin with
      | File path -> Filter_target.filter_paths paths path
      | GitBlob { paths = target_paths; _ } ->
          target_paths
          |> List.exists (fun (_, path_at_commit) ->
                 Filter_target.filter_paths paths path_at_commit))
  | _else -> true

(* This is also used by semgrep-proprietary. *)
(* TODO: reduce memory allocation by using only one call to List.filter?
   or something even better to reduce the time spent on each target in
   case we have a high number of rules and a high fraction of irrelevant
   rules? *)
let rules_for_target ~analyzer ~products ~origin ~respect_rule_paths rules =
  let rules =
    rules_for_analyzer ~analyzer rules
    |> List.filter (fun r ->
           products |> List.exists (Out.equal_product r.Rule.product))
  in
  if respect_rule_paths then
    rules
    |> List.filter (fun (r : R.rule) ->
           (* Honor per-rule include/exclude.
              * Note that this also done in pysemgrep, but we need to do it
              * again here for osemgrep which use a different file targeting
              * strategy.
           *)
           rules_for_origin r.paths origin)
  else rules

(*****************************************************************************)
(* SCA *)
(*****************************************************************************)

let lockfile_xtarget_resolve (lockfile : Target.lockfile) : Lockfile_xtarget.t =
  Lockfile_xtarget.resolve Parse_lockfile.parse_manifest
    Parse_lockfile.parse_lockfile lockfile

let rules_for_lockfile_kind ~lockfile_kind rules =
  rules
  |> List_.filter_map (fun ({ Rule.dependency_formula; _ } as r) ->
         match dependency_formula with
         | None -> None
         | Some formula ->
             if
               formula
               |> List.exists (fun R.{ ecosystem; _ } ->
                      Semgrep_output_v1_t.equal_ecosystem ecosystem
                        (Lockfile_kind.to_ecosystem lockfile_kind))
             then Some (r, formula)
             else None)

let supply_chain_rules ~lockfile_kind ~respect_rule_paths ~origin rules =
  let rules = rules_for_lockfile_kind ~lockfile_kind rules in
  if respect_rule_paths then
    rules
    |> List.filter (fun ((r : R.rule), _) -> rules_for_origin r.paths origin)
  else rules

let sca_rules_filtering (target : Target.regular) (rules : Rule.t list) :
    Rule.t list * Match_dependency.dependency_match_table =
  let lockfile_xtarget_opt =
    target.lockfile |> Option.map lockfile_xtarget_resolve
  in
  (* If a rule tried to a find a dependency match and failed, then it will
     never produce any matches of any kind *)
  let _skipped_supply_chain, rules_with_dep_matches =
    match lockfile_xtarget_opt with
    | None -> ([], rules |> List_.map (fun x -> (x, None)))
    | Some lockfile_target ->
        rules
        |> Match_dependency.match_all_dependencies lockfile_target
        |> Either_.partition (function
             | rule, Some [] -> Left rule
             | x -> Right x)
  in
  let dependency_match_table =
    rules_with_dep_matches
    |> List_.filter_map (function
         | _, None -> None
         | rule, Some dep_matches -> Some (fst rule.R.id, dep_matches))
    |> Hashtbl_.hash_of_list
  in

  let rules = rules_with_dep_matches |> List_.map fst in
  (rules, dependency_match_table)

(*****************************************************************************)
(* a "core" scan *)
(*****************************************************************************)

(* build the callback for iter_targets_and_get_matches_and_exn_to_errors *)
let mk_target_handler (config : Core_scan_config.t) (valid_rules : Rule.t list)
    (prefilter_cache_opt : Match_env.prefilter_config) : target_handler =
  (* Note that this function runs in another process *)
  function
  | Lockfile
      ({ path = { internal_path_to_content; origin }; kind; _ } as lockfile) ->
      let lockfile_xtarget = lockfile_xtarget_resolve lockfile in
      let rules =
        supply_chain_rules ~lockfile_kind:kind ~origin
          ~respect_rule_paths:config.respect_rule_paths valid_rules
      in
      let dep_matches =
        rules
        |> List_.map (fun (rule, dep_formula) ->
               Match_dependency.check_rule rule lockfile_xtarget dep_formula)
      in
      let was_scanned =
        scanned_unless_empty_rules rules internal_path_to_content
      in
      (* TODO: run all the right hooks *)
      ( Core_result.collate_rule_results internal_path_to_content dep_matches,
        was_scanned )
  | Regular
      ({
         analyzer;
         products;
         path = { origin; internal_path_to_content = file };
         _;
       } as target) ->
      let rules =
        rules_for_target ~analyzer ~products ~origin
          ~respect_rule_paths:config.respect_rule_paths valid_rules
      in
      let was_scanned = scanned_unless_empty_rules rules file in

      (* TODO: can we skip all of this if there are no applicable
          rules? In particular, can we skip print_cli_progress? *)
      let xtarget = Xtarget.resolve parse_and_resolve_name target in
      let match_hook pm = print_incremental_matches_when_text_mode config pm in
      let xconf =
        {
          Match_env.config = Rule_options.default;
          equivs = parse_equivalences config.equivalences_file;
          nested_formula = false;
          matching_explanations = config.matching_explanations;
          filter_irrelevant_rules = prefilter_cache_opt;
        }
      in
      let rules, dependency_match_table = sca_rules_filtering target rules in
      let matches =
        (* !!Calling Match_rules!! Calling the matching engine!! *)
        Match_rules.check ~match_hook ~timeout:config.timeout
          ~timeout_threshold:config.timeout_threshold ~dependency_match_table
          xconf rules xtarget
        |> set_matches_to_proprietary_origin_if_needed xtarget
      in
      (* So we can display matches incrementally in osemgrep!
          * Note that this is run in a child process of Parmap, so
          * the hook should not rely on shared memory.
      *)
      config.file_match_hook |> Option.iter (fun hook -> hook file matches);
      print_cli_progress config;
      (matches, was_scanned)

(* coupling: with Deep_scan.scan_aux() *)
let scan_exn (caps : caps) (config : Core_scan_config.t)
    (rules : Rule_error.rules_and_invalid * float) : Core_result.t =
  Logs.debug (fun m -> m "Core_scan.scan_exn %s" (Core_scan_config.show config));
  (* the rules *)
  let (valid_rules, invalid_rules), rules_parse_time = rules in
  let (rule_errors : E.t list) =
    invalid_rules |> List_.map E.error_of_invalid_rule
  in
  (* the targets *)
  let targets, skipped = targets_of_config config in

  (* !!Let's go!! *)
  log_scan_inputs config ~targets ~skipped ~valid_rules ~invalid_rules;
  let prefilter_cache_opt =
    if config.filter_irrelevant_rules then
      Match_env.PrefilterWithCache (Hashtbl.create (List.length valid_rules))
    else NoPrefiltering
  in
  let file_results, scanned_targets =
    targets
    |> iter_targets_and_get_matches_and_exn_to_errors
         (caps :> < Cap.fork >)
         config
         (mk_target_handler config valid_rules prefilter_cache_opt)
  in
  let scanned = scanned_of_targets ~targets ~scanned_targets in

  (* the OSS engine was invoked so no interfile langs *)
  let interfile_languages_used = [] in
  let (res : Core_result.t) =
    Core_result.mk_result file_results
      (List_.map (fun r -> (r, `OSS)) valid_rules)
      invalid_rules scanned interfile_languages_used ~rules_parse_time
  in
  let processed_matches, new_errors, new_skipped =
    filter_files_with_too_many_matches_and_transform_as_timeout
      config.max_match_per_file res.processed_matches
  in
  (* concatenate all errors *)
  let errors = rule_errors @ new_errors @ res.errors in
  (* Concatenate all the skipped targets *)
  let skipped_targets = skipped @ new_skipped @ res.skipped_targets in

  (* TODO? should probably remove ~skipped_targets and apply to latest res *)
  log_scan_results config res ~skipped_targets;
  (* TODO: returning, or not skipped_targets does not seem to have any impact
   * on our testsuite, weird. We need to add more tests. Maybe because
   * both pysemgrep and osemgrep do their own skip targets management.
   *)
  { res with processed_matches; errors; skipped_targets }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* This is the main function used by pysemgrep right now.
 * This is also now called from osemgrep.
 * It takes a set of rules and a set of targets and iteratively process those
 * targets.
 * coupling: If you modify this function, you probably need also to modify
 * Deep_scan.scan() in semgrep-pro which is mostly a copy-paste of this file.
 *)
let scan (caps : caps) (config : Core_scan_config.t) : Core_result.result_or_exn
    =
  try
    let timed_rules =
      Common.with_time (fun () -> rules_from_rule_source config.rule_source)
    in
    (* The pre and post processors hook here is currently used
       for the secrets post processor in Pro, and for the autofix
       and nosemgrep post processors in OSS; it is easy to
       hook any pre or post processing step that needs to look at rules and
       results. *)
    Ok
      (Pre_post_core_scan.call_with_pre_and_post_processor Fun.id
         (scan_exn caps) config timed_rules)
  with
  | exn when not !Flag_semgrep.fail_fast ->
      let e = Exception.catch exn in
      Logs.err (fun m ->
          m "Uncaught exn in Core_scan.scan: %s" (Exception.to_string e));
      Error e
