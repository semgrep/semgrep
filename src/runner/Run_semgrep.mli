(*****************************************************************************
 * # Notes
 *
 * ## semgrep CLI (a.k.a. pysemgrep) vs semgrep core
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
 *****************************************************************************)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

val semgrep_dispatch : Runner_config.t -> unit
(** Main entry point to the semgrep-core engine. This is called from Main.ml *)

(*****************************************************************************)
(* Engine functions used in tests or semgrep-core variants *)
(*****************************************************************************)

val semgrep_with_one_pattern : Runner_config.t -> unit
(** this is the function used when running semgrep-core with -e or -f *)

val semgrep_with_rules_and_formatted_output : Runner_config.t -> unit
(** [semgrep_with_rules_and_formatted_output config] calls
    [semgrep_with_raw_results_and_exn_handler] and then
    [output_semgrep_results] on the results
*)

val output_semgrep_results :
  Exception.t option * Core_result.final_result * Fpath.t list ->
  Runner_config.t ->
  unit
(** [output_semgrep_results] takes the results of a semgrep run and
    format the results on stdout either in a JSON or Textual format
    (depending on the value in config.output_format)

    This is the function used when running semgrep-core with -rules.
*)

val semgrep_with_raw_results_and_exn_handler :
  Runner_config.t ->
  Exception.t option * Core_result.final_result * Fpath.t list
(** [semgrep_with_raw_results_and_exn_handler config] runs the semgrep-core
    engine with a starting list of targets and returns
    (success, result, targets).
    The targets are all the files that were considered valid targets for the
    semgrep scan. This excludes files that were filtered out on purpose
    due to being in the wrong language, too big, etc.
    It includes targets that couldn't be scanned, for instance due to
    a parsing error.

    This run the core engine in Match_rules.check on every files, in
    parallel, with some memory limits, and aggregate the results.
*)

type semgrep_with_rules_t =
  (Rule.t list * Rule.invalid_rule_error list) * float ->
  Core_result.final_result * Fpath.t list

val semgrep_with_rules :
  ?match_hook:(string -> Pattern_match.t -> unit) ->
  Runner_config.t ->
  semgrep_with_rules_t

(*****************************************************************************)
(* Pre and Post Processors Hook For Semgrep Pro / Extensions        *)
(*****************************************************************************)

module type Pre_and_post_processor = sig
  type state

  val pre_process : Rule.t list -> Rule.t list * state
  val post_process : state -> Core_result.final_result -> Core_result.final_result
end

val hook_pre_and_post_processor : (module Pre_and_post_processor) ref

val call_with_pre_and_post_processor :
  semgrep_with_rules_t -> semgrep_with_rules_t

(*****************************************************************************)
(* Utilities functions used in tests or semgrep-core variants *)
(*****************************************************************************)

(* used internally but also called by osemgrep *)
val errors_of_invalid_rule_errors :
  Rule.invalid_rule_error list -> Core_error.t list

val replace_named_pipe_by_regular_file : Fpath.t -> Fpath.t
(**
   Copy named pipes created with <(echo 'foo') on the command line
   into a regular file to avoid illegal seeks when reporting match results
   or parsing errors.
   Any file coming from the command line should go through this so as to
   allows easy manual testing.
*)

(* Old hook to support incremental display of matches for semgrep-core
 * in text-mode. Deprecated. Use Runner_config.file_match_results_hook instead
 * now with osemgrep.
 *)
val print_match :
  ?str:string ->
  Runner_config.t ->
  Pattern_match.t ->
  (Metavariable.mvalue -> Tok.t list) ->
  unit

(* This function prints a dot, which is consumed by pysemgrep to update
   the progress bar. See `core_runner.py`
*)
val update_cli_progress : Runner_config.t -> unit

(* TODO: Fpath.t *)
val exn_to_error : Common.filename -> Exception.t -> Core_error.t
(**
  Small wrapper over Semgrep_error_code.exn_to_error to handle also
  semgrep-specific exns that have a position.
  See also JSON_report.json_of_exn for non-target related exn handling.
*)

val mk_rule_table :
  Rule.t list -> string list (* rule IDs *) -> (int, Rule.t) Hashtbl.t
(** Helper to create the table of rules to run for each file **)

val extracted_targets_of_config :
  Runner_config.t ->
  Rule.t list ->
  Input_to_core_t.target list
  * ( Common.filename,
      Match_extract_mode.match_result_location_adjuster )
    Hashtbl.t
(**
   Generate a list of new targets, which are extracted from extract rules.
   The rule ids correspond to the rules to run against the generated
   targets.
*)

val rules_from_rule_source :
  Runner_config.t -> Rule.rules * Rule.invalid_rule_error list
(** Get the rules *)

val targets_of_config :
  Runner_config.t ->
  Rule_ID.t list ->
  Input_to_core_t.targets * Semgrep_output_v1_t.skipped_target list
(**
  Compute the set of targets, either by reading what was passed
  in -target, or by using Find_target.files_of_dirs_or_files.
  The rule ids argument is useful only when you don't use -target.
 *)

val filter_files_with_too_many_matches_and_transform_as_timeout :
  int ->
  Pattern_match.t list ->
  Pattern_match.t list
  * Core_error.t list
  * Semgrep_output_v1_j.skipped_target list

(* TODO: This is used by semgrep-pro and not by semgrep. What is it?
   TODO: Explain what it does if xlang contains multiple langs. *)
val rules_for_xlang : Xlang.t -> Rule.t list -> Rule.t list
val xtarget_of_file : Runner_config.t -> Xlang.t -> Fpath.t -> Xtarget.t

(*
   Sort targets by decreasing size. This is meant for optimizing
   CPU usage when processing targets in parallel on a fixed number of cores.
*)
val sort_targets_by_decreasing_size :
  Input_to_core_t.target list -> Input_to_core_t.target list
