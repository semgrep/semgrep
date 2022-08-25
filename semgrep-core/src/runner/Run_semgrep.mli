(**********************************************************************************
 * # Notes
 *
 * ## semgrep CLI vs semgrep core
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
**********************************************************************************)

val semgrep_dispatch : Runner_config.t -> unit
(** Main entry point to the semgrep engine. This is called from Main.ml *)

(* engine functions used in tests or semgrep-core variants *)

val semgrep_with_one_pattern : Runner_config.t -> unit
(** this is the function used when running semgrep with -e or -f *)

val semgrep_with_rules_and_formatted_output : Runner_config.t -> unit
(** [semgrep_with_rules_and_formatted_output config] calls
    [semgrep_with_raw_results_and_exn_handler] and
    format the results on stdout either in a JSON or Textual format
    (depending on the value in config.output_format)

    This is the function used when running semgrep with -rules.
*)

val semgrep_with_raw_results_and_exn_handler :
  Runner_config.t ->
  Exception.t option * Report.final_result * Common.filename list
(** [semgrep_with_raw_results_and_exn_handler config] runs the semgrep
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

(* utilities functions used in tests or semgrep-core variants *)

val replace_named_pipe_by_regular_file : Common.filename -> Common.filename
(**
   Copy named pipes created with <(echo 'foo') on the command line
   into a regular file to avoid illegal seeks when reporting match results
   or parsing errors.
   Any file coming from the command line should go through this so as to
   allows easy manual testing.
*)

val print_match :
  ?str:string ->
  Runner_config.t ->
  Pattern_match.t ->
  (Metavariable.mvalue -> Parse_info.t list) ->
  unit

val exn_to_error : Common.filename -> Exception.t -> Semgrep_error_code.error
(**
  Small wrapper over Semgrep_error_code.exn_to_error to handle also
  semgrep-specific exns that have a position.
  See also JSON_report.json_of_exn for non-target related exn handling.
*)

val mk_rule_table :
  Rule.rule list -> Rule.rule_id list -> (int, Rule.rule) Hashtbl.t
(** Helper to create the table of rules to run for each file **)

val extract_targets_of_config :
  Runner_config.t ->
  Rule.rule_id list ->
  Rule.extract_rule list ->
  Input_to_core_t.target list
  * ( Common.filename,
      Report.partial_profiling Report.match_result ->
      Report.partial_profiling Report.match_result )
    Hashtbl.t
(**
   Generate the list of targets to run extract rules against given a config,
   the ids for rules to run against the generated targets and a set of extract
   rules used to perform the extraction.
*)

val targets_of_config :
  Runner_config.t ->
  Rule.rule_id list ->
  Input_to_core_t.targets * Output_from_core_t.skipped_target list
(**
  Compute the set of targets, either by reading what was passed
  in -target, or by using Find_target.files_of_dirs_or_files.
 *)

val filter_files_with_too_many_matches_and_transform_as_timeout :
  int ->
  Pattern_match.t list ->
  Pattern_match.t list
  * Semgrep_error_code.error list
  * Output_from_core_j.skipped_target list

val rules_for_xlang : Xlang.t -> Rule.t list -> Rule.t list
val xtarget_of_file : Runner_config.t -> Xlang.t -> Common.filename -> Xtarget.t
