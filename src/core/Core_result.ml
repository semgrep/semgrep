(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* (Core) Scan result information.
 *
 * In addition to the results (matches + errors), we may also report extra
 * information, such as the skipped targets or the profiling times.
 *
 * Yet another way to store matches/findings.
 * Just like for Core_error.ml, "core" results are translated at some point in
 * Semgrep_output_v1.core_output, then processed in pysemgrep (or osemgrep)
 * and translated again in Semgrep_output_v1.cli_output.
 * There's also Core_runner.result in osemgrep.
 *
 * From the simplest matches to the most complex we have:
 * Pattern_match.t (and its alias Rule_match.t)
 * -> Core_result.xxx (this file)
 * -> Semgrep_output_v1.core_output
 *  -> Core_runner.result
 *  -> Semgrep_output_v1.cli_output
 *  -> Semgrep_output_v1.findings
 * LATER: it would be good to remove some intermediate types.
 *)
module E = Core_error
open Core_profiling

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = {
  matches : Pattern_match.t list;
  errors : Core_error.t list;
  skipped_rules : Rule.invalid_rule_error list;
  (* may contain also skipped_target information *)
  extra : Core_profiling.t Core_profiling.debug_info;
  explanations : Matching_explanation.t list;
  rules_by_engine : (Rule_ID.t * Pattern_match.engine_kind) list;
  scanned : Fpath.t list;
}
[@@deriving show]

(* For each file, substitute in the profiling type we have *)

type 'a match_result = {
  matches : Pattern_match.t list;
  errors : E.ErrorSet.t;
      [@printer
        fun fmt errors ->
          fprintf fmt "{ ";
          E.ErrorSet.iter
            (fun error -> fprintf fmt "%s, " (Core_error.show error))
            errors;
          fprintf fmt "}"]
  extra : 'a Core_profiling.debug_info;
  explanations : Matching_explanation.t list;
}
[@@deriving show]

(* TODO: change in result_or_exn *)
type result_and_exn = Exception.t option * t

(*****************************************************************************)
(* Builders *)
(*****************************************************************************)

let empty_times_profiling = { parse_time = 0.0; match_time = 0.0 }

(* TODO: should get rid of that *)
let empty_file_profiling =
  { file = Fpath.v "TODO.fake_file"; rule_times = []; run_time = 0.0 }

let empty_match_result : Core_profiling.times match_result =
  {
    matches = [];
    errors = E.ErrorSet.empty;
    extra = empty_extra empty_times_profiling;
    explanations = [];
  }

let empty_final_result : t =
  {
    matches = [];
    errors = [];
    skipped_rules = [];
    extra = No_info;
    explanations = [];
    rules_by_engine = [];
    scanned = [];
  }

(* Create a match result *)
let make_match_result matches errors profiling =
  let extra =
    match !mode with
    | MDebug -> Debug { skipped_targets = []; profiling }
    | MTime -> Time { profiling }
    | MNo_info -> No_info
  in
  { matches; errors; extra; explanations = [] }

(*****************************************************************************)
(* Augment reported information with profiling info *)
(*****************************************************************************)

let modify_match_result_profiling result f =
  let extra =
    (* should match mode *)
    match result.extra with
    | Debug { skipped_targets; profiling } ->
        Debug { skipped_targets; profiling = f profiling }
    | Time { profiling } -> Time { profiling = f profiling }
    | No_info -> No_info
  in
  { result with extra }

let add_run_time :
    float -> partial_profiling match_result -> file_profiling match_result =
 fun run_time match_result ->
  modify_match_result_profiling match_result (fun { file; rule_times } ->
      { file; rule_times; run_time })

let add_rule : Rule.rule -> times match_result -> rule_profiling match_result =
 fun rule match_result ->
  modify_match_result_profiling match_result (fun { parse_time; match_time } ->
      { rule_id = fst rule.Rule.id; parse_time; match_time })

(*****************************************************************************)
(* Aggregate *)
(*****************************************************************************)

(* Helper to aggregate the shared parts of results *)
let collate_results init_extra unzip_extra base_case_extra final_extra results =
  let unzip_results l =
    let rec unzip all_matches all_errors (all_skipped_targets, all_profiling)
        all_explanations = function
      | { matches; errors; extra; explanations } :: l ->
          unzip (matches :: all_matches) (errors :: all_errors)
            (unzip_extra extra all_skipped_targets all_profiling)
            (explanations :: all_explanations)
            l
      | [] ->
          ( List.rev all_matches,
            List.rev all_errors,
            base_case_extra all_skipped_targets all_profiling,
            List.rev all_explanations )
    in
    unzip [] [] init_extra [] l
  in
  let matches, errors, (skipped_targets, profiling), explanations =
    unzip_results results
  in
  {
    matches = List.flatten matches;
    (* We deduplicate errors here to avoid repeat PartialParsing errors
       which can arise when multiple rules generate the same error. This is
       done for consistency with other parsing errors, like ParseError or
       LexicalError, which are only reported once per file, not rule.

       See also the note in semgrep_output_v1.atd.
    *)
    errors = List.fold_left E.ErrorSet.union E.ErrorSet.empty errors;
    extra = final_extra skipped_targets profiling;
    explanations = List.flatten explanations;
  }

(* Aggregate a list of pattern results into one result *)
let collate_pattern_results (results : Core_profiling.times match_result list) :
    Core_profiling.times match_result =
  let init_extra = ([], { parse_time = 0.0; match_time = 0.0 }) in

  let unzip_profiling (a : Core_profiling.times) (b : Core_profiling.times) =
    let ({ match_time; parse_time } : Core_profiling.times) = a in
    let ({ match_time = all_match_time; parse_time = all_parse_time }
          : Core_profiling.times) =
      b
    in
    {
      match_time = match_time +. all_match_time;
      parse_time = parse_time +. all_parse_time;
    }
  in

  let unzip_extra extra all_skipped_targets all_profiling =
    (* should match mode *)
    match extra with
    | Core_profiling.Debug { skipped_targets; profiling } ->
        ( skipped_targets :: all_skipped_targets,
          unzip_profiling profiling all_profiling )
    | Core_profiling.Time { profiling } ->
        (all_skipped_targets, unzip_profiling profiling all_profiling)
    | Core_profiling.No_info -> (all_skipped_targets, all_profiling)
  in

  let base_case_extra all_skipped_targets all_profiling =
    (List.rev all_skipped_targets, all_profiling)
  in

  let final_extra skipped_targets profiling =
    match !mode with
    | Core_profiling.MDebug ->
        Core_profiling.Debug
          { skipped_targets = List.flatten skipped_targets; profiling }
    | Core_profiling.MTime -> Core_profiling.Time { profiling }
    | Core_profiling.MNo_info -> Core_profiling.No_info
  in

  collate_results init_extra unzip_extra base_case_extra final_extra results

(* Aggregate a list of rule results into one result for the target *)
let collate_rule_results (file : Fpath.t)
    (results : rule_profiling match_result list) :
    partial_profiling match_result =
  let init_extra = ([], []) in

  let unzip_extra extra all_skipped_targets all_profiling =
    match extra with
    | Debug { skipped_targets; profiling } ->
        (skipped_targets :: all_skipped_targets, profiling :: all_profiling)
    | Time { profiling } -> (all_skipped_targets, profiling :: all_profiling)
    | No_info -> (all_skipped_targets, all_profiling)
  in

  let base_case_extra all_skipped_targets all_profiling =
    (List.rev all_skipped_targets, List.rev all_profiling)
  in

  let final_extra skipped_targets profiling =
    match !mode with
    | MDebug ->
        Debug
          {
            skipped_targets = List.flatten skipped_targets;
            profiling = { file; rule_times = profiling };
          }
    | MTime -> Time { profiling = { file; rule_times = profiling } }
    | MNo_info -> No_info
  in

  collate_results init_extra unzip_extra base_case_extra final_extra results

(*****************************************************************************)
(* Final result *)
(*****************************************************************************)

(* Aggregate a list of target results into one final result *)
let make_final_result
    (results : Core_profiling.file_profiling match_result list)
    (rules_with_engine : (Rule.t * Pattern_match.engine_kind) list)
    (skipped_rules : Rule.invalid_rule_error list) (scanned : Fpath.t list)
    ~rules_parse_time =
  (* contenating information from the match_result list *)
  let matches = results |> List.concat_map (fun x -> x.matches) in
  let explanations = results |> List.concat_map (fun x -> x.explanations) in
  let errors =
    results |> List.concat_map (fun x -> x.errors |> E.ErrorSet.elements)
  in

  (* Create extra *)
  let get_skipped_targets result =
    (* TODO? shouldn't we always compute the skipped_target? *)
    match result.extra with
    | Debug { skipped_targets; profiling = _profiling } -> skipped_targets
    | Time _profiling -> []
    | No_info -> []
  in
  let get_profiling result =
    match result.extra with
    | Debug { skipped_targets = _skipped_targets; profiling } -> profiling
    | Time { profiling } -> profiling
    | No_info ->
        logger#debug
          "Mismatch between mode and result while creating final result";
        empty_file_profiling
  in
  let extra =
    let mk_profiling () =
      let file_times = results |> Common.map get_profiling in
      {
        rules = Common.map fst rules_with_engine;
        rules_parse_time;
        file_times;
        (* Notably, using the `top_heap_words` does not measure cumulative
           memory usage across concurrent processes, meaning that if the most
           amount of memory is consumed by forked processes, we would need to
           multiply by the numbrer of processes to estimate the true maximum.
        *)
        max_memory_bytes = (Gc.quick_stat ()).top_heap_words * Sys.word_size;
      }
    in
    match !mode with
    | MDebug ->
        let skipped_targets =
          results |> List.concat_map (fun x -> get_skipped_targets x)
        in
        Debug { skipped_targets; profiling = mk_profiling () }
    | MTime -> Time { profiling = mk_profiling () }
    | MNo_info -> No_info
  in
  {
    matches;
    errors;
    extra;
    skipped_rules;
    explanations;
    rules_by_engine =
      rules_with_engine |> Common.map (fun (r, ek) -> (fst r.Rule.id, ek));
    scanned;
  }
