(******************************************************************************
 * Full result information
 *
 * In addition to the results (matches + errors), we also may report extra
 * information, such as the skipped targets or the profiling times. Many
 * of the types here are created to collect profiling information in as
 * well-typed a manner as possible. Creating a type for practically every
 * stage that reports matches is annoying, but prevents us from relying on
 * dummy values or unlabeled tuples.
 *
 * Another challenge we face is that the extra information can take a lot
 * of memory, which scales with both the number of rules and files. On
 * large repos, this is the most significant factor driving semgrep's
 * memory consumption. Therefore, if the skipped targets (for example) are
 * not being used, we don't want to save them. On the other hand, we want
 * to feel confident in the correctness of the code and make it easy to
 * know what is or isn't being saved. And we want our code to be relatively
 * readable.
 *
 * The debug_info type attempts to solve this. It has a variant for each
 * verbosity mode semgrep-core may be invoked with, which contains all
 * the fields semgrep requests from semgrep-core in that mode. Each result
 * contains debug_info in addition to the always-reported fields. The
 * variant of debug_info used in the results is always determined either
 * by the mode, which is a global set in Main.ml after the arguments are
 * read, or by a previous result. In this way we ensure that the fields
 * stored in the result are determined by the arguments passed by the user.
 *
 * Alternatives considered to the extra field:
 * - storing the information but just ommitting it in the final result (I
 *   tried this but it still uses too much memory)
 * - using option types within the result types for field and a global
 *   config to decide which fields are in use
 *
 * I considered the latter, but I'm not a fan of mix-and-match option types.
 * Maybe I just think it makes it tempting to use map_option, where here
 * the assumption that is being made feels more obvious. It also makes it
 * extra clear what information is being used for each mode and encourages
 * us to do the work of deciding what should be included instead of giving
 * the user choice that they probably don't know how to make intelligently.
 * That being said, if we wanted users to be able to "mix-and-match" request
 * fields, we should move to the option-types paradigm. Also, the collate
 * functions are quite ugly. I'm open to argument.
 *
 * TODO: We could use atdgen to specify those to factorize type definitions
 * that have to be present anyway in Output_from_core.atd.
 *****************************************************************************)

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Options for what extra debugging information to output.
   These are generally memory intensive fields that aren't strictly needed *)
(*****************************************************************************)

(* Coupling: the debug_info variant of each result record should always
      be the same as the mode's variant *)
type debug_mode = MDebug | MTime | MNo_info [@@deriving show]

type 'a debug_info =
  (* -debug: save all the information that could be useful *)
  | Debug of {
      skipped_targets : Output_from_core_t.skipped_target list;
      profiling : 'a;
    }
  (* -json_time: save just profiling information; currently our metrics record this *)
  | Time of { profiling : 'a }
  (* save nothing else *)
  | No_info
[@@deriving show]

let mode = ref MNo_info

(*****************************************************************************)
(* Different formats for profiling information as we have access to more data *)
(*****************************************************************************)

(* Save time information as we run each rule *)

type rule_profiling = {
  rule_id : Rule.rule_id;
  parse_time : float;
  match_time : float;
}
[@@deriving show]

type times = { parse_time : float; match_time : float }

(* Save time information as we run each file *)

type file_profiling = {
  file : Common.filename;
  rule_times : rule_profiling list;
  run_time : float;
}
[@@deriving show]

type partial_profiling = {
  file : Common.filename;
  rule_times : rule_profiling list;
}
[@@deriving show]

(* Result object for the entire rule *)

type final_profiling = {
  rules : Rule.rule list;
  rules_parse_time : float;
  file_times : file_profiling list;
  (* This is meant to represent the maximum amount of memory used by
     Semgrep during the course of its execution.

     This is useful to emit with the other profiling data for telemetry
     purposes, particuarly as it relates to measuring memory management
     with DeepSemgrep.

     It's not important that this number be incredibly precise, but
     measuring general trends is useful for ascertaining our memory
     usage.
  *)
  max_memory_bytes : int;
}
[@@deriving show]

type rule_id_and_engine_kind = Rule.ID.t * Pattern_match.engine_kind
[@@deriving show]

type final_result = {
  matches : Pattern_match.t list;
  errors : Semgrep_error_code.error list;
  skipped_rules : Rule.invalid_rule_error list;
  extra : final_profiling debug_info;
  explanations : Matching_explanation.t list;
  rules_by_engine : rule_id_and_engine_kind list;
}
[@@deriving show]

(* For each file, substitute in the profiling type we have *)

module ErrorSet = Set.Make (struct
  type t = Semgrep_error_code.error

  let compare = compare
end)

type 'a match_result = {
  matches : Pattern_match.t list;
  errors : ErrorSet.t;
      [@printer
        fun fmt errors ->
          fprintf fmt "{ ";
          ErrorSet.iter
            (fun error ->
              fprintf fmt "%s, " (Semgrep_error_code.show_error error))
            errors;
          fprintf fmt "}"]
  extra : 'a debug_info;
  explanations : Matching_explanation.t list;
}
[@@deriving show]

(*****************************************************************************)
(* Create empty versions of profiling/results objects *)
(*****************************************************************************)

let empty_partial_profiling file = { file; rule_times = [] }
let empty_file_profiling = { file = ""; rule_times = []; run_time = 0.0 }

let empty_rule_profiling rule =
  { rule_id = fst rule.Rule.id; parse_time = 0.0; match_time = 0.0 }

let empty_times_profiling = { parse_time = 0.0; match_time = 0.0 }

let empty_extra profiling =
  match !mode with
  | MDebug -> Debug { skipped_targets = []; profiling }
  | MTime -> Time { profiling }
  | MNo_info -> No_info

let empty_semgrep_result =
  {
    matches = [];
    errors = ErrorSet.empty;
    extra = empty_extra empty_times_profiling;
    explanations = [];
  }

let empty_final_result =
  {
    matches = [];
    errors = [];
    skipped_rules = [];
    extra = No_info;
    explanations = [];
    rules_by_engine = [];
  }

(*****************************************************************************)
(* Helpful functions *)
(*****************************************************************************)

(* Create a match result *)
let make_match_result matches errors profiling =
  let extra =
    match !mode with
    | MDebug -> Debug { skipped_targets = []; profiling }
    | MTime -> Time { profiling }
    | MNo_info -> No_info
  in
  { matches; errors; extra; explanations = [] }

(* Augment reported information with additional info *)

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
    errors = List.fold_left ErrorSet.union ErrorSet.empty errors;
    extra = final_extra skipped_targets profiling;
    explanations = List.flatten explanations;
  }

(* Aggregate a list of pattern results into one result *)
let collate_pattern_results results =
  let init_extra = ([], { parse_time = 0.0; match_time = 0.0 }) in

  let unzip_profiling { match_time; parse_time }
      { match_time = all_match_time; parse_time = all_parse_time } =
    {
      match_time = match_time +. all_match_time;
      parse_time = parse_time +. all_parse_time;
    }
  in

  let unzip_extra extra all_skipped_targets all_profiling =
    (* should match mode *)
    match extra with
    | Debug { skipped_targets; profiling } ->
        ( skipped_targets :: all_skipped_targets,
          unzip_profiling profiling all_profiling )
    | Time { profiling } ->
        (all_skipped_targets, unzip_profiling profiling all_profiling)
    | No_info -> (all_skipped_targets, all_profiling)
  in

  let base_case_extra all_skipped_targets all_profiling =
    (List.rev all_skipped_targets, all_profiling)
  in

  let final_extra skipped_targets profiling =
    match !mode with
    | MDebug ->
        Debug { skipped_targets = List.flatten skipped_targets; profiling }
    | MTime -> Time { profiling }
    | MNo_info -> No_info
  in

  collate_results init_extra unzip_extra base_case_extra final_extra results

(* Aggregate a list of rule results into one result for the target *)
let collate_rule_results :
    string -> rule_profiling match_result list -> partial_profiling match_result
    =
 fun file results ->
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

(* Aggregate a list of target results into one final result *)
let make_final_result results
    (rules_with_engine : (Rule.t * Pattern_match.engine_kind) list)
    ~rules_parse_time =
  let matches = results |> List.concat_map (fun x -> x.matches) in
  let errors =
    results |> List.concat_map (fun x -> x.errors |> ErrorSet.elements)
  in
  let explanations = results |> List.concat_map (fun x -> x.explanations) in
  let final_rules =
    Common.map (fun (r, ek) -> (fst r.Rule.id, ek)) rules_with_engine
  in

  (* Create extra *)
  let get_skipped_targets result =
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
    skipped_rules = [];
    explanations;
    rules_by_engine = final_rules;
  }
