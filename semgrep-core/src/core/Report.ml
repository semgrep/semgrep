(********************************************************************************
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
 ********************************************************************************)

let logger = Logging.get_logger [ __MODULE__ ]

(********************************************************************************)
(* Options for what extra debugging information to output.
   These are generally memory intensive fields that aren't strictly needed *)
(********************************************************************************)

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
  | Time of 'a
  (* save nothing else *)
  | No_info
[@@deriving show]

let mode = ref MNo_info

(********************************************************************************)
(* Different formats for profiling information as we have access to more data *)
(********************************************************************************)

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
}
[@@deriving show]

type final_result = {
  matches : Pattern_match.t list;
  errors : Semgrep_error_code.error list;
  skipped_rules : Rule.invalid_rule_error list;
  extra : final_profiling debug_info;
}
[@@deriving show]

(* For each file, substitute in the profiling type we have *)

type 'a match_result = {
  matches : Pattern_match.t list;
  errors : Semgrep_error_code.error list;
  extra : 'a debug_info;
}
[@@deriving show]

(********************************************************************************)
(* Create empty versions of profiling/results objects *)
(********************************************************************************)

let empty_partial_profiling file = { file; rule_times = [] }
let empty_file_profiling = { file = ""; rule_times = []; run_time = 0.0 }

let empty_rule_profiling rule =
  { rule_id = fst rule.Rule.id; parse_time = 0.0; match_time = 0.0 }

let empty_times_profiling = { parse_time = 0.0; match_time = 0.0 }

let empty_extra profiling =
  match !mode with
  | MDebug -> Debug { skipped_targets = []; profiling }
  | MTime -> Time profiling
  | MNo_info -> No_info

let empty_semgrep_result =
  { matches = []; errors = []; extra = empty_extra empty_times_profiling }

let empty_final_result =
  { matches = []; errors = []; skipped_rules = []; extra = No_info }

(********************************************************************************)
(* Helpful functions *)
(********************************************************************************)

(* Create a match result *)
let make_match_result matches errors profiling =
  let extra =
    match !mode with
    | MDebug -> Debug { skipped_targets = []; profiling }
    | MTime -> Time profiling
    | MNo_info -> No_info
  in
  { matches; errors; extra }

(* Augment reported information with additional info *)

let modify_match_result_profiling { matches; errors; extra } f =
  let extra =
    (* should match mode *)
    match extra with
    | Debug { skipped_targets; profiling } ->
        Debug { skipped_targets; profiling = f profiling }
    | Time profiling -> Time (f profiling)
    | No_info -> No_info
  in
  { matches; errors; extra }

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
    let rec unzip all_matches all_errors (all_skipped_targets, all_profiling) =
      function
      | { matches; errors; extra } :: l ->
          unzip (matches :: all_matches) (errors :: all_errors)
            (unzip_extra extra all_skipped_targets all_profiling)
            l
      | [] ->
          ( List.rev all_matches,
            List.rev all_errors,
            base_case_extra all_skipped_targets all_profiling )
    in
    unzip [] [] init_extra l
  in
  let matches, errors, (skipped_targets, profiling) = unzip_results results in
  {
    matches = List.flatten matches;
    errors = List.flatten errors;
    extra = final_extra skipped_targets profiling;
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
    | Time profiling ->
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
    | MTime -> Time profiling
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
    | Time profiling -> (all_skipped_targets, profiling :: all_profiling)
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
    | MTime -> Time { file; rule_times = profiling }
    | MNo_info -> No_info
  in

  collate_results init_extra unzip_extra base_case_extra final_extra results

(* Aggregate a list of target results into one final result *)
let make_final_result results rules ~rules_parse_time =
  let matches = results |> Common.map (fun x -> x.matches) |> List.flatten in
  let errors = results |> Common.map (fun x -> x.errors) |> List.flatten in

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
    | Time profiling -> profiling
    | No_info ->
        logger#debug
          "Mismatch between mode and result while creating final result";
        empty_file_profiling
  in
  let extra =
    let mk_profiling () =
      let file_times = results |> Common.map (fun x -> get_profiling x) in
      { rules; rules_parse_time; file_times }
    in
    match !mode with
    | MDebug ->
        let skipped_targets =
          results |> Common.map (fun x -> get_skipped_targets x) |> List.flatten
        in
        Debug { skipped_targets; profiling = mk_profiling () }
    | MTime -> Time (mk_profiling ())
    | MNo_info -> No_info
  in
  { matches; errors; extra; skipped_rules = [] }
