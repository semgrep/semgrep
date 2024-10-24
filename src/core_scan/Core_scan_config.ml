(* LATER: osemgrep: not needed after osemgrep migration done *)
type output_format =
  | Text
  (* In JSON mode, we might need to display intermediate '.' in the
   * output for pysemgrep to track progress as well as extra targets
   * found by extract-mode rules, hence the bool below.
   *)
  | Json of bool (* dots *)
  (* for osemgrep *)
  | NoOutput
[@@deriving show]

(*
   'Rule_file' is for the semgrep-core CLI.
   'Rules' is for osemgrep or when for some reason the rules had to be
    preparsed.
*)
type rule_source = Rule_file of Fpath.t | Rules of Rule.t list

(* old: was [@@deriving show] but when using --config p/default
 * the logs were getting too big
 *)
let pp_rule_source (fmt : Format.formatter) (x : rule_source) : unit =
  match x with
  | Rule_file x -> Format.fprintf fmt "Rule_file (%a)" Fpath.pp x
  | Rules xs ->
      (* TODO: we should use Scan_CLI max_log_list_entries
       * and Output.too_much_data, but hard to pass that in
       *)
      if List.length xs > 100 then
        Format.fprintf fmt "<TOO MANY RULES TO DISPLAY (%d)>" (List.length xs)
      else Format.fprintf fmt "Rules (%a)" Rule.pp_rules xs

(*
   'Target_file' is for the semgrep-core CLI which gets a list of
   paths as an explicit list rather than by discovering files by scanning
   folders recursively.
   'Targets' is used by osemgrep, which also takes care of identifying
   targets but doesn't have to put them in a file since we stay in the
   same process and we bypass the semgrep-core CLI.
*)
type target_source = Target_file of Fpath.t | Targets of Target.t list
[@@deriving show]

(* This is mostly the flags of the semgrep-core program.
 * LATER: should delete or merge with osemgrep Core_runner.conf
 *)
type t = {
  (* Main flags, input *)
  rule_source : rule_source;
  target_source : target_source;
  equivalences_file : Fpath.t option;
  (* output and result tweaking *)
  output_format : output_format;
  report_time : bool;
  matching_explanations : bool;
  strict : bool;
  (* respect or not the paths: directive in a rule. Useful to set to false
   * in a testing context as in `semgrep test`
   *)
  respect_rule_paths : bool;
  (* Hook to display match results incrementally, after a file has been fully
   * processed. Note that this hook run in a child process of Parmap
   * in Core_scan.scan(), so the hook should not rely on shared memory!
   * This is also now used in Runner_service.ml and Git_remote.ml.
   *)
  file_match_hook : (Fpath.t -> Core_result.matches_single_file -> unit) option;
  (* Limits *)
  (* maximum time to spend running a rule on a single file *)
  timeout : float;
  (* maximum number of rules that can timeout on a file *)
  timeout_threshold : int;
  max_memory_mb : int;
  max_match_per_file : int;
  ncores : int;
  (* a.k.a -fast (on by default) *)
  filter_irrelevant_rules : bool;
  (* telemetry *)
  tracing : Tracing.config option;
}
[@@deriving show]

(*
   Default values for all the semgrep-core command-line arguments and options.

   Its values can be inherited using the 'with' syntax:

    let my_config = {
      Runner_config.default with
      debug = true;
      ncores = 3;
    }
*)
let default =
  {
    (* Main flags *)
    rule_source = Rules [];
    target_source = Targets [];
    equivalences_file = None;
    (* alt: NoOutput but then would need a -text in Core_CLI.ml *)
    output_format = Text;
    report_time = false;
    matching_explanations = false;
    strict = false;
    respect_rule_paths = true;
    file_match_hook = None;
    (* Limits *)
    (* maximum time to spend running a rule on a single file *)
    timeout = 0.;
    (* maximum number of rules that can timeout on a file *)
    timeout_threshold = 0;
    max_memory_mb = 0;
    max_match_per_file = 10_000;
    ncores = 1;
    (* a.k.a -fast, on by default *)
    filter_irrelevant_rules = true;
    (* debugging and telemetry flags *)
    tracing = None;
  }
