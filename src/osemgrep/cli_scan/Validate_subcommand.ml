open Common
module Out = Semgrep_output_v1_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This module performs rule validation by checking whether a rule is
 * correct by detecting different kinds of errors:
 *
 *  (1) YAML syntax errors (e.g., unclosed quote). We detect those
 *    errors thanks to our YAML parser and Yaml_to_generic.ml
 *
 *  (2) Semgrep rule schema errors (e.g., missing 'message:' field). We
 *    detect those thanks to Parse_rule.ml
 *
 *  (3) "Logical" errors, by running Semgrep rules (also called "meta" rules)
 *     on those target rules. We run Semgrep on Semgrep! We detect
 *     those thanks to the 'p/semgrep-rule-lints' ruleset.
 *
 *  (4) TODO Other errors that can't be detected easily using Semgrep rules. We
 *    detect those thanks to Check_rule.ml.
 *
 * There is currently no 'semgrep validate' subcommand. Rule validation is
 * ran via 'semgrep scan --validate --config ...' but internally it's quite
 * similar to a subcommand.
 * LATER: at some point we probably want to make this an actual subcommand
 * (Brendon is in favor of it).
 *
 * LATER: merge with Check_rule.ml (we can remove lots of code in it).
 *
 * Ideally we should move this file in a separate ../metachecking/ folder,
 * but this file depends on cli_scan/ and cli_scan/ depends on this file
 * because it's called from Scan_subcommand.ml, so it must remain here.
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* a slice of Scan_CLI.conf *)
type conf = {
  (* Right now we use --config to get the list of rules for validate, but
   * positional arguments to pass after 'semgrep validate' would be more
   * appropriate (e.g., semgrep validate foo.yaml).
   * Do we allow to --validate --config p/python ?
   *)
  rules_source : Rules_source.t;
  core_runner_conf : Core_runner.conf;
  logging_level : Logs.level option;
}
[@@deriving show]

(* The "meta" rules are stored in the semgrep-rules public repository here:
 * https://github.com/returntocorp/semgrep-rules/tree/develop/yaml/semgrep
 *
 * The pack below is defined in the semgrep-rules-pack private repository here:
 * https://github.com/returntocorp/semgrep-rule-packs/blob/master/semgrep-rule-lints.json
 *)
let metarules_pack = "p/semgrep-rule-lints"

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let run (conf : conf) : Exit_code.t =
  let settings = Semgrep_settings.load () in
  let token_opt = settings.api_token in
  (* Checking (1) and (2). Parsing the rules is already a form of validation.
   * Before running metachecks on those rules, we make sure we can parse them.
   * TODO: report not only Rule.invalid_rule_errors but all Rule.error for (1)
   * in Config_resolver.errors.
   *)
  let rules_and_origin =
    Rule_fetching.rules_from_rules_source ~token_opt ~rewrite_rule_ids:true
      ~registry_caching:false conf.rules_source
  in
  let rules, errors =
    Rule_fetching.partition_rules_and_errors rules_and_origin
  in
  (* Checking (3) *)
  let metacheck_errors =
    match conf.rules_source with
    | Rules_source.Pattern _ -> []
    | Rules_source.Configs _xs ->
        (* In a validate context, rules are actually targets of metarules.
         * alt: could also process Configs to compute the targets.
         *)
        let targets =
          rules_and_origin
          |> Common.map_filter (fun x ->
                 (* TODO: stricter: warn if no origin (meaning URL or registry) *)
                 x.Rule_fetching.origin)
        in
        let metarules_and_origin =
          Rule_fetching.rules_from_dashdash_config ~token_opt
            ~registry_caching:false
            (Semgrep_dashdash_config.parse_config_string metarules_pack)
        in
        let metarules, metaerrors =
          Rule_fetching.partition_rules_and_errors metarules_and_origin
        in
        if metaerrors <> [] then
          Error.abort (spf "error in metachecks! please fix %s" metarules_pack);

        let res =
          Core_runner.invoke_semgrep_core conf.core_runner_conf metarules []
            targets
        in

        (* TODO? sanity check errors below too? *)
        let { Out.results; errors = _; _ } =
          Cli_json_output.cli_output_of_core_results
            ~logging_level:conf.logging_level res
        in
        (* TOPORT?
                ... run -check_rules in semgrep-core ...
                parsed_errors += [
                  core_error_to_semgrep_error(e) for e in core_output.errors
                ]
                return dedup_errors(parsed_errors)
             ...
             def dedup_errors(errors: List[SemgrepCoreError]) -> List[SemgrepCoreError]:
                return list({uniq_error_id(e): e for e in errors}.values())

           def uniq_error_id(
               error: SemgrepCoreError,
           ) -> Tuple[int, Path, core.Position, core.Position, str]:
               return (
                   error.code,
                   Path(error.core.location.path),
                   error.core.location.start,
                   error.core.location.end,
                   error.core.message,
               )
        *)
        (* metarules match results are actually metacheck errors *)
        results
  in

  (* TODO: checking (4) *)

  (* Summarizing findings (errors) *)
  let num_errors = List.length errors + List.length metacheck_errors in
  (* was logger.info, but works without --verbose, so Logs.app better *)
  Logs.app (fun m ->
      m "Configuration is %s - found %d configuration error(s), and %d rule(s)."
        (if num_errors =|= 0 then "valid" else "invalid")
        num_errors (List.length rules));
  (* coupling: with Check_rule.error and use of SemgrepMatchFound *)
  metacheck_errors
  |> List.iter (fun (x : Out.cli_match) ->
         Logs.err (fun m ->
             m "Semgrep match found at line %s:%d\n%s" x.path x.start.line
               x.extra.message));
  match num_errors with
  | 0 -> Exit_code.ok
  | _else_ ->
      (* was a raise SemgrepError originally *)
      Error.abort "Please fix the above errors and try again."
