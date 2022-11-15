(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* There is currently no 'semgrep validate' subcommand. Rule validations are run via
 * 'semgrep scan --validate ...' but internally it's quite similar to
 * a subcommand.
 *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* LATER: at some point we may want a Validate_CLI.conf instead of
 * abusing Scan_CLI.conf *)
let run (conf : Scan_CLI.conf) : Exit_code.t =
  let rules_and_origin = Config_resolver.rules_from_conf conf in
  let rules, errors =
    Config_resolver.partition_rules_and_errors rules_and_origin
  in
  let metacheck_errors =
    match conf.rules_source with
    | Scan_CLI.Configs _xs ->
        (* TODO: run metachecks on the config files
            try:
               metacheck_errors = CoreRunner(
                     jobs=jobs,
                        timeout=timeout,
                        max_memory=max_memory,
                        timeout_threshold=timeout_threshold,
                        optimizations=optimizations,
                    ).validate_configs(config)
              except SemgrepError as e:
                    metacheck_errors = [e]
            ...
            if config_errors:
              output_handler.handle_semgrep_errors(config_errors)
              output_handler.output({}, all_targets=set(), filtered_rules=[])
        *)
        []
    | Scan_CLI.Pattern _ -> []
  in
  let all_errors = errors @ metacheck_errors in
  (* was logger.info, but works without --verbose, so Logs.app better *)
  Logs.app (fun m ->
      m "Configuration is %s - found %d configuration error(s), and %d rule(s)."
        (if all_errors = [] then "valid" else "invalid")
        (List.length all_errors) (List.length rules));
  if all_errors <> [] (* was a raise SemgrepError originally *) then
    Error.abort "Please fix the above errors and try again.";
  Exit_code.ok
