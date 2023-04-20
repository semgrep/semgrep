(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-scan command, execute it and exit.

   Translated mainly from scan.py, with parts translated also
   from semgrep_main.py and core_runner.py.
*)

open File.Operators
module Out = Semgrep_output_v1_t

(*****************************************************************************)
(* Logging/Profiling/Debugging *)
(*****************************************************************************)

(* ugly: also partially done in CLI.ml *)
let setup_logging (conf : Scan_CLI.conf) =
  (* For osemgrep we use the Logs library instead of the Logger
   * library in pfff. We had a few issues with Logger (which is a small
   * wrapper around the easy_logging library), and we don't really want
   * the logging in semgrep-core to interfere with the proper
   * logging/output we want in osemgrep, so this is a good opportunity
   * to evaluate a new logging library.
   *)
  Logs_helpers.setup_logging ~force_color:conf.force_color
    ~level:conf.logging_level;
  (* TOPORT
        # Setup file logging
        # env.user_log_file dir must exist
        env.user_log_file.parent.mkdir(parents=True, exist_ok=True)
        file_handler = logging.FileHandler(env.user_log_file, "w")
        file_formatter = logging.Formatter(
            "%(asctime)s - %(name)s - %(levelname)s - %(message)s"
        )
        file_handler.setLevel(logging.DEBUG)
        file_handler.setFormatter(file_formatter)
        logger.addHandler(file_handler)
  *)
  Logs.debug (fun m -> m "Logging setup for semgrep scan");
  Logs.debug (fun m -> m "Semgrep version: %s" Version.version);
  Logs.debug (fun m ->
      m "Executed as: %s" (Sys.argv |> Array.to_list |> String.concat " "));

  (* Easy_logging setup. We should avoid to use Logger in osemgrep/
   * and use Logs instead, but it is still useful to get the semgrep-core
   * logging information at runtime, hence this call.
   *)
  let debug =
    match conf.logging_level with
    | Some Logs.Debug -> true
    | _else_ -> false
  in
  Logging_helpers.setup ~debug
    ~log_config_file:(Fpath.v "log_config.json")
    ~log_to_file:None;
  ()

(* ugly: also partially done in CLI.ml *)
let setup_profiling (conf : Scan_CLI.conf) =
  (* TOADAPT
      if config.debug then Report.mode := MDebug
      else if config.report_time then Report.mode := MTime
      else Report.mode := MNo_info;
  *)
  if conf.profile then (
    (* ugly: no need to set Common.profile, this was done in CLI.ml *)
    Logs.debug (fun m -> m "Profile mode On");
    Logs.debug (fun m -> m "disabling -j when in profiling mode");
    { conf with core_runner_conf = { conf.core_runner_conf with num_jobs = 1 } })
  else conf

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

(* python: this used to be done in a _final_raise method from output.py
 * but better separation of concern to do it here.
 *)
let exit_code_of_errors ~strict (errors : Out.core_error list) : Exit_code.t =
  match List.rev errors with
  | [] -> Exit_code.ok
  (* TODO? why do we look at the last error? What about the other errors? *)
  | x :: _ -> (
      (* alt: raise a Semgrep_error that would be catched by CLI_Common
       * wrapper instead of returning an exit code directly? *)
      match () with
      | _ when x.severity = Out.Error ->
          Cli_json_output.exit_code_of_error_type x.error_type
      | _ when strict -> Cli_json_output.exit_code_of_error_type x.error_type
      | _else_ -> Exit_code.ok)

let exit_code_of_cli_errors (errors : Out.cli_error list) : Exit_code.t =
  match List.rev errors with
  | [] -> Exit_code.ok
  | x :: _ -> Exit_code.of_int x.Out.code

(*****************************************************************************)
(* Scan Summary and Skipped output *)
(*****************************************************************************)

let errors_to_skipped (errors : Out.core_error list) : Out.skipped_target list =
  errors
  |> Common.map (fun Out.{ location; message; rule_id; _ } ->
         Out.
           {
             path = location.path;
             reason = Analysis_failed_parser_or_internal_error;
             details = message;
             rule_id;
           })

let analyze_skipped (skipped : Out.skipped_target list) =
  let groups =
    Common.group_by
      (fun (Out.{ reason; _ } : Out.skipped_target) ->
        match reason with
        | Out.Gitignore_patterns_match
        | Semgrepignore_patterns_match ->
            `Semgrepignore
        | Too_big
        | Exceeded_size_limit ->
            `Size
        | Cli_include_flags_do_not_match -> `Include
        | Cli_exclude_flags_match -> `Exclude
        | Always_skipped
        | Analysis_failed_parser_or_internal_error
        | Excluded_by_config
        | Wrong_language
        | Minified
        | Binary
        | Irrelevant_rule
        | Too_many_matches ->
            `Other)
      skipped
  in
  ( (try List.assoc `Semgrepignore groups with
    | Not_found -> []),
    (try List.assoc `Include groups with
    | Not_found -> []),
    (try List.assoc `Exclude groups with
    | Not_found -> []),
    (try List.assoc `Size groups with
    | Not_found -> []),
    try List.assoc `Other groups with
    | Not_found -> [] )

let pp_summary ppf
    (( _respect_git_ignore,
       max_target_bytes,
       semgrep_ignored,
       include_ignored,
       exclude_ignored,
       file_size_ignored,
       other_ignored,
       errors ) :
      bool
      * int
      * Out.skipped_target list
      * Out.skipped_target list
      * Out.skipped_target list
      * Out.skipped_target list
      * Out.skipped_target list
      * Out.skipped_target list) =
  Fmt_helpers.pp_heading ppf "Scan summary";
  (* TODO
        if self.target_manager.baseline_handler:
            limited_fragments.append(
                "Scan was limited to files changed since baseline commit."
            )
  *)
  (* TODO
        elif self.target_manager.respect_git_ignore:
            # Each target could be a git repo, and we respect the git ignore
            # of each target, so to be accurate with this print statement we
            # need to check if any target is a git repo and not just the cwd
            targets_not_in_git = 0
            dir_targets = 0
            for t in self.target_manager.targets:
                if t.path.is_dir():
                    dir_targets += 1
                    try:
                        t.files_from_git_ls()
                    except (subprocess.SubprocessError, FileNotFoundError):
                        targets_not_in_git += 1
                        continue
            if targets_not_in_git != dir_targets:
                limited_fragments.append(f"Scan was limited to files tracked by git.")
  *)
  let opt_msg msg = function
    | [] -> None
    | xs -> Some (string_of_int (List.length xs) ^ " " ^ msg)
  in
  let out_skipped =
    let mb = string_of_int Stdlib.(max_target_bytes / 1000 / 1000) in
    Common.map_filter Fun.id
      [
        opt_msg "files not matching --include patterns" include_ignored;
        opt_msg "files matching --exclude patterns" exclude_ignored;
        opt_msg ("files larger than " ^ mb ^ " MB") file_size_ignored;
        opt_msg "files matching .semgrepignore patterns" semgrep_ignored;
        opt_msg "other files ignored" other_ignored;
      ]
  in
  let out_partial =
    opt_msg
      "files only partially analyzed due to a parsing or internal Semgrep error"
      errors
  in
  match (out_skipped, out_partial) with
  | [], None -> ()
  | xs, parts ->
      (* TODO if limited_fragments:
              for fragment in limited_fragments:
                  message += f"\n  {fragment}" *)
      Fmt.pf ppf "Some files were skipped or only partially analyzed.@.";
      Option.iter (fun txt -> Fmt.pf ppf "  Partially scanned: %s@." txt) parts;
      (match xs with
      | [] -> ()
      | xs ->
          Fmt.pf ppf "  Scan skipped: %s@." (String.concat ", " xs);
          Fmt.pf ppf
            "  For a full list of skipped files, run semgrep with the \
             --verbose flag.@.");
      Fmt.pf ppf "@."

let pp_skipped ppf
    (( respect_git_ignore,
       max_target_bytes,
       semgrep_ignored,
       include_ignored,
       exclude_ignored,
       file_size_ignored,
       other_ignored,
       errors ) :
      bool
      * int
      * Out.skipped_target list
      * Out.skipped_target list
      * Out.skipped_target list
      * Out.skipped_target list
      * Out.skipped_target list
      * Out.skipped_target list) =
  Fmt.pf ppf "%s@.Files skipped:@.%s@." (String.make 40 '=')
    (String.make 40 '=');
  Fmt_helpers.pp_heading ppf "Files skipped";
  (* TODO: always skipped *)
  Fmt.pf ppf " %a@." Fmt.(styled `Bold string) "Skipped by .gitignore:";
  if respect_git_ignore then (
    Fmt.pf ppf " %a@.@."
      Fmt.(styled `Bold string)
      "(Disable by passing --no-git-ignore)";
    Fmt.pf ppf "  • <all files not listed by `git ls-files` were skipped>@.")
  else (
    Fmt.pf ppf " %a@.@."
      Fmt.(styled `Bold string)
      "(Disabled with --no-git-ignore)";
    Fmt.pf ppf "  • <none>@.");
  Fmt.pf ppf "@.";

  let pp_list (xs : Out.skipped_target list) =
    match xs with
    | [] -> Fmt.pf ppf "  • <none>@."
    | xs ->
        List.iter
          (fun (Out.{ path; _ } : Out.skipped_target) ->
            Fmt.pf ppf "  • %s@." path)
          (List.sort
             (fun (a : Out.skipped_target) (b : Out.skipped_target) ->
               String.compare a.path b.path)
             xs)
  in

  Fmt.pf ppf " %a@. %a@.@."
    Fmt.(styled `Bold string)
    "Skipped by .semgrepignore:"
    Fmt.(styled `Bold string)
    "(See: \
     https://semgrep.dev/docs/ignoring-files-folders-code/#understanding-semgrep-defaults)";
  pp_list semgrep_ignored;
  Fmt.pf ppf "@.";

  Fmt.pf ppf " %a@.@."
    Fmt.(styled `Bold string)
    "Skipped by --include patterns:";
  pp_list include_ignored;
  Fmt.pf ppf "@.";

  Fmt.pf ppf " %a@.@."
    Fmt.(styled `Bold string)
    "Skipped by --exclude patterns:";
  pp_list exclude_ignored;
  Fmt.pf ppf "@.";

  Fmt.pf ppf " %a@. %a@.@."
    Fmt.(styled `Bold string)
    ("Skipped by limiting to files smaller than "
    ^ string_of_int max_target_bytes
    ^ " bytes:")
    Fmt.(styled `Bold string)
    "(Adjust with the --max-target-bytes flag)";
  pp_list file_size_ignored;
  Fmt.pf ppf "@.";

  Fmt.pf ppf " %a@.@." Fmt.(styled `Bold string) "Skipped for other reasons:";
  pp_list other_ignored;
  Fmt.pf ppf "@.";

  Fmt.pf ppf " %a@.@."
    Fmt.(styled `Bold string)
    "Skipped by analysis failure due to parsing or internal Semgrep error";
  pp_list errors;
  Fmt.pf ppf "@."

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run (conf : Scan_CLI.conf) : Exit_code.t =
  setup_logging conf;
  (* return a new conf because can adjust conf.num_jobs (-j) *)
  let conf = setup_profiling conf in
  Logs.debug (fun m -> m "conf = %s" (Scan_CLI.show_conf conf));
  Metrics_.configure conf.metrics;

  match () with
  (* "alternate modes" where no search is performed.
   * coupling: if you add a new alternate mode, you probably need to modify
   * Scan_CLI.cmdline_term.combine.rules_source match cases and allow
   * more cases returning an empty 'Configs []'.
   * TODO? stricter: we should allow just one of those alternate modes.
   *)
  | _ when conf.version ->
      (* alt: we could use Common.pr, but because '--quiet' doc says
       * "Only output findings.", a version is not a finding so
       * we use Logs.app (which is filtered by --quiet).
       *)
      Logs.app (fun m -> m "%s" Version.version);
      (* TOPORT: if enable_version_check: version_check() *)
      Exit_code.ok
  | _ when conf.show_supported_languages ->
      Logs.app (fun m -> m "supported languages are: %s" Xlang.supported_xlangs);
      Exit_code.ok
  (* LATER: this should be real separate subcommands instead of abusing
   * semgrep scan flags
   *)
  | _ when conf.test <> None -> Test_subcommand.run (Common2.some conf.test)
  | _ when conf.validate <> None ->
      Validate_subcommand.run (Common2.some conf.validate)
  | _ when conf.dump <> None -> Dump_subcommand.run (Common2.some conf.dump)
  | _else_ -> (
      (* --------------------------------------------------------- *)
      (* Let's go *)
      (* --------------------------------------------------------- *)
      let rules_and_origins =
        Rule_fetching.rules_from_rules_source conf.rules_source
      in
      Logs.debug (fun m ->
          rules_and_origins
          |> List.iter (fun x ->
                 m "rules = %s" (Rule_fetching.show_rules_and_origin x)));
      let rules, errors =
        Rule_fetching.partition_rules_and_errors rules_and_origins
      in
      let filtered_rules =
        Rule_filtering.filter_rules conf.rule_filtering_conf rules
      in
      let pp_rule_sources ppf = function
        | Rule_fetching.Pattern _ -> Format.pp_print_string ppf "pattern"
        | Configs [ x ] -> Format.fprintf ppf "1 config %s" x
        | Configs xs -> Format.fprintf ppf "%d configs" (List.length xs)
      in
      Logs.info (fun m ->
          m "running %d rules from %a"
            (List.length filtered_rules)
            pp_rule_sources conf.rules_source);
      (* TODO should output whether .semgrepignore is found and used
         (as done in semgrep_main.py get_file_ignore()) *)
      Logs.info (fun m ->
          m "Rules:%s" "";
          let exp, normal =
            List.partition
              (fun rule -> rule.Rule.severity = Rule.Experiment)
              filtered_rules
          in
          let rule_id r = fst r.Rule.id in
          let sorted =
            List.sort (fun r1 r2 -> String.compare (rule_id r1) (rule_id r2))
          in
          List.iter (fun rule -> m "- %s" (rule_id rule)) (sorted normal);
          match exp with
          | [] -> ()
          | __non_empty__ ->
              m "Experimental rules:%s" "";
              List.iter (fun rule -> m "- %s" (rule_id rule)) (sorted exp));
      let targets, semgrepignored_targets =
        Find_target.get_targets conf.targeting_conf conf.target_roots
      in
      Logs.debug (fun m ->
          m "target roots: [%s" "";
          conf.target_roots |> List.iter (fun root -> m "  %s" !!root);
          m "]%s" "");
      Logs.debug (fun m ->
          m "skipped targets: [%s" "";
          semgrepignored_targets
          |> List.iter (fun x ->
                 m "  %s" (Output_from_core_t.show_skipped_target x));
          m "]%s" "");
      Logs.info (fun m ->
          semgrepignored_targets
          |> List.iter (fun (x : Output_from_core_t.skipped_target) ->
                 m "Ignoring %s due to %s (%s)" x.Output_from_core_t.path
                   (Output_from_core_t.show_skip_reason
                      x.Output_from_core_t.reason)
                   x.Output_from_core_t.details));
      Logs.debug (fun m ->
          m "selected targets: [%s" "";
          targets
          |> List.iter (fun file -> m "target = %s" (Fpath.to_string file));
          m "]%s" "");
      let (res : Core_runner.result) =
        Core_runner.invoke_semgrep_core
          ~respect_git_ignore:conf.targeting_conf.respect_git_ignore
          conf.core_runner_conf filtered_rules errors targets
      in

      let errors_skipped = errors_to_skipped res.core.errors in
      let semgrepignored, included, excluded, size, other_ignored =
        analyze_skipped semgrepignored_targets
      in
      let res =
        let skipped_targets =
          semgrepignored_targets @ errors_skipped @ res.core.skipped_targets
        in
        (* Add the targets that were semgrepignored or errorneous *)
        let core = { res.core with skipped_targets } in
        { res with core }
      in

      (* outputting the result! in JSON/Text/... depending on conf *)
      let cli_errors = Output.output_result conf res in

      Logs.info (fun m ->
          m "%a" pp_skipped
            ( conf.targeting_conf.respect_git_ignore,
              conf.targeting_conf.max_target_bytes,
              semgrepignored,
              included,
              excluded,
              size,
              other_ignored,
              errors_skipped ));
      Logs.app (fun m ->
          m "%a" pp_summary
            ( conf.targeting_conf.respect_git_ignore,
              conf.targeting_conf.max_target_bytes,
              semgrepignored,
              included,
              excluded,
              size,
              other_ignored,
              errors_skipped ));
      Logs.app (fun m ->
          m "Ran %d rules on %d files: %d findings@."
            (List.length filtered_rules)
            (List.length targets)
            (List.length res.core.matches));

      (* TOPORT? was in formater/base.py
         def keep_ignores(self) -> bool:
           """
           Return True if ignored findings should be passed to this formatter;
           False otherwise.
           Ignored findings can still be distinguished using their _is_ignore property.
           """
           return False
      *)
      match cli_errors with
      | [] ->
          (* final result for the shell *)
          exit_code_of_errors ~strict:conf.strict res.core.errors
      | errors -> exit_code_of_cli_errors errors)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (argv : string array) : Exit_code.t =
  let conf = Scan_CLI.parse_argv argv in
  run conf
