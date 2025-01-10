open Common
module Arg = Cmdliner.Arg
module Term = Cmdliner.Term
module Cmd = Cmdliner.Cmd
module H = Cmdliner_
module Show = Show_CLI
module C = CLI_common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 'semgrep scan' command-line arguments processing.
 *
 * Translated partially from scan.py
 *
 * coupling: https://semgrep.dev/docs/cli-reference#semgrep-scan-command-options
 * updated automatically by update-help-command.yml in the semgrep-docs repo
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(*
   The result of parsing a 'semgrep scan' command.

   LATER: we could actually define this structure in ATD, so people could
   programmatically set the command-line arguments they want if they
   want to programmatically call Semgrep. This structure could also
   be versioned so people can rely on a stable CLI "API".
*)
type conf = {
  (* Main configuration options *)
  (* mix of --pattern/--lang/--replacement, --config *)
  rules_source : Rules_source.t;
  (* the roots can be files, directories, or even symlinks *)
  target_roots : Scanning_root.t list;
  (* Rules/targets refinements *)
  rule_filtering_conf : Rule_filtering.conf;
  targeting_conf : Find_targets.conf;
  (* Other configuration options *)
  error_on_findings : bool;
  rewrite_rule_ids : bool;
  (* Engine selection *)
  engine_type : Engine_type.t;
  autofix : bool;
  (* Performance options *)
  core_runner_conf : Core_runner.conf;
  (* file or URL (None means output to stdout) *)
  output : string option;
  output_conf : Output.conf;
  incremental_output : bool;
  (* Networking options *)
  metrics : Metrics_.config;
  version_check : bool;
  (* Debugging/logging/profiling options *)
  common : CLI_common.conf;
  trace : bool;
  trace_endpoint : string option;
  (* Ugly: should be in separate subcommands *)
  version : bool;
  show : Show_CLI.conf option;
  validate : Validate_CLI.conf option;
  test : Test_CLI.conf option;
  allow_local_builds : bool;
  ls : bool;
  ls_format : Ls_subcommand.format;
}
[@@deriving show]

(* alt: we could split even more the content of this variable in different
 * files, but it's also nice to have almost everything in one place.
 *)
let default : conf =
  {
    rules_source = Configs [ "auto" ];
    target_roots = [ Scanning_root.of_string "." ];
    targeting_conf = Find_targets.default_conf;
    (* alt: could move in a Rule_filtering.default *)
    rule_filtering_conf =
      {
        Rule_filtering.exclude_rule_ids = [];
        severity = [];
        exclude_products = [];
      };
    autofix = false;
    (* alt: could move in a Core_runner.default *)
    core_runner_conf = Core_runner.default_conf;
    error_on_findings = false;
    (* could be move in CLI_common.default_conf? *)
    common =
      {
        profile = false;
        logging_level = Some Logs.Warning;
        maturity = Maturity.Default;
      };
    trace = false;
    trace_endpoint = None;
    engine_type = OSS;
    output = None;
    output_conf = Output.default;
    incremental_output = false;
    rewrite_rule_ids = true;
    (* will send metrics only if the user uses the registry or the app *)
    metrics = Metrics_.Auto;
    version_check = true;
    (* ugly: should be separate subcommands *)
    version = false;
    show = None;
    validate = None;
    test = None;
    allow_local_builds = false;
    ls = false;
    ls_format = Ls_subcommand.default_format;
  }

(*************************************************************************)
(* Command-line flags *)
(*************************************************************************)
(* The o_ below stands for option (as in command-line argument option) *)

(* ------------------------------------------------------------------ *)
(* Networking related options *)
(* ------------------------------------------------------------------ *)

let o_metrics : Metrics_.config Term.t =
  let info =
    Arg.info [ "metrics" ]
      ~env:(Cmd.Env.info "SEMGREP_SEND_METRICS")
      ~doc:
        {|Configures how usage metrics are sent to the Semgrep server. If
'auto', metrics are sent whenever the --config value pulls from the
Semgrep server. If 'on', metrics are always sent. If 'off', metrics
are disabled altogether and not sent. If absent, the
SEMGREP_SEND_METRICS environment variable value will be used. If no
environment variable, defaults to 'auto'.
|}
  in
  Arg.value (Arg.opt Metrics_.converter default.metrics info)

(* alt: was in "Performance and memory options" before *)
let o_version_check : bool Term.t =
  H.negatable_flag_with_env [ "enable-version-check" ]
    ~neg_options:[ "disable-version-check" ]
    ~default:default.version_check
    ~env:(Cmd.Env.info "SEMGREP_ENABLE_VERSION_CHECK")
    ~doc:
      {|Checks Semgrep servers to see if the latest version is run; disabling
 this may reduce exit time after returning results.
|}

(* ------------------------------------------------------------------ *)
(* Path options *)
(* ------------------------------------------------------------------ *)
(* TOPORT:
 * "By default, Semgrep scans all git-tracked files with extensions matching
 *  rules' languages. These options alter which files Semgrep scans."
 *)

let o_exclude : string list Term.t =
  let info =
    Arg.info [ "exclude" ] ~docv:"PATTERN"
      ~doc:
        (*
         Note that osemgrep also supports negated "de-ignore" patterns such
         as used in '--exclude=tests --exclude=!tests/main.c' to
         re-include tests/main.c.
         We're a bit evasive about this for now since pysemgrep and osemgrep
         differ in that respect.
      *)
        {|Skip any file or directory whose path that matches $(docv).
'--exclude=*.py' will ignore the following: 'foo.py', 'src/foo.py',
'foo.py/bar.sh'.
'--exclude=tests' will ignore 'tests/foo.py' as well as 'a/b/tests/c/foo.py'.
Multiple '--exclude' options may be specified.
$(docv) is a glob-style pattern that uses the same syntax as gitignore
and semgrepignore, which is documented at
https://git-scm.com/docs/gitignore#_pattern_format
|}
  in
  Arg.value (Arg.opt_all Arg.string [] info)

let o_include : string list Term.t =
  let info =
    Arg.info [ "include" ] ~docv:"PATTERN"
      ~doc:
        {|Specify files or directories that should be scanned by semgrep,
excluding other files.
This filter is applied after these other filters: '--exclude' options,
any filtering done by git (or other SCM), and filtering by '.semgrepignore'
files. Multiple '--include' options can be specified. A file path is selected
if it matches at least one of the include patterns.
$(docv) is a glob-style pattern such as 'foo.*' that
must match the path. For example,
specifying the language with '-l javascript' might preselect files
'src/foo.jsx' and 'lib/bar.js'. Specifying one of '--include=src',
'--include=*.jsx', or '--include=src/foo.*' will restrict the
selection to the single file 'src/foo.jsx'. A choice of multiple
'--include' patterns can be specified. For example, '--include=foo.*
--include=bar.*' will select both 'src/foo.jsx' and
'lib/bar.js'. Glob-style patterns follow the syntax supported by
gitignore and semgrepignore, which is documented at
https://git-scm.com/docs/gitignore#_pattern_format
|}
  in
  Arg.value (Arg.opt_all Arg.string [] info)

let o_max_target_bytes : int Term.t =
  let default = default.targeting_conf.max_target_bytes in
  let info =
    Arg.info [ "max-target-bytes" ]
      ~doc:
        (spf
           {|Maximum size for a file to be scanned by Semgrep, e.g
'1.5MB'. Any input program larger than this will be ignored. A zero or
negative value disables this filter. Defaults to %d bytes|}
           default)
  in

  Arg.value (Arg.opt Cmdliner_.number_of_bytes_converter default info)

(*
   TODO: deprecate this confusing option as soon as we have alternatives
   and we're migrated to osemgrep for file targeting.
*)
let o_use_git : bool Term.t =
  H.negatable_flag [ "use-git-ignore" ] ~neg_options:[ "no-git-ignore" ]
    ~default:default.targeting_conf.respect_gitignore
    ~doc:
      {|'--no-git-ignore' causes semgrep to not call 'git' and not consult
        '.gitignore' files to determine which files semgrep should scan.
        As a result of '--no-git-ignore', gitignored files and git submodules
        will be scanned.
        This flag has no effect if the scanning root is not
        in a git repository.
        '--use-git-ignore' is semgrep's default behavior.|}

let o_ignore_semgrepignore_files : bool Term.t =
  let info =
    Arg.info
      [ "x-ignore-semgrepignore-files" ]
      ~doc:
        {|[INTERNAL] Ignore all '.semgrepignore' files found in the project
tree for the purpose of selecting target files to be scanned by semgrep.
Other filters may still apply.
THIS OPTION IS NOT PART OF THE SEMGREP API AND MAY
CHANGE OR DISAPPEAR WITHOUT NOTICE.
|}
  in
  Arg.value (Arg.flag info)

let o_scan_unknown_extensions : bool Term.t =
  let default = default.targeting_conf.always_select_explicit_targets in
  H.negatable_flag
    [ "scan-unknown-extensions" ]
    ~neg_options:[ "skip-unknown-extensions" ]
    ~default
    ~doc:
      (spf
         {|If true, target files specified directly on the command line
will bypass normal language detection. They will be analyzed according to
the value of --lang if applicable, or otherwise with the analyzers/languages
specified in the Semgrep rule(s) regardless of file extension or file type.
This setting doesn't apply to target files discovered by scanning folders.
Defaults to %b.
|}
         default)

(* alt: could be put in the Display options with nosem *)
let o_baseline_commit : string option Term.t =
  let info =
    Arg.info [ "baseline-commit" ]
      ~doc:
        {|Only show results that are not found in this commit hash. Aborts run
if not currently in a git directory, there are unstaged changes, or
given baseline hash doesn't exist.
|}
      ~env:(Cmd.Env.info "SEMGREP_BASELINE_COMMIT")
    (* TOPORT: support also SEMGREP_BASELINE_REF; unfortunately cmdliner
             supports only one environment variable per option *)
  in
  Arg.value (Arg.opt Arg.(some string) None info)

let o_diff_depth : int Term.t =
  let info =
    Arg.info [ "diff-depth" ]
      ~doc:
        {|The depth of the Pro (interfile) differential scan, the number of
       steps (both in the caller and callee sides) from the targets in the
       call graph tracked by the deep preprocessor. Only applied in differential
       scan mode. Default to 2.
       |}
  in
  Arg.value (Arg.opt Arg.int default.targeting_conf.diff_depth info)

(* ------------------------------------------------------------------ *)
(* Performance and memory options *)
(* ------------------------------------------------------------------ *)

let o_num_jobs : int Term.t =
  let info =
    Arg.info [ "j"; "jobs" ]
      ~doc:
        {|Number of subprocesses to use to run checks in
parallel. Defaults to the number of cores detected on the system
(1 if using --pro).
|}
  in
  Arg.value (Arg.opt Arg.int default.core_runner_conf.num_jobs info)

let o_max_memory_mb : int Term.t =
  let default = default.core_runner_conf.max_memory_mb in
  let info =
    Arg.info [ "max-memory" ]
      ~doc:
        {|Maximum system memory in MiB to use during the interfile pre-processing
phase, or when running a rule on a single file. If set to 0, will
not have memory limit. Defaults to 0. For CI scans that use the Pro Engine,
defaults to 5000 MiB.
|}
  in
  Arg.value (Arg.opt Arg.int default info)

let o_optimizations : bool Term.t =
  let parse = function
    | "all" -> Ok true
    | "none" -> Ok false
    | other -> Error (spf "unsupported value %S" other)
  in
  let print fmt = function
    | true -> Format.pp_print_string fmt "all"
    | false -> Format.pp_print_string fmt "none"
  in
  let converter = Arg.conv' (parse, print) in
  let info =
    Arg.info [ "optimizations" ]
      ~doc:
        {|Turn on/off optimizations. Default = 'all'.
Use 'none' to turn all optimizations off.
|}
  in
  Arg.value (Arg.opt converter default.core_runner_conf.optimizations info)

let o_timeout : float Term.t =
  let default = default.core_runner_conf.timeout in
  let info =
    Arg.info [ "timeout" ]
      ~doc:
        (spf
           {|Maximum time to spend running a rule on a single file in
seconds. If set to 0 will not have time limit. Defaults to %.1f s.
|}
           default)
  in
  (*TOPORT: envvar="SEMGREP_TIMEOUT" *)
  Arg.value (Arg.opt Arg.float default info)

let o_timeout_threshold : int Term.t =
  let default = default.core_runner_conf.timeout_threshold in
  let info =
    Arg.info [ "timeout-threshold" ]
      ~doc:
        (spf
           {|Maximum number of rules that can time out on a file before
the file is skipped. If set to 0 will not have limit. Defaults to %d.
|}
           default)
  in
  Arg.value (Arg.opt Arg.int default info)

(* TODO: currently just used in pysemgrep and semgrep-core-proprietary *)
let o_timeout_interfile : int Term.t =
  let default = 0 in
  let info =
    Arg.info [ "interfile-timeout" ]
      ~doc:
        {|Maximum time to spend on interfile analysis. If set to 0 will not
have time limit. Defaults to 0 s for all CLI scans. For CI scans, it defaults
to 3 hours.|}
  in
  Arg.value (Arg.opt Arg.int default info)

(* ------------------------------------------------------------------ *)
(* Display options *)
(* ------------------------------------------------------------------ *)

(* alt: could use Fmt_cli.style_renderer, which supports --color=xxx but
 * better be backward compatible with how semgrep was doing it before
 *)
let o_force_color : bool Term.t =
  H.negatable_flag_with_env [ "force-color" ] ~neg_options:[ "no-force-color" ]
    ~default:default.output_conf.force_color
      (* TOPORT? need handle SEMGREP_COLOR_NO_COLOR or NO_COLOR
       * # https://no-color.org/
       *)
    ~env:(Cmd.Env.info "SEMGREP_FORCE_COLOR")
    ~doc:
      {|Always include ANSI color in the output, even if not writing to
a TTY; defaults to using the TTY status
|}

let o_max_chars_per_line : int Term.t =
  let info =
    Arg.info [ "max-chars-per-line" ]
      ~doc:"Maximum number of characters to show per line."
  in
  Arg.value (Arg.opt Arg.int default.output_conf.max_chars_per_line info)

let o_max_lines_per_finding : int Term.t =
  let info =
    Arg.info
      [ "max-lines-per-finding" ]
      ~doc:
        {|Maximum number of lines of code that will be shown for each match
before trimming (set to 0 for unlimited).|}
  in
  Arg.value (Arg.opt Arg.int default.output_conf.max_lines_per_finding info)

let o_max_log_list_entries : int Term.t =
  let default = default.output_conf.max_log_list_entries in
  let info =
    Arg.info [ "max-log-list-entries" ]
      ~doc:
        (spf
           {|Maximum number of entries that will be shown in the log (e.g.,
list of rule ids, list of skipped files).
A zero or negative value disables this filter. Defaults to %d|}
           default)
  in

  Arg.value (Arg.opt Arg.int default info)

let o_dataflow_traces : bool Term.t =
  let info =
    Arg.info [ "dataflow-traces" ]
      ~doc:
        {|Explain how non-local values reach the location of a finding (only affects text and SARIF output).|}
  in
  Arg.value (Arg.flag info)

let o_matching_explanations : bool Term.t =
  let info =
    Arg.info
      [ "matching-explanations" ]
      ~doc:
        {|Add debugging information in the JSON output to trace how
different parts of a rule are matched (a.k.a., "Inspect Rule"
in the Semgrep playground)|}
  in
  Arg.value (Arg.flag info)

let o_rewrite_rule_ids : bool Term.t =
  H.negatable_flag [ "rewrite-rule-ids" ] ~neg_options:[ "no-rewrite-rule-ids" ]
    ~default:default.rewrite_rule_ids
    ~doc:
      {|Rewrite rule ids when they appear in nested sub-directories
(Rule 'foo' in test/rules.yaml will be renamed 'test.foo').
|}

let o_time : bool Term.t =
  H.negatable_flag [ "time" ] ~neg_options:[ "no-time" ]
    ~default:default.core_runner_conf.time_flag
    ~doc:
      {|Include a timing summary with the results. If output format is json,
 provides times for each pair (rule, target). This feature is meant for internal use and may be changed or removed without warning. At the current moment, --trace is better supported.
|}

let o_trace : bool Term.t =
  H.negatable_flag [ "trace" ] ~neg_options:[ "no-trace" ]
    ~default:default.trace
    ~doc:
      {|Record traces from Semgrep scans to help debugging. This feature is
meant for internal use and may be changed or removed without warning.
|}

let o_trace_endpoint : string option Term.t =
  let info =
    Arg.info [ "trace-endpoint" ]
      ~env:(Cmd.Env.info "SEMGREP_OTEL_ENDPOINT")
      ~doc:
        {|Endpoint to send OpenTelemetry traces to, if `--trace` is present.
The value may be `semgrep-prod` (default), `semgrep-dev`,
`semgrep-local`, or any valid URL.  This feature is meant for
internal use and may be changed or removed wihtout warning.
|}
  in
  Arg.value (Arg.opt Arg.(some string) None info)

let o_nosem : bool Term.t =
  H.negatable_flag ~default:true [ "enable-nosem" ]
    ~neg_options:[ "disable-nosem" ]
    ~doc:
      {|Enables 'nosem'. Findings will not be reported on lines containing
          a 'nosem' comment at the end. Enabled by default.|}

let o_output : string option Term.t =
  let info =
    Arg.info [ "o"; "output" ]
      ~doc:
        "Save search results to a file or post to URL. Default is to print to \
         stdout."
  in
  Arg.value (Arg.opt Arg.(some string) None info)

(* ------------------------------------------------------------------ *)
(* Output formats (mutually exclusive) *)
(* ------------------------------------------------------------------ *)
let o_text : bool Term.t =
  let info = Arg.info [ "text" ] ~doc:{|Output results in text format.|} in
  Arg.value (Arg.flag info)

let o_json : bool Term.t =
  let info =
    Arg.info [ "json" ] ~doc:{|Output results in Semgrep's JSON format.|}
  in
  Arg.value (Arg.flag info)

let o_incremental_output : bool Term.t =
  let info =
    Arg.info [ "incremental-output" ]
      ~doc:{|Output results incrementally. REQUIRES --experimental|}
  in
  Arg.value (Arg.flag info)

(* osemgrep-only: *)
let o_files_with_matches : bool Term.t =
  let info =
    Arg.info [ "files-with-matches" ]
      ~doc:
        {|Output only the names of files containing matches.
REQUIRES --experimental|}
  in
  Arg.value (Arg.flag info)

let o_emacs : bool Term.t =
  let info =
    Arg.info [ "emacs" ] ~doc:{|Output results in Emacs single-line format.|}
  in
  Arg.value (Arg.flag info)

let o_vim : bool Term.t =
  let info =
    Arg.info [ "vim" ] ~doc:{|Output results in vim single-line format.|}
  in
  Arg.value (Arg.flag info)

let o_sarif : bool Term.t =
  let info = Arg.info [ "sarif" ] ~doc:{|Output results in SARIF format.|} in
  Arg.value (Arg.flag info)

let o_gitlab_sast : bool Term.t =
  let info =
    Arg.info [ "gitlab-sast" ] ~doc:{|Output results in GitLab SAST format.|}
  in
  Arg.value (Arg.flag info)

let o_gitlab_secrets : bool Term.t =
  let info =
    Arg.info [ "gitlab-secrets" ]
      ~doc:{|Output results in GitLab Secrets format.|}
  in
  Arg.value (Arg.flag info)

let o_junit_xml : bool Term.t =
  let info =
    Arg.info [ "junit-xml" ] ~doc:{|Output results in JUnit XML format.|}
  in
  Arg.value (Arg.flag info)

(* ------------------------------------------------------------------ *)
(* Write additional outputs *)
(* ------------------------------------------------------------------ *)

let make_o_format_outputs : ?fancy:string -> string -> string list Term.t =
 fun ?fancy format ->
  let fancy_format =
    match fancy with
    | None -> format
    | Some str -> str
  in
  let info =
    Arg.info
      [ format ^ "-output" ]
      ~doc:
        ("Write a copy of the " ^ fancy_format
       ^ " output to a file or post to URL.")
  in
  Arg.value (Arg.opt_all Arg.string [] info)

let o_text_outputs = make_o_format_outputs "text"
let o_json_outputs = make_o_format_outputs "json"
let o_emacs_outputs = make_o_format_outputs "emacs"
let o_vim_outputs = make_o_format_outputs "vim"
let o_sarif_outputs = make_o_format_outputs ~fancy:"SARIF" "sarif"

let o_gitlab_sast_outputs =
  make_o_format_outputs ~fancy:"GitLab SAST" "gitlab-sast"

let o_gitlab_secrets_outputs =
  make_o_format_outputs ~fancy:"GitLab Secrets" "gitlab-secrets"

let o_junit_xml_outputs = make_o_format_outputs ~fancy:"JUnit XML" "junit-xml"

(* ------------------------------------------------------------------ *)
(* Run Secrets Post Processors                                  *)
(* ------------------------------------------------------------------ *)

let o_secrets : bool Term.t =
  let info =
    Arg.info [ "secrets" ]
      ~doc:
        {|Run Semgrep Secrets product, including support for secret validation.
          Requires access to Secrets, contact support@semgrep.com for more
          information.|}
  in
  Arg.value (Arg.flag info)

let o_no_secrets_validation : bool Term.t =
  let info =
    Arg.info [ "no-secrets-validation" ] ~doc:{|Disables secret validation.|}
  in
  Arg.value (Arg.flag info)

let o_allow_untrusted_validators : bool Term.t =
  let info =
    Arg.info
      [ "allow-untrusted-validators" ]
      ~doc:
        {|Allows running rules with validators from origins other than semgrep.dev. Avoid running rules from origins you don't trust.|}
  in
  Arg.value (Arg.flag info)

let o_historical_secrets : bool Term.t =
  let info =
    Arg.info [ "historical-secrets" ]
      ~doc:{|Scans git history using Secrets rules.|}
  in
  Arg.value (Arg.flag info)

(* ------------------------------------------------------------------ *)
(* Engine type (mutually exclusive) *)
(* ------------------------------------------------------------------ *)

let o_oss : bool Term.t =
  let info =
    Arg.info [ "oss-only" ]
      ~doc:
        {|Run using only the OSS engine, even if the Semgrep Pro toggle is on.
This may still run Pro rules, but only using the OSS features.
|}
  in
  Arg.value (Arg.flag info)

let o_pro_languages : bool Term.t =
  let info =
    Arg.info [ "pro-languages" ]
      ~doc:
        ("Enable Pro languages (currently Apex, C#, and Elixir). " ^ C.blurb_pro)
  in
  Arg.value (Arg.flag info)

let o_pro_intrafile : bool Term.t =
  let info =
    Arg.info [ "pro-intrafile" ]
      ~doc:
        ("Intra-file inter-procedural taint analysis. Implies --pro-languages. "
       ^ C.blurb_pro)
  in
  Arg.value (Arg.flag info)

let o_pro_path_sensitive : bool Term.t =
  let info =
    Arg.info [ "pro-path-sensitive" ]
      ~doc:("Path sensitivity. Implies --pro-intrafile. " ^ C.blurb_pro)
  in
  Arg.value (Arg.flag info)

let o_pro : bool Term.t =
  let info =
    Arg.info [ "pro" ]
      ~doc:
        ("Inter-file analysis and Pro languages (currently Apex, C#, and \
          Elixir. " ^ C.blurb_pro)
  in
  Arg.value (Arg.flag info)

(* ------------------------------------------------------------------ *)
(* Configuration options ('scan' only, not reused in 'ci') *)
(* ------------------------------------------------------------------ *)
let o_config : string list Term.t =
  let info =
    Arg.info [ "c"; "f"; "config" ]
      ~env:(Cmd.Env.info "SEMGREP_RULES")
      ~doc:
        {|YAML configuration file, directory of YAML files ending in
.yml|.yaml, URL of a configuration file, or Semgrep registry entry name.

Use --config auto to automatically obtain rules tailored to this project;
your project URL will be used to log in to the Semgrep registry.

To run multiple rule files simultaneously, use --config before every YAML,
URL, or Semgrep registry entry name.
For example `semgrep --config p/python --config myrules/myrule.yaml`

See https://semgrep.dev/docs/writing-rules/rule-syntax for information on
configuration file format.
|}
  in
  Arg.value (Arg.opt_all Arg.string [] info)

let o_pattern : string option Term.t =
  let info =
    Arg.info [ "e"; "pattern" ]
      ~doc:
        {|Code search pattern. See https://semgrep.dev/docs/writing-rules/pattern-syntax for information on pattern features.
|}
  in
  Arg.value (Arg.opt Arg.(some string) None info)

let o_lang : string option Term.t =
  let info =
    Arg.info [ "l"; "lang" ]
      ~doc:
        {|Parse pattern and all files in specified language.
Must be used with -e/--pattern.
|}
  in
  Arg.value (Arg.opt Arg.(some string) None info)

let o_replacement : string option Term.t =
  let info =
    Arg.info [ "replacement" ]
      ~doc:
        {|An autofix expression that will be applied to any matches found
with --pattern. Only valid with a command-line specified pattern.
|}
  in
  Arg.value (Arg.opt Arg.(some string) None info)

let o_autofix : bool Term.t =
  H.negatable_flag [ "a"; "autofix" ] ~neg_options:[ "no-autofix" ]
    ~default:default.autofix
    ~doc:
      {|Apply autofix patches. WARNING: data loss can occur with this flag.
Make sure your files are stored in a version control system. Note that
this mode is experimental and not guaranteed to function properly.
|}

let o_dryrun : bool Term.t =
  H.negatable_flag [ "dryrun" ] ~neg_options:[ "no-dryrun" ]
    ~default:default.output_conf.fixed_lines
    ~doc:
      {|If --dryrun, does not write autofixes to a file. This will print the
changes to the console. This lets you see the changes before you commit to
them. Only works with the --autofix flag. Otherwise does nothing.
|}

let o_error : bool Term.t =
  H.negatable_flag [ "error" ] ~neg_options:[ "no-error" ]
    ~default:default.error_on_findings
    ~doc:{|Exit 1 if there are findings. Useful for CI and scripts.|}

let o_strict : bool Term.t =
  H.negatable_flag [ "strict" ] ~neg_options:[ "no-strict" ]
    ~default:default.output_conf.strict
    ~doc:
      {|Return a nonzero exit code when WARN level errors are encountered.
Fails early if invalid configuration files are present.
Defaults to --no-strict.
|}

(* In theory we should also accept EXPERIMENT and INVENTORY *)
let o_severity : Rule.severity list Term.t =
  let info =
    Arg.info [ "severity" ]
      ~doc:
        {|Report findings only from rules matching the supplied severity
level. By default all applicable rules are run. Can add multiple times.
Each should be one of INFO, WARNING, or ERROR.
|}
  in
  Arg.value
    (Arg.opt_all
       (Cmdliner.Arg.enum
          [ ("INFO", `Info); ("WARNING", `Warning); ("ERROR", `Error) ])
       [] info)

let o_exclude_rule_ids : string list Term.t =
  let info =
    Arg.info [ "exclude-rule" ]
      ~doc:{|Skip any rule with the given id. Can add multiple times.|}
  in
  Arg.value (Arg.opt_all Arg.string [] info)

let o_exclude_minified_files : bool Term.t =
  H.negatable_flag
    [ "exclude-minified-files" ]
    ~neg_options:[ "no-exclude-minified-files" ]
    ~default:default.targeting_conf.exclude_minified_files
    ~doc:
      {|Skip minified files. These are files that are > 7% whitespace, or who
        have a large number of bytes per line. By default minified files are
   scanned |}

(* ------------------------------------------------------------------ *)
(* Alternate modes *)
(* ------------------------------------------------------------------ *)
(* TOPORT: "No search is performed in these modes"
 * coupling: if you add an option here, you probably also need to modify
 * the sanity checking code around --config to allow empty --config
 * with this new alternate mode.
 *)

let o_version : bool Term.t =
  let info = Arg.info [ "version" ] ~doc:{|Show the version and exit.|} in
  Arg.value (Arg.flag info)

(* ugly: this should be a separate subcommand, not a flag of semgrep scan,
 * like 'semgrep show supported-languages'
 *)
let o_show_supported_languages : bool Term.t =
  let info =
    Arg.info
      [ "show-supported-languages" ]
      ~doc:
        {|Print a list of languages that are currently supported by Semgrep.|}
  in
  Arg.value (Arg.flag info)

(* ugly: this should be a separate subcommand, not a flag of semgrep scan *)
let o_test : bool Term.t =
  let info = Arg.info [ "test" ] ~doc:{|Run test suite.|} in
  Arg.value (Arg.flag info)

(* ugly: this should be a separate subcommand, not a flag of semgrep scan *)
let o_validate : bool Term.t =
  let info =
    Arg.info [ "validate" ]
      ~doc:
        {|Validate configuration file(s).
This will check YAML files for errors and run 'p/semgrep-rule-lints' on
the YAML files. No search is performed.
|}
  in
  Arg.value (Arg.flag info)

(* ugly: this should be a separate subcommand, not a flag of semgrep scan *)
let o_dump_ast : bool Term.t =
  let info =
    Arg.info [ "dump-ast" ]
      ~doc:
        {|If --dump-ast, shows AST of the input file or passed expression
and then exit (can use --json).
|}
  in
  Arg.value (Arg.flag info)

(* ugly: this should be a separate subcommand, not a flag of semgrep scan.
 * python: Click offer the hidden=True flag to not show it in --help
 * but cmdliner does not have an equivalent I think. Anyway this
 * command should soon disappear anyway.
 *)
let o_dump_engine_path : bool Term.t =
  let info = Arg.info [ "dump-engine-path" ] ~doc:{|<internal, do not use>|} in
  Arg.value (Arg.flag info)

(* LATER: this should not be needed *)
let o_dump_command_for_core : bool Term.t =
  let info =
    Arg.info [ "d"; "dump-command-for-core" ] ~doc:{|<internal, do not use>|}
  in
  Arg.value (Arg.flag info)

(* This is just intended to be around temporarily while we roll out and test the feature. Once we
   are confident that the lockfileless
   approach will not cause failures for customers, we should remove this flag and replace it with
   a flag to _disable_ dynamic dependency resolution.
   TODO: (bk) delete this flag
*)
let o_allow_local_builds : bool Term.t =
  let info =
    Arg.info [ "allow-local-builds" ]
      ~doc:
        {|Experimental: allow building projects contained in the repository. This allows Semgrep to identify dependencies
          and dependency relationships when lockfiles are not present or are insufficient. However, building code may inherently
          require the execution of code contained in the scanned project or in its dependencies, which is a security risk.|}
  in
  Arg.value (Arg.flag info)

(* ------------------------------------------------------------------ *)
(* Test and debug options *)
(* ------------------------------------------------------------------ *)

(* ------------------------------------------------------------------ *)
(* Positional arguments *)
(* ------------------------------------------------------------------ *)

let o_target_roots : string list Term.t =
  let info =
    Arg.info [] ~docv:"TARGETS"
      ~doc:{|Files or folders to be scanned by semgrep.|}
  in
  Arg.value
    (Arg.pos_all Arg.string
       (default.target_roots |> List_.map Scanning_root.to_string)
       info)

(* ------------------------------------------------------------------ *)
(* !!NEW arguments!! not in pysemgrep *)
(* ------------------------------------------------------------------ *)

let o_project_root : string option Term.t =
  let info =
    Arg.info [ "project-root" ]
      ~doc:
        {|Semgrep normally determines the type of project (git or novcs)
          and the project root automatically. The project root is then used
          to locate and use '.gitignore' and '.semgrepignore' files which
          determine target files that should be ignored by semgrep.
          This option forces the project root to be a specific folder
          and assumes a local project without version control (novcs).
          This option is useful to ensure the '.semgrepignore' file that
          may exist at the project root is consulted when the scanning root
          is not the current folder '.'.
          A valid project root must be a folder (path referencing
          a directory) whose physical path
          is a prefix of the physical path of the scanning roots passed
          on the command line. For example, the command
          'semgrep scan --project-root . src' is valid
          if '.' is '/home/me' and 'src' is a directory or a symbolic link
          to a '/home/me/sources' directory or a symbolic link to a
          'sources' directory but not if it is a symbolic link to
          a directory '/var/sources' (assuming '/var' is not a symbolic link).
          REQUIRES --experimental|}
  in
  Arg.value (Arg.opt Arg.(some string) None info)

let o_remote : string option Term.t =
  let info =
    Arg.info [ "remote" ]
      ~doc:
        {|Remote will quickly check out and scan a remote git repository of
        the format "http[s]://<WEBSITE>/.../<REPO>.git". Must be run with
        --pro. Incompatible with --project-root. Note this requires an empty
        CWD as this command will clone the repository into the CWD.
        REQUIRES --experimental|}
  in
  Arg.value (Arg.opt Arg.(some string) None info)

(*
   Let's use the following convention: the prefix '--x-' means "forbidden"
   or "experimental".
*)
let o_ls : bool Term.t =
  let info =
    Arg.info [ "x-ls" ]
      ~doc:
        {|[INTERNAL] List the selected target files
before any rule-specific or language-specific filtering. Then exit.
The default output format is one path per line.
THIS OPTION IS NOT PART OF THE SEMGREP API AND MAY
CHANGE OR DISAPPEAR WITHOUT NOTICE.
|}
  in
  Arg.value (Arg.flag info)

let o_ls_long : bool Term.t =
  let info =
    Arg.info [ "x-ls-long" ]
      ~doc:
        {|[INTERNAL] Show selected targets and skipped targets with reasons why
they were skipped, using an unspecified output format.
Implies --x-ls.
THIS OPTION IS NOT PART OF THE SEMGREP API AND MAY
CHANGE OR DISAPPEAR WITHOUT NOTICE.
|}
  in
  Arg.value (Arg.flag info)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*
   Return a list of files or folders that exist.
   Stdin and named pipes are converted to temporary regular files.
   The bool indicates that some paths were converted to temporary files without
   a particular file name or extension.

   experimental = we're sure that we won't invoke pysemgrep later with the
   same argv; allows us to consume stdin and named pipes.
*)
let replace_target_roots_by_regular_files_where_needed (caps : < Cap.tmp >)
    ~(experimental : bool) (target_roots : string list) :
    Scanning_root.t list * bool =
  let imply_always_select_explicit_targets = ref false in
  let target_roots =
    target_roots
    |> List_.map (fun str ->
           match str with
           | "-" ->
               imply_always_select_explicit_targets := true;
               if experimental then
                 (* consumes stdin, preventing command-line forwarding to
                    pysemgrep or another osemgrep! *)
                 CapTmp.replace_stdin_by_regular_file caps#tmp
                   ~prefix:"osemgrep-stdin-" ()
               else
                 (* remove this hack when no longer forward the command line
                    to another program *)
                 Fpath.v "/dev/stdin"
           | str ->
               let orig_path = Fpath.v str in
               if
                 experimental
                 && Skip_target.filter_file_access_permissions orig_path
                    |> Result.is_ok
               then (
                 match
                   CapTmp.replace_named_pipe_by_regular_file_if_needed caps#tmp
                     ~prefix:"osemgrep-named-pipe-" (Fpath.v str)
                 with
                 | None -> orig_path
                 | Some new_path ->
                     imply_always_select_explicit_targets := true;
                     new_path)
               else orig_path)
    |> List_.map Scanning_root.of_fpath
  in
  if !imply_always_select_explicit_targets then
    Logs.info (fun m ->
        m
          "Implying --scan-unknown-extensions due to explicit targets being \
           stdin or named pipes");
  (target_roots, !imply_always_select_explicit_targets)

(*****************************************************************************)
(* Subconf helpers *)
(*****************************************************************************)

(* alt: use an output_cmdline_term, core_runner_cmdline_term, etc
 * and leverage the nice ability of cmdliner to be composable.
 *)

let rule_source_conf ~config ~pattern ~lang ~replacement ~allow_empty_config
    ~maturity : Rules_source.t =
  let explicit_analyzer = Option.map Analyzer.of_string lang in
  match (config, (pattern, explicit_analyzer, replacement)) with
  (* TOPORT: handle get_project_url() if empty Configs? *)
  | [], (None, _, _) ->
      (* TOPORT: raise with Exit_code.missing_config *)
      (* TOPORT? use instead
         "No config given and {DEFAULT_CONFIG_FILE} was not found. Try running with --help to debug or if you want to download a default config, try running with --config r2c" *)
      if allow_empty_config then Rules_source.Configs []
      else (
        Migration.abort_if_use_of_legacy_dot_semgrep_yml ();
        (* config is set to auto if not otherwise specified and when we're not trying another inferred subcommand *)
        default.rules_source)
  | [], (Some pat, Some analyzer, fix) ->
      (* may raise a Failure (will be caught in CLI.safe_run) *)
      Rules_source.Pattern (pat, Some analyzer, fix)
  | _, (Some pat, None, fix) -> (
      match maturity with
      (* osemgrep-only: better: can use -e without -l! *)
      | Maturity.Develop -> Rules_source.Pattern (pat, None, fix)
      | _else_ ->
          (* alt: "language must be specified when a pattern is passed" *)
          Error.abort "-e/--pattern and -l/--lang must both be specified")
  | _, (None, Some _, _) ->
      (* stricter: error not detected in original semgrep *)
      Error.abort "-e/--pattern and -l/--lang must both be specified"
  | _, (None, _, Some _) ->
      Error.abort
        "command-line replacement flag can only be used with command-line \
         pattern; when using a config file add the fix: key instead"
  (* TOPORT? handle [x], _ and rule passed inline, python: util.is_rules*)
  | xs, (None, None, None) -> Rules_source.Configs xs
  | _ :: _, (Some _, _, _) ->
      Error.abort "Mutually exclusive options --config/--pattern"

let project_root_conf ~project_root ~remote : Find_targets.project_root option =
  let is_git_repo remote =
    remote |> Git_wrapper.remote_repo_name |> Option.is_some
  in
  match (project_root, remote) with
  | Some root, None ->
      Some (Find_targets.Filesystem (Rfpath.of_string_exn root))
  | None, Some url when is_git_repo url ->
      (* CWD must be empty for this to work *)
      let has_files = not (List_.null (List_files.list (Fpath.v "."))) in
      if has_files then
        Error.abort
          "Cannot use --remote with a git remote when the current directory is \
           not empty";
      let url = Uri.of_string url in
      Some (Find_targets.Git_remote { url })
  | None, Some _url ->
      Error.abort
        "Remote arg is not a valid git remote, expected something like \
         http[s]://website.com/.../<REPONAME>.git"
  | Some _, Some _ ->
      Error.abort "Cannot use both --project-root and --remote at the same time"
  | _ -> None

(* reused in Ci_CLI.ml *)
let output_format_conf ~text ~files_with_matches ~json ~emacs ~vim ~sarif
    ~gitlab_sast ~gitlab_secrets ~junit_xml : Output_format.t =
  let all_flags =
    [ json; emacs; vim; sarif; gitlab_sast; gitlab_secrets; junit_xml ]
  in
  let cnt =
    all_flags |> List.fold_left (fun acc b -> if b then acc + 1 else acc) 0
  in
  if cnt >= 2 then
    (* TOPORT: list the possibilities *)
    Error.abort "Mutually exclusive options --json/--emacs/--vim/--sarif/...";
  match () with
  | _ when text -> Output_format.Text
  | _ when files_with_matches -> Output_format.Files_with_matches
  | _ when json -> Output_format.Json
  | _ when emacs -> Output_format.Emacs
  | _ when vim -> Output_format.Vim
  | _ when sarif -> Output_format.Sarif
  | _ when gitlab_sast -> Output_format.Gitlab_sast
  | _ when gitlab_secrets -> Output_format.Gitlab_secrets
  | _ when junit_xml -> Output_format.Junit_xml
  | _else_ -> default.output_conf.output_format

(* reused in Ci_CLI.ml *)
let outputs_conf ~text_outputs ~json_outputs ~emacs_outputs ~vim_outputs
    ~sarif_outputs ~gitlab_sast_outputs ~gitlab_secrets_outputs
    ~junit_xml_outputs =
  [
    (Output_format.Text, text_outputs);
    (Output_format.Json, json_outputs);
    (Output_format.Emacs, emacs_outputs);
    (Output_format.Vim, vim_outputs);
    (Output_format.Sarif, sarif_outputs);
    (Output_format.Gitlab_sast, gitlab_sast_outputs);
    (Output_format.Gitlab_secrets, gitlab_secrets_outputs);
    (Output_format.Junit_xml, junit_xml_outputs);
  ]
  |> List.fold_left
       (fun outputs (output_format, outputs_for_specific_format) ->
         outputs_for_specific_format
         |> List.fold_left
              (fun outputs output_destination ->
                let key = Some output_destination in
                if Map_.mem key outputs then
                  (* TODO: Should probably error here. *)
                  outputs
                else Map_.add key output_format outputs)
              outputs)
       Map_.empty

(* reused in Ci_CLI.ml *)
let engine_type_conf ~oss ~pro_lang ~pro_intrafile ~pro ~secrets
    ~no_secrets_validation ~allow_untrusted_validators ~pro_path_sensitive :
    Engine_type.t =
  (* This first bit just rules out mutually exclusive options. *)
  if oss && secrets then
    Error.abort "Cannot run secrets scan with OSS engine (--oss specified).";
  if
    [ oss; pro_lang; pro_intrafile; pro ]
    |> List.filter Fun.id |> List.length > 1
  then
    Error.abort
      "Mutually exclusive options --oss/--pro-languages/--pro-intrafile/--pro";
  (* Now select the engine type *)
  if oss then Engine_type.OSS
  else
    let analysis =
      Engine_type.(
        match () with
        | _ when pro -> Interfile
        | _ when pro_intrafile -> Interprocedural
        | _ -> Intraprocedural)
    in
    let extra_languages = pro || pro_lang || pro_intrafile in
    let secrets_config =
      if secrets && not no_secrets_validation then
        Some Engine_type.{ allow_all_origins = allow_untrusted_validators }
      else None
    in
    let code_config =
      if pro || pro_lang || pro_intrafile then Some () else None
    in
    (* Currently we don't run SCA in osemgrep *)
    let supply_chain_config = None in
    match (extra_languages, analysis, secrets_config) with
    | false, Intraprocedural, None -> OSS
    | _ ->
        PRO
          {
            extra_languages;
            analysis;
            code_config;
            secrets_config;
            supply_chain_config;
            path_sensitive = pro_path_sensitive;
          }
(*****************************************************************************)
(* Alternate subcommand subconf *)
(*****************************************************************************)

let show_CLI_conf ~dump_ast ~dump_engine_path ~dump_command_for_core
    ~show_supported_languages ~target_roots ~pattern ~lang ~json ~pro ~common :
    Show_CLI.conf option =
  match () with
  | _ when dump_ast -> (
      let target_roots =
        if target_roots =*= default.target_roots then [] else target_roots
      in
      match (pattern, lang, target_roots) with
      | Some str, Some lang_str, [] ->
          Some
            {
              Show.show_kind = Show.DumpPattern (str, Lang.of_string lang_str);
              json;
              common;
            }
      | None, Some lang_str, [ file ] ->
          Some
            {
              Show.show_kind =
                Show.DumpAST
                  (Scanning_root.to_fpath file, Lang.of_string lang_str);
              json;
              common;
            }
      | _, None, _ ->
          Error.abort "--dump-ast and -l/--lang must both be specified"
      (* stricter: alt: could dump all targets *)
      | None, Some _, _ :: _ :: _ ->
          Error.abort "--dump-ast requires exactly one target file"
      (* stricter: better error message *)
      | None, Some _, [] ->
          Error.abort "--dump-ast needs either a target or a -e pattern"
      (* stricter: *)
      | Some _, _, _ :: _ ->
          Error.abort "Can't specify both -e and a target for --dump-ast")
  | _ when dump_engine_path ->
      Some { Show.show_kind = Show.DumpEnginePath pro; json; common }
  | _ when dump_command_for_core ->
      Some { Show.show_kind = Show.DumpCommandForCore; json; common }
  | _ when show_supported_languages ->
      Some { Show.show_kind = Show.SupportedLanguages; json; common }
  | _else_ -> None

let validate_CLI_conf ~validate ~rules_source ~core_runner_conf ~common ~pro :
    Validate_CLI.conf option =
  if validate then
    match rules_source with
    | Rules_source.Configs [] ->
        (* TOPORT? was a Logs.err but seems better as an abort *)
        Error.abort
          "Nothing to validate, use the --config or --pattern flag to specify \
           a rule"
    | Configs (_ :: _)
    | Pattern _ ->
        Some { rules_source; pro; core_runner_conf; common }
  else None

let test_CLI_conf ~test ~target_roots ~config ~json ~optimizations
    ~test_ignore_todo ~strict ~pro ~common : Test_CLI.conf option =
  if test then
    let target =
      Test_CLI.target_kind_of_roots_and_config
        (List_.map Scanning_root.to_fpath target_roots)
        config
    in
    Some
      Test_CLI.
        {
          target;
          pro;
          strict;
          json;
          optimizations;
          ignore_todo = test_ignore_todo;
          common;
          matching_diagnosis = false;
        }
  else None

(*****************************************************************************)
(* Turn argv into a conf *)
(*****************************************************************************)

(* coupling: if you modify this function, you might need to modify
 * also Ci_CLI.scan_subset_cmdline_term
 *)
let cmdline_term caps ~allow_empty_config : conf Term.t =
  (* !The parameters must be in alphabetic orders to match the order
     of the corresponding '$ o_xx $' further below!
  *)
  let combine allow_local_builds allow_untrusted_validators autofix
      baseline_commit common config dataflow_traces diff_depth dryrun dump_ast
      dump_command_for_core dump_engine_path emacs emacs_outputs error exclude_
      exclude_minified_files exclude_rule_ids files_with_matches force_color
      gitlab_sast gitlab_sast_outputs gitlab_secrets gitlab_secrets_outputs
      _historical_secrets include_ incremental_output json json_outputs
      junit_xml junit_xml_outputs lang matching_explanations max_chars_per_line
      max_lines_per_finding max_log_list_entries max_memory_mb max_target_bytes
      metrics num_jobs no_secrets_validation nosem optimizations oss output
      pattern pro project_root pro_intrafile pro_lang pro_path_sensitive remote
      replacement rewrite_rule_ids sarif sarif_outputs scan_unknown_extensions
      secrets severity show_supported_languages strict target_roots test
      test_ignore_todo text text_outputs time_flag timeout
      _timeout_interfileTODO timeout_threshold trace trace_endpoint use_git
      validate version version_check vim vim_outputs
      x_ignore_semgrepignore_files x_ls x_ls_long =
    (* Print a warning if any of the internal or experimental options.
       We don't want users to start relying on these. *)
    if x_ignore_semgrepignore_files || x_ls || x_ls_long then
      Logs.warn (fun m ->
          m
            "!!! You're using one or more options starting with '--x-'. These \
             options are not part of the semgrep API. They will change or will \
             be removed without notice !!! ");
    let target_roots, imply_always_select_explicit_targets =
      replace_target_roots_by_regular_files_where_needed caps
        ~experimental:(common.CLI_common.maturity =*= Maturity.Experimental)
        target_roots
    in
    let force_project_root = project_root_conf ~project_root ~remote in
    let explicit_targets =
      (* This is for determining whether a target path appears on the command
         line. As long as this holds, it's ok to include folders. *)
      target_roots
      |> List_.map Scanning_root.to_fpath
      |> Find_targets.Explicit_targets.of_list
    in

    let output_format : Output_format.t =
      output_format_conf ~text ~files_with_matches ~json ~emacs ~vim ~sarif
        ~gitlab_sast ~gitlab_secrets ~junit_xml
    in
    (* TODO: Actually handle additional output files *)
    (* _outputs is currently just parsed to support pysemgrep *)
    let _outputs =
      outputs_conf ~text_outputs ~json_outputs ~emacs_outputs ~vim_outputs
        ~sarif_outputs ~gitlab_sast_outputs ~gitlab_secrets_outputs
        ~junit_xml_outputs
    in
    let output_conf : Output.conf =
      {
        output_format;
        max_chars_per_line;
        max_lines_per_finding;
        force_color;
        show_dataflow_traces = dataflow_traces;
        strict;
        fixed_lines = dryrun;
        skipped_files =
          (match common.logging_level with
          | Some (Info | Debug) -> true
          | _else_ -> false);
        max_log_list_entries;
      }
    in

    let engine_type : Engine_type.t =
      engine_type_conf ~oss ~pro_lang ~pro_intrafile ~pro ~secrets
        ~no_secrets_validation ~allow_untrusted_validators ~pro_path_sensitive
    in
    let rules_source : Rules_source.t =
      match (config, pattern) with
      (* ugly: when using --dump-ast, we can pass a pattern or a target,
       * but in the case of a target that means there is no config
       * but we still don't want to abort, hence this empty Configs.
       * Same for --version, --show-supported-langages, etc., hence
       * this ugly special case returning an empty Configs.
       *)
      | [], None
        when dump_ast || dump_engine_path || validate || test || version
             || show_supported_languages ->
          Rules_source.Configs []
      | _ ->
          rule_source_conf ~config ~pattern ~lang ~replacement
            ~allow_empty_config ~maturity:common.maturity
    in
    let core_runner_conf : Core_runner.conf =
      {
        Core_runner.num_jobs;
        optimizations;
        timeout;
        timeout_threshold;
        max_memory_mb;
        dataflow_traces;
        nosem;
        strict;
        time_flag;
        matching_explanations;
      }
    in
    let include_ =
      match include_ with
      | [] -> None
      | nonempty -> Some nonempty
    in
    let respect_gitignore = use_git in
    let force_novcs_project = force_project_root <> None || not use_git in
    let targeting_conf : Find_targets.conf =
      {
        force_project_root;
        force_novcs_project;
        exclude = exclude_;
        include_;
        baseline_commit;
        diff_depth;
        max_target_bytes;
        always_select_explicit_targets =
          scan_unknown_extensions || imply_always_select_explicit_targets;
        explicit_targets;
        respect_gitignore;
        respect_semgrepignore_files = not x_ignore_semgrepignore_files;
        exclude_minified_files;
      }
    in
    let rule_filtering_conf : Rule_filtering.conf =
      {
        Rule_filtering.exclude_rule_ids =
          List_.map Rule_ID.of_string_exn exclude_rule_ids;
        severity;
        exclude_products = [];
      }
    in

    (* ugly: dump should be a separate subcommand.
     * alt: we could move this code in a Dump_subcommand.validate_cli_args()
     *)
    let show : Show_CLI.conf option =
      show_CLI_conf ~dump_ast ~dump_engine_path ~dump_command_for_core
        ~show_supported_languages ~target_roots ~pattern ~lang ~json ~pro
        ~common
    in
    (* ugly: validate should be a separate subcommand.
     * alt: we could move this code in a Validate_subcommand.cli_args()
     *)
    let validate : Validate_CLI.conf option =
      validate_CLI_conf ~validate ~rules_source ~core_runner_conf ~common ~pro
    in
    (* ugly: test should be a separate subcommand *)
    let test : Test_CLI.conf option =
      test_CLI_conf ~test ~target_roots ~config ~json ~optimizations
        ~test_ignore_todo ~strict ~pro ~common
    in
    (* more sanity checks *)
    if
      (List.mem "auto" config
      || rules_source =*= Rules_source.Configs [ "auto" ])
      && metrics =*= Metrics_.Off
    then
      Error.abort
        "Cannot create auto config when metrics are off. Please allow metrics \
         or run with a specific config.";

    (* warnings.
     * ugly: TODO: remove the Default guard once we get the warning message
     * in osemgrep equal to the one in pysemgrep or when we remove
     * this sanity checks in pysemgrep and just rely on osemgrep to do it.
     *)
    if include_ <> None && exclude_ <> [] && common.maturity <> Maturity.Default
    then
      Logs.warn (fun m ->
          m
            "Paths that match both --include and --exclude will be skipped by \
             Semgrep.");

    if trace_endpoint <> None && not trace then
      Logs.warn (fun m ->
          m
            "The --trace-endpoint flag or SEMGREP_OTEL_ENDPOINT environment \
             variable is specified without --trace.\n\
             If you intend to enable tracing, please also add the --trace flag.");
    let ls, ls_format =
      (* --x-ls-long implies --x-ls *)
      if x_ls_long then (true, Ls_subcommand.Long)
      else if x_ls then (true, Ls_subcommand.default_format)
      else (false, Ls_subcommand.default_format)
    in
    {
      rules_source;
      target_roots;
      rule_filtering_conf;
      targeting_conf;
      core_runner_conf;
      error_on_findings = error;
      autofix;
      metrics;
      version_check;
      output;
      output_conf;
      incremental_output;
      engine_type;
      rewrite_rule_ids;
      common;
      (* ugly: *)
      version;
      show;
      validate;
      test;
      trace;
      trace_endpoint;
      allow_local_builds;
      ls;
      ls_format;
    }
  in
  (* Term defines 'const' but also the '$' operator *)
  Term.(
    (* !the o_xxx must be in alphabetic orders to match the parameters of
     * combine above! *)
    const combine $ o_allow_local_builds $ o_allow_untrusted_validators
    $ o_autofix $ o_baseline_commit $ CLI_common.o_common $ o_config
    $ o_dataflow_traces $ o_diff_depth $ o_dryrun $ o_dump_ast
    $ o_dump_command_for_core $ o_dump_engine_path $ o_emacs $ o_emacs_outputs
    $ o_error $ o_exclude $ o_exclude_minified_files $ o_exclude_rule_ids
    $ o_files_with_matches $ o_force_color $ o_gitlab_sast
    $ o_gitlab_sast_outputs $ o_gitlab_secrets $ o_gitlab_secrets_outputs
    $ o_historical_secrets $ o_include $ o_incremental_output $ o_json
    $ o_json_outputs $ o_junit_xml $ o_junit_xml_outputs $ o_lang
    $ o_matching_explanations $ o_max_chars_per_line $ o_max_lines_per_finding
    $ o_max_log_list_entries $ o_max_memory_mb $ o_max_target_bytes $ o_metrics
    $ o_num_jobs $ o_no_secrets_validation $ o_nosem $ o_optimizations $ o_oss
    $ o_output $ o_pattern $ o_pro $ o_project_root $ o_pro_intrafile
    $ o_pro_languages $ o_pro_path_sensitive $ o_remote $ o_replacement
    $ o_rewrite_rule_ids $ o_sarif $ o_sarif_outputs $ o_scan_unknown_extensions
    $ o_secrets $ o_severity $ o_show_supported_languages $ o_strict
    $ o_target_roots $ o_test $ Test_CLI.o_test_ignore_todo $ o_text
    $ o_text_outputs $ o_time $ o_timeout $ o_timeout_interfile
    $ o_timeout_threshold $ o_trace $ o_trace_endpoint $ o_use_git $ o_validate
    $ o_version $ o_version_check $ o_vim $ o_vim_outputs
    $ o_ignore_semgrepignore_files $ o_ls $ o_ls_long)

let doc = "run semgrep rules on files"

(* TODO: document the exit codes as defined in Exit_code.mli *)
let man : Cmdliner.Manpage.block list =
  [
    `S Cmdliner.Manpage.s_description;
    `P
      "Searches TARGET paths for matches to rules or patterns. Defaults to \
       searching entire current working directory.";
    `P "To get started quickly, run";
    `Pre "semgrep --config auto .";
    `P
      "This will automatically fetch rules for your project from the Semgrep \
       Registry. NOTE: Using `--config auto` will log in to the Semgrep \
       Registry with your project URL.";
    `P "For more information about Semgrep, go to https://semgrep.dev.";
    `P
      "NOTE: By default, Semgrep will report pseudonymous usage metrics to its \
       server if you pull your configuration from the Semgrep registry. To \
       learn more about how and why these metrics are collected, please see \
       https://semgrep.dev/docs/metrics. To modify this behavior, see the \
       --metrics option below.";
  ]
  @ CLI_common.help_page_bottom

let cmdline_info : Cmd.info = Cmd.info "semgrep scan" ~doc ~man

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_argv (caps : < Cap.tmp >) (argv : string array) : conf =
  let cmd : conf Cmd.t =
    Cmd.v cmdline_info (cmdline_term caps ~allow_empty_config:false)
  in
  CLI_common.eval_value ~argv cmd
