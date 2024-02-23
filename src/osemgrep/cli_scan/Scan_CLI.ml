open Common
module Arg = Cmdliner.Arg
module Term = Cmdliner.Term
module Cmd = Cmdliner.Cmd
module H = Cmdliner_
module Show = Show_CLI

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep scan' command-line arguments processing.

   Translated partially from scan.py
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
  (* can be a list of files or directories *)
  target_roots : Scanning_root.t list;
  (* Rules/targets refinements *)
  rule_filtering_conf : Rule_filtering.conf;
  targeting_conf : Find_targets.conf;
  (* Other configuration options *)
  error_on_findings : bool;
  rewrite_rule_ids : bool;
  (* Engine selection *)
  engine_type : Engine_type.t;
  (* Performance options *)
  core_runner_conf : Core_runner.conf;
  (* file or URL (None means output to stdout) *)
  output : string option;
  output_conf : Output.conf;
  (* Networking options *)
  metrics : Metrics_.config;
  registry_caching : bool; (* similar to core_runner_conf.ast_caching *)
  version_check : bool;
  common : CLI_common.conf;
  (* Ugly: should be in separate subcommands *)
  version : bool;
  show : Show_CLI.conf option;
  validate : Validate_subcommand.conf option;
  test : Test_CLI.conf option;
  trace : bool;
  ls : bool;
}
[@@deriving show]

(* We could split the content of this variable in different files, e.g.,
 * targeting_conf default could be move in a Find_targets.default, but
 * it's also nice to have everything in one place.
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
    (* alt: could move in a Core_runner.default *)
    core_runner_conf =
      {
        (* Maxing out number of cores used to 16 if more not requested to
         * not overload on large machines.
         * Also, hardcode num_jobs to 1 for non-unix (i.e. Windows) because
         * we don't believe that Parmap works in those environments
         * TODO: figure out a solution for Windows multi-processing (OCaml 5 in the worst case)
         *)
        Core_runner.num_jobs =
          min 16 (if Sys.unix then Parmap_helpers.get_cpu_count () else 1);
        timeout = 5.0;
        (* ^ seconds, keep up-to-date with User_settings.ml and constants.py *)
        timeout_threshold = 3;
        max_memory_mb = 0;
        optimizations = true;
        dataflow_traces = false;
        matching_explanations = false;
        time_flag = false;
        nosem = true;
        strict = false;
        (* better to set to false for now; annoying to add --ast-caching to
         * each command, but while we're still developing osemgrep it is
         * better to eliminate some source of complexity by default.
         *)
        ast_caching = false;
      };
    error_on_findings = false;
    (* could be move in CLI_common.default_conf? *)
    common =
      {
        profile = false;
        logging_level = Some Logs.Warning;
        maturity = Maturity.Default;
      };
    engine_type = OSS;
    output = None;
    output_conf = Output.default;
    rewrite_rule_ids = true;
    (* will send metrics only if the user uses the registry or the app *)
    metrics = Metrics_.Auto;
    (* like ast_caching, better to default to false for now *)
    registry_caching = false;
    version_check = true;
    (* ugly: should be separate subcommands *)
    version = false;
    show = None;
    validate = None;
    test = None;
    trace = false;
    ls = false;
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
    Arg.info [ "exclude" ]
      ~doc:
        {|Skip any file or directory that matches this pattern;
--exclude='*.py' will ignore the following: foo.py, src/foo.py, foo.py/bar.sh.
--exclude='tests' will ignore tests/foo.py as well as a/b/tests/c/foo.py.
Can add multiple times. If present, any --include directives are ignored.
|}
  in
  Arg.value (Arg.opt_all Arg.string [] info)

let o_include : string list Term.t =
  let info =
    Arg.info [ "include" ]
      ~doc:
        {|Filter files or directories by path. The argument is a
glob-style pattern such as 'foo.*' that must match the path. This is
an extra filter in addition to other applicable filters. For example,
specifying the language with '-l javascript' might preselect files
'src/foo.jsx' and 'lib/bar.js'.  Specifying one of '--include=src',
'-- include=*.jsx', or '--include=src/foo.*' will restrict the
selection to the single file 'src/foo.jsx'. A choice of multiple '--
include' patterns can be specified. For example, '--include=foo.*
--include=bar.*' will select both 'src/foo.jsx' and
'lib/bar.js'. Glob-style patterns follow the syntax supported by
python, which is documented at
https://docs.python.org/3/library/glob.html
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

let o_respect_gitignore : bool Term.t =
  H.negatable_flag [ "use-git-ignore" ] ~neg_options:[ "no-git-ignore" ]
    ~default:default.targeting_conf.respect_gitignore
    ~doc:
      {|Skip files ignored by git. Scanning starts from the root
folder specified on the Semgrep command line. Normally, if the
scanning root is within a git repository, only the tracked files and
the new files would be scanned. Git submodules and git- ignored files
would normally be skipped. --no-git-ignore will disable git-aware
filtering. Setting this flag does nothing if the scanning root is not
in a git repository.
|}

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
        {|Maximum system memory to use running a rule on a single file in MiB.
If set to 0 will not have memory limit. Defaults to 0. For CI scans
that use the Pro Engine, it defaults to 5000 MiB.
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
 provides times for each pair (rule, target).
|}

let o_trace : bool Term.t =
  H.negatable_flag [ "trace" ] ~neg_options:[ "no-trace" ]
    ~default:default.trace
    ~doc:{|Upload a trace of the scan to our endpoint (rule, target).
|}

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
(* Run Secrets Post Processors                                  *)
(* ------------------------------------------------------------------ *)

let o_secrets : bool Term.t =
  let info =
    Arg.info
      [ "beta-testing-secrets-enabled" ]
      ~doc:
        {|Please use --secrets instead of --beta-testing-secrets.
          Requires Semgrep Secrets, contact support@semgrep.com for more
          information on this.|}
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
      ~doc:{|Run postprocessors from untrusted sources.|}
  in
  Arg.value (Arg.flag info)

(* ------------------------------------------------------------------ *)
(* Engine type (mutually exclusive) *)
(* ------------------------------------------------------------------ *)

let o_oss : bool Term.t =
  let info =
    Arg.info [ "oss-only" ]
      ~doc:
        {|Run using only OSS features, even if the Semgrep Pro toggle is on.|}
  in
  Arg.value (Arg.flag info)

let blurb =
  "Requires Semgrep Pro Engine. See https://semgrep.dev/products/pro-engine/ \
   for more."

let o_pro_languages : bool Term.t =
  let info =
    Arg.info [ "pro-languages" ]
      ~doc:("Enable Pro languages (currently Apex and Elixir). " ^ blurb)
  in
  Arg.value (Arg.flag info)

let o_pro_intrafile : bool Term.t =
  let info =
    Arg.info [ "pro-intrafile" ]
      ~doc:
        ("Intra-file inter-procedural taint analysis. Implies --pro-languages. "
       ^ blurb)
  in
  Arg.value (Arg.flag info)

let o_pro : bool Term.t =
  let info =
    Arg.info [ "pro" ]
      ~doc:
        ("Inter-file analysis and Pro languages (currently Apex and Elixir). "
       ^ blurb)
  in
  Arg.value (Arg.flag info)

(* ------------------------------------------------------------------ *)
(* Configuration options *)
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
    ~default:default.output_conf.autofix
    ~doc:
      {|Apply autofix patches. WARNING: data loss can occur with this flag.
Make sure your files are stored in a version control system. Note that
this mode is experimental and not guaranteed to function properly.
|}

let o_dryrun : bool Term.t =
  H.negatable_flag [ "dryrun" ] ~neg_options:[ "no-dryrun" ]
    ~default:default.output_conf.dryrun
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
        {|The project root for gitignore and semgrepignore purposes is
          detected automatically from the presence of a .git/ directory in
          the current directory or one of its parents. If not found,
          the current directory is used as the project root. This option
          forces a specific directory to be the project root. This is useful
          for testing or for restoring compatibility with older semgrep
          implementations that only looked for a .semgrepignore file
          in the current directory.|}
  in
  Arg.value (Arg.opt Arg.(some string) None info)

let o_remote : string option Term.t =
  let info =
    Arg.info [ "remote" ]
      ~doc:
        {|Remote will quickly checkout and scan a remote git repository of
        the format "http[s]://<WEBSITE>/.../<REPO>.git". Must be run with
        --pro Incompatible with --project-root|}
  in
  Arg.value (Arg.opt Arg.(some string) None info)

let o_ast_caching : bool Term.t =
  H.negatable_flag [ "ast-caching" ] ~neg_options:[ "no-ast-caching" ]
    ~default:default.core_runner_conf.ast_caching
    ~doc:
      {|Store in ~/.semgrep/cache/asts/ the parsed ASTs to speedup things.
Requires --experimental.
|}

(* TODO: add also an --offline flag? what about metrics? *)
let o_registry_caching : bool Term.t =
  H.negatable_flag [ "registry-caching" ] ~neg_options:[ "no-registry-caching" ]
    ~default:default.registry_caching
    ~doc:
      {|Cache for 24 hours in ~/.semgrep/cache rules from the registry.
Requires --experimental.
|}

(*
   Let's use the following convention: the prefix '--x-' means "forbidden"
   or "experimental".
*)
let o_ls : bool Term.t =
  let info =
    Arg.info [ "x-ls" ]
      ~doc:
        {|[INTERNAL] List the selected target files and the skipped target
files before any rule-specific or language-specific filtering. Then exit.
The output format is unspecified.
THIS OPTION IS NOT PART OF THE SEMGREP API AND MAY
CHANGE OR DISAPPEAR WITHOUT NOTICE.
|}
  in
  Arg.value (Arg.flag info)

(*****************************************************************************)
(* Turn argv into a conf *)
(*****************************************************************************)

(*
   Return a list of files or folders that exist.
   Stdin and named pipes are converted to temporary regular files.
   The bool indicates that some paths were converted to temporary files without
   a particular file name or extension.

   experimental = we're sure that we won't invoke pysemgrep later with the
   same argv; allows us to consume stdin and named pipes.
*)
let replace_target_roots_by_regular_files_where_needed ~(experimental : bool)
    (target_roots : string list) : Scanning_root.t list * bool =
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
                 UTmp.replace_stdin_by_regular_file ~prefix:"osemgrep-stdin-" ()
               else
                 (* remove this hack when no longer forward the command line
                    to another program *)
                 Fpath.v "/dev/stdin"
           | str ->
               let orig_path = Fpath.v str in
               if experimental then (
                 match
                   UTmp.replace_named_pipe_by_regular_file_if_needed
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

let cmdline_term ~allow_empty_config : conf Term.t =
  (* !The parameters must be in alphabetic orders to match the order
   * of the corresponding '$ o_xx $' further below! *)
  let combine allow_untrusted_validators ast_caching autofix baseline_commit
      common config dataflow_traces diff_depth dryrun dump_ast
      dump_command_for_core dump_engine_path emacs error exclude_
      exclude_rule_ids force_color gitlab_sast gitlab_secrets include_ json
      junit_xml lang ls matching_explanations max_chars_per_line
      max_lines_per_finding max_memory_mb max_target_bytes metrics num_jobs
      no_secrets_validation nosem optimizations oss output pattern pro
      project_root pro_intrafile pro_lang registry_caching remote replacement
      respect_gitignore rewrite_rule_ids sarif scan_unknown_extensions secrets
      severity show_supported_languages strict target_roots test
      test_ignore_todo text time_flag timeout _timeout_interfileTODO
      timeout_threshold trace validate version version_check vim =
    (* ugly: call setup_logging ASAP so the Logs.xxx below are displayed
     * correctly *)
    Std_msg.setup ?highlight_setting:(if force_color then Some On else None) ();
    Logs_.setup_logging ~level:common.CLI_common.logging_level ();
    let target_roots, imply_always_select_explicit_targets =
      replace_target_roots_by_regular_files_where_needed
        ~experimental:(common.maturity =*= Maturity.Experimental)
        target_roots
    in
    let project_root =
      let is_git_repo remote =
        remote |> Git_wrapper.remote_repo_name |> Option.is_some
      in
      match (project_root, remote) with
      | Some root, None ->
          Some (Find_targets.Filesystem (Rfpath.of_string_exn root))
      | None, Some url when is_git_repo url ->
          let checkout_path =
            match !Semgrep_envvars.v.remote_clone_dir with
            | Some dir -> Rfpath.of_fpath_exn dir
            | None ->
                Git_wrapper.temporary_remote_checkout_path url
                |> Rfpath.of_fpath_exn
          in
          let url = Uri.of_string url in
          Some (Find_targets.Git_remote { url; checkout_path })
      | None, Some _url ->
          Error.abort
            "Remote arg is not a valid git remote, expected something like \
             http[s]://website.com/.../<REPONAME>.git"
      | Some _, Some _ ->
          Error.abort
            "Cannot use both --project-root and --remote at the same time"
      | _ -> None
    in
    let explicit_targets =
      (* This is for determining whether a target path appears on the command
         line. As long as this holds, it's ok to include folders. *)
      target_roots
      |> List_.map Scanning_root.to_fpath
      |> Find_targets.Explicit_targets.of_list
    in

    let output_format =
      let all_flags =
        [ json; emacs; vim; sarif; gitlab_sast; gitlab_secrets; junit_xml ]
      in
      let cnt =
        all_flags |> List.fold_left (fun acc b -> if b then acc + 1 else acc) 0
      in
      if cnt >= 2 then
        (* TOPORT: list the possibilities *)
        Error.abort
          "Mutually exclusive options --json/--emacs/--vim/--sarif/...";
      match () with
      | _ when text -> Output_format.Text
      | _ when json -> Output_format.Json
      | _ when emacs -> Output_format.Emacs
      | _ when vim -> Output_format.Vim
      | _ when sarif -> Output_format.Sarif
      | _ when gitlab_sast -> Output_format.Gitlab_sast
      | _ when gitlab_secrets -> Output_format.Gitlab_secrets
      | _ when junit_xml -> Output_format.Junit_xml
      | _else_ -> default.output_conf.output_format
    in
    let output_conf : Output.conf =
      {
        nosem;
        autofix;
        dryrun;
        strict;
        force_color;
        output_format;
        max_chars_per_line;
        max_lines_per_finding;
        logging_level = common.logging_level;
      }
    in

    let engine_type =
      (* This first bit just rules out mutually exclusive options. *)
      if oss && secrets then
        Error.abort
          "Mutually exclusive options --oss/--beta-testing-secrets-enabled";
      if
        [ oss; pro_lang; pro_intrafile; pro ]
        |> List.filter Fun.id |> List.length > 1
      then
        Error.abort
          "Mutually exclusive options \
           --oss/--pro-languages/--pro-intrafile/--pro";
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
              }
    in
    let explicit_analyzer = Option.map Xlang.of_string lang in
    let rules_source =
      match (config, (pattern, explicit_analyzer, replacement)) with
      (* ugly: when using --dump-ast, we can pass a pattern or a target,
       * but in the case of a target that means there is no config
       * but we still don't want to abort, hence this empty Configs.
       * Same for --version, --show-supported-langages, etc., hence
       * this ugly special case returning an empty Configs.
       *)
      | [], (None, _, _)
        when dump_ast || dump_engine_path || validate || test || version
             || show_supported_languages ->
          Rules_source.Configs []
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
          match common.maturity with
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
    in
    let core_runner_conf =
      {
        Core_runner.num_jobs;
        optimizations;
        timeout;
        timeout_threshold;
        max_memory_mb;
        ast_caching;
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
    let targeting_conf : Find_targets.conf =
      {
        project_root;
        exclude = exclude_;
        include_;
        baseline_commit;
        diff_depth;
        max_target_bytes;
        always_select_explicit_targets =
          scan_unknown_extensions || imply_always_select_explicit_targets;
        explicit_targets;
        respect_gitignore;
      }
    in
    let rule_filtering_conf =
      {
        Rule_filtering.exclude_rule_ids =
          List_.map Rule_ID.of_string exclude_rule_ids;
        severity;
        exclude_products = [];
      }
    in

    (* ugly: dump should be a separate subcommand.
     * alt: we could move this code in a Dump_subcommand.validate_cli_args()
     *)
    let show =
      match () with
      | _ when dump_ast -> (
          let target_roots =
            if target_roots =*= default.target_roots then [] else target_roots
          in
          match (pattern, lang, target_roots) with
          | Some str, Some lang_str, [] ->
              Some
                {
                  Show.show_kind =
                    Show.DumpPattern (str, Lang.of_string lang_str);
                  json;
                }
          | None, Some lang_str, [ file ] ->
              Some
                {
                  Show.show_kind =
                    Show.DumpAST
                      (Scanning_root.to_fpath file, Lang.of_string lang_str);
                  json;
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
          Some { Show.show_kind = Show.DumpEnginePath pro; json }
      | _ when dump_command_for_core ->
          Some { Show.show_kind = Show.DumpCommandForCore; json }
      | _ when show_supported_languages ->
          Some { Show.show_kind = Show.SupportedLanguages; json }
      | _else_ -> None
    in
    (* ugly: validate should be a separate subcommand.
     * alt: we could move this code in a Validate_subcommand.cli_args()
     *)
    let validate =
      if validate then
        match rules_source with
        | Configs [] ->
            (* TOPORT? was a Logs.err but seems better as an abort *)
            Error.abort
              "Nothing to validate, use the --config or --pattern flag to \
               specify a rule"
        | Configs (_ :: _)
        | Pattern _ ->
            Some { Validate_subcommand.rules_source; core_runner_conf; common }
      else None
    in
    (* ugly: test should be a separate subcommand *)
    let test =
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
              strict;
              json;
              optimizations;
              ignore_todo = test_ignore_todo;
              common;
            }
      else None
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
    {
      rules_source;
      target_roots;
      rule_filtering_conf;
      targeting_conf;
      core_runner_conf;
      error_on_findings = error;
      metrics;
      registry_caching;
      version_check;
      output;
      output_conf;
      engine_type;
      rewrite_rule_ids;
      common;
      (* ugly: *)
      version;
      show;
      validate;
      test;
      trace;
      ls;
    }
  in
  (* Term defines 'const' but also the '$' operator *)
  Term.(
    (* !the o_xxx must be in alphabetic orders to match the parameters of
     * combine above! *)
    const combine $ o_allow_untrusted_validators $ o_ast_caching $ o_autofix
    $ o_baseline_commit $ CLI_common.o_common $ o_config $ o_dataflow_traces
    $ o_diff_depth $ o_dryrun $ o_dump_ast $ o_dump_command_for_core
    $ o_dump_engine_path $ o_emacs $ o_error $ o_exclude $ o_exclude_rule_ids
    $ o_force_color $ o_gitlab_sast $ o_gitlab_secrets $ o_include $ o_json
    $ o_junit_xml $ o_lang $ o_ls $ o_matching_explanations
    $ o_max_chars_per_line $ o_max_lines_per_finding $ o_max_memory_mb
    $ o_max_target_bytes $ o_metrics $ o_num_jobs $ o_no_secrets_validation
    $ o_nosem $ o_optimizations $ o_oss $ o_output $ o_pattern $ o_pro
    $ o_project_root $ o_pro_intrafile $ o_pro_languages $ o_registry_caching
    $ o_remote $ o_replacement $ o_respect_gitignore $ o_rewrite_rule_ids
    $ o_sarif $ o_scan_unknown_extensions $ o_secrets $ o_severity
    $ o_show_supported_languages $ o_strict $ o_target_roots $ o_test
    $ Test_CLI.o_test_ignore_todo $ o_text $ o_time $ o_timeout
    $ o_timeout_interfile $ o_timeout_threshold $ o_trace $ o_validate
    $ o_version $ o_version_check $ o_vim)

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

let parse_argv (argv : string array) : conf =
  let cmd : conf Cmd.t =
    Cmd.v cmdline_info (cmdline_term ~allow_empty_config:false)
  in
  CLI_common.eval_value ~argv cmd
