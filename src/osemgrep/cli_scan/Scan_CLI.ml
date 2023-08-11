open Common

(* Provides the 'Arg', 'Cmd', 'Manpage', and 'Term' modules. *)
open Cmdliner
module H = Cmdliner_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep scan' command-line arguments processing.

   Translated partially from scan.py

   TOPORT? all those shell_complete() click functions?
*)

(* TODO: use parser/printer pair for file paths using Fpath.t so that
   we don't have to convert manually from string to fpath for each
   file option offered by the CLI. Add it to CLI_common. *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)
(*
   The result of parsing a 'semgrep scan' command.

   LATER: we could actually define this structure in ATD, so people could
   programmatically set the command-line arguments they want if they
   want to programmatically call Semgrep.
*)
type conf = {
  (* Main configuration options *)
  (* mix of --pattern/--lang/--replacement, --config *)
  rules_source : Rules_source.t;
  (* can be a list of files or directories *)
  target_roots : Fpath.t list;
  (* Rules/targets refinements *)
  rule_filtering_conf : Rule_filtering.conf;
  targeting_conf : Find_targets.conf;
  (* Other configuration options *)
  nosem : bool;
  autofix : bool;
  dryrun : bool;
  error_on_findings : bool;
  strict : bool;
  rewrite_rule_ids : bool;
  time_flag : bool;
  (* Engine selection *)
  engine_type : Engine_type.t;
  (* Performance options *)
  core_runner_conf : Core_runner.conf;
  (* Display options *)
  (* mix of --json, --emacs, --vim, etc. *)
  output_format : Output_format.t;
  force_color : bool;
  max_chars_per_line : int;
  max_lines_per_finding : int;
  (* Networking options *)
  metrics : Metrics_.config;
  registry_caching : bool; (* similar to core_runner_conf.ast_caching *)
  version_check : bool;
  common : CLI_common.conf;
  (* Ugly: should be in separate subcommands *)
  version : bool;
  show_supported_languages : bool;
  skip_postprocessor_rules : bool;
  dump : Dump_subcommand.conf option;
  validate : Validate_subcommand.conf option;
  test : Test_subcommand.conf option;
}
[@@deriving show]

(* We could split the content of this variable in different files, e.g.,
 * targeting_conf default could be move in a Find_targets.default, but
 * it's also nice to have everything in one place.
 *)
let default : conf =
  {
    (* alt: Configs [ "auto" ]? *)
    rules_source = Configs [];
    target_roots = [ Fpath.v "." ];
    (* alt: could move in a Target_manager.default *)
    targeting_conf =
      {
        (* the project root is inferred from the presence of .git, otherwise
           falls back to the current directory. Should it be offered as
           a command-line option? In osemgrep, a .semgrepignore at the
           git project root will be honored unlike in legacy semgrep
           if we're in a subfolder. *)
        Find_targets.project_root = None;
        exclude = [];
        include_ = None;
        baseline_commit = None;
        max_target_bytes = 1_000_000 (* 1 MB *);
        respect_git_ignore = true;
        scan_unknown_extensions = false;
      };
    (* alt: could move in a Rule_filtering.default *)
    rule_filtering_conf =
      { Rule_filtering.exclude_rule_ids = []; severity = [] };
    (* alt: could move in a Core_runner.default *)
    core_runner_conf =
      {
        (* Maxing out number of cores used to 16 if more not requested to
         * not overload on large machines
         *)
        Core_runner.num_jobs = min 16 (Parmap_helpers.get_cpu_count ());
        timeout = 30.0 (* seconds *);
        timeout_threshold = 3;
        max_memory_mb = 0;
        optimizations = true;
        (* better to set to false for now; annoying to add --ast-caching to
         * each command, but while we're still developing osemgrep it is
         * better to eliminate some source of complexity by default.
         *)
        ast_caching = false;
      };
    autofix = false;
    dryrun = false;
    error_on_findings = false;
    strict = false;
    (* could be move in CLI_common.default_conf? *)
    common =
      {
        profile = false;
        logging_level = Some Logs.Warning;
        (* or set to Experimental by default when we release osemgrep? *)
        maturity = None;
      };
    time_flag = false;
    engine_type = OSS;
    output_format = Output_format.Text;
    force_color = false;
    max_chars_per_line = 160;
    max_lines_per_finding = 10;
    rewrite_rule_ids = true;
    metrics = Metrics_.Auto;
    (* like maturity, should maybe be set to false when we release osemgrep *)
    registry_caching = false;
    version_check = true;
    (* ugly: should be separate subcommands *)
    version = false;
    show_supported_languages = false;
    skip_postprocessor_rules = false;
    dump = None;
    validate = None;
    test = None;
    nosem = true;
  }

(*************************************************************************)
(* Helpers *)
(*************************************************************************)

(*************************************************************************)
(* Command-line flags *)
(*************************************************************************)
(* The o_ below stands for option (as in command-line argument option) *)

(* ------------------------------------------------------------------ *)
(* Networking related options (New) *)
(* ------------------------------------------------------------------ *)

(* TOPORT? there's also a --disable-metrics and --enable-metrics
 * but they are marked as legacy flags, so maybe not worth porting
 *)
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
(* TOPORT "Path options" *)
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
  let info =
    Arg.info [ "max-target-bytes" ]
      ~doc:
        ({|Maximum size for a file to be scanned by Semgrep, e.g
'1.5MB'. Any input program larger than this will be ignored. A zero or
negative value disables this filter. Defaults to |}
        ^ string_of_int default.targeting_conf.max_target_bytes
        ^ {| bytes.
|})
  in
  Arg.value
    (Arg.opt Cmdliner_helpers.number_of_bytes_converter
       default.targeting_conf.max_target_bytes info)

let o_respect_git_ignore : bool Term.t =
  H.negatable_flag [ "use-git-ignore" ] ~neg_options:[ "no-git-ignore" ]
    ~default:default.targeting_conf.respect_git_ignore
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
  H.negatable_flag
    [ "scan-unknown-extensions" ]
    ~neg_options:[ "skip-unknown-extensions" ]
    ~default:default.targeting_conf.scan_unknown_extensions
    ~doc:
      {|If true, explicit files will be scanned using the language specified
in --lang. If --skip-unknown-extensions, these files will not be scanned.
|}

(* alt: could be put in the Display options with nosem *)
let o_baseline_commit : string option Term.t =
  let info =
    Arg.info [ "baseline_commit" ]
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

(* ------------------------------------------------------------------ *)
(* TOPORT: "Performance and memory options" *)
(* ------------------------------------------------------------------ *)

let o_num_jobs : int Term.t =
  let info =
    Arg.info [ "j"; "jobs" ]
      ~doc:
        {|Number of subprocesses to use to run checks in
parallel. Defaults to the number of cores detected on the system.
|}
  in
  Arg.value (Arg.opt Arg.int default.core_runner_conf.num_jobs info)

let o_max_memory_mb : int Term.t =
  let info =
    Arg.info [ "max-memory-mb" ]
      ~doc:
        {|Maximum system memory to use running a rule on a single file
in MB. If set to 0 will not have memory limit. Defaults to 0.
|}
  in
  Arg.value (Arg.opt Arg.int default.core_runner_conf.max_memory_mb info)

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
  let info =
    Arg.info
      [ "timeout" ] (* TOPORT: use default value in doc. switch to int? *)
      ~doc:
        {|Maximum time to spend running a rule on a single file in
seconds. If set to 0 will not have time limit. Defaults to 30 s.
|}
  in
  (*TOPORT: envvar="SEMGREP_TIMEOUT" *)
  Arg.value (Arg.opt Arg.float default.core_runner_conf.timeout info)

let o_timeout_threshold : int Term.t =
  let info =
    Arg.info [ "timeout-threshold" ]
      ~doc:
        {|Maximum number of rules that can time out on a file before
the file is skipped. If set to 0 will not have limit. Defaults to 3.
|}
  in
  Arg.value (Arg.opt Arg.int default.core_runner_conf.timeout_threshold info)

(* ------------------------------------------------------------------ *)
(* TOPORT "Display options" *)
(* ------------------------------------------------------------------ *)

(* alt: could use Fmt_cli.style_renderer, which supports --color=xxx but
 * better be backward compatible with how semgrep was doing it before
 *)
let o_force_color : bool Term.t =
  H.negatable_flag_with_env [ "force-color" ] ~neg_options:[ "no-force-color" ]
    ~default:default.force_color
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
  Arg.value (Arg.opt Arg.int default.max_chars_per_line info)

let o_max_lines_per_finding : int Term.t =
  let info =
    Arg.info
      [ "max-lines-per-finding" ]
      ~doc:
        {|Maximum number of lines of code that will be shown for each match before
trimming (set to 0 for unlimited).|}
  in
  Arg.value (Arg.opt Arg.int default.max_lines_per_finding info)

let o_rewrite_rule_ids : bool Term.t =
  H.negatable_flag [ "rewrite-rule-ids" ] ~neg_options:[ "no-rewrite-rule-ids" ]
    ~default:default.rewrite_rule_ids
    ~doc:
      {|Rewrite rule ids when they appear in nested sub-directories
(Rule 'foo' in test/rules.yaml will be renamed 'test.foo').
|}

let o_time : bool Term.t =
  H.negatable_flag [ "time" ] ~neg_options:[ "no-time" ]
    ~default:default.time_flag
    ~doc:
      {|Include a timing summary with the results. If output format is json,
 provides times for each pair (rule, target).
|}

let o_nosem : bool Term.t =
  H.negatable_flag ~default:true [ "enable-nosem" ]
    ~neg_options:[ "disable-nosem" ]
    ~doc:
      {|Enables 'nosem'. Findings will not be reported on lines containing
          a 'nosem' comment at the end. Enabled by default.|}

(* ------------------------------------------------------------------ *)
(* TOPORT "Output formats" (mutually exclusive) *)
(* ------------------------------------------------------------------ *)
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

(* ------------------------------------------------------------------ *)
(* TOPORT "Engine type" (mutually exclusive) *)
(* ------------------------------------------------------------------ *)

let o_oss : bool Term.t =
  let info = Arg.info [ "oss" ] ~doc:{|Run with the OSS engine.|} in
  Arg.value (Arg.flag info)

let o_pro_languages : bool Term.t =
  let info =
    Arg.info [ "pro-languages" ]
      ~doc:
        {|Run a language only supported by the pro engine without extra analysis features.|}
  in
  Arg.value (Arg.flag info)

let o_pro_intrafile : bool Term.t =
  let info =
    Arg.info [ "pro-intrafile" ]
      ~doc:{|Run with intrafile cross-function analysis.|}
  in
  Arg.value (Arg.flag info)

let o_pro : bool Term.t =
  let info =
    Arg.info [ "pro" ]
      ~doc:{|Run with all pro engine features including cross-file analysis.|}
  in
  Arg.value (Arg.flag info)

(* ------------------------------------------------------------------ *)
(* TOPORT "Configuration options" *)
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
    ~default:default.dryrun
    ~doc:
      {|If --dryrun, does not write autofixes to a file. This will print the changes
to the console. This lets you see the changes before you commit to them. Only
works with the --autofix flag. Otherwise does nothing.
|}

let o_severity : Severity.rule_severity list Term.t =
  let info =
    Arg.info [ "severity" ]
      ~doc:
        {|Report findings only from rules matching the supplied severity
level. By default all applicable rules are run. Can add multiple times.
Each should be one of INFO, WARNING, or ERROR.
|}
  in
  Arg.value (Arg.opt_all Severity.converter [] info)

let o_exclude_rule_ids : string list Term.t =
  let info =
    Arg.info [ "exclude-rule" ]
      ~doc:{|Skip any rule with the given id. Can add multiple times.|}
  in
  Arg.value (Arg.opt_all Arg.string [] info)

(* ------------------------------------------------------------------ *)
(* TOPORT "Alternate modes" *)
(* ------------------------------------------------------------------ *)
(* TOPORT: "No search is performed in these modes" *)

let o_version : bool Term.t =
  let info = Arg.info [ "version" ] ~doc:{|Show the version and exit.|} in
  Arg.value (Arg.flag info)

(* ugly: this should be a separate subcommand, not a flag of semgrep scan,
 * like 'semgrep info supported-languages'
 *)
let o_show_supported_languages : bool Term.t =
  let info =
    Arg.info
      [ "show-supported-languages" ]
      ~doc:
        {|Print a list of languages that are currently supported by Semgrep.|}
  in
  Arg.value (Arg.flag info)

let o_skip_postprocessor_rules : bool Term.t =
  let info =
    Arg.info [ "show-supported-languages" ] ~doc:{|Skip post processor rules.|}
  in
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

(* ------------------------------------------------------------------ *)
(* TOPORT "Test and debug options" *)
(* ------------------------------------------------------------------ *)

(* alt: could be in the "alternate modes" section
 * ugly: this should be a separate subcommand, not a flag of semgrep scan
 *)
let o_test : bool Term.t =
  let info = Arg.info [ "test" ] ~doc:{|Run test suite.|} in
  Arg.value (Arg.flag info)

let o_test_ignore_todo : bool Term.t =
  H.negatable_flag [ "test-ignore-todo" ] ~neg_options:[ "no-test-ignore-todo" ]
    ~default:false
    ~doc:
      {|If --test-ignore-todo, ignores rules marked as '#todoruleid:' in
test files.
|}

(* alt: in configuration option *)
let o_error : bool Term.t =
  H.negatable_flag [ "error" ] ~neg_options:[ "no-error" ]
    ~default:default.error_on_findings
    ~doc:{|Exit 1 if there are findings. Useful for CI and scripts.|}

let o_strict : bool Term.t =
  H.negatable_flag [ "strict" ] ~neg_options:[ "no-strict" ]
    ~default:default.strict
    ~doc:
      {|Return a nonzero exit code when WARN level errors are encountered.
Fails early if invalid configuration files are present.
Defaults to --no-strict.
|}

(* ------------------------------------------------------------------ *)
(* Positional arguments *)
(* ------------------------------------------------------------------ *)

let o_target_roots : string list Term.t =
  let info =
    Arg.info [] ~docv:"TARGETS"
      ~doc:{|Files or folders to be scanned by semgrep.|}
  in
  Arg.value
    (Arg.pos_all Arg.string (default.target_roots |> File.Path.to_strings) info)

(* ------------------------------------------------------------------ *)
(* !!NEW arguments!! not in pysemgrep *)
(* ------------------------------------------------------------------ *)

let o_dump_config : string option Term.t =
  let info = Arg.info [ "dump-config" ] ~doc:{|<undocumented>|} in
  Arg.value (Arg.opt Arg.(some string) None info)

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

let o_ast_caching : bool Term.t =
  H.negatable_flag [ "ast-caching" ] ~neg_options:[ "no-ast-caching" ]
    ~default:default.core_runner_conf.ast_caching
    ~doc:{|Store in ~/.semgrep/cache/asts/ the parsed ASTs to speedup things.|}

(* TODO: add also an --offline flag? what about metrics? *)
let o_registry_caching : bool Term.t =
  H.negatable_flag [ "registry-caching" ] ~neg_options:[ "no-registry-caching" ]
    ~default:default.registry_caching
    ~doc:{|Cache for 24 hours in ~/.semgrep/cache rules from the registry.|}

(*****************************************************************************)
(* Turn argv into a conf *)
(*****************************************************************************)

let cmdline_term ~allow_empty_config : conf Term.t =
  (* !The parameters must be in alphabetic orders to match the order
   * of the corresponding '$ o_xx $' further below! *)
  let combine ast_caching autofix baseline_commit common config dryrun dump_ast
      dump_config emacs error exclude exclude_rule_ids force_color include_ json
      lang max_chars_per_line max_lines_per_finding max_memory_mb
      max_target_bytes metrics num_jobs nosem optimizations oss pattern pro
      project_root pro_intrafile pro_lang registry_caching replacement
      respect_git_ignore rewrite_rule_ids scan_unknown_extensions severity
      show_supported_languages skip_postprocessor_rules strict target_roots test
      test_ignore_todo time_flag timeout timeout_threshold validate version
      version_check vim =
    (* ugly: call setup_logging ASAP so the Logs.xxx below are displayed
     * correctly *)
    Logs_helpers.setup_logging ~force_color
      ~level:common.CLI_common.logging_level;

    (* to remove at some point *)
    let registry_caching, ast_caching =
      match common.maturity with
      | Some CLI_common.MDevelop -> (registry_caching, ast_caching)
      | None
      | Some (CLI_common.MExperimental | CLI_common.MLegacy) ->
          Logs.debug (fun m ->
              m "disabling registry and AST caching unless --develop");
          (false, false)
    in
    let include_ =
      match include_ with
      | [] -> None
      | nonempty -> Some nonempty
    in
    let target_roots = target_roots |> File.Path.of_strings in

    let output_format =
      match (json, emacs, vim) with
      | false, false, false -> default.output_format
      | true, false, false -> Output_format.Json
      | false, true, false -> Output_format.Emacs
      | false, false, true -> Output_format.Vim
      | _else_ ->
          (* TOPORT: list the possibilities *)
          Error.abort "Mutually exclusive options --json/--emacs/--vim"
    in
    let engine_type =
      match (oss, pro_lang, pro_intrafile, pro) with
      | false, false, false, false -> default.engine_type
      | true, false, false, false -> OSS
      | false, true, false, false -> PRO Engine_type.Language_only
      | false, false, true, false -> PRO Engine_type.Intrafile
      | false, false, false, true -> PRO Engine_type.Interfile
      | _else_ ->
          (* TOPORT: list the possibilities *)
          Error.abort
            "Mutually exclusive options \
             --oss/--pro-languages/--pro-intrafile/--pro"
    in
    let rules_source =
      match (config, (pattern, lang, replacement)) with
      (* ugly: when using --dump-ast, we can pass a pattern or a target,
       * but in the case of a target that means there is no config
       * but we still don't want to abort, hence this empty Configs.
       * Same for --version, --show-supported-langages, hence
       * this ugly special case returning an empty Configs.
       *)
      | [], (None, _, _)
        when dump_ast || dump_config <> None || validate || test || version
             || show_supported_languages ->
          Rules_source.Configs []
      (* TOPORT: handle get_project_url() if empty Configs? *)
      | [], (None, _, _) ->
          (* alt: default.rules_source *)
          (* TOPORT: raise with Exit_code.missing_config *)
          (* TOPORT? use instead
             "No config given and {DEFAULT_CONFIG_FILE} was not found. Try running with --help to debug or if you want to download a default config, try running with --config r2c" *)
          if allow_empty_config then Rules_source.Configs []
          else
            Error.abort
              "No config given. Run with `--config auto` or see \
               https://semgrep.dev/docs/running-rules/ for instructions on \
               running with a specific config"
      | [], (Some pat, Some str, fix) ->
          (* may raise a Failure (will be caught in CLI.safe_run) *)
          let xlang = Xlang.of_string str in
          Rules_source.Pattern (pat, Some xlang, fix)
      | _, (Some pat, None, fix) -> (
          match common.maturity with
          (* osemgrep-only: better: can use -e without -l! *)
          | Some CLI_common.MDevelop -> Rules_source.Pattern (pat, None, fix)
          | None
          | Some (CLI_common.MExperimental | CLI_common.MLegacy) ->
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
      }
    in
    let targeting_conf =
      {
        Find_targets.project_root = Option.map Fpath.v project_root;
        exclude;
        include_;
        baseline_commit;
        max_target_bytes;
        scan_unknown_extensions;
        respect_git_ignore;
      }
    in
    let rule_filtering_conf =
      {
        Rule_filtering.exclude_rule_ids =
          Common.map Rule_ID.of_string exclude_rule_ids;
        severity;
      }
    in

    (* ugly: dump should be a separate subcommand.
     * alt: we could move this code in a Dump_subcommand.validate_cli_args()
     *)
    let dump =
      match () with
      | _ when dump_ast -> (
          let target_roots =
            if target_roots =*= default.target_roots then [] else target_roots
          in
          match (pattern, lang, target_roots) with
          | Some str, Some lang_str, [] ->
              Some
                {
                  Dump_subcommand.target =
                    Dump_subcommand.Pattern (str, Lang.of_string lang_str);
                  json;
                }
          | None, Some lang_str, [ file ] ->
              Some
                {
                  Dump_subcommand.target =
                    Dump_subcommand.File (file, Lang.of_string lang_str);
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
      | _ when dump_config <> None ->
          let config = Common2.some dump_config in
          Some { Dump_subcommand.target = Dump_subcommand.Config config; json }
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
    (* ugly: test should be a separate subcommand.
     * alt: we could move this code in a Test_subcommand.cli_args()
     *)
    let test =
      if test then
        let target =
          match (target_roots, config) with
          | [ x ], [ config ] ->
              let file_str = Fpath.to_string x in
              if Sys.file_exists file_str && Sys.is_directory file_str then
                Test_subcommand.Dir (x, Some config)
              else Test_subcommand.File (x, config)
          | [ x ], [] ->
              let file_str = Fpath.to_string x in
              if Sys.is_directory file_str then Test_subcommand.Dir (x, None)
              else
                (* was raise Exception but cleaner abort I think *)
                Error.abort
                  "--config is required when running a test on single file"
          | _ :: _ :: _, _ ->
              (* stricter: better error message '(directory or file)' *)
              Error.abort
                "only one target (directory or file) allowed for tests"
          | _, _ :: _ :: _ ->
              (* stricter: removed 'config directory' *)
              Error.abort "only one config allowed for tests"
          (* target_roots should always contain at least ["."] *)
          | [], _ -> assert false
        in
        Some
          {
            Test_subcommand.target;
            strict;
            json;
            optimizations;
            ignore_todo = test_ignore_todo;
          }
      else None
    in

    (* sanity checks *)
    if List.mem "auto" config && metrics =*= Metrics_.Off then
      Error.abort
        "Cannot create auto config when metrics are off. Please allow metrics \
         or run with a specific config.";

    (* warnings *)
    if include_ <> None && exclude <> [] then
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
      autofix;
      dryrun;
      error_on_findings = error;
      force_color;
      max_chars_per_line;
      max_lines_per_finding;
      metrics;
      registry_caching;
      version_check;
      output_format;
      engine_type;
      rewrite_rule_ids;
      strict;
      time_flag;
      common;
      (* ugly: *)
      version;
      show_supported_languages;
      skip_postprocessor_rules;
      dump;
      validate;
      test;
      nosem;
    }
  in
  (* Term defines 'const' but also the '$' operator *)
  Term.(
    (* !the o_xxx must be in alphabetic orders to match the parameters of
     * combine above! *)
    const combine $ o_ast_caching $ o_autofix $ o_baseline_commit
    $ CLI_common.o_common $ o_config $ o_dryrun $ o_dump_ast $ o_dump_config
    $ o_emacs $ o_error $ o_exclude $ o_exclude_rule_ids $ o_force_color
    $ o_include $ o_json $ o_lang $ o_max_chars_per_line
    $ o_max_lines_per_finding $ o_max_memory_mb $ o_max_target_bytes $ o_metrics
    $ o_num_jobs $ o_nosem $ o_optimizations $ o_oss $ o_pattern $ o_pro
    $ o_project_root $ o_pro_intrafile $ o_pro_languages $ o_registry_caching
    $ o_replacement $ o_respect_git_ignore $ o_rewrite_rule_ids
    $ o_scan_unknown_extensions $ o_severity $ o_show_supported_languages
    $ o_skip_postprocessor_rules $ o_strict $ o_target_roots $ o_test
    $ o_test_ignore_todo $ o_time $ o_timeout $ o_timeout_threshold $ o_validate
    $ o_version $ o_version_check $ o_vim)

let doc = "run semgrep rules on files"

(* TODO: document the exit codes as defined in Exit_code.mli *)
let man : Manpage.block list =
  [
    `S Manpage.s_description;
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
