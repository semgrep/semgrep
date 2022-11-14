open Common

(* Provide 'Arg', 'Cmd', 'Manpage', and 'Term' modules. *)
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

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)
(*
   The result of parsing a 'semgrep scan' command.

   Field order: alphabetic.
   This facilitates insertion, deduplication, and removal of options.
*)
type conf = {
  autofix : bool;
  baseline_commit : string option;
  dryrun : bool;
  exclude : string list;
  exclude_rule_ids : string list;
  force_color : bool;
  include_ : string list;
  (* mix of --debug, --quiet, --verbose *)
  logging_level : Logs.level option;
  max_memory_mb : int;
  max_target_bytes : int;
  metrics : Metrics.State.t;
  num_jobs : int;
  optimizations : bool;
  (* mix of --json, --emacs, --vim, etc. *)
  output_format : Output_format.t;
  respect_git_ignore : bool;
  rewrite_rule_ids : bool;
  (* mix of --pattern/--lang, --config *)
  rules_source : rules_source;
  scan_unknown_extensions : bool;
  severity : Severity.rule_severity list;
  show_supported_languages : bool;
  strict : bool;
  target_roots : string list;
  time_flag : bool;
  timeout : float;
  timeout_threshold : int;
  version : bool;
  version_check : bool;
}

and rules_source =
  (* -e and -l *)
  | Pattern of string * Xlang.t
  (* --config *)
  | Configs of string list
[@@deriving show]

let default : conf =
  {
    autofix = false;
    baseline_commit = None;
    rules_source = Configs [ "auto" ];
    dryrun = false;
    exclude = [];
    exclude_rule_ids = [];
    force_color = false;
    include_ = [];
    max_memory_mb = 0;
    max_target_bytes = 1_000_000 (* 1 MB *);
    metrics = Metrics.State.Auto;
    num_jobs = Parmap_helpers.get_cpu_count ();
    optimizations = true;
    output_format = Output_format.Text;
    logging_level = Some Logs.Warning;
    respect_git_ignore = true;
    rewrite_rule_ids = true;
    scan_unknown_extensions = false;
    severity = [];
    show_supported_languages = false;
    strict = false;
    target_roots = [ "." ];
    time_flag = false;
    timeout = 30.0 (* seconds *);
    timeout_threshold = 3;
    version = false;
    version_check = true;
  }

(*************************************************************************)
(* Helpers *)
(*************************************************************************)

let _validate_lang option lang_str =
  match lang_str with
  | None -> failwith (spf "%s and -l/--lang must both be specified" option)
  | Some lang -> lang

(*************************************************************************)
(* Command-line flags *)
(*************************************************************************)

(* ------------------------------------------------------------------ *)
(* No group *)
(* ------------------------------------------------------------------ *)

let o_autofix : bool Term.t =
  H.negatable_flag [ "a"; "autofix" ] ~neg_options:[ "no-autofix" ]
    ~default:default.autofix
    ~doc:
      {|Apply autofix patches. WARNING: data loss can occur with this flag.
Make sure your files are stored in a version control system. Note that
this mode is experimental and not guaranteed to function properly.
|}

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

(* TOPORT? there's also a --disable-metrics and --enable-metrics
 * but they are marked as legacy flags, so maybe not worth porting
 *)
let o_metrics : Metrics.State.t Term.t =
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
  Arg.value (Arg.opt Metrics.State.converter default.metrics info)

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
specifying the language with '-l javascript' migh preselect files
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
    Arg.info
      [ "max-target-bytes" ]
      (* TOPORT: should use default.max_target_bytes in message *)
      ~doc:
        {|Maximum size for a file to be scanned by Semgrep, e.g
'1.5MB'. Any input program larger than this will be ignored. A zero or
negative value disables this filter. Defaults to 1000000 bytes.
|}
  in
  (* TOPORT: support '1.5MB' and such, see bytesize.py *)
  Arg.value (Arg.opt Arg.int default.max_target_bytes info)

let o_respect_git_ignore : bool Term.t =
  H.negatable_flag [ "use-git-ignore" ] ~neg_options:[ "no-git-ignore" ]
    ~default:default.respect_git_ignore
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
    ~default:default.scan_unknown_extensions
    ~doc:
      {|If true, explicit files will be scanned using the language specified
in --lang. If --skip-unknown-extensions, these files will not be scanned.
|}

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
  Arg.value (Arg.opt Arg.int default.num_jobs info)

let o_max_memory_mb : int Term.t =
  let info =
    Arg.info [ "max-memory-mb" ]
      ~doc:
        {|Maximum system memory to use running a rule on a single file
in MB. If set to 0 will not have memory limit. Defaults to 0.
|}
  in
  Arg.value (Arg.opt Arg.int default.max_memory_mb info)

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
  Arg.value (Arg.opt converter default.optimizations info)

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
  Arg.value (Arg.opt Arg.float default.timeout info)

let o_timeout_threshold : int Term.t =
  let info =
    Arg.info [ "timeout-threshold" ]
      ~doc:
        {|Maximum number of rules that can time out on a file before
the file is skipped. If set to 0 will not have limit. Defaults to 3.
|}
  in
  Arg.value (Arg.opt Arg.int default.timeout_threshold info)

let o_version_check : bool Term.t =
  H.negatable_flag [ "enable-version-check" ]
    ~neg_options:[ "disable-version-check" ]
    ~default:default.version_check
    ~env:(Cmd.Env.info "SEMGREP_ENABLE_VERSION_CHECK")
    ~doc:
      {|Checks Semgrep servers to see if the latest version is run; disabling
 this may reduce exit time after returning results.
|}

(* ------------------------------------------------------------------ *)
(* TOPORT "Display options" *)
(* ------------------------------------------------------------------ *)

(* alt: could use Fmt_cli.style_renderer, which supports --color=xxx but
 * better be backward compatible with how semgrep was doing it before
 *)
let o_force_color : bool Term.t =
  H.negatable_flag [ "force-color" ] ~neg_options:[ "no-force-color" ]
    ~default:default.force_color
      (* TOPORT? need handle SEMGREP_COLOR_NO_COLOR or NO_COLOR
       * # https://no-color.org/
       *)
    ~env:(Cmd.Env.info "SEMGREP_FORCE_COLOR")
    ~doc:
      {|Always include ANSI color in the output, even if not writing to
a TTY; defaults to using the TTY status
|}

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

(* ------------------------------------------------------------------ *)
(* TOPORT "Verbosity options" *)
(* ------------------------------------------------------------------ *)
(* alt: we could use Logs_cli.level(), but by defining our own flags
 * we can give better ~doc:. We lose the --verbosity=Level though.
 *)
let o_quiet : bool Term.t =
  let info = Arg.info [ "q"; "quiet" ] ~doc:{|Only output findings.|} in
  Arg.value (Arg.flag info)

let o_verbose : bool Term.t =
  let info =
    Arg.info [ "v"; "verbose" ]
      ~doc:
        {|Show more details about what rules are running, which files
failed to parse, etc.
|}
  in
  Arg.value (Arg.flag info)

let o_debug : bool Term.t =
  let info =
    Arg.info [ "debug" ]
      ~doc:{|All of --verbose, but with additional debugging information.|}
  in
  Arg.value (Arg.flag info)

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

let o_show_supported_languages : bool Term.t =
  let info =
    Arg.info
      [ "show-supported-languages" ]
      ~doc:
        {|Print a list of languages that are currently supported by Semgrep.|}
  in
  Arg.value (Arg.flag info)

(* ------------------------------------------------------------------ *)
(* TOPORT "Test and debug options" *)
(* ------------------------------------------------------------------ *)
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
  Arg.value (Arg.pos_all Arg.string default.target_roots info)

(*****************************************************************************)
(* Turn argv into a conf *)
(*****************************************************************************)

let cmdline_term : conf Term.t =
  let combine autofix baseline_commit config debug dryrun emacs exclude
      exclude_rule_ids force_color include_ json lang max_memory_mb
      max_target_bytes metrics num_jobs optimizations pattern quiet
      respect_git_ignore rewrite_rule_ids scan_unknown_extensions severity
      show_supported_languages strict target_roots time_flag timeout
      timeout_threshold verbose version version_check vim =
    let logging_level =
      match (verbose, debug, quiet) with
      | false, false, false -> Some Logs.Warning
      | true, false, false -> Some Logs.Info
      | false, true, false -> Some Logs.Debug
      | false, false, true -> None
      | _else_ ->
          (* TOPORT: list the possibilities *)
          Error.abort "mutually exclusive options --quiet/--verbose/--debug"
    in
    (* ugly: call setup_logging ASAP so the Logs.xxx below are displayed
     * correctly *)
    Logs_helpers.setup_logging ~force_color ~level:logging_level;

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
    let rules_source =
      match (config, pattern, lang) with
      (* TODO? report an error if no config given? *)
      | [], None, None -> default.rules_source
      | [], Some pat, Some str ->
          (* may raise Failure *)
          let xlang = Xlang.of_string str in
          Pattern (pat, xlang)
      | _, Some _, None ->
          Error.abort "-e/--pattern and -l/--lang must both be specified"
      | _, None, Some _ ->
          (* stricter: error not detected in original semgrep *)
          Error.abort "-e/--pattern and -l/--lang must both be specified"
      | xs, None, None -> Configs xs
      | _ :: _, Some _, _ ->
          Error.abort "Mutually exclusive options --config/--pattern"
    in
    (* sanity checks *)
    if List.mem "auto" config && metrics = Metrics.State.Off then
      Error.abort
        "Cannot create auto config when metrics are off. Please allow metrics \
         or run with a specific config.";

    (* warnings *)
    if include_ <> [] && exclude <> [] then
      Logs.warn (fun m ->
          m
            "Paths that match both --include and --exclude will be skipped by \
             Semgrep.");

    {
      autofix;
      baseline_commit;
      rules_source;
      dryrun;
      exclude_rule_ids;
      exclude;
      force_color;
      include_;
      logging_level;
      max_memory_mb;
      max_target_bytes;
      metrics;
      num_jobs;
      optimizations;
      output_format;
      respect_git_ignore;
      rewrite_rule_ids;
      scan_unknown_extensions;
      severity;
      show_supported_languages;
      strict;
      target_roots;
      time_flag;
      timeout;
      timeout_threshold;
      version;
      version_check;
    }
  in
  (* Term defines 'const' but also the '$' operator *)
  Term.(
    const combine $ o_autofix $ o_baseline_commit $ o_config $ o_debug
    $ o_dryrun $ o_emacs $ o_exclude $ o_exclude_rule_ids $ o_force_color
    $ o_include $ o_json $ o_lang $ o_max_memory_mb $ o_max_target_bytes
    $ o_metrics $ o_num_jobs $ o_optimizations $ o_pattern $ o_quiet
    $ o_respect_git_ignore $ o_rewrite_rule_ids $ o_scan_unknown_extensions
    $ o_severity $ o_show_supported_languages $ o_strict $ o_target_roots
    $ o_time $ o_timeout $ o_timeout_threshold $ o_verbose $ o_version
    $ o_version_check $ o_vim)

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
       server if you pull your configuration from the Semgrep registy. To \
       learn more about how and why these metrics are collected, please see \
       https://semgrep.dev/docs/metrics. To modify this behavior, see the \
       --metrics option below.";
  ]
  @ CLI_common.help_page_bottom

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_argv (argv : string array) : (conf, Exit_code.t) result =
  let info : Cmd.info = Cmd.info "semgrep scan" ~doc ~man in
  let cmd : conf Cmd.t = Cmd.v info cmdline_term in
  CLI_common.eval_value ~argv cmd
