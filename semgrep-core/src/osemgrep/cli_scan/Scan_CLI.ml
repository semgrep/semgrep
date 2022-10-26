open Printf

(* Provide 'Term', 'Arg', and 'Manpage' modules. *)
open Cmdliner
module H = Cmdliner_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep scan' subcommand

   Translated from scan.py
*)

(* TOPORT: all those shell_complete() click functions? *)

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
  dryrun : bool;
  baseline_commit : string option;
  (* TOPORT: can have multiple calls to --config, so string list here *)
  config : string;
  exclude : string list;
  include_ : string list;
  lang : string option;
  logging_level : Logs.level option;
  max_memory_mb : int;
  max_target_bytes : int;
  metrics : Metrics.State.t;
  num_jobs : int;
  optimizations : bool;
  output_format : Constants.output_format;
  pattern : string option;
  respect_git_ignore : bool;
  strict : bool;
  target_roots : string list;
  timeout : float;
  timeout_threshold : int;
}

let get_cpu_count () : int =
  (* Parmap subtracts 1 from the number of detected cores.
     This comes with no guarantees. *)
  max 1 (Parmap.get_default_ncores () + 1)

let default : conf =
  {
    autofix = false;
    dryrun = false;
    baseline_commit = None;
    config = "auto";
    exclude = [];
    include_ = [];
    lang = None;
    max_memory_mb = 0;
    max_target_bytes = Constants.default_max_target_size;
    metrics = Metrics.State.Auto;
    num_jobs = get_cpu_count ();
    optimizations = true;
    output_format = Constants.Text;
    pattern = None;
    logging_level = Some Logs.Warning;
    respect_git_ignore = true;
    strict = false;
    target_roots = [ "." ];
    timeout = float_of_int Constants.default_timeout;
    timeout_threshold = 3;
  }

(*************************************************************************)
(* Various utilities *)
(*************************************************************************)

let _validate_lang option lang_str =
  match lang_str with
  | None -> failwith (sprintf "%s and -l/--lang must both be specified" option)
  | Some lang -> lang

(*************************************************************************)
(* Command-line parsing: turn argv into conf *)
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

let o_dryrun : bool Term.t =
  H.negatable_flag [ "dryrun" ] ~neg_options:[ "no-dryrun" ]
    ~default:default.dryrun
    ~doc:
      {| If --dryrun, does not write autofixes to a file. This will print the changes
to the console. This lets you see the changes before you commit to them. Only
works with the --autofix flag. Otherwise does nothing.
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

let o_metrics =
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

let o_exclude =
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

let o_include =
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

let o_max_target_bytes =
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

let o_respect_git_ignore =
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

(* ------------------------------------------------------------------ *)
(* TOPORT: "Performance and memory options" *)
(* ------------------------------------------------------------------ *)

let o_num_jobs =
  let info =
    Arg.info [ "j"; "jobs" ]
      ~doc:
        {|Number of subprocesses to use to run checks in
parallel. Defaults to the number of cores detected on the system.
|}
  in
  Arg.value (Arg.opt Arg.int default.num_jobs info)

let o_max_memory_mb =
  let info =
    Arg.info [ "max-memory-mb" ]
      ~doc:
        {|Maximum system memory to use running a rule on a single file
in MB. If set to 0 will not have memory limit. Defaults to 0.
|}
  in
  Arg.value (Arg.opt Arg.int default.max_memory_mb info)

let o_optimizations =
  let parse = function
    | "all" -> Ok true
    | "none" -> Ok false
    | other -> Error (sprintf "unsupported value %S" other)
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

let o_timeout =
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

let o_timeout_threshold =
  let info =
    Arg.info [ "timeout-threshold" ]
      ~doc:
        {|Maximum number of rules that can time out on a file before
the file is skipped. If set to 0 will not have limit. Defaults to 3.
|}
  in
  Arg.value (Arg.opt Arg.int default.timeout_threshold info)

(* ------------------------------------------------------------------ *)
(* TOPORT "Display options" *)
(* ------------------------------------------------------------------ *)

(* TODO? use Fmt_cli.style_renderer ? *)

(* ------------------------------------------------------------------ *)
(* TOPORT "Verbosity options" *)
(* ------------------------------------------------------------------ *)
(* alt: we could use Logs_cli.level(), but by defining our own flags
 * we can give better ~doc:. We lose the --verbosity=Level though.
 *)
let o_quiet =
  let info = Arg.info [ "q"; "quiet" ] ~doc:{|Only output findings.|} in
  Arg.value (Arg.flag info)

let o_verbose =
  let info =
    Arg.info [ "v"; "verbose" ]
      ~doc:
        {|Show more details about what rules are running, which files
failed to parse, etc.
|}
  in
  Arg.value (Arg.flag info)

let o_debug =
  let info =
    Arg.info [ "debug" ]
      ~doc:{|All of --verbose, but with additional debugging information.|}
  in
  Arg.value (Arg.flag info)

(* ------------------------------------------------------------------ *)
(* TOPORT "Output formats" (mutually exclusive) *)
(* ------------------------------------------------------------------ *)
let o_json =
  let info =
    Arg.info [ "json" ] ~doc:{|Output results in Semgrep's JSON format.|}
  in
  Arg.value (Arg.flag info)

let o_emacs =
  let info =
    Arg.info [ "emacs" ] ~doc:{|Output results in Emacs single-line format.|}
  in
  Arg.value (Arg.flag info)

let o_vim =
  let info =
    Arg.info [ "vim" ] ~doc:{|Output results in vim single-line format.|}
  in
  Arg.value (Arg.flag info)

(* ------------------------------------------------------------------ *)
(* TOPORT "Configuration options" *)
(* ------------------------------------------------------------------ *)
(* TOPORT: multiple = true *)
let o_config =
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
  Arg.value (Arg.opt Arg.string default.config info)

let o_pattern =
  let info =
    Arg.info [ "e"; "pattern" ]
      ~doc:
        {|Code search pattern. See https://semgrep.dev/docs/writing-rules/pattern-syntax for information on pattern features.
|}
  in
  Arg.value (Arg.opt Arg.(some string) None info)

let o_lang =
  let info =
    Arg.info [ "l"; "lang" ]
      ~doc:
        {|Parse pattern and all files in specified language.
Must be used with -e/--pattern.
|}
  in
  Arg.value (Arg.opt Arg.(some string) None info)

(* ------------------------------------------------------------------ *)
(* TOPORT "Alternate modes" *)
(* ------------------------------------------------------------------ *)

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
(* positional arguments *)
(* ------------------------------------------------------------------ *)

let o_target_roots =
  let info =
    Arg.info [] ~docv:"TARGETS"
      ~doc:{|Files or folders to be scanned by semgrep.
|}
  in
  Arg.value (Arg.pos_all Arg.string default.target_roots info)

(*****************************************************************************)
(* Subcommand 'scan' *)
(*****************************************************************************)

let cmdline_term : conf Term.t =
  let combine autofix dryrun baseline_commit config debug emacs exclude include_
      json lang max_memory_mb max_target_bytes metrics num_jobs optimizations
      pattern quiet respect_git_ignore strict target_roots timeout
      timeout_threshold verbose vim =
    let output_format =
      match (json, emacs, vim) with
      | false, false, false -> default.output_format
      | true, false, false -> Constants.Json
      | false, true, false -> Constants.Emacs
      | false, false, true -> Constants.Vim
      | _else_ ->
          (* TOPORT: list the possibilities *)
          failwith "Mutually exclusive options"
    in
    let logging_level =
      match (verbose, debug, quiet) with
      | false, false, false -> Some Logs.Warning
      | true, false, false -> Some Logs.Info
      | false, true, false -> Some Logs.Debug
      | false, false, true -> None (* TOPORT: list the possibilities *)
      | _else_ -> failwith "mutually exclusive options"
    in
    {
      autofix;
      dryrun;
      baseline_commit;
      config;
      exclude;
      include_;
      lang;
      logging_level;
      max_memory_mb;
      max_target_bytes;
      metrics;
      num_jobs;
      optimizations;
      output_format;
      pattern;
      respect_git_ignore;
      strict;
      target_roots;
      timeout;
      timeout_threshold;
    }
  in
  (* Term defines 'const' but also the '$' operator *)
  Term.(
    const combine $ o_autofix $ o_dryrun $ o_baseline_commit $ o_config
    $ o_debug $ o_emacs $ o_exclude $ o_include $ o_json $ o_lang
    $ o_max_memory_mb $ o_max_target_bytes $ o_metrics $ o_num_jobs
    $ o_optimizations $ o_pattern $ o_quiet $ o_respect_git_ignore $ o_strict
    $ o_target_roots $ o_timeout $ o_timeout_threshold $ o_verbose $ o_vim)

let doc = "run semgrep rules on files"

(* TODO: document the exit codes as defined in Error.mli *)
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
  match Cmd.eval_value ~argv cmd with
  | Error _n -> Error Exit_code.fatal
  | Ok ok -> (
      match ok with
      | `Ok config -> Ok config
      | `Version
      | `Help ->
          Error Exit_code.ok)
