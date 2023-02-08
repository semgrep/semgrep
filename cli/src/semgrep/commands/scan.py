import os
import tempfile
from itertools import chain
from pathlib import Path
from typing import Any
from typing import Callable
from typing import cast
from typing import List
from typing import Optional
from typing import Sequence
from typing import Set
from typing import Tuple

import click
from click.shell_completion import CompletionItem
from click_option_group import MutuallyExclusiveOptionGroup
from click_option_group import optgroup

import semgrep.config_resolver
import semgrep.semgrep_main
import semgrep.test
from semgrep import __VERSION__
from semgrep import bytesize
from semgrep.app.registry import list_current_public_rulesets
from semgrep.app.version import get_no_findings_msg
from semgrep.commands.wrapper import handle_command_errors
from semgrep.constants import Colors
from semgrep.constants import DEFAULT_MAX_CHARS_PER_LINE
from semgrep.constants import DEFAULT_MAX_LINES_PER_FINDING
from semgrep.constants import DEFAULT_MAX_TARGET_SIZE
from semgrep.constants import DEFAULT_TIMEOUT
from semgrep.constants import EngineType
from semgrep.constants import MAX_CHARS_FLAG_NAME
from semgrep.constants import MAX_LINES_FLAG_NAME
from semgrep.constants import OutputFormat
from semgrep.constants import RuleSeverity
from semgrep.core_runner import CoreRunner
from semgrep.dump_ast import dump_parsed_ast
from semgrep.error import SemgrepError
from semgrep.metrics import MetricsState
from semgrep.notifications import possibly_notify_user
from semgrep.output import OutputHandler
from semgrep.output import OutputSettings
from semgrep.project import get_project_url
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatchMap
from semgrep.semgrep_core import SemgrepCore
from semgrep.semgrep_types import LANGUAGE
from semgrep.state import get_state
from semgrep.target_manager import write_pipes_to_disk
from semgrep.util import abort
from semgrep.util import with_color
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


ScanReturn = Optional[Tuple[RuleMatchMap, List[SemgrepError], List[Rule], Set[Path]]]


def __validate_lang(option: str, lang: Optional[str]) -> str:
    if lang is None:
        abort(f"{option} and -l/--lang must both be specified")
    return cast(str, lang)


def __get_severity_options(
    context: click.Context, _param: str, incomplete: str
) -> List[Any]:
    return [
        CompletionItem(e.value) for e in RuleSeverity if e.value.startswith(incomplete)
    ]


def __get_language_options(
    context: click.Context, _param: str, incomplete: str
) -> List[Any]:
    return [
        CompletionItem(e)
        for e in LANGUAGE.all_language_keys
        if e.startswith(incomplete)
    ]


def __get_size_options(
    context: click.Context, _param: str, incomplete: str
) -> List[Any]:
    if incomplete.isnumeric():
        sizes = [f"{incomplete}{u}" for u in bytesize.UNITS.keys()]
        return [CompletionItem(s) for s in sizes if s.startswith(incomplete)]
    else:
        return []


def __get_file_options(
    context: click.Context, _param: str, incomplete: str
) -> List[Any]:
    return [CompletionItem(f, type="file") for f in os.listdir(".")]


def __get_config_options(
    context: click.Context, _param: str, incomplete: str
) -> List[Any]:
    if incomplete[:2] == "p/":
        # Get list of rulesets
        rulesets = list_current_public_rulesets()
        rulesets = list(
            filter(lambda r: "hidden" not in r or not r["hidden"], rulesets)
        )
        rulesets_names = list(map(lambda r: f"p/{r['name']}", rulesets))

        return [CompletionItem(r) for r in rulesets_names if r.startswith(incomplete)]
    else:
        files = filter(
            lambda f: f.endswith(".yaml") or f.endswith(".yml"), os.listdir(".")
        )
        return [CompletionItem(f) for f in list(files) if f.startswith(incomplete)]


def __get_optimization_options(
    context: click.Context, _param: str, incomplete: str
) -> List[Any]:
    return [CompletionItem("all"), CompletionItem("none")]


class MetricsStateType(click.ParamType):
    name = "metrics_state"

    def get_metavar(self, _param: click.Parameter) -> str:
        return "[auto|on|off]"

    def shell_complete(
        self, context: click.Context, _param: click.Parameter, incomplete: str
    ) -> List[Any]:
        return [
            CompletionItem(e) for e in ["auto", "on", "off"] if e.startswith(incomplete)
        ]

    def convert(
        self,
        value: Any,
        _param: Optional["click.Parameter"],
        ctx: Optional["click.Context"],
    ) -> Any:
        if value is None:
            return None
        if isinstance(value, str):
            lower = value.lower()
            if lower == "auto":
                return MetricsState.AUTO
            # Support setting via old environment variable values 0/1/true/false
            if lower == "on" or lower == "1" or lower == "true":
                return MetricsState.ON
            if lower == "off" or lower == "0" or lower == "false":
                return MetricsState.OFF
        self.fail("expected 'auto', 'on', or 'off'")


METRICS_STATE_TYPE = MetricsStateType()


# Slightly increase the help width from default 80 characters, to improve readability
CONTEXT_SETTINGS = {"max_content_width": 90}

_scan_options: List[Callable] = [
    click.help_option("--help", "-h", help=("Show this message and exit.")),
    click.option(
        "-a",
        "--autofix/--no-autofix",
        is_flag=True,
        help="""
            Apply autofix patches. WARNING: data loss can occur with this
            flag. Make sure your files are stored in a version control system.
            Note that this mode is experimental and not guaranteed to function properly.
        """,
    ),
    click.option(
        "--baseline-commit",
        help="""
            Only show results that are not found in this commit hash. Aborts run if not currently
            in a git directory, there are unstaged changes, or given baseline hash doesn't exist
        """,
        envvar=["SEMGREP_BASELINE_COMMIT", "SEMGREP_BASELINE_REF"],
    ),
    click.option(
        "--metrics",
        "metrics",
        type=METRICS_STATE_TYPE,
        help="""
            Configures how usage metrics are sent to the Semgrep server.
            If 'auto', metrics are sent whenever the --config value pulls from the Semgrep server.
            If 'on', metrics are always sent.
            If 'off', metrics are disabled altogether and not sent.
            If absent, the SEMGREP_SEND_METRICS environment variable value will be used.
            If no environment variable, defaults to 'auto'.
        """,
        envvar="SEMGREP_SEND_METRICS",
    ),
    click.option(
        "--disable-metrics",
        "metrics_legacy",
        is_flag=True,
        type=METRICS_STATE_TYPE,
        flag_value="off",
        hidden=True,
    ),
    click.option(
        "--enable-metrics",
        "metrics_legacy",
        is_flag=True,
        type=METRICS_STATE_TYPE,
        flag_value="on",
        hidden=True,
    ),
    optgroup.group(
        "Path options",
        help="""
            By default, Semgrep scans all git-tracked files with extensions matching rules' languages.
            These options alter which files Semgrep scans.
        """,
    ),
    optgroup.option(
        "--exclude",
        multiple=True,
        default=[],
        help="""
            Skip any file or directory that matches this pattern; --exclude='*.py' will ignore
            the following: foo.py, src/foo.py, foo.py/bar.sh. --exclude='tests' will ignore tests/foo.py
            as well as a/b/tests/c/foo.py. Can add multiple times. If present, any --include directives
            are ignored.
        """,
        shell_complete=__get_file_options,
    ),
    optgroup.option(
        "--exclude-rule",
        multiple=True,
        default=[],
        help="""
            Skip any rule with the given id. Can add multiple times.
        """,
        shell_complete=__get_file_options,
    ),
    optgroup.option(
        "--include",
        multiple=True,
        default=[],
        help="""
            Filter files or directories by path. The argument is a
            glob-style pattern such as 'foo.*' that must match the path.
            This is an extra filter in addition to other applicable filters.
            For example, specifying the language with '-l javascript' migh
            preselect files 'src/foo.jsx' and 'lib/bar.js'. Specifying one of
            '--include=src', '--include=*.jsx', or '--include=src/foo.*'
            will restrict the selection to the single file 'src/foo.jsx'.
            A choice of multiple '--include' patterns can be specified.
            For example, '--include=foo.* --include=bar.*' will select
            both 'src/foo.jsx' and 'lib/bar.js'.
            Glob-style patterns follow the syntax supported by python,
            which is documented at https://docs.python.org/3/library/glob.html
        """,
        shell_complete=__get_file_options,
    ),
    optgroup.option(
        "--max-target-bytes",
        type=bytesize.ByteSizeType(),
        default=DEFAULT_MAX_TARGET_SIZE,
        help=f"""
            Maximum size for a file to be scanned by Semgrep, e.g '1.5MB'. Any input
            program larger than this will be ignored. A zero or negative value disables
            this filter. Defaults to {DEFAULT_MAX_TARGET_SIZE} bytes.
        """,
        shell_complete=__get_size_options,
    ),
    optgroup.option(
        "--use-git-ignore/--no-git-ignore",
        is_flag=True,
        default=True,
        help="""
            Skip files ignored by git. Scanning starts from the root folder specified on
            the Semgrep command line. Normally, if the scanning root is within a git
            repository, only the tracked files and the new files would be scanned. Git
            submodules and git-ignored files would normally be skipped. --no-git-ignore
            will disable git-aware filtering. Setting this flag does nothing if the
            scanning root is not in a git repository.
        """,
    ),
    optgroup.option(
        "--scan-unknown-extensions/--skip-unknown-extensions",
        is_flag=True,
        default=False,
        help="""
            If true, explicit files will be scanned using the language specified in
            --lang. If --skip-unknown-extensions, these files will not be scanned.
            Defaults to false.
        """,
    ),
    optgroup.group("Performance and memory options"),
    optgroup.option(
        "--enable-version-check/--disable-version-check",
        is_flag=True,
        default=True,
        envvar="SEMGREP_ENABLE_VERSION_CHECK",
        help="""
            Checks Semgrep servers to see if the latest version is run; disabling this
            may reduce exit time after returning results.
        """,
    ),
    optgroup.option(
        "-j",
        "--jobs",
        type=int,
        help="""
            Number of subprocesses to use to run checks in parallel. Defaults to the
            number of cores on the system (1 if using --pro).
        """,
    ),
    optgroup.option(
        "--max-memory",
        type=int,
        help="""
            Maximum system memory to use running a rule on a single file in MiB. If set to
            0 will not have memory limit. Defaults to 0 for all CLI scans. For CI scans
            that use the pro engine, it defaults to 5000 MiB
        """,
    ),
    optgroup.option(
        "--optimizations",
        default="all",
        type=click.Choice(["all", "none"]),
        help="Turn on/off optimizations. Default = 'all'. Use 'none' to turn all optimizations off.",
        shell_complete=__get_optimization_options,
    ),
    optgroup.option(
        "--timeout",
        type=int,
        default=DEFAULT_TIMEOUT,
        help=f"""
            Maximum time to spend running a rule on a single file in seconds. If set to 0
            will not have time limit. Defaults to {DEFAULT_TIMEOUT} s.
        """,
        envvar="SEMGREP_TIMEOUT",
    ),
    optgroup.option(
        "--timeout-threshold",
        type=int,
        default=3,
        help="""
            Maximum number of rules that can timeout on a file before the file is
            skipped. If set to 0 will not have limit. Defaults to 3.
        """,
    ),
    # TODO: Move to Semgrep Pro Engine group ?
    optgroup.option(
        "--interfile-timeout",
        type=int,
        help=f"""
            Maximum time to spend on interfile analysis. If set to 0 will not have
            time limit. Defaults to 0 s for all CLI scans. For CI scans, it defaults
            to 3 hours.
        """,
    ),
    optgroup.option(
        "--core-opts",
        hidden=True,
        type=str,
        help="Flags to pass semgrep-core when executing",
    ),
    optgroup.group("Display options"),
    optgroup.option(
        "--enable-nosem/--disable-nosem",
        is_flag=True,
        default=True,
        help="""
            --enable-nosem enables 'nosem'. Findings will not be reported on lines
            containing a 'nosem' comment at the end. Enabled by default.
        """,
    ),
    optgroup.option(
        "--force-color/--no-force-color",
        is_flag=True,
        help="""
            Always include ANSI color in the output, even if not writing to a TTY;
            defaults to using the TTY status
        """,
    ),
    optgroup.option(
        MAX_CHARS_FLAG_NAME,
        type=int,
        default=DEFAULT_MAX_CHARS_PER_LINE,
        help="Maximum number of characters to show per line.",
    ),
    optgroup.option(
        MAX_LINES_FLAG_NAME,
        type=int,
        default=DEFAULT_MAX_LINES_PER_FINDING,
        help="""
            Maximum number of lines of code that will be shown for each match before
            trimming (set to 0 for unlimited).
        """,
    ),
    optgroup.option(
        "--dataflow-traces",
        is_flag=True,
        help="Explain how non-local values reach the location of a finding (only affects text and SARIF output).",
    ),
    optgroup.option(
        "-o",
        "--output",
        help="Save search results to a file or post to URL. Default is to print to stdout.",
        shell_complete=__get_file_options,
    ),
    optgroup.option(
        "--rewrite-rule-ids/--no-rewrite-rule-ids",
        is_flag=True,
        default=True,
        help="""
            Rewrite rule ids when they appear in nested sub-directories (Rule 'foo' in
            test/rules.yaml will be renamed 'test.foo').
        """,
    ),
    optgroup.option(
        "--time/--no-time",
        "time_flag",
        is_flag=True,
        default=False,
        help="""
            Include a timing summary with the results. If output format is json, provides
            times for each pair (rule, target).
        """,
    ),
    optgroup.group("Verbosity options", cls=MutuallyExclusiveOptionGroup),
    optgroup.option(
        "-q",
        "--quiet",
        is_flag=True,
        help=("Only output findings."),
    ),
    optgroup.option(
        "-v",
        "--verbose",
        is_flag=True,
        help=(
            "Show more details about what rules are running, which files failed to parse, etc."
        ),
    ),
    optgroup.option(
        "--debug",
        is_flag=True,
        help="All of --verbose, but with additional debugging information.",
    ),
    optgroup.group(
        "Output formats",
        cls=MutuallyExclusiveOptionGroup,
        help="Uses ASCII output if no format specified.",
    ),
    optgroup.option(
        "--emacs",
        is_flag=True,
        help="Output results in Emacs single-line format.",
    ),
    optgroup.option(
        "--json", is_flag=True, help="Output results in Semgrep's JSON format."
    ),
    optgroup.option(
        "--gitlab-sast",
        is_flag=True,
        help="Output results in GitLab SAST format.",
    ),
    optgroup.option(
        "--gitlab-secrets",
        is_flag=True,
        help="Output results in GitLab Secrets format.",
    ),
    optgroup.option(
        "--junit-xml", is_flag=True, help="Output results in JUnit XML format."
    ),
    optgroup.option("--sarif", is_flag=True, help="Output results in SARIF format."),
    optgroup.option(
        "--vim",
        is_flag=True,
        help="Output results in vim single-line format.",
    ),
    optgroup.group("Semgrep Pro Engine options"),
    optgroup.option(
        "--pro-languages",
        is_flag=True,
        help="Enable Pro languages (currently just Apex). Requires Semgrep Pro Engine, contact support@r2c.dev for more information on this.",
    ),
    optgroup.option(
        "--pro-intrafile",
        is_flag=True,
        help="Intra-file inter-procedural taint analysis. Implies --pro-languages. Requires Semgrep Pro Engine, contact support@r2c.dev for more information on this.",
    ),
    optgroup.option(
        "--pro",
        is_flag=True,
        help="Inter-file analysis and Pro languages (currently just Apex). Requires Semgrep Pro Engine, contact support@r2c.dev for more information on this.",
    ),
]


def scan_options(func: Callable) -> Callable:
    for option in reversed(_scan_options):
        func = option(func)
    return func


@click.command(context_settings=CONTEXT_SETTINGS)
@click.argument("targets", nargs=-1, type=click.Path(allow_dash=True))
@click.option(
    "--replacement",
    help="""
        An autofix expression that will be applied to any matches found with --pattern.
        Only valid with a command-line specified pattern.
    """,
)
@optgroup.group("Configuration options", cls=MutuallyExclusiveOptionGroup)
@optgroup.option(
    "--config",
    "-c",
    "-f",
    multiple=True,
    help="""
        YAML configuration file, directory of YAML files ending in
        .yml|.yaml, URL of a configuration file, or Semgrep registry entry name.
        \n\n
        Use --config auto to automatically obtain rules tailored to this project; your project URL will be used to log in
         to the Semgrep registry.
        \n\n
        To run multiple rule files simultaneously, use --config before every YAML, URL, or Semgrep registry entry name.
         For example `semgrep --config p/python --config myrules/myrule.yaml`
        \n\n
        See https://semgrep.dev/docs/writing-rules/rule-syntax for information on configuration file format.
    """,
    shell_complete=__get_config_options,
    envvar="SEMGREP_RULES",
)
@optgroup.option(
    "--pattern",
    "-e",
    help="Code search pattern. See https://semgrep.dev/docs/writing-rules/pattern-syntax for information on pattern features.",
)
@click.option(
    "--lang",
    "-l",
    help="Parse pattern and all files in specified language. Must be used with -e/--pattern.",
    shell_complete=__get_language_options,
)
@click.option(
    "--dryrun/--no-dryrun",
    is_flag=True,
    default=False,
    help="""
        If --dryrun, does not write autofixes to a file. This will print the changes
        to the console. This lets you see the changes before you commit to them. Only
        works with the --autofix flag. Otherwise does nothing.
    """,
)
@click.option(
    "--severity",
    multiple=True,
    type=click.Choice(["INFO", "WARNING", "ERROR"]),
    help="""
        Report findings only from rules matching the supplied severity level. By
        default all applicable rules are run. Can add multiple times. Each should
        be one of INFO, WARNING, or ERROR.
    """,
    shell_complete=__get_severity_options,
)
@click.option(
    "--show-supported-languages",
    is_flag=True,
    help=("Print a list of languages that are currently supported by Semgrep."),
)
@optgroup.group("Alternate modes", help="No search is performed in these modes")
@optgroup.option(
    "--validate",
    is_flag=True,
    default=False,
    help="Validate configuration file(s). This will check YAML files for errors and run 'p/semgrep-rule-lints' on the YAML files. No search is performed.",
)
@optgroup.option(
    "--version", is_flag=True, default=False, help="Show the version and exit."
)
@optgroup.group("Test and debug options")
@optgroup.option("--test", is_flag=True, default=False, help="Run test suite.")
@optgroup.option(
    "--test-ignore-todo/--no-test-ignore-todo",
    is_flag=True,
    default=False,
    help="If --test-ignore-todo, ignores rules marked as '#todoruleid:' in test files.",
)
@optgroup.option(
    "--dump-ast/--no-dump-ast",
    is_flag=True,
    default=False,
    help="""
        If --dump-ast, shows AST of the input file or passed expression and then exit
        (can use --json).
    """,
)
@click.option(
    "--error/--no-error",
    "error_on_findings",
    is_flag=True,
    help="Exit 1 if there are findings. Useful for CI and scripts.",
)
@click.option(
    "--strict/--no-strict",
    is_flag=True,
    default=False,
    help="Return a nonzero exit code when WARN level errors are encountered. Fails early if invalid configuration files are present. Defaults to --no-strict.",
)
# These flags are deprecated or experimental - users should not
# rely on their existence, or their output being stable
@click.option("--dump-command-for-core", "-d", is_flag=True, hidden=True)
# DEPRECATED: --deep to be removed by Feb 2023 launch
@click.option(
    "--deep",
    "-x",
    is_flag=True,
    hidden=True
    # help="contact support@r2c.dev for more information on this"
)
# DEPRECATED: --interproc to be removed by Feb 2023 launch
@click.option(
    "--interproc",
    is_flag=True,
    hidden=True
    # help="contact support@r2c.dev for more information on this"
)
# DEPRECATED: --interfile to be removed by Feb 2023 launch
@click.option(
    "--interfile",
    is_flag=True,
    hidden=True
    # help="contact support@r2c.dev for more information on this"
)
@click.option(
    "--dump-engine-path",
    is_flag=True,
    hidden=True
    # help="contact support@r2c.dev for more information on this"
)
@scan_options
@handle_command_errors
def scan(
    *,
    autofix: bool,
    baseline_commit: Optional[str],
    config: Optional[Tuple[str, ...]],
    core_opts: Optional[str],
    debug: bool,
    deep: bool,
    dump_engine_path: bool,
    interproc: bool,
    interfile: bool,
    pro_languages: bool,
    pro_intrafile: bool,
    pro: bool,
    dryrun: bool,
    dump_ast: bool,
    dump_command_for_core: bool,
    emacs: bool,
    enable_nosem: bool,
    enable_version_check: bool,
    error_on_findings: bool,
    exclude: Optional[Tuple[str, ...]],
    exclude_rule: Optional[Tuple[str, ...]],
    force_color: bool,
    gitlab_sast: bool,
    gitlab_secrets: bool,
    include: Optional[Tuple[str, ...]],
    jobs: Optional[int],
    json: bool,
    junit_xml: bool,
    lang: Optional[str],
    max_chars_per_line: int,
    max_lines_per_finding: int,
    max_memory: Optional[int],
    max_target_bytes: int,
    metrics: Optional[MetricsState],
    metrics_legacy: Optional[MetricsState],
    optimizations: str,
    dataflow_traces: bool,
    output: Optional[str],
    pattern: Optional[str],
    quiet: bool,
    replacement: Optional[str],
    rewrite_rule_ids: bool,
    sarif: bool,
    scan_unknown_extensions: bool,
    severity: Optional[Tuple[str, ...]],
    show_supported_languages: bool,
    strict: bool,
    targets: Sequence[str],
    test: bool,
    test_ignore_todo: bool,
    time_flag: bool,
    timeout: int,
    timeout_threshold: int,
    interfile_timeout: Optional[int],
    use_git_ignore: bool,
    validate: bool,
    verbose: bool,
    version: bool,
    vim: bool,
) -> ScanReturn:
    """
    Run semgrep rules on files

    Searches TARGET paths for matches to rules or patterns. Defaults to searching entire current working directory.

    To get started quickly, run

        semgrep --config auto .

    This will automatically fetch rules for your project from the Semgrep Registry. NOTE: Using `--config auto` will
    log in to the Semgrep Registry with your project URL.

    For more information about Semgrep, go to https://semgrep.dev.

    NOTE: By default, Semgrep will report pseudonymous usage metrics to its server if you pull your configuration from
    the Semgrep registy. To learn more about how and why these metrics are collected, please see
    https://semgrep.dev/docs/metrics. To modify this behavior, see the --metrics option below.
    """

    if version:
        print(__VERSION__)
        if enable_version_check:
            from semgrep.app.version import version_check

            version_check()
        return None

    if dump_engine_path:
        print(SemgrepCore.path())
        return None

    if show_supported_languages:
        click.echo(LANGUAGE.show_suppported_languages_message())
        return None

    state = get_state()
    state.metrics.configure(metrics, metrics_legacy)
    state.terminal.configure(
        verbose=verbose, debug=debug, quiet=quiet, force_color=force_color
    )

    if include and exclude:
        logger.warning(
            with_color(
                Colors.yellow,
                "Paths that match both --include and --exclude will be skipped by Semgrep.",
            )
        )

    if pattern is not None and lang is None:
        abort("-e/--pattern and -l/--lang must both be specified")

    if (config and "auto" in config) and metrics == MetricsState.OFF:
        abort(
            "Cannot create auto config when metrics are off. Please allow metrics or run with a specific config."
        )

    # People have more flexibility on local scans so --max-memory and --pro-timeout is set to unlimited
    if not max_memory:
        max_memory = 0  # unlimited
    if not interfile_timeout:
        interfile_timeout = 0  # unlimited

    output_time = time_flag

    # Note this must be after the call to `terminal.configure` so that verbosity is respected
    possibly_notify_user()

    # change cwd if using docker
    if not targets:
        semgrep.config_resolver.adjust_for_docker()
        targets = (os.curdir,)

    output_format = OutputFormat.TEXT
    if json:
        output_format = OutputFormat.JSON
    elif gitlab_sast:
        output_format = OutputFormat.GITLAB_SAST
    elif gitlab_secrets:
        output_format = OutputFormat.GITLAB_SECRETS
    elif junit_xml:
        output_format = OutputFormat.JUNIT_XML
    elif sarif:
        output_format = OutputFormat.SARIF
    elif emacs:
        output_format = OutputFormat.EMACS
    elif vim:
        output_format = OutputFormat.VIM

    if deep:
        abort("The experimental flag --deep has been renamed to --interfile.")
    if interproc:
        abort("The experimental flag --interproc has been renamed to --pro-intrafile.")
    if interfile:
        abort("The experimental flag --interfile has been renamed to --pro.")

    if pro:
        # Inter-file + Pro languages
        engine = EngineType.PRO_INTERFILE
    elif pro_intrafile:
        # Intra-file (inter-proc) + Pro languages
        engine = EngineType.PRO_INTRAFILE
    elif pro_languages:
        # Just Pro languages
        engine = EngineType.PRO_LANG
    else:
        engine = EngineType.OSS

    # Turn on `dataflow_traces` by default for inter-procedural taint analysis.
    if engine is EngineType.PRO_INTRAFILE or engine is EngineType.PRO_INTERFILE:
        dataflow_traces = True

    output_settings = OutputSettings(
        output_format=output_format,
        output_destination=output,
        error_on_findings=error_on_findings,
        strict=strict,
        verbose_errors=verbose,
        timeout_threshold=timeout_threshold,
        output_time=output_time,
        output_per_finding_max_lines_limit=max_lines_per_finding,
        output_per_line_max_chars_limit=max_chars_per_line,
        dataflow_traces=dataflow_traces,
    )

    if test:
        # the test code (which isn't a "test" per se but is actually machinery to evaluate semgrep performance)
        # uses managed_output internally
        semgrep.test.test_main(
            target=targets,
            config=config,
            test_ignore_todo=test_ignore_todo,
            strict=strict,
            json=json,
            optimizations=optimizations,
            engine=engine,
        )

    run_has_findings = False

    # The 'optional_stdin_target' context manager must remain before
    # 'managed_output'. Output depends on file contents so we cannot have
    # already deleted the temporary stdin file.
    with tempfile.TemporaryDirectory() as pipes_dir:
        # mostly repeating the loop in write_pipes_to_disk to detect if we
        # need --scan-unknown-extensions.
        for t in targets:
            if t == "-" or Path(t).is_fifo():
                logger.debug("stdin or piped targets, adding --scan-unknown-extensions")
                scan_unknown_extensions = True

        targets = write_pipes_to_disk(targets, Path(pipes_dir))

        output_handler = OutputHandler(output_settings)
        return_data: ScanReturn = None

        if dump_ast:
            dump_parsed_ast(json, __validate_lang("--dump-ast", lang), pattern, targets)
        elif validate:
            if not (pattern or lang or config):
                logger.error(
                    f"Nothing to validate, use the --config or --pattern flag to specify a rule"
                )
            else:
                resolved_configs, config_errors = semgrep.config_resolver.get_config(
                    pattern, lang, config or [], project_url=get_project_url()
                )

                # Run metachecks specifically on the config files
                if config:
                    try:
                        metacheck_errors = CoreRunner(
                            jobs=jobs,
                            engine=engine,
                            timeout=timeout,
                            max_memory=max_memory,
                            timeout_threshold=timeout_threshold,
                            interfile_timeout=interfile_timeout,
                            optimizations=optimizations,
                            core_opts_str=core_opts,
                        ).validate_configs(config)
                    except SemgrepError as e:
                        metacheck_errors = [e]

                config_errors = list(chain(config_errors, metacheck_errors))

                valid_str = "invalid" if config_errors else "valid"
                rule_count = len(resolved_configs.get_rules(True))
                logger.info(
                    f"Configuration is {valid_str} - found {len(config_errors)} configuration error(s), and {rule_count} rule(s)."
                )
                if config_errors:
                    output_handler.handle_semgrep_errors(config_errors)
                    output_handler.output({}, all_targets=set(), filtered_rules=[])
                    raise SemgrepError("Please fix the above errors and try again.")
        else:
            try:
                (
                    filtered_matches_by_rule,
                    semgrep_errors,
                    _renamed_targets,
                    ignore_log,
                    filtered_rules,
                    profiler,
                    output_extra,
                    shown_severities,
                    _,
                ) = semgrep.semgrep_main.main(
                    core_opts_str=core_opts,
                    dump_command_for_core=dump_command_for_core,
                    engine=engine,
                    output_handler=output_handler,
                    target=targets,
                    pattern=pattern,
                    lang=lang,
                    configs=(config or []),
                    no_rewrite_rule_ids=(not rewrite_rule_ids),
                    jobs=jobs,
                    include=include,
                    exclude=exclude,
                    exclude_rule=exclude_rule,
                    max_target_bytes=max_target_bytes,
                    replacement=replacement,
                    strict=strict,
                    autofix=autofix,
                    dryrun=dryrun,
                    disable_nosem=(not enable_nosem),
                    no_git_ignore=(not use_git_ignore),
                    timeout=timeout,
                    max_memory=max_memory,
                    timeout_threshold=timeout_threshold,
                    interfile_timeout=interfile_timeout,
                    skip_unknown_extensions=(not scan_unknown_extensions),
                    severity=severity,
                    optimizations=optimizations,
                    baseline_commit=baseline_commit,
                )
            except SemgrepError as e:
                output_handler.handle_semgrep_errors([e])
                output_handler.output({}, all_targets=set(), filtered_rules=[])
                raise e

            output_handler.output(
                filtered_matches_by_rule,
                all_targets=output_extra.all_targets,
                ignore_log=ignore_log,
                profiler=profiler,
                filtered_rules=filtered_rules,
                profiling_data=output_extra.profiling_data,
                explanations=output_extra.explanations,
                severities=shown_severities,
                print_summary=True,
            )

            run_has_findings = any(filtered_matches_by_rule.values())

            return_data = (
                filtered_matches_by_rule,
                semgrep_errors,
                filtered_rules,
                output_extra.all_targets,
            )

    if enable_version_check:
        from semgrep.app.version import version_check

        version_check()

    if not run_has_findings and enable_version_check:
        msg = get_no_findings_msg()
        # decouple CLI from app - if functionality removed, do not fail
        if msg:
            logger.info(msg)

    return return_data
