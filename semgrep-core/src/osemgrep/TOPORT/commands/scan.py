# PARTIALLY PORTED TO OCAML. DELETE PARTS AS YOU PORT THEM.

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
from semgrep.constants import DEFAULT_TIMEOUT
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
from semgrep.semgrep_types import LANGUAGE
from semgrep.state import get_state
from semgrep.target_manager import write_pipes_to_disk
from semgrep.util import abort
from semgrep.util import with_color
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


ScanReturn = Optional[Tuple[RuleMatchMap, List[SemgrepError], List[Rule], Set[Path]]]


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
        "--exclude-rule",
        multiple=True,
        default=[],
        help="""
            Skip any rule with the given id. Can add multiple times.
        """,
        shell_complete=__get_file_options,
    ),
    optgroup.option(
        "--scan-unknown-extensions/--skip-unknown-extensions",
        is_flag=True,
        default=True,
        help="""
            If true, explicit files will be scanned using the language specified in
            --lang. If --skip-unknown-extensions, these files will not be scanned
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
        help="Explain how non-local values reach the location of a finding (only affects text output).",
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

    optgroup.group(
        "Output formats",
        cls=MutuallyExclusiveOptionGroup,
        help="Uses ASCII output if no format specified.",
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
)
# These flags are deprecated or experimental - users should not
# rely on their existence, or their output being stable
@click.option(
    "--deep",
    "-x",
    is_flag=True,
    hidden=True
    # help="contact support@r2c.dev for more information on this"
)
@scan_options
@handle_command_errors
def scan(
    *,
    deep: bool,
    dryrun: bool,
    dump_ast: bool,
    enable_nosem: bool,
    enable_version_check: bool,
    error_on_findings: bool,
    exclude_rule: Optional[Tuple[str, ...]],
    force_color: bool,
    gitlab_sast: bool,
    gitlab_secrets: bool,
    junit_xml: bool,
    max_chars_per_line: int,
    max_lines_per_finding: int,
    metrics_legacy: Optional[MetricsState],
    optimizations: str,
    dataflow_traces: bool,
    output: Optional[str],
    replacement: Optional[str],
    rewrite_rule_ids: bool,
    sarif: bool,
    scan_unknown_extensions: bool,
    severity: Optional[Tuple[str, ...]],
    show_supported_languages: bool,
    test: bool,
    test_ignore_todo: bool,
    time_flag: bool,
    validate: bool,
    version: bool,
) -> ScanReturn:

    if version:
        print(__VERSION__)
        if enable_version_check:
            from semgrep.app.version import version_check

            version_check()
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

    output_time = time_flag

    # Note this must be after the call to `terminal.configure` so that verbosity is respected
    possibly_notify_user()

    # change cwd if using docker
    if not targets:
        semgrep.config_resolver.adjust_for_docker()
        targets = (os.curdir,)

    if gitlab_sast:
        output_format = OutputFormat.GITLAB_SAST
    elif gitlab_secrets:
        output_format = OutputFormat.GITLAB_SECRETS
    elif junit_xml:
        output_format = OutputFormat.JUNIT_XML
    elif sarif:
        output_format = OutputFormat.SARIF

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
            deep=deep,
        )

    run_has_findings = False

    # The 'optional_stdin_target' context manager must remain before
    # 'managed_output'. Output depends on file contents so we cannot have
    # already deleted the temporary stdin file.
    with tempfile.TemporaryDirectory() as pipes_dir:
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
                            timeout=timeout,
                            max_memory=max_memory,
                            timeout_threshold=timeout_threshold,
                            optimizations=optimizations,
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
                    all_targets,
                    _,
                    ignore_log,
                    filtered_rules,
                    profiler,
                    profiling_data,
                    _,
                    explanations,
                    shown_severities,
                    _,
                ) = semgrep.semgrep_main.main(
                    deep=deep,
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
                all_targets=all_targets,
                ignore_log=ignore_log,
                profiler=profiler,
                filtered_rules=filtered_rules,
                profiling_data=profiling_data,
                explanations=explanations,
                severities=shown_severities,
                print_summary=True,
            )

            run_has_findings = any(filtered_matches_by_rule.values())

            return_data = (
                filtered_matches_by_rule,
                semgrep_errors,
                filtered_rules,
                all_targets,
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
