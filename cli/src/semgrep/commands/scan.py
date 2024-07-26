# THIS FILE IS DEPRECATED! DO NOT MODIFY FLAGS HERE! INSTEAD MODIFY Scan_CLI.ml
import os
import tempfile
from itertools import chain
from pathlib import Path
from typing import Any
from typing import Callable
from typing import Dict
from typing import List
from typing import Optional
from typing import Sequence
from typing import Set
from typing import Tuple

import click
from click_option_group import MutuallyExclusiveOptionGroup
from click_option_group import optgroup

import semgrep.config_resolver
import semgrep.run_scan
import semgrep.test
from semgrep import __VERSION__
from semgrep import bytesize
from semgrep import tracing
from semgrep.app.version import get_no_findings_msg
from semgrep.commands.install import determine_semgrep_pro_path
from semgrep.commands.wrapper import handle_command_errors
from semgrep.constants import Colors
from semgrep.constants import DEFAULT_DIFF_DEPTH
from semgrep.constants import DEFAULT_MAX_CHARS_PER_LINE
from semgrep.constants import DEFAULT_MAX_LINES_PER_FINDING
from semgrep.constants import DEFAULT_MAX_TARGET_SIZE
from semgrep.constants import DEFAULT_TIMEOUT
from semgrep.constants import MAX_CHARS_FLAG_NAME
from semgrep.constants import MAX_LINES_FLAG_NAME
from semgrep.constants import OutputFormat
from semgrep.core_runner import CoreRunner
from semgrep.engine import EngineType
from semgrep.error import SemgrepError
from semgrep.git import get_project_url
from semgrep.metrics import MetricsState
from semgrep.notifications import possibly_notify_user
from semgrep.output import OutputHandler
from semgrep.output import OutputSettings
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatchMap
from semgrep.semgrep_core import SemgrepCore
from semgrep.state import get_state
from semgrep.target_manager import ALL_PRODUCTS
from semgrep.target_manager import write_pipes_to_disk
from semgrep.util import abort
from semgrep.util import with_color
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


class MetricsStateType(click.ParamType):
    name = "metrics_state"

    def get_metavar(self, _param: click.Parameter) -> str:
        return "[auto|on|off]"

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

# This subset of scan options is reused in ci.py
_scan_options: List[Callable] = [
    click.help_option("--help", "-h"),
    click.option(
        "-a",
        "--autofix/--no-autofix",
        is_flag=True,
    ),
    click.option(
        "--baseline-commit",
        envvar=["SEMGREP_BASELINE_COMMIT", "SEMGREP_BASELINE_REF"],
    ),
    click.option(
        "--metrics",
        "metrics",
        type=METRICS_STATE_TYPE,
        envvar="SEMGREP_SEND_METRICS",
    ),
    optgroup.group(
        "Path options",
    ),
    optgroup.option(
        "--exclude",
        multiple=True,
        default=[],
    ),
    optgroup.option(
        "--exclude-rule",
        multiple=True,
        default=[],
    ),
    optgroup.option(
        "--include",
        multiple=True,
        default=[],
    ),
    optgroup.option(
        "--max-target-bytes",
        type=bytesize.ByteSizeType(),
        default=DEFAULT_MAX_TARGET_SIZE,
    ),
    optgroup.option(
        "--use-git-ignore/--no-git-ignore",
        is_flag=True,
        default=True,
    ),
    optgroup.option(
        "--scan-unknown-extensions/--skip-unknown-extensions",
        is_flag=True,
        default=False,
    ),
    optgroup.group("Performance and memory options"),
    optgroup.option(
        "--enable-version-check/--disable-version-check",
        is_flag=True,
        default=True,
        envvar="SEMGREP_ENABLE_VERSION_CHECK",
    ),
    optgroup.option(
        "-j",
        "--jobs",
        type=int,
    ),
    optgroup.option(
        "--max-memory",
        type=int,
    ),
    optgroup.option(
        "--optimizations",
        default="all",
        type=click.Choice(["all", "none"]),
    ),
    optgroup.option(
        "--timeout",
        type=int,
        default=DEFAULT_TIMEOUT,
        envvar="SEMGREP_TIMEOUT",
    ),
    optgroup.option(
        "--timeout-threshold",
        type=int,
        default=3,
    ),
    # TODO: Move to Semgrep Pro Engine group ?
    optgroup.option(
        "--interfile-timeout",
        type=int,
    ),
    optgroup.group("Display options"),
    optgroup.option(
        "--enable-nosem/--disable-nosem",
        is_flag=True,
        default=True,
    ),
    optgroup.option(
        "--force-color/--no-force-color",
        is_flag=True,
    ),
    optgroup.option(
        MAX_CHARS_FLAG_NAME,
        type=int,
        default=DEFAULT_MAX_CHARS_PER_LINE,
    ),
    optgroup.option(
        MAX_LINES_FLAG_NAME,
        type=int,
        default=DEFAULT_MAX_LINES_PER_FINDING,
    ),
    optgroup.option(
        "--dataflow-traces",
        default=None,
        is_flag=True,
    ),
    optgroup.option(
        "-o",
        "--output",
    ),
    optgroup.option(
        "--rewrite-rule-ids/--no-rewrite-rule-ids",
        is_flag=True,
        default=True,
    ),
    optgroup.option(
        "--time/--no-time",
        "time_flag",
        is_flag=True,
        default=False,
    ),
    optgroup.option(
        "--trace/--no-trace",
        "trace",
        is_flag=True,
        default=False,
    ),
    optgroup.option(
        "--trace-endpoint",
        envvar="SEMGREP_OTEL_ENDPOINT",
        default=None,
    ),
    optgroup.option(
        "--matching-explanations",
        is_flag=True,
        default=False,
    ),
    optgroup.group("Verbosity options", cls=MutuallyExclusiveOptionGroup),
    optgroup.option(
        "-q",
        "--quiet",
        is_flag=True,
    ),
    optgroup.option(
        "-v",
        "--verbose",
        is_flag=True,
    ),
    optgroup.option(
        "--debug",
        is_flag=True,
    ),
    optgroup.group(
        "Output formats",
        cls=MutuallyExclusiveOptionGroup,
    ),
    optgroup.option(
        "--text",
        "output_format",
        type=OutputFormat,
        flag_value=OutputFormat.TEXT,
        default=True,
    ),
    optgroup.option(
        "--emacs",
        "output_format",
        type=OutputFormat,
        flag_value=OutputFormat.EMACS,
    ),
    optgroup.option(
        "--json",
        "output_format",
        type=OutputFormat,
        flag_value=OutputFormat.JSON,
    ),
    optgroup.option(
        "--gitlab-sast",
        "output_format",
        type=OutputFormat,
        flag_value=OutputFormat.GITLAB_SAST,
    ),
    optgroup.option(
        "--gitlab-secrets",
        "output_format",
        type=OutputFormat,
        flag_value=OutputFormat.GITLAB_SECRETS,
    ),
    optgroup.option(
        "--junit-xml",
        "output_format",
        type=OutputFormat,
        flag_value=OutputFormat.JUNIT_XML,
    ),
    optgroup.option(
        "--sarif",
        "output_format",
        type=OutputFormat,
        flag_value=OutputFormat.SARIF,
    ),
    optgroup.option(
        "--vim",
        "output_format",
        type=OutputFormat,
        flag_value=OutputFormat.VIM,
    ),
    # Names of this group are "outputs_<format>" so that they end up
    # next to "output" and "output_format" in argument lists.
    optgroup.group(
        "Write additional outputs to file",
    ),
    optgroup.option(
        "--text-output",
        "outputs_text",
        multiple=True,
        default=[],
    ),
    optgroup.option("--emacs-output", "outputs_emacs", multiple=True, default=[]),
    optgroup.option("--json-output", "outputs_json", multiple=True, default=[]),
    optgroup.option(
        "--gitlab-sast-output", "outputs_gitlab_sast", multiple=True, default=[]
    ),
    optgroup.option(
        "--gitlab-secrets-output", "outputs_gitlab_secrets", multiple=True, default=[]
    ),
    optgroup.option(
        "--junit-xml-output", "outputs_junit_xml", multiple=True, default=[]
    ),
    optgroup.option("--sarif-output", "outputs_sarif", multiple=True, default=[]),
    optgroup.option("--vim-output", "outputs_vim", multiple=True, default=[]),
    optgroup.group("Semgrep Pro Engine options"),
    optgroup.option(
        "--pro",
        "requested_engine",
        type=EngineType,
        flag_value=EngineType.PRO_INTERFILE,
    ),
    optgroup.option(
        "--pro-intrafile",
        "requested_engine",
        type=EngineType,
        flag_value=EngineType.PRO_INTRAFILE,
    ),
    optgroup.option(
        "--pro-languages",
        "requested_engine",
        type=EngineType,
        flag_value=EngineType.PRO_LANG,
    ),
    optgroup.option(
        "--pro-path-sensitive", "path_sensitive", is_flag=True, default=False
    ),
    optgroup.option(
        "--oss-only",
        "requested_engine",
        type=EngineType,
        flag_value=EngineType.OSS,
    ),
    optgroup.option(
        "--diff-depth",
        type=int,
        default=DEFAULT_DIFF_DEPTH,
    ),
    optgroup.option("--dump-command-for-core", "-d", is_flag=True, hidden=True),
    optgroup.option(
        "--no-secrets-validation",
        "disable_secrets_validation_flag",
        is_flag=True,
        hidden=True,
    ),
    optgroup.option(
        "--historical-secrets",
        "historical_secrets",
        is_flag=True,
    ),
    optgroup.option(
        "--allow-untrusted-validators",
        "allow_untrusted_validators",
        is_flag=True,
    ),
]


def collect_additional_outputs(
    outputs_text: List[str],
    outputs_emacs: List[str],
    outputs_json: List[str],
    outputs_vim: List[str],
    outputs_gitlab_sast: List[str],
    outputs_gitlab_secrets: List[str],
    outputs_junit_xml: List[str],
    outputs_sarif: List[str],
) -> Dict[Optional[str], OutputFormat]:
    output_formats = [
        (OutputFormat.TEXT, outputs_text),
        (OutputFormat.EMACS, outputs_emacs),
        (OutputFormat.VIM, outputs_vim),
        (OutputFormat.JSON, outputs_json),
        (OutputFormat.GITLAB_SAST, outputs_gitlab_sast),
        (OutputFormat.GITLAB_SECRETS, outputs_gitlab_secrets),
        (OutputFormat.JUNIT_XML, outputs_junit_xml),
        (OutputFormat.SARIF, outputs_sarif),
    ]
    outputs: Dict[Optional[str], OutputFormat] = {}

    for output_format, output_destinations in output_formats:
        for output_destination in output_destinations:
            if output_destination in outputs:
                other_format = outputs[output_destination]
                if other_format != output_format:
                    abort(
                        f"Can't write multiple outputs to the same desitination: "
                        f"{other_format} and {output_format} "
                        f"both output to {output_destination}."
                    )
            else:
                outputs[output_destination] = output_format
    return outputs


def scan_options(func: Callable) -> Callable:
    for option in reversed(_scan_options):
        func = option(func)
    return func


# Those are the scan-only options (not reused in ci.py)
@click.command()
@click.argument("targets", nargs=-1, type=click.Path(allow_dash=True))
@click.option(
    "--replacement",
)
@optgroup.group("Configuration options", cls=MutuallyExclusiveOptionGroup)
@optgroup.option(
    "--config",
    "-c",
    "-f",
    multiple=True,
    envvar="SEMGREP_RULES",
)
@optgroup.option(
    "--pattern",
    "-e",
)
@click.option(
    "--lang",
    "-l",
)
@click.option(
    "--dryrun/--no-dryrun",
    is_flag=True,
    default=False,
)
@click.option(
    "--severity",
    multiple=True,
    type=click.Choice(["INFO", "WARNING", "ERROR"]),
)
@optgroup.group("Alternate modes")
@optgroup.option(
    "--validate",
    is_flag=True,
    default=False,
)
@optgroup.option("--version", is_flag=True, default=False)
@optgroup.option(
    "--x-ls",
    is_flag=True,
    default=False,
)
@optgroup.group("Test and debug options")
@optgroup.option("--test", is_flag=True, default=False)
@optgroup.option(
    "--test-ignore-todo/--no-test-ignore-todo",
    is_flag=True,
    default=False,
)
@click.option(
    "--error/--no-error",
    "error_on_findings",
    is_flag=True,
)
@click.option(
    "--strict/--no-strict",
    is_flag=True,
    default=False,
)
# These flags are deprecated or experimental - users should not
# rely on their existence, or their output being stable
@click.option("--dump-engine-path", is_flag=True, hidden=True)
@click.option(
    "--secrets",
    "run_secrets_flag",
    is_flag=True,
)
@optgroup.group("Osemgrep migration options")
@optgroup.option(
    "--use-osemgrep-sarif",
    "use_osemgrep_sarif",
    is_flag=True,
    default=False,
)
@scan_options
@handle_command_errors
def scan(
    *,
    autofix: bool,
    baseline_commit: Optional[str],
    config: Optional[Tuple[str, ...]],
    debug: bool,
    diff_depth: int,
    dump_engine_path: bool,
    requested_engine: Optional[EngineType],
    run_secrets_flag: bool,
    disable_secrets_validation_flag: bool,
    historical_secrets: bool,
    dryrun: bool,
    dump_command_for_core: bool,
    enable_nosem: bool,
    enable_version_check: bool,
    error_on_findings: bool,
    exclude: Optional[Tuple[str, ...]],
    exclude_rule: Optional[Tuple[str, ...]],
    force_color: bool,
    include: Optional[Tuple[str, ...]],
    jobs: Optional[int],
    lang: Optional[str],
    matching_explanations: bool,
    max_chars_per_line: int,
    max_lines_per_finding: int,
    max_memory: Optional[int],
    max_target_bytes: int,
    metrics: Optional[MetricsState],
    optimizations: str,
    dataflow_traces: bool,
    output: Optional[str],
    output_format: OutputFormat,
    outputs_text: List[str],
    outputs_emacs: List[str],
    outputs_json: List[str],
    outputs_vim: List[str],
    outputs_gitlab_sast: List[str],
    outputs_gitlab_secrets: List[str],
    outputs_junit_xml: List[str],
    outputs_sarif: List[str],
    pattern: Optional[str],
    quiet: bool,
    replacement: Optional[str],
    rewrite_rule_ids: bool,
    allow_untrusted_validators: bool,
    scan_unknown_extensions: bool,
    severity: Optional[Tuple[str, ...]],
    strict: bool,
    targets: Sequence[str],
    test: bool,
    test_ignore_todo: bool,
    time_flag: bool,
    timeout: int,
    timeout_threshold: int,
    interfile_timeout: Optional[int],
    trace: bool,
    trace_endpoint: Optional[str],
    use_git_ignore: bool,
    use_osemgrep_sarif: bool,
    validate: bool,
    verbose: bool,
    version: bool,
    x_ls: bool,
    path_sensitive: bool,
) -> Optional[Tuple[RuleMatchMap, List[SemgrepError], List[Rule], Set[Path]]]:
    if version:
        print(__VERSION__)
        if enable_version_check:
            from semgrep.app.version import version_check

            version_check()
        return None

    # I wish there was an easy way to leverage the engine_params from the
    # new GET /api/cli/scans endpoint here but that info is not available
    # until we fetch the rules which happens further along when processing
    # the config.
    if config and "secrets" in config:
        # If the user has specified --config secrets, we should enable secrets
        # so the engine is properly chosen.
        run_secrets_flag = True

    # Handled error outside engine type for more actionable advice.
    if run_secrets_flag and requested_engine is EngineType.OSS:
        abort(
            "Cannot run secrets scan with OSS engine (--oss specified). Semgrep Secrets is a proprietary extension."
        )

    state = get_state()
    state.traces.configure(trace, trace_endpoint)
    with tracing.TRACER.start_as_current_span("semgrep.commands.scan"):
        engine_type = EngineType.decide_engine_type(
            logged_in=state.app_session.token is not None,
            engine_flag=requested_engine,
            run_secrets=run_secrets_flag,
            interfile_diff_scan_enabled=diff_depth >= 0,
        )

        # this is useful for our CI job to find where semgrep-core (or semgrep-core-proprietary)
        # is installed and check if the binary is statically linked.
        if dump_engine_path:
            if engine_type == EngineType.OSS:
                print(SemgrepCore.path())
            else:
                print(determine_semgrep_pro_path())
            return None

        if dataflow_traces is None:
            dataflow_traces = engine_type.has_dataflow_traces

        state.metrics.configure(metrics)
        state.terminal.configure(
            verbose=verbose,
            debug=debug,
            quiet=quiet,
            force_color=force_color,
            output_format=output_format,
        )
        # to capture the stderr of semgrep-core or to let semgrep-core reuse
        # the stderr of pysemgrep to display logs as soon as they are produced
        # pysemgrep-only: not needed for osemgrep obviously
        capture_core_stderr = not debug

        if include and exclude:
            logger.warning(
                with_color(
                    Colors.yellow,
                    "Paths that match both --include and --exclude will be skipped by Semgrep.",
                )
            )

        if pattern is not None and lang is None:
            abort("-e/--pattern and -l/--lang must both be specified")

        if config and "auto" in config and metrics == MetricsState.OFF:
            abort(
                "Cannot create auto config when metrics are off. Please allow metrics or run with a specific config."
            )

        # People have more flexibility on local scans so --max-memory and --pro-timeout is set to unlimited
        if not max_memory:
            max_memory = 0  # unlimited
        if not interfile_timeout:
            interfile_timeout = 0  # unlimited

        # Note this must be after the call to `terminal.configure` so that verbosity is respected
        possibly_notify_user()

        # change cwd if using docker
        if not targets:
            semgrep.config_resolver.adjust_for_docker()
            targets = (os.curdir,)

        use_osemgrep_to_format: Set[OutputFormat] = set()
        if use_osemgrep_sarif:
            use_osemgrep_to_format.add(OutputFormat.SARIF)

        outputs = collect_additional_outputs(
            outputs_text=outputs_text,
            outputs_emacs=outputs_emacs,
            outputs_json=outputs_json,
            outputs_vim=outputs_vim,
            outputs_gitlab_sast=outputs_gitlab_sast,
            outputs_gitlab_secrets=outputs_gitlab_secrets,
            outputs_junit_xml=outputs_junit_xml,
            outputs_sarif=outputs_sarif,
        )

        output_settings = OutputSettings(
            outputs=outputs,
            output_format=output_format,
            output_destination=output,
            error_on_findings=error_on_findings,
            strict=strict,
            verbose_errors=verbose,
            timeout_threshold=timeout_threshold,
            output_time=time_flag,
            output_per_finding_max_lines_limit=max_lines_per_finding,
            output_per_line_max_chars_limit=max_chars_per_line,
            dataflow_traces=dataflow_traces,
            use_osemgrep_to_format=use_osemgrep_to_format,
        )

        if test:
            if len(outputs) > 0:
                abort("The --test option doesn't support additional outputs to files.")
            # the test code (which isn't a "test" per se but is actually
            # machinery to evaluate semgrep performance) uses
            # managed_output internally
            semgrep.test.test_main(
                target=targets,
                config=config,
                test_ignore_todo=test_ignore_todo,
                strict=strict,
                json=output_format == OutputFormat.JSON,
                optimizations=optimizations,
                engine_type=engine_type,
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
                    logger.debug(
                        "stdin or piped targets, adding --scan-unknown-extensions"
                    )
                    scan_unknown_extensions = True

            targets = write_pipes_to_disk(targets, Path(pipes_dir))

            output_handler = OutputHandler(output_settings)
            return_data: Optional[
                Tuple[RuleMatchMap, List[SemgrepError], List[Rule], Set[Path]]
            ] = None

            if validate:
                if not (pattern or lang or config):
                    logger.error(
                        f"Nothing to validate, use the --config or --pattern flag to specify a rule"
                    )
                else:
                    (
                        resolved_configs,
                        config_errors,
                    ) = semgrep.config_resolver.get_config(
                        pattern, lang, config or [], project_url=get_project_url()
                    )

                    # Run metachecks specifically on the config files
                    if config:
                        try:
                            metacheck_errors = CoreRunner(
                                jobs=jobs,
                                engine_type=engine_type,
                                timeout=timeout,
                                max_memory=max_memory,
                                timeout_threshold=timeout_threshold,
                                interfile_timeout=interfile_timeout,
                                trace=trace,
                                trace_endpoint=trace_endpoint,
                                capture_stderr=capture_core_stderr,
                                optimizations=optimizations,
                                allow_untrusted_validators=allow_untrusted_validators,
                                path_sensitive=path_sensitive,
                            ).validate_configs(config)
                        except SemgrepError as e:
                            metacheck_errors = [e]

                    config_errors = list(chain(config_errors, metacheck_errors))

                    valid_str = "invalid" if config_errors else "valid"
                    # NOTE: get_rules will de-duplicate rules as the same rule can appear across multiple config packs
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
                        _dependencies,
                        _dependency_parser_errors,
                        executed_rule_count,
                        missed_rule_count,
                    ) = semgrep.run_scan.run_scan(
                        diff_depth=diff_depth,
                        dump_command_for_core=dump_command_for_core,
                        time_flag=time_flag,
                        matching_explanations=matching_explanations,
                        engine_type=engine_type,
                        run_secrets=run_secrets_flag,
                        disable_secrets_validation=disable_secrets_validation_flag,
                        historical_secrets=historical_secrets,
                        output_handler=output_handler,
                        target=targets,
                        pattern=pattern,
                        lang=lang,
                        configs=(config or ["auto"]),
                        no_rewrite_rule_ids=(not rewrite_rule_ids),
                        jobs=jobs,
                        include=include,
                        exclude={product: (exclude or ()) for product in ALL_PRODUCTS},
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
                        trace=trace,
                        trace_endpoint=trace_endpoint,
                        skip_unknown_extensions=(not scan_unknown_extensions),
                        allow_untrusted_validators=allow_untrusted_validators,
                        severity=severity,
                        optimizations=optimizations,
                        baseline_commit=baseline_commit,
                        x_ls=x_ls,
                        path_sensitive=path_sensitive,
                        capture_core_stderr=capture_core_stderr,
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
                    extra=output_extra,
                    explanations=output_extra.core.explanations,
                    severities=shown_severities,
                    print_summary=True,
                    engine_type=engine_type,
                    executed_rule_count=executed_rule_count,
                    missed_rule_count=missed_rule_count,
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
