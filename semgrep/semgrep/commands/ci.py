import os
import sys
import time
from pathlib import Path
from typing import Iterable
from typing import List
from typing import Optional
from typing import Sequence
from typing import Tuple

import click

import semgrep.semgrep_main
from semgrep.commands.login import Authentication
from semgrep.commands.scan import CONTEXT_SETTINGS
from semgrep.commands.scan import scan_options
from semgrep.constants import OutputFormat
from semgrep.error import FATAL_EXIT_CODE
from semgrep.error import INVALID_API_KEY_EXIT_CODE
from semgrep.error import SemgrepError
from semgrep.ignores import IGNORE_FILE_NAME
from semgrep.meta import generate_meta_from_environment
from semgrep.metric_manager import metric_manager
from semgrep.output import OutputHandler
from semgrep.output import OutputSettings
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatchMap
from semgrep.semgrep_app import ScanHandler
from semgrep.types import MetricsState
from semgrep.util import set_flags
from semgrep.verbose_logging import getLogger


logger = getLogger(__name__)

# These patterns are excluded via --exclude regardless of other ignore configuration
ALWAYS_EXCLUDE_PATTERNS = [".semgrep/", ".semgrep_logs/"]

# These patterns are excluded via --exclude unless the user provides their own .semgrepignore
DEFAULT_EXCLUDE_PATTERNS = ["test/", "tests/", "*_test.go"]


def yield_valid_patterns(patterns: Iterable[str]) -> Iterable[str]:
    """
    Parses patterns from semgrep.dev and returns the lines that
    are non-empty and do not start with #
    """
    for pattern in patterns:
        pattern = pattern.strip()

        if not pattern:
            continue
        if pattern.startswith("#"):
            continue

        yield pattern


def yield_exclude_paths(requested_patterns: Sequence[str]) -> Iterable[str]:
    patterns = [*yield_valid_patterns(requested_patterns), *ALWAYS_EXCLUDE_PATTERNS]
    if Path(IGNORE_FILE_NAME).is_file() and not requested_patterns:
        patterns.extend(DEFAULT_EXCLUDE_PATTERNS)

    yield from patterns


@click.command(context_settings=CONTEXT_SETTINGS)
@click.pass_context
@scan_options
@click.option(
    "--audit-on",
    envvar=["SEMGREP_AUDIT_ON"],
    multiple=True,
    type=str,
    hidden=True,
)
def ci(
    ctx: click.Context,
    *,
    audit_on: Sequence[str],
    autofix: bool,
    baseline_commit: Optional[str],
    debug: bool,
    dryrun: bool,
    enable_nosem: bool,
    enable_version_check: bool,
    exclude: Optional[Tuple[str, ...]],
    force_color: bool,
    include: Optional[Tuple[str, ...]],
    jobs: int,
    max_chars_per_line: int,
    max_lines_per_finding: int,
    max_memory: int,
    max_target_bytes: int,
    optimizations: str,
    output: Optional[str],
    quiet: bool,
    rewrite_rule_ids: bool,
    scan_unknown_extensions: bool,
    time_flag: bool,
    timeout: int,
    timeout_threshold: int,
    use_git_ignore: bool,
    verbose: bool,
) -> None:
    set_flags(verbose=verbose, debug=debug, quiet=quiet, force_color=force_color)
    # Metrics always on for `semgrep ci`
    metric_manager.configure(MetricsState.ON, MetricsState.ON)

    # Check that we have valid api token
    token = Authentication.get_token()
    if not token:
        logger.info("run `semgrep login` before using `semgrep ci`")
        sys.exit(INVALID_API_KEY_EXIT_CODE)
    if not Authentication.is_valid_token(token):
        logger.info(
            "API token not valid. Try to run `semgrep logout` and `semgrep login` again.",
        )
        sys.exit(INVALID_API_KEY_EXIT_CODE)

    scan_handler = ScanHandler(token)

    metadata = generate_meta_from_environment(baseline_commit)

    try:
        logger.info("Fetching configuration from semgrep.dev")
        scan_handler.start_scan(metadata.to_dict())
    except Exception as e:
        logger.info(f"Failed to start scan so exiting...")
        exit_code = scan_handler.fail_open_exit_code(
            metadata.repo_name, FATAL_EXIT_CODE
        )
        if exit_code == 0:
            logger.info(
                "Fail Open is configured for this repository on semgrep.dev so exiting with code 0",
            )
        sys.exit(exit_code)

    # Append ignores configured on semgrep.dev
    assert exclude is not None  # exclude is default empty tuple
    exclude = (
        *exclude,
        *yield_exclude_paths(scan_handler.ignore_patterns),
    )
    logger.info(
        f"Adding ignore patterns configured on semgrep.dev as `--exclude` options: {exclude}"
    )

    output_settings = OutputSettings(
        output_format=OutputFormat.TEXT,
        output_destination=output,
        verbose_errors=verbose,
        timeout_threshold=timeout_threshold,
        output_time=time_flag,
        output_per_finding_max_lines_limit=max_lines_per_finding,
        output_per_line_max_chars_limit=max_chars_per_line,
    )
    output_handler = OutputHandler(output_settings)
    start = time.time()

    try:
        (
            filtered_matches_by_rule,
            semgrep_errors,
            all_targets,
            ignore_log,
            filtered_rules,
            profiler,
            profiling_data,
            shown_severities,
        ) = semgrep.semgrep_main.main(
            output_handler=output_handler,
            target=[os.curdir],  # semgrep ci only scans cwd
            pattern=None,
            lang=None,
            configs=(scan_handler.scan_rules_url,),
            no_rewrite_rule_ids=(not rewrite_rule_ids),
            jobs=jobs,
            include=include,
            exclude=exclude,
            max_target_bytes=max_target_bytes,
            autofix=autofix,
            dryrun=dryrun,
            disable_nosem=(not enable_nosem),
            no_git_ignore=(not use_git_ignore),
            timeout=timeout,
            max_memory=max_memory,
            timeout_threshold=timeout_threshold,
            skip_unknown_extensions=(not scan_unknown_extensions),
            optimizations=optimizations,
            baseline_commit=metadata.base_commit_ref,
        )
    except SemgrepError as e:
        output_handler.handle_semgrep_errors([e])
        output_handler.output({}, all_targets=set(), filtered_rules=[])
        logger.info(f"Encountered error when running rules: {e}")
        if isinstance(e, SemgrepError):
            exit_code = e.code
        else:
            exit_code = FATAL_EXIT_CODE
        exit_code = scan_handler.report_failure(exit_code)
        if exit_code == 0:
            logger.info(
                "Fail Open is configured for this repository on semgrep.dev so exiting with code 0",
            )
        sys.exit(exit_code)

    total_time = time.time() - start

    # Split up rules into respective categories:
    blocking_rules: List[Rule] = []
    nonblocking_rules: List[Rule] = []
    cai_rules: List[Rule] = []
    for rule in filtered_rules:
        if rule.is_blocking:
            blocking_rules.append(rule)
        else:
            if "r2c-internal-cai" in rule.id:
                cai_rules.append(rule)
            else:
                nonblocking_rules.append(rule)

    # Split up matches into respective categories
    blocking_matches_by_rule: RuleMatchMap = {}
    nonblocking_matches_by_rule: RuleMatchMap = {}
    cai_matches_by_rule: RuleMatchMap = {}
    for k, v in filtered_matches_by_rule.items():
        if k.is_blocking:
            blocking_matches_by_rule[k] = v
        else:
            if "r2c-internal-cai" in k.id:
                cai_matches_by_rule[k] = v
            else:
                nonblocking_matches_by_rule[k] = v

    num_cai_findings = sum(len(v) for v in cai_matches_by_rule.values())
    num_nonblocking_findings = sum(len(v) for v in nonblocking_matches_by_rule.values())

    output_handler.output(
        blocking_matches_by_rule,
        all_targets=all_targets,
        ignore_log=ignore_log,
        profiler=profiler,
        filtered_rules=filtered_rules,
        profiling_data=profiling_data,
        severities=shown_severities,
    )
    logger.info(
        f"Ran {len(blocking_rules)} blocking rules, {len(nonblocking_rules)} audit rules, and {len(cai_rules)} internal rules used for rule recommendations."
    )
    if num_nonblocking_findings:
        logger.info(
            f"{num_nonblocking_findings} findings were from rules in audit rule board. These non-blocking findings are not displayed."
        )

    logger.info("Reporting findings to semgrep.dev ...")
    scan_handler.report_findings(
        filtered_matches_by_rule,
        semgrep_errors,
        filtered_rules,
        all_targets,
        total_time,
    )
    logger.info(f"Success.")

    audit_mode = metadata.event_name in audit_on
    if num_nonblocking_findings > 0:
        if audit_mode:
            logger.info(
                f"Audit mode is on for {metadata.event_name}, so exiting with code 0 even if matches found",
            )
            exit_code = 0
        else:
            logger.info("Has findings for blocking rules so exiting with code 1")
            exit_code = 1
    else:
        logger.info("No findings so exiting with code 0")
        exit_code = 0

    sys.exit(exit_code)
