import os
import sys
import time
from contextlib import contextmanager
from pathlib import Path
from typing import Iterable
from typing import Iterator
from typing import List
from typing import Optional
from typing import Sequence
from typing import Tuple

import click

import semgrep.semgrep_main
from semgrep.app import auth
from semgrep.app.scans import ScanHandler
from semgrep.commands.scan import CONTEXT_SETTINGS
from semgrep.commands.scan import scan_options
from semgrep.commands.wrapper import handle_command_errors
from semgrep.constants import OutputFormat
from semgrep.error import FATAL_EXIT_CODE
from semgrep.error import INVALID_API_KEY_EXIT_CODE
from semgrep.error import SemgrepError
from semgrep.ignores import IGNORE_FILE_NAME
from semgrep.meta import generate_meta_from_environment
from semgrep.meta import GithubMeta
from semgrep.meta import GitMeta
from semgrep.metrics import MetricsState
from semgrep.output import OutputHandler
from semgrep.output import OutputSettings
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatchMap
from semgrep.state import get_state
from semgrep.util import git_check_output
from semgrep.util import unit_str
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


@contextmanager
def fix_head_if_github_action(metadata: GitMeta) -> Iterator[None]:
    """
    GHA can checkout the incorrect commit for a PR (it will create a fake merge commit),
    so we need to reset the head to the actual PR branch head before continuing.

    Assumes cwd is a valid git project and that if we are in github-actions pull_request,
    metadata.head_branch_hash point to head commit of current branch
    """
    if isinstance(metadata, GithubMeta) and metadata.is_pull_request_event:
        assert metadata.head_branch_hash is not None  # Not none when github action PR
        assert metadata.base_branch_hash is not None

        logger.info("Fixing git state for github action pull request")

        logger.debug("Calling git rev-parse HEAD")
        stashed_rev = git_check_output(["git", "rev-parse", "HEAD"])
        logger.debug(f"stashed_rev: {stashed_rev}")

        logger.info(
            f"Not on head ref: {metadata.head_branch_hash}; checking that out now."
        )
        git_check_output(["git", "checkout", metadata.head_branch_hash])

        try:
            yield
        finally:
            logger.info(f"Returning to original head revision {stashed_rev}")
            git_check_output(["git", "checkout", stashed_rev])
    else:
        # Do nothing
        yield


@click.command(context_settings=CONTEXT_SETTINGS)
@click.pass_context
@scan_options
@click.option(
    "--audit-on",
    envvar="SEMGREP_AUDIT_ON",
    multiple=True,
    type=str,
    hidden=True,
)
@click.option(
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
    envvar="SEMGREP_RULES",
)
@click.option(
    "--dry-run",
    is_flag=True,
    help="""
        When set, will not start a scan on semgrep.dev and will not report findings.
        Instead will print out json objects it would have sent.
    """,
)
@click.option(
    "--sca",
    is_flag=True,
    hidden=True,
)
@handle_command_errors
def ci(
    ctx: click.Context,
    *,
    audit_on: Sequence[str],
    autofix: bool,
    baseline_commit: Optional[str],
    core_opts: Optional[str],
    config: Optional[Tuple[str, ...]],
    debug: bool,
    dry_run: bool,
    emacs: bool,
    enable_nosem: bool,
    enable_version_check: bool,
    exclude: Optional[Tuple[str, ...]],
    force_color: bool,
    gitlab_sast: bool,
    gitlab_secrets: bool,
    include: Optional[Tuple[str, ...]],
    jobs: int,
    json: bool,
    junit_xml: bool,
    max_chars_per_line: int,
    max_lines_per_finding: int,
    max_memory: int,
    max_target_bytes: int,
    metrics: Optional[MetricsState],
    metrics_legacy: Optional[MetricsState],
    optimizations: str,
    output: Optional[str],
    sarif: bool,
    quiet: bool,
    rewrite_rule_ids: bool,
    sca: bool,
    scan_unknown_extensions: bool,
    time_flag: bool,
    timeout_threshold: int,
    timeout: int,
    use_git_ignore: bool,
    verbose: bool,
    vim: bool,
) -> None:
    """
    The recommended way to run semgrep in CI

    In pull_request/merge_request (PR/MR) contexts, `semgrep ci` will only report findings
    that were introduced by the PR/MR.

    When logged in, `semgrep ci` runs rules configured on Semgrep App and sends findings
    to your findings dashboard.

    Only displays findings that were marked as blocking.
    """
    state = get_state()
    state.terminal.configure(
        verbose=verbose, debug=debug, quiet=quiet, force_color=force_color
    )

    state.metrics.configure(metrics, metrics_legacy)
    scan_handler = None

    token = state.app_session.token
    if not token and not config:
        # Not logged in and no explicit config
        logger.info("run `semgrep login` before using `semgrep ci` or set `--config`")
        sys.exit(INVALID_API_KEY_EXIT_CODE)
    elif not token and config:
        # Not logged in but has explicit config
        logger.info(f"Running `semgrep ci` without API token but with configs {config}")
    elif token and config:
        # Logged in but has explicit config
        logger.info(
            "Cannot run `semgrep ci` while logged in and with explicit config. Use semgrep.dev to configure rules to run."
        )
        sys.exit(FATAL_EXIT_CODE)
    elif token:
        if not auth.is_valid_token(token):
            logger.info(
                "API token not valid. Try to run `semgrep logout` and `semgrep login` again.",
            )
            sys.exit(INVALID_API_KEY_EXIT_CODE)
        scan_handler = ScanHandler(dry_run)
    else:  # impossible state… until we break the code above
        raise RuntimeError("The token and/or config are misconfigured")

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

    output_settings = OutputSettings(
        output_format=output_format,
        output_destination=output,
        verbose_errors=verbose,
        timeout_threshold=timeout_threshold,
        output_time=time_flag,
        output_per_finding_max_lines_limit=max_lines_per_finding,
        output_per_line_max_chars_limit=max_chars_per_line,
    )
    output_handler = OutputHandler(output_settings)
    metadata = generate_meta_from_environment(baseline_commit)

    logger.info("Scan environment:")
    logger.info(
        f"  versions    - semgrep {semgrep.__VERSION__} on python {sys.version_info.major}.{sys.version_info.minor}.{sys.version_info.micro}"
    )
    logger.info(
        f"  environment - running in environment {metadata.environment}, triggering event is {metadata.event_name}"
    )
    to_server = (
        ""
        if state.env.semgrep_url == "https://semgrep.dev"
        else f" to {state.env.semgrep_url}"
    )
    if scan_handler:
        logger.info(
            f"  semgrep.dev - authenticated{to_server} as {scan_handler.deployment_name}"
        )
    if sca:
        logger.info("  running an SCA scan")
    logger.info("")

    try:
        with fix_head_if_github_action(metadata):
            if scan_handler:
                metadata_dict = metadata.to_dict()
                metadata_dict["is_sca_scan"] = sca

            try:
                logger.info("Fetching configuration from semgrep.dev")
                # Note this needs to happen within fix_head_if_github_action
                # so that metadata of current commit is correct
                if scan_handler:
                    scan_handler.start_scan(metadata_dict)
                    config = (scan_handler.scan_rules_url,)
            except Exception as e:
                import traceback

                traceback.print_exc()
                logger.info(f"Could not start scan {e}")
                sys.exit(FATAL_EXIT_CODE)

            # Append ignores configured on semgrep.dev
            requested_excludes = scan_handler.ignore_patterns if scan_handler else []
            if requested_excludes:
                logger.info(
                    f"Adding ignore patterns configured on semgrep.dev as `--exclude` options: {exclude}"
                )

            assert exclude is not None  # exclude is default empty tuple
            exclude = (*exclude, *yield_exclude_paths(requested_excludes))

            assert config  # Config has to be defined here. Helping mypy out
            start = time.time()
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
                core_opts_str=core_opts,
                output_handler=output_handler,
                target=[os.curdir],  # semgrep ci only scans cwd
                pattern=None,
                lang=None,
                configs=config,
                no_rewrite_rule_ids=(not rewrite_rule_ids),
                jobs=jobs,
                include=include,
                exclude=exclude,
                max_target_bytes=max_target_bytes,
                autofix=scan_handler.autofix if scan_handler else False,
                dryrun=True,
                # Always true, as we want to always report all findings, even
                # ignored ones, to the backend
                disable_nosem=True,
                no_git_ignore=(not use_git_ignore),
                timeout=timeout,
                max_memory=max_memory,
                timeout_threshold=timeout_threshold,
                skip_unknown_extensions=(not scan_unknown_extensions),
                optimizations=optimizations,
                baseline_commit=metadata.merge_base_ref,
            )
    except SemgrepError as e:
        output_handler.handle_semgrep_errors([e])
        output_handler.output({}, all_targets=set(), filtered_rules=[])
        logger.info(f"Encountered error when running rules: {e}")
        if isinstance(e, SemgrepError):
            exit_code = e.code
        else:
            exit_code = FATAL_EXIT_CODE
        if scan_handler:
            scan_handler.report_failure(exit_code)
        sys.exit(exit_code)

    total_time = time.time() - start

    # Split up rules into respective categories:
    blocking_rules: List[Rule] = []
    nonblocking_rules: List[Rule] = []
    cai_rules: List[Rule] = []
    for rule in filtered_rules:
        if "r2c-internal-cai" in rule.id:
            cai_rules.append(rule)
        else:
            if rule.is_blocking:
                blocking_rules.append(rule)
            else:
                nonblocking_rules.append(rule)

    # Split up matches into respective categories
    blocking_matches_by_rule: RuleMatchMap = {}
    nonblocking_matches_by_rule: RuleMatchMap = {}
    cai_matches_by_rule: RuleMatchMap = {}

    # Since we keep nosemgrep disabled for the actual scan, we have to apply
    # that flag here
    keep_ignored = not enable_nosem or output_handler.formatter.keep_ignores()
    for rule, matches in filtered_matches_by_rule.items():
        if "r2c-internal-cai" in rule.id:
            cai_matches_by_rule[rule] = [
                match for match in matches if not match.is_ignored or keep_ignored
            ]
        else:
            # Filter out any matches that are triaged as ignored on the app
            if scan_handler:
                matches = list(
                    filter(
                        lambda match: match.syntactic_id  # type: ignore
                        not in scan_handler.skipped_syntactic_ids
                        and match.match_based_id
                        not in scan_handler.skipped_match_based_ids,
                        matches,
                    )
                )
            if rule.is_blocking:
                blocking_matches_by_rule[rule] = [
                    match for match in matches if not match.is_ignored or keep_ignored
                ]
            else:
                nonblocking_matches_by_rule[rule] = [
                    match for match in matches if not match.is_ignored or keep_ignored
                ]

    num_nonblocking_findings = sum(len(v) for v in nonblocking_matches_by_rule.values())
    num_blocking_findings = sum(len(v) for v in blocking_matches_by_rule.values())

    output_handler.output(
        {**blocking_matches_by_rule, **nonblocking_matches_by_rule},
        all_targets=all_targets,
        ignore_log=ignore_log,
        profiler=profiler,
        filtered_rules=filtered_rules,
        profiling_data=profiling_data,
        severities=shown_severities,
    )

    logger.info("CI scan completed successfully.")
    logger.info(
        f"  Found {unit_str(num_blocking_findings + num_nonblocking_findings, 'finding')} ({num_blocking_findings} blocking) from {unit_str(len(blocking_rules) + len(nonblocking_rules), 'rule')}."
    )
    if scan_handler:
        logger.info("  Uploading findings to Semgrep App.")
        scan_handler.report_findings(
            filtered_matches_by_rule,
            semgrep_errors,
            filtered_rules,
            all_targets,
            total_time,
            metadata.commit_datetime,
        )

    audit_mode = metadata.event_name in audit_on
    if num_blocking_findings > 0:
        if audit_mode:
            logger.info(
                f"  Audit mode is on for {metadata.event_name}, so exiting with code 0 even if matches found",
            )
            exit_code = 0
        else:
            logger.info("  Has findings for blocking rules so exiting with code 1")
            exit_code = 1
    else:
        logger.info("  No blocking findings so exiting with code 0")
        exit_code = 0

    if enable_version_check:
        from semgrep.app.version import version_check

        version_check()

    sys.exit(exit_code)
