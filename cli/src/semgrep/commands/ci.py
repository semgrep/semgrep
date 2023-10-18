import atexit
import os
import sys
import time
from collections import defaultdict
from pathlib import Path
from typing import Iterable
from typing import List
from typing import Optional
from typing import Sequence
from typing import Tuple

import click
from rich.padding import Padding
from rich.progress import Progress
from rich.progress import SpinnerColumn
from rich.progress import TextColumn
from rich.table import Table

import semgrep.run_scan
import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.app import auth
from semgrep.app.scans import ScanCompleteResult
from semgrep.app.scans import ScanHandler
from semgrep.commands.install import run_install_semgrep_pro
from semgrep.commands.scan import scan_options
from semgrep.commands.wrapper import handle_command_errors
from semgrep.console import console
from semgrep.console import Title
from semgrep.constants import OutputFormat
from semgrep.engine import EngineType
from semgrep.error import FATAL_EXIT_CODE
from semgrep.error import INVALID_API_KEY_EXIT_CODE
from semgrep.error import MISSING_CONFIG_EXIT_CODE
from semgrep.error import SemgrepError
from semgrep.ignores import IGNORE_FILE_NAME
from semgrep.meta import generate_meta_from_environment
from semgrep.meta import GithubMeta
from semgrep.meta import GitMeta
from semgrep.metrics import MetricsState
from semgrep.output import OutputHandler
from semgrep.output import OutputSettings
from semgrep.project import ProjectConfig
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
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

# Conversion of product codes to product names
PRODUCT_NAMES_MAP = {
    "sast": "Code",
    "sca": "Supply Chain",
    "secrets": "Secrets",
}


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


def fix_head_if_github_action(metadata: GitMeta) -> None:
    """
    GHA can checkout the incorrect commit for a PR (it will create a fake merge commit),
    so we need to reset the head to the actual PR branch head before continuing.

    Assumes cwd is a valid git project and that if we are in github-actions pull_request,
    metadata.head_branch_hash point to head commit of current branch
    """
    if not (isinstance(metadata, GithubMeta) and metadata.is_pull_request_event):
        return

    assert metadata.head_branch_hash is not None  # Not none when github action PR
    assert metadata.base_branch_hash is not None

    logger.info("Fixing git state for github action pull request")

    logger.debug("Calling git rev-parse HEAD")
    stashed_rev = git_check_output(["git", "rev-parse", "HEAD"])
    logger.debug(f"stashed_rev: {stashed_rev}")

    logger.info(f"Not on head ref: {metadata.head_branch_hash}; checking that out now.")
    git_check_output(["git", "checkout", metadata.head_branch_hash])

    atexit.register(git_check_output, ["git", "checkout", stashed_rev], os.getcwd())


@click.command()
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
    envvar="SEMGREP_RULES",
)
@click.option(
    "--dry-run",
    is_flag=True,
)
@click.option(
    "--supply-chain",
    is_flag=True,
)
@click.option("--code", is_flag=True, hidden=True)
@click.option("--beta-testing-secrets", is_flag=True, hidden=True)
@click.option(
    "--secrets",
    "run_secrets_flag",
    is_flag=True,
    hidden=True,
)
@click.option(
    "--suppress-errors/--no-suppress-errors",
    "suppress_errors",
    default=True,
    envvar="SEMGREP_SUPPRESS_ERRORS",
)
@handle_command_errors
def ci(
    ctx: click.Context,
    *,
    audit_on: Sequence[str],
    autofix: bool,
    baseline_commit: Optional[str],
    # TODO: Remove after October 2023. Left for a error message
    # redirect to `--secrets` aka run_secrets_flag.
    beta_testing_secrets: bool,
    code: bool,
    config: Optional[Tuple[str, ...]],
    debug: bool,
    diff_depth: int,
    dump_command_for_core: bool,
    dry_run: bool,
    enable_nosem: bool,
    enable_version_check: bool,
    exclude: Optional[Tuple[str, ...]],
    exclude_rule: Optional[Tuple[str, ...]],
    suppress_errors: bool,
    force_color: bool,
    include: Optional[Tuple[str, ...]],
    jobs: int,
    matching_explanations: bool,
    max_chars_per_line: int,
    max_lines_per_finding: int,
    max_memory: Optional[int],
    max_target_bytes: int,
    metrics: Optional[MetricsState],
    optimizations: str,
    dataflow_traces: Optional[bool],
    output: Optional[str],
    output_format: OutputFormat,
    requested_engine: EngineType,
    quiet: bool,
    rewrite_rule_ids: bool,
    run_secrets_flag: bool,
    allow_untrusted_postprocessors: bool,
    supply_chain: bool,
    scan_unknown_extensions: bool,
    time_flag: bool,
    timeout_threshold: int,
    timeout: int,
    interfile_timeout: Optional[int],
    use_git_ignore: bool,
    verbose: bool,
) -> None:
    state = get_state()
    state.terminal.configure(
        verbose=verbose,
        debug=debug,
        quiet=quiet,
        force_color=force_color,
        output_format=output_format,
    )

    state.metrics.configure(metrics)
    state.error_handler.configure(suppress_errors)
    scan_handler = None

    token = state.app_session.token
    if not token and not config:
        # Not logged in and no explicit config
        logger.info("run `semgrep login` before using `semgrep ci` or set `--config`")
        sys.exit(INVALID_API_KEY_EXIT_CODE)
    elif not token and config:
        # Not logged in but has explicit config
        pass
    elif token and config:
        # Logged in but has explicit config
        logger.info(
            "Cannot run `semgrep ci` with --config while logged in. The `semgrep ci` command will upload findings to semgrep-app and those findings must come from rules configured there. Drop the `--config` to use rules configured on semgrep.dev or log out."
        )
        sys.exit(FATAL_EXIT_CODE)
    elif token:
        deployment_name = auth.get_deployment_from_token(token)
        if not deployment_name:
            logger.info(
                "API token not valid. Try to run `semgrep logout` and `semgrep login` again.",
            )
            sys.exit(INVALID_API_KEY_EXIT_CODE)
        scan_handler = ScanHandler(dry_run=dry_run, deployment_name=deployment_name)
    else:  # impossible state… until we break the code above
        raise RuntimeError("The token and/or config are misconfigured")

    if beta_testing_secrets:
        logger.info("Please use --secrets instead of --beta-testing-secrets")
        sys.exit(FATAL_EXIT_CODE)

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

    console.print(Title("Debugging Info"))
    debugging_table = Table.grid(padding=(0, 1))
    debugging_table.add_row(
        "versions",
        "-",
        f"semgrep [bold]{semgrep.__VERSION__}[/bold] on python [bold]{sys.version_info.major}.{sys.version_info.minor}.{sys.version_info.micro}[/bold]",
    )
    debugging_table.add_row(
        "environment",
        "-",
        f"running in environment [bold]{metadata.environment}[/bold], triggering event is [bold]{metadata.event_name}[/bold]",
    )

    console.print(Title("Scan Environment", order=2))
    console.print(debugging_table, markup=True)

    fix_head_if_github_action(metadata)

    try:
        # Note this needs to happen within fix_head_if_github_action
        # so that metadata of current commit is correct
        if scan_handler:
            console.print(Title("Connection", order=2))

            # Build project_metadata
            project_meta: out.ProjectMetadata = metadata.to_project_metadata()
            project_meta.is_sca_scan = supply_chain
            project_meta.is_code_scan = code
            project_meta.is_secrets_scan = run_secrets_flag

            # TODO: move ProjectConfig to ATD too
            project_config = ProjectConfig.load_all()

            # Build scan_metadata
            if code:
                scan_handler.scan_metadata.requested_products.append(
                    out.Product(out.SAST())
                )
            if supply_chain:
                scan_handler.scan_metadata.requested_products.append(
                    out.Product(out.SCA())
                )
            if run_secrets_flag:
                scan_handler.scan_metadata.requested_products.append(
                    out.Product(out.Secrets())
                )

            with Progress(
                TextColumn("  {task.description}"),
                SpinnerColumn(spinner_name="simpleDotsScrolling"),
                console=console,
            ) as progress_bar:
                at_url_maybe = (
                    f" at [bold]{state.env.semgrep_url}[/bold]"
                    if state.env.semgrep_url != "https://semgrep.dev"
                    else ""
                )

                start_scan_desc = f"Reporting start of scan for [bold]{scan_handler.deployment_name}[/bold]"
                start_scan_task = progress_bar.add_task(start_scan_desc)
                scan_handler.start_scan(project_meta, project_config)
                if scan_handler.scan_id:
                    start_scan_desc += f" (scan_id={scan_handler.scan_id})"
                progress_bar.update(
                    start_scan_task, completed=100, description=start_scan_desc
                )

                connection_task = progress_bar.add_task(
                    f"Fetching configuration from Semgrep Cloud Platform{at_url_maybe}"
                )
                scan_handler.fetch_and_init_scan_config(project_meta.to_json())
                progress_bar.update(connection_task, completed=100)

                product_names = [
                    PRODUCT_NAMES_MAP.get(p) or p for p in scan_handler.enabled_products
                ]
                products_str = ", ".join(product_names) or "None"
                products_task = progress_bar.add_task(
                    f"Enabled products: [bold]{products_str}[/bold]"
                )
                progress_bar.update(products_task, completed=100)

            if (
                scan_handler.rules == '{"rules":[]}'
                and scan_handler.enabled_products == ["sast"]
            ):
                console.print(
                    f"No rules configured. Visit {state.env.semgrep_url}/orgs/-/policies to configure rules to scan your code.\n"
                )
                sys.exit(MISSING_CONFIG_EXIT_CODE)

            config = (scan_handler.rules,)

    except Exception as e:
        import traceback

        traceback.print_exc()
        logger.info(f"Could not start scan {e}")
        sys.exit(FATAL_EXIT_CODE)

    # Handled error outside engine type for more actionable advice.
    if run_secrets_flag and requested_engine is EngineType.OSS:
        logger.info(
            "The --secrets and --oss flags are incompatible. Semgrep Secrets is a proprietary extension of Open Source Semgrep."
        )
        sys.exit(FATAL_EXIT_CODE)

    run_secrets = run_secrets_flag or bool(
        scan_handler and "secrets" in scan_handler.enabled_products
    )

    engine_type = EngineType.decide_engine_type(
        requested_engine=requested_engine,
        scan_handler=scan_handler,
        git_meta=metadata,
        run_secrets=run_secrets,
        enable_pro_diff_scan=diff_depth >= 0,
    )

    # set default settings for selected engine type
    if dataflow_traces is None:
        dataflow_traces = engine_type.has_dataflow_traces

    if max_memory is None:
        max_memory = engine_type.default_max_memory

    if interfile_timeout is None:
        interfile_timeout = engine_type.default_interfile_timeout

    if engine_type.is_pro:
        console.print(Padding(Title("Engine", order=2), (1, 0, 0, 0)))
        if run_secrets:
            console.print("Semgrep Secrets requires Semgrep Pro Engine")
        if engine_type.check_if_installed():
            console.print(
                f"Using Semgrep Pro Version: [bold]{engine_type.get_pro_version()}[/bold]",
                markup=True,
            )
            console.print(
                f"Installed at [bold]{engine_type.get_binary_path()}[/bold]",
                markup=True,
            )
        else:
            run_install_semgrep_pro()

    try:
        excludes_from_app = scan_handler.ignore_patterns if scan_handler else []

        assert exclude is not None  # exclude is default empty tuple
        exclude = (*exclude, *yield_exclude_paths(excludes_from_app))
        assert config  # Config has to be defined here. Helping mypy out
        start = time.time()

        if scan_handler and not scan_handler.enabled_products:
            raise SemgrepError(
                "No products are enabled for this organization. Please enable a product in the Settings > Deployment tab of Semgrep Cloud Platform or reach out to support@semgrep.com for assistance."
            )

        # TODO? we're not passing time_flag below (or matching_explanations),
        # is it indended?
        (
            filtered_matches_by_rule,
            semgrep_errors,
            renamed_targets,
            ignore_log,
            filtered_rules,
            profiler,
            output_extra,
            shown_severities,
            dependencies,
            dependency_parser_errors,
            num_executed_rules,
            contributions,
        ) = semgrep.run_scan.run_scan(
            engine_type=engine_type,
            run_secrets=run_secrets,
            output_handler=output_handler,
            target=[os.curdir],  # semgrep ci only scans cwd
            pattern=None,
            lang=None,
            configs=config,
            no_rewrite_rule_ids=(not rewrite_rule_ids),
            dump_command_for_core=dump_command_for_core,
            jobs=jobs,
            include=include,
            exclude=exclude,
            exclude_rule=exclude_rule,
            max_target_bytes=max_target_bytes,
            autofix=scan_handler.autofix if scan_handler else False,
            dryrun=True,
            # Always true, as we want to always report all findings, even
            # ignored ones, to the backend
            disable_nosem=True,
            no_git_ignore=(not use_git_ignore),
            timeout=timeout,
            max_memory=max_memory,
            interfile_timeout=interfile_timeout,
            timeout_threshold=timeout_threshold,
            skip_unknown_extensions=(not scan_unknown_extensions),
            allow_untrusted_postprocessors=allow_untrusted_postprocessors,
            optimizations=optimizations,
            baseline_commit=metadata.merge_base_ref,
            baseline_commit_is_mergebase=True,
            diff_depth=diff_depth,
            dump_contributions=True,
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
    prev_scan_rules: List[Rule] = []
    cai_rules: List[Rule] = []
    for rule in filtered_rules:
        if "r2c-internal-cai" in rule.id:
            cai_rules.append(rule)
        elif rule.from_transient_scan:
            prev_scan_rules.append(rule)
        elif rule.is_blocking:
            blocking_rules.append(rule)
        else:
            nonblocking_rules.append(rule)

    # Split up matches into respective categories
    non_cai_matches_by_rule: RuleMatchMap = defaultdict(list)
    blocking_matches: List[RuleMatch] = []
    nonblocking_matches: List[RuleMatch] = []
    cai_matches: List[RuleMatch] = []

    # Remove the prev scan matches by the rules that are in the current scan
    # Done before the next loop to avoid interfering with ignore logic
    removed_prev_scan_matches = {
        rule: [match for match in matches]
        for rule, matches in filtered_matches_by_rule.items()
        if (not rule.from_transient_scan)
    }

    # Since we keep nosemgrep disabled for the actual scan, we have to apply
    # that flag here
    keep_ignored = not enable_nosem or output_handler.formatter.keep_ignores()
    for rule, matches in removed_prev_scan_matches.items():
        # Filter out any matches that are triaged as ignored on the app
        if scan_handler:
            matches = [
                match
                for match in matches
                if match.syntactic_id not in scan_handler.skipped_syntactic_ids
                and match.match_based_id not in scan_handler.skipped_match_based_ids
            ]

        for match in matches:
            if match.is_ignored and not keep_ignored:
                continue

            applicable_result_list = (
                cai_matches
                if "r2c-internal-cai" in rule.id
                else blocking_matches
                if match.is_blocking
                else nonblocking_matches
            )
            applicable_result_list.append(match)
            if "r2c-internal-cai" not in rule.id:
                non_cai_matches_by_rule[rule].append(match)

    num_nonblocking_findings = len(nonblocking_matches)
    num_blocking_findings = len(blocking_matches)

    output_handler.output(
        non_cai_matches_by_rule,
        all_targets=output_extra.all_targets,
        ignore_log=ignore_log,
        profiler=profiler,
        filtered_rules=[*blocking_rules, *nonblocking_rules],
        extra=output_extra,
        severities=shown_severities,
        is_ci_invocation=True,
        engine_type=engine_type,
    )

    logger.info("CI scan completed successfully.")
    logger.info(
        f"  Found {unit_str(num_blocking_findings + num_nonblocking_findings, 'finding')} ({num_blocking_findings} blocking) from {unit_str(num_executed_rules, 'rule')}."
    )

    complete_result: ScanCompleteResult | None = None
    if scan_handler:
        with Progress(
            TextColumn("  {task.description}"),
            SpinnerColumn(spinner_name="simpleDotsScrolling"),
            console=console,
        ) as progress_bar:
            complete_result = scan_handler.report_findings(
                filtered_matches_by_rule,
                semgrep_errors,
                filtered_rules,
                output_extra.all_targets,
                renamed_targets,
                ignore_log.unsupported_lang_paths,
                output_extra.parsing_data,
                total_time,
                metadata.commit_datetime,
                dependencies,
                dependency_parser_errors,
                contributions,
                engine_type,
                progress_bar,
            )

        if complete_result.success:
            logger.info("  View results in Semgrep Cloud Platform:")
        else:
            logger.info(
                "  Semgrep Cloud Platform is still processing the results of the scan, they will be available soon:"
            )

        logger.info(
            f"    https://semgrep.dev/orgs/{scan_handler.deployment_name}/findings"
        )
        if "r2c-internal-project-depends-on" in scan_handler.rules:
            logger.info(
                f"    https://semgrep.dev/orgs/{scan_handler.deployment_name}/supply-chain"
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

    if complete_result and complete_result.app_block_override and not audit_mode:
        logger.info(
            f"  semgrep.dev is suggesting a non-zero exit code ({complete_result.app_block_reason})"
        )
        exit_code = 1

    if enable_version_check:
        from semgrep.app.version import version_check

        version_check()

    sys.exit(exit_code)
