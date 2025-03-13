import atexit
import json
import os
import sys
import time
from collections import defaultdict
from pathlib import Path
from typing import List
from typing import Mapping
from typing import Optional
from typing import Sequence
from typing import Tuple

import click
from attr import evolve
from rich.padding import Padding
from rich.progress import Progress
from rich.progress import SpinnerColumn
from rich.progress import TextColumn
from rich.table import Table

import semgrep.app.auth as auth
import semgrep.rpc_call
import semgrep.run_scan
import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep import tracing
from semgrep.app.project_config import ProjectConfig
from semgrep.app.scans import ScanHandler
from semgrep.commands.install import run_install_semgrep_pro
from semgrep.commands.scan import collect_additional_outputs
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
from semgrep.git import git_check_output
from semgrep.git import is_git_repo_empty
from semgrep.git import is_git_repo_root_approx
from semgrep.ignores import SEMGREPIGNORE_FILE_NAME
from semgrep.meta import generate_meta_from_environment
from semgrep.meta import GithubMeta
from semgrep.meta import GitMeta
from semgrep.metrics import MetricsState
from semgrep.output import OutputHandler
from semgrep.output import OutputSettings
from semgrep.parsing_data import ParsingData
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.rule_match import RuleMatchMap
from semgrep.state import get_state
from semgrep.target_manager import ALL_PRODUCTS
from semgrep.target_manager import SAST_PRODUCT
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


def is_valid_pattern(pattern: str) -> bool:
    """
    Parses patterns from semgrep.dev and returns the lines that
    are non-empty and do not start with #
    """
    pattern = pattern.strip()
    return pattern != "" and not pattern.startswith("#")


def get_exclude_paths(
    requested_patterns: Optional[Mapping[out.Product, Sequence[str]]],
) -> Mapping[out.Product, Sequence[str]]:
    patterns = {
        product: (
            [
                pattern.strip()
                for pattern in requested_patterns.get(product, [])
                if is_valid_pattern(pattern)
            ]
            if requested_patterns
            else []
        )
        for product in ALL_PRODUCTS
    }

    for product in ALL_PRODUCTS:
        patterns[product].extend(ALWAYS_EXCLUDE_PATTERNS)
        # This logic isn't clear to me, since I don't see why adding these
        # default patterns is done here or why it would depend on
        # .semgrepignore. But, we've had this for a while, so leaving it not to
        # potentially break things.
        if Path(SEMGREPIGNORE_FILE_NAME).is_file() and not requested_patterns:
            patterns[product].extend(DEFAULT_EXCLUDE_PATTERNS)

    return patterns


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
@click.option(
    "--secrets",
    "run_secrets_flag",
    is_flag=True,
)
@click.option(
    "--suppress-errors/--no-suppress-errors",
    "suppress_errors",
    default=True,
    envvar="SEMGREP_SUPPRESS_ERRORS",
)
@click.option(
    "--subdir",
    type=click.Path(allow_dash=True, path_type=Path),
)
@click.option(
    "--internal-ci-scan-results",
    "internal_ci_scan_results",
    is_flag=True,
    hidden=True,
)
@click.option(
    "--x-dump-rule-partitions",
    "dump_n_rule_partitions",
    type=int,
    default=0,
    hidden=True,
)
@click.option(
    "--x-dump-rule-partitions-dir",
    "dump_rule_partitions_dir",
    type=click.Path(allow_dash=True, path_type=Path),
    hidden=True,
)
@click.option(
    "--x-partial-config",
    "partial_config",
    type=click.Path(allow_dash=True, path_type=Path),
    hidden=True,
)
@click.option(
    "--x-partial-output",
    "partial_output",
    type=click.Path(allow_dash=True, path_type=Path),
    hidden=True,
)
@handle_command_errors
def ci(
    ctx: click.Context,
    *,
    audit_on: Sequence[str],
    autofix: bool,
    baseline_commit: Optional[str],
    historical_secrets: bool,
    internal_ci_scan_results: bool,
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
    max_log_list_entries: int,
    max_memory: Optional[int],
    max_target_bytes: int,
    metrics: Optional[MetricsState],
    optimizations: str,
    dataflow_traces: Optional[bool],
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
    requested_engine: EngineType,
    quiet: bool,
    rewrite_rule_ids: bool,
    run_secrets_flag: bool,
    disable_secrets_validation_flag: bool,
    allow_untrusted_validators: bool,
    supply_chain: bool,
    scan_unknown_extensions: bool,
    subdir: Optional[Path],
    time_flag: bool,
    timeout_threshold: int,
    timeout: int,
    interfile_timeout: Optional[int],
    trace: bool,
    trace_endpoint: str,
    use_git_ignore: bool,
    semgrepignore_v2: Optional[bool],
    force_novcs_project: bool,  # unused but needed to receive options from 'scan'
    force_project_root: Optional[
        str
    ],  # unused but needed to receive options from 'scan'
    verbose: bool,
    x_tr: bool,
    path_sensitive: bool,
    allow_local_builds: bool,
    dump_n_rule_partitions: Optional[int],
    dump_rule_partitions_dir: Optional[Path],
    partial_config: Optional[Path],
    partial_output: Optional[Path],
) -> None:
    state = get_state()

    state.traces.configure(trace, trace_endpoint)
    with tracing.TRACER.start_as_current_span("semgrep.commands.ci"):
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
        capture_core_stderr = not debug

        if subdir:
            subdir = subdir.resolve()  # normalize path & resolve symlinks
            # subdir.is_relative_to(Path.cwd()) is only available from Python 3.9
            try:
                subdir = subdir.relative_to(Path.cwd())
            except ValueError:
                logger.info(
                    "`semgrep ci --subdir` must be given a directory that is actually a subdirectory of the current directory"
                )
                sys.exit(FATAL_EXIT_CODE)

        if not is_git_repo_root_approx():
            logger.info(
                "WARNING: `semgrep ci` is meant to be run from the root of a git repo.\nWhen `semgrep ci` is not run from a git repo, it will not be able to perform all operations.\nWhen `semgrep ci` is run from a git repo, but not the root, links in the uploaded findings may be broken.\n\nTo run `semgrep ci` on only a subdirectory of a git repo, see `--subdir`."
            )

        if config and partial_config:
            logger.info(
                "The `--config` and `--x-partial-config` flags are mutually exclusive. They serve different purposes."
            )
            sys.exit(FATAL_EXIT_CODE)

        if (dump_n_rule_partitions and not dump_rule_partitions_dir) or (
            not dump_n_rule_partitions and dump_rule_partitions_dir
        ):
            logger.info(
                "Both or none of --x-dump-rule-partitions and --x-dump-rule-partitions-dir must be specified."
            )
            sys.exit(FATAL_EXIT_CODE)

        if partial_config and not partial_output:
            logger.info(
                "When --x-partial-config is specified, --x-partial-output must also be specified."
            )
            sys.exit(FATAL_EXIT_CODE)

        token = state.app_session.token
        if not token and not config:
            # Not logged in and no explicit config
            logger.info(
                "run `semgrep login` before using `semgrep ci` or use `semgrep scan` and set `--config`"
            )
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
            # Dumping partitions also signals the start of a scan.
            # Save the scan ID here so it can be used at a later stage.
            dump_scan_id_path = None
            if dump_rule_partitions_dir:
                dump_scan_id_path = dump_rule_partitions_dir / "scan_id.txt"

            # Partial scans also implies a dry run. We'll be saving results
            # to disk, but not upload them yet.
            if partial_output:
                dry_run = True

            scan_handler = ScanHandler(
                dry_run=dry_run,
                partial_output=partial_output,
                dump_scan_id_path=dump_scan_id_path,
            )
        else:  # impossible state… until we break the code above
            raise RuntimeError("The token and/or config are misconfigured")

        # Empty repo case:
        # `semgrep ci` should report success if we are scanning an empty repo;
        # check if the repo was empty and exit accordingly only after
        # 1) Informing the App that a scan started
        # 2) Sending scan results to the App
        #
        # The rest of the ``
        if is_git_repo_empty():
            # Prep any necessary metadata
            engine_type = EngineType.decide_engine_type(
                logged_in=auth.is_logged_in_weak(),
                engine_flag=requested_engine,
                ci_scan_handler=scan_handler,
            )

            # A lot of project metadata depends on git commands that fail in
            # empty repos; we gather whatever metadata we can and move forwards
            project_metadata = generate_meta_from_environment(
                None, subdir
            ).to_project_metadata()
            project_config = ProjectConfig.load_all()
            contributions = semgrep.rpc_call.contributions()
            if scan_handler:
                with Progress(
                    TextColumn("  {task.description}"),
                    SpinnerColumn(spinner_name="simpleDotsScrolling"),
                    console=console,
                ) as progress_bar:
                    # Inform the App that the scan started & ended
                    scan_handler.start_scan(project_metadata, project_config)
                    scan_handler.report_findings(
                        dict(),
                        [],
                        set(),
                        set(),
                        frozenset(),
                        0,  # Inform app that we are exiting with code 0
                        ParsingData(),
                        0.0,
                        "",
                        dict(),
                        [],
                        [],
                        contributions,
                        engine_type,
                        progress_bar,
                    )
                    sys.exit(0)

        # For account admins to force suppressing errors. This overrides the CLI flag!
        if scan_handler and scan_handler.always_suppress_errors:
            state.error_handler.configure(suppress_errors=True)

        metadata = generate_meta_from_environment(baseline_commit, subdir)

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
                    start_scan_desc = "Initializing scan"
                    start_scan_task = progress_bar.add_task(start_scan_desc)
                    scan_handler.start_scan(project_meta, project_config)
                    extra_fields = []
                    if scan_handler.deployment_name:
                        extra_fields.append(
                            f"deployment={scan_handler.deployment_name}"
                        )
                    if scan_handler.scan_id:
                        extra_fields.append(f"scan_id={scan_handler.scan_id}")
                    if extra_fields:
                        start_scan_desc += f" ({', '.join(extra_fields)})"
                    progress_bar.update(
                        start_scan_task, completed=100, description=start_scan_desc
                    )

                    product_names = [
                        PRODUCT_NAMES_MAP.get(p) or p
                        for p in scan_handler.enabled_products
                    ]
                    products_str = ", ".join(product_names) or "None"
                    products_task = progress_bar.add_task(
                        f"Enabled products: [bold]{products_str}[/bold]"
                    )
                    progress_bar.update(products_task, completed=100)

                if scan_handler.rules == '{"rules":[]}' and set(
                    scan_handler.enabled_products
                ).issubset({"sast", "secrets"}):
                    console.print(
                        f"No rules configured. Visit {state.env.semgrep_url}/orgs/-/policies to configure rules to scan your code.\n"
                    )
                    sys.exit(MISSING_CONFIG_EXIT_CODE)

                # Partial config overrides the config we get from the app,
                # but we still need to communicate with the app to get other
                # configs such as products, deployment ID, etc.
                if partial_config:
                    config = (str(partial_config),)
                else:
                    config = (scan_handler.rules,)

        except Exception as e:
            import traceback

            traceback.print_exc()
            logger.info(f"Could not start scan {e}")
            sys.exit(FATAL_EXIT_CODE)

        # Enable beta features
        if scan_handler and scan_handler.generic_slow_rollout:
            # slow rollout for pro diff scan
            diff_depth = 2

        # Handled error outside engine type for more actionable advice.
        if run_secrets_flag and requested_engine is EngineType.OSS:
            logger.info(
                "The --secrets and --oss-only flags are incompatible. Semgrep Secrets is a proprietary extension of Open Source Semgrep."
            )
            sys.exit(FATAL_EXIT_CODE)

        run_secrets = run_secrets_flag or bool(
            # Run without secrets, regardless of the enabled products, if the --oss-only flag was passed.
            (not requested_engine is EngineType.OSS)
            and scan_handler
            and "secrets" in scan_handler.enabled_products
        )

        if not run_secrets and historical_secrets:
            logger.info("Cannot run historical secrets scan without secrets enabled.")
            sys.exit(FATAL_EXIT_CODE)

        supply_chain_only = supply_chain and not code and not run_secrets
        engine_type = EngineType.decide_engine_type(
            logged_in=auth.is_logged_in_weak(),
            engine_flag=requested_engine,
            run_secrets=run_secrets,
            interfile_diff_scan_enabled=diff_depth >= 0,
            ci_scan_handler=scan_handler,
            git_meta=metadata,
            supply_chain_only=supply_chain_only,
        )

        # Temporary. See scan.py for details.
        #
        # Testing: enable the prints below and run 'semgrep ci'.
        #
        # coupling: see identical code in scan.py
        use_semgrepignore_v2: bool
        if semgrepignore_v2 is None:
            # !!!DISABLED!!! the switch is postponed to semgrep 1.113! (same in scan.py)
            # use_semgrepignore_v2 = True if engine_type is EngineType.OSS else False
            use_semgrepignore_v2 = False
        else:
            use_semgrepignore_v2 = semgrepignore_v2
        # For troubleshooting if needed in an emergency since we don't have tests:
        # print(f"engine_type: {engine_type}")
        # print(f"use_semgrepignore_v2: {use_semgrepignore_v2}")

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
                    soft_wrap=True,
                )
            else:
                run_install_semgrep_pro()

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
            verbose_errors=verbose,
            timeout_threshold=timeout_threshold,
            output_time=time_flag,
            output_per_finding_max_lines_limit=max_lines_per_finding,
            output_per_line_max_chars_limit=max_chars_per_line,
            dataflow_traces=dataflow_traces,
            max_log_list_entries=max_log_list_entries,
        )
        output_handler = OutputHandler(output_settings)

        per_product_excludes = {
            product: [*exclude] if exclude else [] for product in ALL_PRODUCTS
        }
        excludes_from_app = (
            {
                product: [ignore.value for ignore in ignores]
                for product, ignores in scan_handler.ignore_patterns.value.items()
            }
            if scan_handler
            else None
        )
        additional_exclude_paths = get_exclude_paths(excludes_from_app)
        for product in ALL_PRODUCTS:
            per_product_excludes[product].extend(additional_exclude_paths[product])

        scanning_root = os.curdir
        if subdir:
            scanning_root += f"/{subdir}"

        # Base arguments for actually running the scan. This is done here so we can
        # re-use this in the event we need to perform a second scan. Currently the
        # only case for this is a separate "historical" scan, where we scan the git
        # history for secrets. This must be split since the targeting logic for the
        # historical scans is entirely in pro, but otherwise here is still
        # performed by the python. Once osemgrep is complete we need only combine
        # the two target lists and perform one scan.
        run_scan_args = {
            "engine_type": engine_type,
            "run_secrets": run_secrets,
            "disable_secrets_validation": disable_secrets_validation_flag,
            "output_handler": output_handler,
            "scanning_roots": [scanning_root],
            "pattern": None,
            "lang": None,
            "configs": config,
            "no_rewrite_rule_ids": (not rewrite_rule_ids),
            "dump_command_for_core": dump_command_for_core,
            "jobs": jobs,
            "include": include,
            "exclude": per_product_excludes,
            "exclude_rule": exclude_rule,
            "max_target_bytes": max_target_bytes,
            "autofix": scan_handler.autofix if scan_handler else False,
            "dryrun": True,
            # Always true, as we want to always report all findings, even
            # ignored ones, to the backend
            "disable_nosem": True,
            "no_git_ignore": (not use_git_ignore),
            "timeout": timeout,
            "max_memory": max_memory,
            "interfile_timeout": interfile_timeout,
            "trace": trace,
            "trace_endpoint": trace_endpoint,
            "timeout_threshold": timeout_threshold,
            "skip_unknown_extensions": (not scan_unknown_extensions),
            "allow_untrusted_validators": allow_untrusted_validators,
            "optimizations": optimizations,
            "baseline_commit": metadata.merge_base_ref,
            "baseline_commit_is_mergebase": True,
            "diff_depth": diff_depth,
            "capture_core_stderr": capture_core_stderr,
            "allow_local_builds": allow_local_builds,
            "x_tr": x_tr,
            "dump_n_rule_partitions": dump_n_rule_partitions,
            "dump_rule_partitions_dir": dump_rule_partitions_dir,
            "ptt_enabled": scan_handler.ptt_enabled if scan_handler else False,
            "resolve_all_deps_in_diff_scan": (
                scan_handler.resolve_all_deps_in_diff_scan if scan_handler else False
            ),
            "symbol_analysis": scan_handler.symbol_analysis if scan_handler else False,
            "use_semgrepignore_v2": use_semgrepignore_v2,
        }

        try:
            start = time.time()

            if scan_handler:
                if not scan_handler.enabled_products:
                    raise SemgrepError(
                        "No products are enabled for this organization. Please enable a product in the Settings > Deployment tab of Semgrep Cloud Platform or reach out to support@semgrep.com for assistance."
                    )

                if dump_n_rule_partitions and scan_handler.enabled_products != ["sast"]:
                    raise SemgrepError(
                        "--x-dump-rule-partitions is only compatible with SAST."
                    )

            # TODO? we're not passing time_flag below (or matching_explanations),
            # is it intended?
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
                _executed_rule_count,
                _missed_rule_count,
                all_subprojects,
            ) = semgrep.run_scan.run_scan(**run_scan_args)
        except SemgrepError as e:
            # We place output_handler calls after scan_handler calls
            # because the output handler may raise an exception further
            # for the top-level error handler to handle.
            #
            # We'd still like scan_handler to notify the app for failures
            # so we do it before calling output handler.
            if isinstance(e, SemgrepError):
                exit_code = e.code
            else:
                exit_code = FATAL_EXIT_CODE
            if scan_handler:
                scan_handler.report_failure(exit_code)

            output_handler.handle_semgrep_errors([e])
            output_handler.output({}, all_targets=set(), filtered_rules=[])
            logger.info(f"Encountered error when running rules: {e}")

            sys.exit(exit_code)

        # Run a separate scan for historical. This is due to the split in how the
        # file targeting works. If file targeting could all be handled in the same
        # place we wouldn't need this seprarately. There are some benefits
        # (e.g., separate progress bar), but it would simplify output logic if we
        # simply had one "scan".
        run_historical_secrets_scan = (
            run_secrets and scan_handler and scan_handler.historical_config.enabled
        ) or historical_secrets

        if run_historical_secrets_scan and metadata.merge_base_ref:
            logger.info(
                f"Historical scanning was enabled, but is not yet supported on diff scans."
            )
        elif run_historical_secrets_scan:
            try:
                console.print(Title("Secrets Historical Scan"))

                (
                    historical_filtered_matches_by_rule,
                    historical_semgrep_errors,
                    # Don't care about historically renamed targets
                    # Seems like this is just ignored by app anyway.
                    _historical_renamed_targets,
                    # Only used in metrics; too noisy for now.
                    _historical_ignore_log,
                    historical_filtered_rules,
                    # For metrics; not yet sent.
                    _historical_profiler,
                    # Wrapper for some extra info; not currently sent for
                    # historical scans.
                    _historical_output_extra,
                    # Severity filtering should be the same, since the same
                    # settings have been passed. So we can just use the "normal"
                    # shown_severities.
                    _historical_shown_severities,
                    # Not relevant for secrets.
                    _historical_dependencies,
                    _historical_dependency_parser_errors,
                    # Usage limits currently only consider last 30 days.
                    _executed_rule_count,
                    _missed_rule_count,
                    _historical_all_subprojects,
                ) = semgrep.run_scan.run_scan(
                    **run_scan_args,
                    historical_secrets=True,
                )

                for key, value in historical_filtered_matches_by_rule.items():
                    filtered_matches_by_rule[key].extend(value)

                semgrep_errors.extend(historical_semgrep_errors)
                filtered_rules.extend(historical_filtered_rules)

            except SemgrepError as e:
                # We know the non-historical scan completed successfully (since
                # otherwise the program would exit), so we can just inform the
                # user of the issue here.
                output_handler.handle_semgrep_errors([e])
                logger.info(
                    f"Encountered error when running rules for historical scan: {e}"
                )
                logger.info(f"Finalizing non-historical results")

        total_time = time.time() - start

        # Split up rules into respective categories:
        blocking_rules: List[Rule] = []
        nonblocking_rules: List[Rule] = []
        for rule in filtered_rules:
            if "r2c-internal-cai" in rule.id:
                pass
            elif rule.from_transient_scan:
                pass
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

        # Since we keep nosemgrep disabled for the actual scan, we have to
        # apply that flag here.
        # If there are multiple outputs and any request to keep_ignores
        # then all outputs keep the ignores. The only output format that
        # keep ignored matches currently is sarif.
        keep_ignored = not enable_nosem or output_handler.keep_ignores()
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
                if match.match.extra.is_ignored and not keep_ignored:
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
        filtered_rules = [*blocking_rules, *nonblocking_rules]

        # After computing the number of blocking/non-blocking findings, here
        # is were the cli comes up with the suggested exit code that we send
        # to the semgrep app
        #
        # NOTE: this is not the exit code the cli will use to exit with, as
        # the cli depends on the apps response to compute its own exit code!
        cli_suggested_exit_code = 1 if num_blocking_findings > 0 else 0

        if scan_handler is None:
            # This is a slightly weird case, where we're running `semgrep ci` while logged out with an explicitly provided config
            # There's no app to report to, so we just print the results immediately
            if not internal_ci_scan_results:
                output_handler.output(
                    non_cai_matches_by_rule,
                    all_targets=output_extra.all_targets,
                    engine_type=engine_type,
                    ignore_log=ignore_log,
                    profiler=profiler,
                    filtered_rules=filtered_rules,
                    extra=output_extra,
                    severities=shown_severities,
                    is_ci_invocation=True,
                    print_summary=False,
                )

            logger.info("CI scan completed successfully.")

        complete_result: out.CiScanCompleteResponse | None = None
        contributions = semgrep.rpc_call.contributions()
        if scan_handler:
            with Progress(
                TextColumn("  {task.description}"),
                SpinnerColumn(spinner_name="simpleDotsScrolling"),
                console=console,
            ) as progress_bar:
                complete_result = scan_handler.report_findings(
                    filtered_matches_by_rule,
                    filtered_rules,
                    output_extra.all_targets,
                    renamed_targets,
                    ignore_log.unsupported_lang_paths(product=SAST_PRODUCT),
                    cli_suggested_exit_code,
                    output_extra.parsing_data,
                    total_time,
                    metadata.commit_datetime,
                    dependencies,
                    dependency_parser_errors,
                    all_subprojects,
                    contributions,
                    engine_type,
                    progress_bar,
                )
            app_blocked_mids = set()
            if (
                complete_result.app_block_override
                and len(complete_result.app_blocking_match_based_ids) > 0
            ):
                num_blocking_findings = 0
                num_nonblocking_findings = 0
                app_blocked_mids = set(
                    [x.value for x in complete_result.app_blocking_match_based_ids]
                )
                for matches in non_cai_matches_by_rule.values():
                    for i in range(len(matches)):
                        if matches[i].match_based_id in app_blocked_mids:
                            matches[i] = evolve(matches[i], blocked_by_app=True)

                        if matches[i].is_blocking:
                            num_blocking_findings += 1
                        else:
                            num_nonblocking_findings += 1

            # Before we finish the scan, let's upload our symbol analysis if we have it.
            # This is "scan-adjacent information", which is information we want to save,
            # but doesn't really have to do with the meat of the scan (findings, etc).
            # We upload it separately, and outsource to `osemgrep` so we don't duplicate
            # the implementation.
            if (
                output_extra.core.symbol_analysis is not None
                and scan_handler.scan_id
                and token
            ):
                logger.debug(
                    f"Attempting to upload symbol analysis of {len(output_extra.core.symbol_analysis.value)} symbols"
                )
                symbol_analysis = output_extra.core.symbol_analysis
                semgrep.rpc_call.upload_symbol_analysis(
                    token, scan_handler.scan_id, symbol_analysis
                )

            if not internal_ci_scan_results:
                output_handler.output(
                    non_cai_matches_by_rule,
                    all_targets=output_extra.all_targets,
                    engine_type=engine_type,
                    ignore_log=ignore_log,
                    profiler=profiler,
                    filtered_rules=filtered_rules,
                    extra=output_extra,
                    severities=shown_severities,
                    is_ci_invocation=True,
                    print_summary=False,
                )

            logger.info("CI scan completed successfully.")

            if internal_ci_scan_results:
                # console.print() would go to stderr; here we print() directly to stdout
                print(
                    json.dumps(
                        (
                            scan_handler.ci_scan_results.to_json()
                            if scan_handler.ci_scan_results
                            else {}
                        ),
                        sort_keys=True,
                        default=lambda x: x.to_json(),
                    )
                )

            # These logs are not relevant when dry running
            if not dry_run:
                if complete_result.success:
                    logger.info("  View results in Semgrep Cloud Platform:")
                else:
                    logger.info(
                        "  Semgrep Cloud Platform is still processing the results of the scan, they will be available soon:"
                    )

                ref_if_available = f"&ref={metadata.branch}" if metadata.branch else ""

                logger.info(
                    f"    {state.env.semgrep_url}/orgs/{scan_handler.deployment_name}/findings?repo={metadata.repo_display_name}{ref_if_available}"
                )
                if "r2c-internal-project-depends-on" in scan_handler.rules:
                    logger.info(
                        f"    {state.env.semgrep_url}/orgs/{scan_handler.deployment_name}/supply-chain/vulnerabilities?repo={metadata.repo_display_name}{ref_if_available}"
                    )

        # Although the cli came up with a suggested exit code, we could still
        # exit with a different exit code, as the cli depends on the app to
        # compute its own exit code.
        audit_mode = metadata.event_name in audit_on

        # The app is saying that some specific findings are blocking
        app_policy_block = (
            complete_result
            and complete_result.app_block_override
            and complete_result.app_blocking_match_based_ids
        )

        # The app is saying that there is some other reason for blocking (currently only ever used for SCA license scanning)
        app_other_block = (
            complete_result
            and complete_result.app_block_override
            and complete_result.app_block_reason
        )

        if cli_suggested_exit_code == 1 or app_policy_block:
            if audit_mode:
                logger.info(
                    f"  Audit mode is on for {metadata.event_name}, so exiting with code 0 even if matches found",
                )
                exit_code = 0
            else:
                exit_code = 1
                logger.info("  Has findings for blocking rules so exiting with code 1")
                if app_other_block:
                    assert (
                        complete_result is not None
                    )  # Here to appease mypy. This must be true by the definition of `app_other_block`
                    logger.info(complete_result.app_block_reason)
        else:
            if app_other_block and not audit_mode:
                # When the app causes the CLI to block for SCA license reasons, we need to make
                # it clear to the user that there is a reason for the block, but when
                # the app causes the CLI to block because of certain findings, we already
                # display them as blocking, so there's no need to say
                # "we blocked because the app said these are blocking"
                assert (
                    complete_result is not None
                )  # Here to appease mypy. This must be true by the definition of `app_other_block`
                logger.info(
                    f"  semgrep.dev is suggesting a non-zero exit code ({complete_result.app_block_reason})"
                )
                exit_code = 1
            else:
                logger.info("  No blocking findings so exiting with code 0")
                exit_code = 0

        if enable_version_check:
            from semgrep.app.version import version_check

            version_check()

        sys.exit(exit_code)
