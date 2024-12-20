##############################################################################
# Prelude
##############################################################################
# Running a semgrep scan and returning a structured result (RuleMatchMap,
# List[SemgrepError], etc.). Most of the CLI arguments processing is done
# in commands/scan.py (but some of the CLI argument sanity checking is
# still done here).
#
# old: this file used to be the entry point of semgrep but became a regular
# file when we introduced semgrep commands (e.g., scan, login). This file
# is now called from commands/scan.py and commands/ci.py instead.
# old: this file used to be called semgrep_main.py
#
import json
import sys
import time
import traceback
from io import StringIO
from os import environ
from pathlib import Path
from sys import getrecursionlimit
from sys import setrecursionlimit
from typing import Any
from typing import Collection
from typing import Dict
from typing import List
from typing import Mapping
from typing import Optional
from typing import Sequence
from typing import Set
from typing import Tuple
from typing import Union

from boltons.iterutils import partition
from rich.progress import Progress
from rich.progress import SpinnerColumn
from rich.progress import TextColumn

import semgrep.scan_report as scan_report
import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.parsers.util import DependencyParserError
from semgrep import __VERSION__
from semgrep import tracing
from semgrep.autofix import apply_fixes
from semgrep.config_resolver import ConfigLoader
from semgrep.config_resolver import get_config
from semgrep.console import console
from semgrep.constants import DEFAULT_DIFF_DEPTH
from semgrep.constants import DEFAULT_TIMEOUT
from semgrep.constants import OutputFormat
from semgrep.constants import TOO_MUCH_DATA
from semgrep.core_runner import CoreRunner
from semgrep.core_runner import Plan
from semgrep.dependency_aware_rule import dependencies_range_match_any
from semgrep.dependency_aware_rule import parse_depends_on_yaml
from semgrep.engine import EngineType
from semgrep.error import DependencyResolutionError
from semgrep.error import InvalidScanningRootError
from semgrep.error import MISSING_CONFIG_EXIT_CODE
from semgrep.error import select_real_errors
from semgrep.error import SemgrepError
from semgrep.exclude_rules import filter_exclude_rule
from semgrep.git import BaselineHandler
from semgrep.git import get_project_url
from semgrep.ignores import FileIgnore
from semgrep.ignores import IGNORE_FILE_NAME
from semgrep.ignores import Parser
from semgrep.nosemgrep import filter_ignored
from semgrep.output import DEFAULT_SHOWN_SEVERITIES
from semgrep.output import OutputHandler
from semgrep.output import OutputSettings
from semgrep.output_extra import OutputExtra
from semgrep.profile_manager import ProfileManager
from semgrep.resolve_subprojects import resolve_subprojects
from semgrep.rpc_call import dump_rule_partitions
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatches
from semgrep.rule_match import RuleMatchMap
from semgrep.semgrep_interfaces.semgrep_metrics import Any_ as AnySecretsOrigin
from semgrep.semgrep_interfaces.semgrep_metrics import CodeConfig
from semgrep.semgrep_interfaces.semgrep_metrics import SecretsConfig
from semgrep.semgrep_interfaces.semgrep_metrics import SecretsOrigin
from semgrep.semgrep_interfaces.semgrep_metrics import Semgrep as SemgrepSecretsOrigin
from semgrep.semgrep_interfaces.semgrep_metrics import SupplyChainConfig
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Product
from semgrep.semgrep_types import JOIN_MODE
from semgrep.state import get_state
from semgrep.subproject import ResolvedSubproject
from semgrep.subproject import UnresolvedSubproject
from semgrep.target_manager import FileTargetingLog
from semgrep.target_manager import SAST_PRODUCT
from semgrep.target_manager import SCA_PRODUCT
from semgrep.target_manager import SECRETS_PRODUCT
from semgrep.target_manager import TargetManager
from semgrep.target_mode import TargetModeConfig
from semgrep.util import unit_str
from semgrep.verbose_logging import getLogger


logger = getLogger(__name__)

##############################################################################
# Helper functions
##############################################################################


def get_file_ignore(max_log_list_entries: int) -> FileIgnore:
    TEMPLATES_DIR = Path(__file__).parent / "templates"
    try:
        workdir = Path.cwd()
    except FileNotFoundError:
        workdir = Path.home()
        logger.warn(
            f"Current working directory does not exist! Instead checking {workdir} for .semgrepignore files"
        )

    # Meant to be used only by semgrep-action
    if "SEMGREP_R2C_INTERNAL_EXPLICIT_SEMGREPIGNORE" in environ:
        semgrepignore_path = Path(
            environ["SEMGREP_R2C_INTERNAL_EXPLICIT_SEMGREPIGNORE"]
        ).resolve()
        logger.verbose("Using explicit semgrepignore file from environment variable")
    else:
        semgrepignore_path = Path(workdir / IGNORE_FILE_NAME)
        if not semgrepignore_path.is_file():
            logger.verbose(
                "No .semgrepignore found. Using default .semgrepignore rules. See the docs for the list of default ignores: https://semgrep.dev/docs/cli-usage/#ignore-files"
            )
            semgrepignore_path = TEMPLATES_DIR / IGNORE_FILE_NAME
        else:
            logger.verbose("using path ignore rules from user provided .semgrepignore")

    with semgrepignore_path.open() as f:
        file_ignore = FileIgnore.from_unprocessed_patterns(
            base_path=workdir,
            patterns=Parser(file_path=semgrepignore_path, base_path=workdir).parse(f),
            max_log_list_entries=max_log_list_entries,
        )

    return file_ignore


def file_ignore_to_ignore_profiles(
    file_ignore: FileIgnore,
) -> Dict[Product, FileIgnore]:
    # TODO: This pattern encodes the default Targeting Profiles
    # of .semgrepignore. Don't hardcode this like it is.
    return {
        SAST_PRODUCT: file_ignore,
        SCA_PRODUCT: file_ignore,
        SECRETS_PRODUCT: FileIgnore(
            file_ignore.base_path, frozenset(), max_log_list_entries=0
        ),
    }


def remove_matches_in_baseline(
    head_matches_by_rule: RuleMatchMap,
    baseline_matches_by_rule: RuleMatchMap,
    file_renames: Dict[str, Path],
) -> RuleMatchMap:
    """
    Remove the matches in head_matches_by_rule that also occur in baseline_matches_by_rule
    """
    logger.verbose("Removing matches that exist in baseline scan")
    kept_matches_by_rule: RuleMatchMap = {}
    num_removed = 0

    for rule, matches in head_matches_by_rule.items():
        if len(matches) == 0:
            continue
        baseline_matches = {
            match.ci_unique_key for match in baseline_matches_by_rule.get(rule, [])
        }
        kept_matches_by_rule[rule] = [
            match
            for match in matches
            if match.get_path_changed_ci_unique_key(file_renames)
            not in baseline_matches
        ]
        num_removed += len(matches) - len(kept_matches_by_rule[rule])

    logger.verbose(
        f"Removed {unit_str(num_removed, 'finding')} that were in baseline scan"
    )
    return kept_matches_by_rule


def filter_dependency_aware_rules(
    dependency_aware_rules: List[Rule],
    resolved_deps: Dict[Ecosystem, List[ResolvedSubproject]],
) -> List[Rule]:
    """Returns the list of filtered rules that have matching dependencies in the project"""
    rules_to_check = [r for r in dependency_aware_rules if r.should_run_on_semgrep_core]
    # List to store filtered rules
    filtered_rules = []

    # Loop through each rule to check for matching dependencies
    for rule in rules_to_check:
        depends_on_keys = rule.project_depends_on
        depends_on_entries = list(parse_depends_on_yaml(depends_on_keys))
        ecosystems = list(rule.ecosystems)

        # Flag to track if we found a matching dependency
        has_matching_dependency = False
        # Check for each ecosystem in the rule
        for ecosystem in ecosystems:
            for sca_project in resolved_deps.get(ecosystem, []):
                deps = list(sca_project.found_dependencies.iter_found_dependencies())
                # Match the dependencies based on version ranges
                dependency_matches = list(
                    dependencies_range_match_any(depends_on_entries, deps)
                )

                # If any dependency matches, we flag it as a match
                if dependency_matches:
                    has_matching_dependency = True
                    break

            if has_matching_dependency:
                break

        # If we found a matching dependency, include this rule
        if has_matching_dependency:
            filtered_rules.append(rule)

    return filtered_rules


# This runs semgrep-core (and also handles SCA and join rules)
@tracing.trace()
def run_rules(
    filtered_rules: List[Rule],
    target_manager: TargetManager,
    core_runner: CoreRunner,
    output_handler: OutputHandler,
    dump_command_for_core: bool,
    time_flag: bool,
    matching_explanations: bool,
    engine_type: EngineType,
    strict: bool,
    # TODO: Use an array of semgrep_output_v1.Product instead of booleans flags for secrets, code, and supply chain
    run_secrets: bool = False,
    disable_secrets_validation: bool = False,
    target_mode_config: Optional[TargetModeConfig] = None,
    *,
    with_code_rules: bool = True,
    with_supply_chain: bool = False,
    allow_local_builds: bool = False,
    ptt_enabled: bool = False,
) -> Tuple[
    RuleMatchMap,
    List[SemgrepError],
    OutputExtra,
    Dict[str, List[FoundDependency]],
    List[DependencyParserError],
    List[Plan],
    List[Union[UnresolvedSubproject, ResolvedSubproject]],
]:
    if not target_mode_config:
        target_mode_config = TargetModeConfig.whole_scan()

    join_rules, rest_of_the_rules = partition(
        filtered_rules, lambda rule: rule.mode == JOIN_MODE
    )

    # Get rules that rely on dependencies from the project's lockfile
    dependency_aware_rules = [r for r in rest_of_the_rules if r.project_depends_on]

    # Initialize data structures for dependencies
    filtered_dependency_aware_rules = []
    dependency_parser_errors: List[DependencyParserError] = []
    sca_dependency_targets: List[Path] = []

    resolved_subprojects: Dict[Ecosystem, List[ResolvedSubproject]] = {}
    unresolved_subprojects: List[UnresolvedSubproject] = []
    all_subprojects: List[Union[ResolvedSubproject, UnresolvedSubproject]] = []

    if len(dependency_aware_rules) > 0:
        # Parse lockfiles to get dependency information, if there are relevant rules
        (
            unresolved_subprojects,
            resolved_subprojects,
            sca_dependency_targets,
        ) = resolve_subprojects(
            target_manager,
            allow_dynamic_resolution=allow_local_builds,
            ptt_enabled=ptt_enabled,
        )
        # for each subproject, split the errors into semgrep errors and parser errors.
        # output the semgrep errors and store the parser errors for printing in print_scan_status below
        all_subprojects.extend(unresolved_subprojects)
        for subprojects in resolved_subprojects.values():
            all_subprojects.extend(subprojects)
        for subproject in all_subprojects:
            dependency_parser_errors.extend(
                [
                    e
                    for e in subproject.resolution_errors
                    if isinstance(e, DependencyParserError)
                ]
            )
            output_handler.handle_semgrep_errors(
                [
                    e
                    for e in subproject.resolution_errors
                    if isinstance(e, DependencyResolutionError)
                ]
            )

        # Filter rules that match the dependencies
        filtered_dependency_aware_rules = filter_dependency_aware_rules(
            dependency_aware_rules, resolved_subprojects
        )

    rest_of_the_rules = [
        r for r in rest_of_the_rules if r not in dependency_aware_rules
    ] + filtered_dependency_aware_rules

    cli_ux = get_state().get_cli_ux_flavor()
    plans = scan_report.print_scan_status(
        filtered_rules,
        target_manager,
        target_mode_config,
        resolved_subprojects,
        dependency_parser_errors,
        cli_ux=cli_ux,
        with_code_rules=with_code_rules,
        with_supply_chain=with_supply_chain,
    )

    # Dispatching to semgrep-core!
    (
        rule_matches_by_rule,
        semgrep_errors,
        output_extra,
    ) = core_runner.invoke_semgrep_core(
        target_manager,
        rest_of_the_rules,
        dump_command_for_core,
        time_flag,
        matching_explanations,
        engine_type,
        strict,
        run_secrets,
        disable_secrets_validation,
        target_mode_config,
        resolved_subprojects,
    )

    if join_rules:
        import semgrep.join_rule as join_rule

        for rule in join_rules:
            join_rule_matches, join_rule_errors = join_rule.run_join_rule(
                rule.raw,
                [target.path for target in target_manager.targets],
                allow_local_builds=allow_local_builds,
                ptt_enabled=ptt_enabled,
            )
            join_rule_matches_set = RuleMatches(rule)
            for m in join_rule_matches:
                join_rule_matches_set.add(m)
            join_rule_matches_by_rule = {
                Rule.from_json(rule.raw): list(join_rule_matches_set)
            }
            rule_matches_by_rule.update(join_rule_matches_by_rule)
            output_handler.handle_semgrep_errors(join_rule_errors)

    if len(dependency_aware_rules) > 0:
        from semgrep.dependency_aware_rule import (
            generate_unreachable_sca_findings,
            generate_reachable_sca_findings,
        )

        for rule in dependency_aware_rules:
            if rule.should_run_on_semgrep_core:
                # If we have a reachability rule (contains a pattern)
                # First we check if each match has a lockfile with the correct vulnerability and turn these into SCA findings
                # Then we generate unreachable findings in all the remaining targeted lockfiles
                # For each rule, we do not want to generate an unreachable finding in a lockfile
                # that already has a reachable finding, so we exclude them
                (
                    dep_rule_matches,
                    dep_rule_errors,
                    already_reachable,
                ) = generate_reachable_sca_findings(
                    rule_matches_by_rule.get(rule, []),
                    rule,
                    resolved_subprojects,
                )
                rule_matches_by_rule[rule] = dep_rule_matches
                output_handler.handle_semgrep_errors(dep_rule_errors)
                (
                    dep_rule_matches,
                    dep_rule_errors,
                ) = generate_unreachable_sca_findings(
                    rule,
                    already_reachable,
                    resolved_subprojects,
                )
                rule_matches_by_rule[rule].extend(dep_rule_matches)
                output_handler.handle_semgrep_errors(dep_rule_errors)
            else:
                (
                    dep_rule_matches,
                    dep_rule_errors,
                ) = generate_unreachable_sca_findings(
                    rule, lambda p, d: False, resolved_subprojects
                )
                rule_matches_by_rule[rule] = dep_rule_matches
                output_handler.handle_semgrep_errors(dep_rule_errors)

        # The caller expects a map from lockfile path to `FoundDependency` items rather than our Subproject representation
        deps_by_lockfile: Dict[str, List[FoundDependency]] = {}
        for ecosystem in resolved_subprojects:
            for proj in resolved_subprojects[ecosystem]:
                (
                    proj_deps_by_lockfile,
                    unknown_lockfile_deps,
                ) = proj.found_dependencies.make_dependencies_by_source_path()
                deps_by_lockfile.update(proj_deps_by_lockfile)

                # We don't really expect to have any dependencies with an unknown lockfile, but we can't enforce
                # this with types due to backwards compatibility guarantees on FoundDependency. If we see any
                # dependencies without lockfile path, we assign them to a fake lockfile at the root of each subproject.
                for dep in unknown_lockfile_deps:
                    if (
                        str(proj.root_dir.joinpath(Path("unknown_lockfile")))
                        not in deps_by_lockfile
                    ):
                        deps_by_lockfile[
                            str(proj.root_dir.joinpath(Path("unknown_lockfile")))
                        ] = []
                    deps_by_lockfile[
                        str(proj.root_dir.joinpath(Path("unknown_lockfile")))
                    ].append(dep)

        for target in sca_dependency_targets:
            output_extra.all_targets.add(target)
    else:
        deps_by_lockfile = {}

    return (
        rule_matches_by_rule,
        semgrep_errors,
        output_extra,
        deps_by_lockfile,
        dependency_parser_errors,
        plans,
        all_subprojects,
    )


# This is used for testing and comparing with osemgrep.
def list_targets_and_exit(
    target_manager: TargetManager, product: out.Product, long_format: bool = False
) -> None:
    targets = target_manager.get_files_for_language(None, product)
    for path in sorted(targets.kept):
        if long_format:
            print(f"selected {path}")
        else:
            print(str(path))
    if long_format:
        for path, reason in target_manager.ignore_log.list_skipped_paths_with_reason():
            print(f"ignored {path} [{reason}]")
    exit(0)


##############################################################################
# Entry points
##############################################################################


# cli/bin/semgrep -> main.py -> cli.py -> commands/scan.py -> run_scan()
# old: this used to be called semgrep.semgrep_main.main
@tracing.trace()
def run_scan(
    *,
    diff_depth: int = DEFAULT_DIFF_DEPTH,
    dump_command_for_core: bool = False,
    time_flag: bool = False,
    matching_explanations: bool = False,
    engine_type: EngineType = EngineType.OSS,
    run_secrets: bool = False,
    disable_secrets_validation: bool = False,
    output_handler: OutputHandler,
    target: Sequence[str],
    historical_secrets: bool = False,
    pattern: Optional[str],
    lang: Optional[str],
    configs: Sequence[
        str
    ],  # NOTE: Since the `ci` command reuses this function, we intentionally do not set a default at this level.
    no_rewrite_rule_ids: bool = False,
    jobs: Optional[int] = None,
    include: Optional[Sequence[str]] = None,
    exclude: Optional[Mapping[Product, Sequence[str]]] = None,
    exclude_rule: Optional[Sequence[str]] = None,
    strict: bool = False,
    autofix: bool = False,
    replacement: Optional[str] = None,
    dryrun: bool = False,
    disable_nosem: bool = False,
    no_git_ignore: bool = False,
    respect_rule_paths: bool = True,
    respect_semgrepignore: bool = True,
    timeout: int = DEFAULT_TIMEOUT,
    max_memory: int = 0,
    interfile_timeout: int = 0,
    trace: bool = False,
    trace_endpoint: Optional[str] = None,
    max_target_bytes: int = 0,
    timeout_threshold: int = 0,
    skip_unknown_extensions: bool = False,
    allow_untrusted_validators: bool = False,
    severity: Optional[Sequence[str]] = None,
    optimizations: str = "none",
    baseline_commit: Optional[str] = None,
    baseline_commit_is_mergebase: bool = False,
    x_ls: bool = False,
    x_ls_long: bool = False,
    path_sensitive: bool = False,
    capture_core_stderr: bool = True,
    allow_local_builds: bool = False,
    dump_n_rule_partitions: Optional[int] = None,
    dump_rule_partitions_dir: Optional[Path] = None,
    ptt_enabled: bool = False,
) -> Tuple[
    RuleMatchMap,
    List[SemgrepError],
    Set[Path],
    FileTargetingLog,
    List[Rule],
    ProfileManager,
    OutputExtra,
    Collection[out.MatchSeverity],
    Dict[str, List[FoundDependency]],
    List[DependencyParserError],
    int,  # Executed Rule Count
    int,  # Missed Rule Count
    List[Union[UnresolvedSubproject, ResolvedSubproject]],
]:
    logger.debug(f"semgrep version {__VERSION__}")

    # Some of the lockfile parsers are defined recursively
    # This does not play well with python's conservative recursion limit, so we manually increase

    if "SEMGREP_PYTHON_RECURSION_LIMIT_INCREASE" in environ:
        recursion_limit_increase = int(
            environ["SEMGREP_PYTHON_RECURSION_LIMIT_INCREASE"]
        )
    else:
        recursion_limit_increase = 500

    setrecursionlimit(getrecursionlimit() + recursion_limit_increase)

    if include is None:
        include = []

    if exclude is None:
        exclude = {}

    if exclude_rule is None:
        exclude_rule = []

    project_url = get_project_url()
    profiler = ProfileManager()

    rule_start_time = time.time()

    includes_remote_config = ConfigLoader.includes_remote_config(configs)
    progress_msg = (
        "Loading rules from registry..."
        if includes_remote_config
        else "Loading rules..."
    )

    with Progress(
        SpinnerColumn(style="green"),
        TextColumn("[bold]{task.description}[/bold]"),
        transient=True,
        console=console,
        disable=(not sys.stderr.isatty()),
    ) as progress:
        task_id = progress.add_task(f"{progress_msg}", total=1)
        configs_obj, config_errors = get_config(
            pattern,
            lang,
            configs,
            replacement=replacement,
            project_url=project_url,
            no_rewrite_rule_ids=no_rewrite_rule_ids,
        )
        progress.remove_task(task_id)

    all_rules = configs_obj.get_rules(no_rewrite_rule_ids)
    profiler.save("config_time", rule_start_time)

    # We determine if SAST / SCA is enabled based on the config str
    with_code_rules = configs_obj.with_code_rules
    with_supply_chain = configs_obj.with_supply_chain
    # TODO: handle de-duplication for pro-rules
    missed_rule_count = configs_obj.missed_rule_count

    # Metrics send part 1: add environment information
    # Must happen after configs are resolved because it is determined
    # then whether metrics are sent or not
    metrics = get_state().metrics
    if metrics.is_enabled:
        metrics.add_project_url(project_url)
        metrics.add_integration_name(environ.get("SEMGREP_INTEGRATION_NAME"))
        metrics.add_configs(configs)
        metrics.add_engine_config(
            engine_type,
            CodeConfig() if with_code_rules else None,
            SecretsConfig(
                SecretsOrigin(AnySecretsOrigin())
                if allow_untrusted_validators
                else SecretsOrigin(SemgrepSecretsOrigin())
            )
            if run_secrets and not disable_secrets_validation
            else None,
            SupplyChainConfig() if with_supply_chain else None,
        )
        metrics.add_is_diff_scan(baseline_commit is not None)
        if engine_type.is_pro:
            metrics.add_diff_depth(diff_depth)

    if not severity:
        shown_severities = DEFAULT_SHOWN_SEVERITIES
        filtered_rules = all_rules
    else:
        shown_severities = {out.MatchSeverity.from_json(s) for s in severity}
        filtered_rules = [
            rule for rule in all_rules if rule.severity in shown_severities
        ]
    filtered_rules = filter_exclude_rule(filtered_rules, exclude_rule)

    if dump_n_rule_partitions:
        rules = {"rules": [r.raw for r in filtered_rules]}
        output_dir = str(dump_rule_partitions_dir)
        args = out.DumpRulePartitionsParams(
            out.RawJson(rules), dump_n_rule_partitions, out.Fpath(output_dir)
        )
        ok = dump_rule_partitions(args)
        if not ok:
            logger.error("An error occurred while dumping rule partitions.")
            sys.exit(2)
        logger.info(f"Successfully dumped rule partitions to {output_dir}")
        sys.exit(0)

    output_handler.handle_semgrep_errors(config_errors)
    real_config_errors = select_real_errors(config_errors)
    if not pattern:
        config_id_if_single = (
            list(configs_obj.valid.keys())[0] if len(configs_obj.valid) == 1 else ""
        )
        invalid_msg = (
            f"({unit_str(len(config_errors), 'config error')})"
            if len(config_errors)
            else ""
        )
        logger.verbose(
            f"running {len(filtered_rules)} rules from {unit_str(len(configs_obj.valid), 'config')} {config_id_if_single} {invalid_msg}".strip()
        )
        if len(real_config_errors) > 0:
            raise SemgrepError(
                f"invalid configuration file found ({len(real_config_errors)} configs were invalid)",
                code=MISSING_CONFIG_EXIT_CODE,
            )
        # NOTE: We should default to config auto if no config was passed in an earlier step,
        #       but if we reach this step without a config, we emit the error below.
        if len(configs_obj.valid) == 0:
            raise SemgrepError(
                """No config given. Run with `--config auto` or see https://semgrep.dev/docs/running-rules/ for instructions on running with a specific config
""",
                code=MISSING_CONFIG_EXIT_CODE,
            )

    # This is after the `not pattern` block, because this error message is less
    # helpful.
    if real_config_errors and strict:
        raise SemgrepError(
            f"Ran with --strict and got {unit_str(len(config_errors), 'error')} while loading configs",
            code=MISSING_CONFIG_EXIT_CODE,
        )

    # Initialize baseline here to fail early on bad args
    baseline_handler = None
    if baseline_commit:
        try:
            baseline_handler = BaselineHandler(
                baseline_commit, is_mergebase=baseline_commit_is_mergebase
            )
        except Exception:
            # Display a trace because we have no idea where the exception
            # was raised.
            exception_with_trace: str = traceback.format_exc()
            raise SemgrepError(
                f"Exception in BaselineHandler initialization: {exception_with_trace}"
            )

    respect_git_ignore = not no_git_ignore
    target_strings = frozenset(Path(t) for t in target)
    too_many_entries = output_handler.settings.max_log_list_entries

    try:
        target_manager = TargetManager(
            includes=include,
            excludes=exclude,
            max_target_bytes=max_target_bytes,
            target_strings=target_strings,
            respect_git_ignore=respect_git_ignore,
            respect_rule_paths=respect_rule_paths,
            baseline_handler=baseline_handler,
            allow_unknown_extensions=not skip_unknown_extensions,
            ignore_profiles=file_ignore_to_ignore_profiles(
                get_file_ignore(too_many_entries)
            ),
            respect_semgrepignore=respect_semgrepignore,
        )
        # Debugging option --x-ls
        if x_ls or x_ls_long:
            list_targets_and_exit(
                target_manager, out.Product(out.SAST()), long_format=x_ls_long
            )
    except InvalidScanningRootError as e:
        raise SemgrepError(e)

    if historical_secrets:
        target_mode_config = TargetModeConfig.historical_scan()
    elif baseline_handler is not None:
        if engine_type.is_interfile:
            target_mode_config = TargetModeConfig.pro_diff_scan(
                # `target_manager.get_all_files()` will only return changed files
                # (diff targets) when baseline_handler is set
                target_manager.get_all_files(),
                diff_depth,
            )
        else:
            target_mode_config = TargetModeConfig.diff_scan()
    else:
        target_mode_config = TargetModeConfig.whole_scan()

    core_start_time = time.time()
    core_runner = CoreRunner(
        jobs=jobs,
        engine_type=engine_type,
        timeout=timeout,
        max_memory=max_memory,
        interfile_timeout=interfile_timeout,
        timeout_threshold=timeout_threshold,
        trace=trace,
        trace_endpoint=trace_endpoint,
        capture_stderr=capture_core_stderr,
        optimizations=optimizations,
        allow_untrusted_validators=allow_untrusted_validators,
        respect_rule_paths=respect_rule_paths,
        path_sensitive=path_sensitive,
    )

    experimental_rules, normal_rules = partition(
        filtered_rules, lambda rule: (isinstance(rule.severity.value, out.Experiment))
    )

    if logger.isEnabledFor(logger.VERBOSE_LOG_LEVEL):
        logger.verbose("Rules:")
        if too_many_entries > 0 and len(normal_rules) > too_many_entries:
            logger.verbose(TOO_MUCH_DATA)
        else:
            for ruleid in sorted(rule.id for rule in normal_rules):
                logger.verbose(f"- {ruleid}")

        if len(experimental_rules) > 0:
            logger.verbose("Experimental Rules:")
            if too_many_entries > 0 and len(experimental_rules) > too_many_entries:
                logger.verbose(TOO_MUCH_DATA)
            else:
                for ruleid in sorted(rule.id for rule in experimental_rules):
                    logger.verbose(f"- {ruleid}")

    (
        rule_matches_by_rule,
        scan_errors,
        output_extra,
        dependencies,
        dependency_parser_errors,
        plans,
        all_subprojects,
    ) = run_rules(
        filtered_rules,
        target_manager,
        core_runner,
        output_handler,
        dump_command_for_core,
        time_flag,
        matching_explanations,
        engine_type,
        strict,
        run_secrets,
        disable_secrets_validation,
        target_mode_config,
        with_code_rules=with_code_rules,
        with_supply_chain=with_supply_chain,
        allow_local_builds=allow_local_builds,
        ptt_enabled=ptt_enabled,
    )
    profiler.save("core_time", core_start_time)
    semgrep_errors: List[SemgrepError] = config_errors + scan_errors
    output_handler.handle_semgrep_errors(semgrep_errors)

    paths_with_matches = list(
        {match.path for matches in rule_matches_by_rule.values() for match in matches}
    )

    findings_count = sum(
        len([match for match in matches if not match.from_transient_scan])
        for matches in rule_matches_by_rule.values()
    )

    # Run baseline if needed
    if baseline_handler:
        logger.info(f"  Current version has {unit_str(findings_count, 'finding')}.")
        logger.info("")
        baseline_targets: Set[Path] = set(paths_with_matches).union(
            set(baseline_handler.status.renamed.values())
        ) - set(baseline_handler.status.added)
        if not paths_with_matches:
            logger.info(
                "Skipping baseline scan, because there are no current findings."
            )
        elif not baseline_targets:
            logger.info(
                "Skipping baseline scan, because all current findings are in files that didn't exist in the baseline commit."
            )
        else:
            logger.info(
                f"Creating git worktree from '{baseline_commit}' to scan baseline."
            )
            baseline_handler.print_git_log()
            logger.info("")
            try:
                with baseline_handler.baseline_context():
                    baseline_target_strings = target_strings
                    baseline_target_mode_config = target_mode_config
                    if target_mode_config.is_pro_diff_scan:
                        scanned = [
                            # Conducting the inter-file diff scan twice with the exact same configuration,
                            # both on the current commit and the baseline commit, could result in the absence
                            # of a newly added file and its dependencies from the baseline run. Consequently,
                            # this may lead to the failure to remove pre-existing findings. A more effective
                            # approach would involve utilizing the same set of scanned diff targets from the
                            # first run in the baseline run. This approach ensures the safe elimination of any
                            # existing findings in the dependency files, even if the original file does not
                            # exist in the baseline commit.
                            Path(t.value)
                            for t in output_extra.core.paths.scanned
                        ]
                        scanned.extend(baseline_handler.status.renamed.values())
                        baseline_target_mode_config = TargetModeConfig.pro_diff_scan(
                            frozenset(
                                t for t in scanned if t.exists() and not t.is_symlink()
                            ),
                            0,  # scanning the same set of files in the second run
                        )
                    else:
                        baseline_target_strings = frozenset(
                            Path(t)
                            for t in baseline_targets
                            if t.exists() and not t.is_symlink()
                        )
                    baseline_target_manager = TargetManager(
                        includes=include,
                        excludes=exclude,
                        max_target_bytes=max_target_bytes,
                        # only target the paths that had a match, ignoring symlinks and non-existent files
                        target_strings=baseline_target_strings,
                        respect_git_ignore=respect_git_ignore,
                        allow_unknown_extensions=not skip_unknown_extensions,
                        ignore_profiles=file_ignore_to_ignore_profiles(
                            get_file_ignore(too_many_entries),
                        ),
                        respect_semgrepignore=respect_semgrepignore,
                    )

                    (
                        baseline_rule_matches_by_rule,
                        baseline_semgrep_errors,
                        _,
                        _,
                        _,
                        _plans,
                        _,
                    ) = run_rules(
                        # only the rules that had a match
                        [
                            rule
                            for rule, matches in rule_matches_by_rule.items()
                            if matches
                        ],
                        baseline_target_manager,
                        core_runner,
                        output_handler,
                        dump_command_for_core,
                        time_flag,
                        matching_explanations,
                        engine_type,
                        strict,
                        run_secrets,
                        disable_secrets_validation,
                        baseline_target_mode_config,
                        allow_local_builds=allow_local_builds,
                        ptt_enabled=ptt_enabled,
                    )
                    rule_matches_by_rule = remove_matches_in_baseline(
                        rule_matches_by_rule,
                        baseline_rule_matches_by_rule,
                        baseline_handler.status.renamed,
                    )
                    output_handler.handle_semgrep_errors(baseline_semgrep_errors)
            except Exception as e:
                raise SemgrepError(e)

    # If there are multiple outputs and any request to keep_ignores
    # then all outputs keep the ignores. The only output format that
    # keep ignored matches currently is sarif.
    ignores_start_time = time.time()
    keep_ignored = disable_nosem or output_handler.keep_ignores()
    filtered_matches_by_rule = filter_ignored(
        rule_matches_by_rule, keep_ignored=keep_ignored
    )
    profiler.save("ignores_time", ignores_start_time)

    profiler.save("total_time", rule_start_time)

    # Metrics send part 2: send results
    if metrics.is_enabled:
        metrics.add_rules(filtered_rules, output_extra.core.time)
        metrics.add_max_memory_bytes(output_extra.core.time)
        metrics.add_targets(output_extra.all_targets, output_extra.core.time)
        metrics.add_findings(filtered_matches_by_rule)
        metrics.add_errors(semgrep_errors)
        metrics.add_profiling(profiler)
        metrics.add_parse_rates(output_extra.parsing_data)
        metrics.add_interfile_languages_used(output_extra.core.interfile_languages_used)
        if engine_type.is_pro and baseline_handler:
            metrics.add_num_diff_scanned(
                {Path(t.value) for t in output_extra.core.paths.scanned}, filtered_rules
            )

    if autofix:
        apply_fixes(filtered_matches_by_rule.kept, dryrun)

    renamed_targets = set(
        baseline_handler.status.renamed.values() if baseline_handler else []
    )
    executed_rule_count = sum(
        max(0, len(plan.rules) - len(plan.unused_rules)) for plan in plans
    )

    return (
        filtered_matches_by_rule.kept,
        semgrep_errors,
        renamed_targets,
        target_manager.ignore_log,
        filtered_rules,
        profiler,
        output_extra,
        shown_severities,
        dependencies,
        dependency_parser_errors,
        executed_rule_count,
        missed_rule_count,
        all_subprojects,
    )


# This is called from join_rule.py and test.py (and maybe tools wrapping
# semgrep)
# old: this used to be called semgrep.semgrep_main.invoke_semgrep()
# and was part of an unofficial Python API but external users should
# instead wrap the CLI, not this internal Python function that will
# soon disappear.
def run_scan_and_return_json(
    config: Path,
    targets: List[Path],
    output_settings: Optional[OutputSettings] = None,
    **kwargs: Any,
) -> Union[Dict[str, Any], str]:
    """
    Return Semgrep results of 'config' on 'targets' as a dict|str
    Uses default arguments of 'run_scan.run_scan' unless overwritten with 'kwargs'
    """
    if output_settings is None:
        output_settings = OutputSettings(output_format=OutputFormat.JSON)

    StringIO()
    output_handler = OutputHandler(output_settings)
    (
        filtered_matches_by_rule,
        _,
        _,
        _,
        filtered_rules,
        profiler,
        output_extra,
        shown_severities,
        _,
        _,
        _,
        _,
        _all_subprojects,
    ) = run_scan(
        output_handler=output_handler,
        target=[str(t) for t in targets],
        pattern="",
        lang="",
        configs=[str(config)],
        **kwargs,
    )

    output_handler.rules = frozenset(filtered_rules)
    output_handler.rule_matches = [
        m for ms in filtered_matches_by_rule.values() for m in ms
    ]
    output_handler.profiler = profiler
    output_handler.severities = shown_severities
    output_handler.explanations = output_extra.core.explanations
    output_handler.extra = output_extra

    outputs = tuple(output_handler._build_outputs())
    if len(outputs) != 1:
        raise RuntimeError("run_scan_and_return_json: expects a single output")

    return json.loads(outputs[0][1])  # type: ignore
