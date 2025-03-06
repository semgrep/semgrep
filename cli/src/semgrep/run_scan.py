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
from typing import FrozenSet
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

import semgrep.error as error
import semgrep.scan_report as scan_report
import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.parsers.util import DependencyParserError
from semgrep import __VERSION__
from semgrep import tracing
from semgrep.autofix import apply_fixes
from semgrep.config_resolver import Config
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
from semgrep.error import InvalidScanningRootError
from semgrep.error import MISSING_CONFIG_EXIT_CODE
from semgrep.error import select_real_errors
from semgrep.error import SemgrepError
from semgrep.exclude_rules import filter_exclude_rule
from semgrep.git import BaselineHandler
from semgrep.git import get_project_url
from semgrep.ignores import parse_semgrepignore_file
from semgrep.ignores import Semgrepignore
from semgrep.ignores import SEMGREPIGNORE_FILE_NAME
from semgrep.metrics import Metrics
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
from semgrep.semgrep_interfaces.semgrep_output_v1 import Product
from semgrep.semgrep_types import JOIN_MODE
from semgrep.state import get_state
from semgrep.subproject import DependencyResolutionConfig
from semgrep.subproject import get_all_source_files
from semgrep.subproject import iter_found_dependencies
from semgrep.subproject import make_dependencies_by_source_path
from semgrep.target_manager import FileTargetingLog
from semgrep.target_manager import SAST_PRODUCT
from semgrep.target_manager import SCA_PRODUCT
from semgrep.target_manager import SECRETS_PRODUCT
from semgrep.target_manager import TargetManager
from semgrep.target_mode import TargetModeConfig
from semgrep.types import FilteredMatches
from semgrep.util import flatten
from semgrep.util import unit_str
from semgrep.verbose_logging import getLogger


logger = getLogger(__name__)

##############################################################################
# Helper functions
##############################################################################


# Some of the lockfile parsers are defined recursively
# This does not play well with python's conservative recursion limit, so we
# manually increase
def adjust_python_recursion_limit() -> None:
    if "SEMGREP_PYTHON_RECURSION_LIMIT_INCREASE" in environ:
        recursion_limit_increase = int(
            environ["SEMGREP_PYTHON_RECURSION_LIMIT_INCREASE"]
        )
    else:
        recursion_limit_increase = 500
    setrecursionlimit(getrecursionlimit() + recursion_limit_increase)


# This is used for testing and comparing with osemgrep.
def list_targets_and_exit(
    target_manager: TargetManager, product: out.Product, long_format: bool = False
) -> None:
    targets = target_manager.get_files_for_language(lang=None, product=product)
    for path in sorted(targets.kept):
        if long_format:
            print(f"selected {path}")
        else:
            print(str(path))
    if long_format:
        for path, reason in target_manager.ignore_log.list_skipped_paths_with_reason():
            print(f"ignored {path} [{reason}]")
    exit(0)


def dump_partitions_and_exit(
    filtered_rules: List[Rule],
    dump_rule_partitions_dir: Optional[Path],
    dump_n_rule_partitions: int,
) -> None:
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


def get_semgrepignore(max_log_list_entries: int) -> Semgrepignore:
    # TEMPLATES_DIR is where we have the default .semgrepignore file
    # in semgrep installation.
    TEMPLATES_DIR = Path(__file__).parent / "templates"
    try:
        workdir = Path.cwd()
    except FileNotFoundError:
        workdir = Path.home()
        logger.warn(
            f"Current working directory does not exist! Instead checking {workdir} for .semgrepignore files"
        )

    semgrepignore_path = Path(workdir / SEMGREPIGNORE_FILE_NAME)
    if not semgrepignore_path.is_file():
        logger.verbose(
            "No .semgrepignore found. Using default .semgrepignore rules. See the docs for the list of default ignores: https://semgrep.dev/docs/cli-usage/#ignore-files"
        )
        # Use the default .semgrepignore file
        semgrepignore_path = TEMPLATES_DIR / SEMGREPIGNORE_FILE_NAME
    else:
        logger.verbose("using path ignore rules from user provided .semgrepignore")

    semgrepignore = parse_semgrepignore_file(
        semgrepignore_path=semgrepignore_path,
        workdir=workdir,
        max_log_list_entries=max_log_list_entries,
    )
    return semgrepignore


# TODO: Explain what is this about, use better names
# TODO: Describe how you intend to implement this in OCaml
def semgrepignore_to_semgrepignore_profiles(
    semgrepignore: Semgrepignore,
) -> Dict[Product, Semgrepignore]:
    # TODO: This pattern encodes the default Targeting Profiles
    #  of .semgrepignore. Don't hardcode this like it is.
    return {
        SAST_PRODUCT: semgrepignore,
        SCA_PRODUCT: semgrepignore,
        # TODO: this looks like a complicated way to ignore the user-specified
        #  .semgrepignore or the fallback stored in TEMPLATES_DIR.
        #  -> need to confirm and simplify.
        SECRETS_PRODUCT: Semgrepignore(
            base_path=semgrepignore.base_path,
            fnmatch_patterns=frozenset(),
        ),
    }


def target_mode_conf(
    historical_secrets: bool,
    baseline_handler: Optional[BaselineHandler],
    engine_type: EngineType,
    diff_depth: int,
    target_manager: TargetManager,
) -> TargetModeConfig:
    if historical_secrets:
        return TargetModeConfig.historical_scan()
    elif baseline_handler is not None:
        if engine_type.is_interfile:
            return TargetModeConfig.pro_diff_scan(
                # `target_manager.get_all_files()` will only return changed files
                # (diff targets) when baseline_handler is set
                target_manager.get_all_files(),
                diff_depth,
            )
        else:
            return TargetModeConfig.diff_scan()
    else:
        return TargetModeConfig.whole_scan()


##############################################################################
# Error management
##############################################################################


def sanity_check_resolved_config(
    real_config_errors: List[SemgrepError], configs_obj: Config
) -> None:
    if len(real_config_errors) > 0:
        raise SemgrepError(
            f"invalid configuration file found ({len(real_config_errors)} configs were invalid)",
            code=MISSING_CONFIG_EXIT_CODE,
        )
    # NOTE: We should default to config auto if no config was passed in an
    # earlier step, but if we reach this step without a config, we emit the
    # error below.
    if len(configs_obj.valid) == 0:
        raise SemgrepError(
            """No config given. Run with `--config auto` or see https://semgrep.dev/docs/running-rules/ for instructions on running with a specific config""",
            code=MISSING_CONFIG_EXIT_CODE,
        )


##############################################################################
# Logging
##############################################################################


def log_running_rules(
    configs_obj: Config,
    config_errors: Sequence[SemgrepError],
    filtered_rules: List[Rule],
) -> None:
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


def log_rules(filtered_rules: List[Rule], too_many_entries: int) -> None:
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


##############################################################################
# Metrics
##############################################################################


# TODO: group the diff scan params and secrets stuff in separate dataclasses
def add_metrics_part1(
    metrics: Metrics,
    project_url: Optional[str],
    engine_type: EngineType,
    configs: Sequence[str],
    configs_obj: Config,
    baseline_commit: Optional[str],
    diff_depth: int,
    run_secrets: bool,
    allow_untrusted_validators: bool,
    disable_secrets_validation: bool,
) -> None:
    # We determine if SAST / SCA is enabled based on the config str
    with_code_rules = configs_obj.with_code_rules
    with_supply_chain = configs_obj.with_supply_chain

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


def add_metrics_part2(
    metrics: Metrics,
    filtered_rules: List[Rule],
    output_extra: OutputExtra,
    filtered_matches_by_rule: FilteredMatches,
    semgrep_errors: List[SemgrepError],
    profiler: ProfileManager,
    engine_type: EngineType,
    baseline_handler: Optional[BaselineHandler],
) -> None:
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


##############################################################################
# DiffScan
##############################################################################


def baseline_handler_opt(
    baseline_commit: Optional[str], baseline_commit_is_mergebase: bool
) -> Optional[BaselineHandler]:
    baseline_handler = None
    if baseline_commit:
        try:
            baseline_handler = BaselineHandler(
                baseline_commit, is_mergebase=baseline_commit_is_mergebase
            )
        except Exception:
            # Display a trace because we have no idea where the exn was raised.
            exception_with_trace: str = traceback.format_exc()
            raise SemgrepError(
                f"Exception in BaselineHandler initialization: {exception_with_trace}"
            )
    return baseline_handler


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


# TODO: group some params in separate dataclasses because ugly to have that many
# params, insane


def baseline_run(
    baseline_handler: BaselineHandler,
    baseline_commit: Optional[str],
    rule_matches_by_rule: RuleMatchMap,
    all_subprojects: List[Union[out.UnresolvedSubproject, out.ResolvedSubproject]],
    scanning_root_strings: FrozenSet[Path],
    target_mode_config: TargetModeConfig,
    output_extra: OutputExtra,
    include: Sequence[str],
    exclude: Mapping[out.Product, Sequence[str]],
    max_target_bytes: int,
    respect_git_ignore: bool,
    skip_unknown_extensions: bool,
    too_many_entries: int,
    respect_semgrepignore: bool,
    core_runner: CoreRunner,
    output_handler: OutputHandler,
    dump_command_for_core: bool,
    time_flag: bool,
    matching_explanations: bool,
    engine_type: EngineType,
    strict: bool,
    run_secrets: bool,
    disable_secrets_validation: bool,
    allow_local_builds: bool,
    ptt_enabled: bool,
    use_semgrepignore_v2: bool,
) -> RuleMatchMap:
    """
    Run baseline scan and return the updated rule_matches_by_rule with baseline matches removed.
    """
    findings_count = sum(
        len([match for match in matches if not match.from_transient_scan])
        for matches in rule_matches_by_rule.values()
    )
    logger.info(f"  Current version has {unit_str(findings_count, 'finding')}.")
    logger.info("")

    # The idea of the baseline scan is that we want to find out which of the
    # findings from the head commit that we just scanned are really "new", and
    # not already present on the baseline commit.
    # We don't want to bother scanning the entire project on the baseline
    # commit, because if we only found matches in one file of a huge monorepo
    # on our head commit scan, it would be a waste of time to scan the entire
    # monorepo on the baseline commit to find out which of those matches were
    # aleady present
    # To this end, the files we want to scan on the baseline commit are the
    # following:

    # All the files that had a match in the head commit
    paths_with_matches = list(
        {match.path for matches in rule_matches_by_rule.values() for match in matches}
    )
    baseline_targets = set(paths_with_matches)

    # For each dependency subproject, if we resolved it in the head commit,
    # and there was a match in any file associated with that subproject,
    # either a code file or a lockfile, we want to include the lockfile and
    # (if present) the manifest file of that subproject.
    # Instead of trying to compute this, we just include all the resolved
    # subprojects, which is guaranteed to include all the files we really need
    baseline_targets |= set(
        flatten(
            [
                get_all_source_files(x.info.dependency_source)
                for x in all_subprojects
                if isinstance(x, out.ResolvedSubproject)
            ]
        )
    )

    # If a file was renamed between the baseline commit and the head commit,
    # [baseline_handler.status.renamed] maps the new path to the old path
    # If a renamed file had matches in the head commit, we still want to
    # scan it in the baseline commit, so we add the original path of all
    # renamed files this technically includes more targets than necessary:
    # if `foo.py` is renamed to `bar.py`, and `bar.py` had no matches in the
    # head commit, we still scan `foo.py` in the baseline commit, but it seems
    # safer to not change this
    baseline_targets |= set(baseline_handler.status.renamed.values())

    # We want to *exclude* any files that were added between the baseline commit
    # and the head commit, because they won't exist on the baseline commit
    baseline_targets -= set(baseline_handler.status.added)

    if not paths_with_matches:
        logger.info("Skipping baseline scan, because there are no current findings.")
    elif not baseline_targets:
        logger.info(
            "Skipping baseline scan, because all current findings are in files that didn't exist in the baseline commit."
        )
    else:
        logger.info(f"Creating git worktree from '{baseline_commit}' to scan baseline.")
        baseline_handler.print_git_log()
        logger.info("")
        try:
            with baseline_handler.baseline_context():
                baseline_scanning_root_strings = scanning_root_strings
                baseline_target_mode_config = target_mode_config
                if target_mode_config.is_pro_diff_scan:
                    # Conducting the inter-file diff scan twice with the exact
                    # same configuration, both on the current commit and the
                    # baseline commit, could result in the absence of a newly
                    # added file and its dependencies from the baseline run.
                    # Consequently, this may lead to the failure to remove
                    # pre-existing findings.
                    # A more effective approach would involve utilizing the same
                    # set of scanned diff targets from the first run in the
                    # baseline run. This approach ensures the safe elimination
                    # of any existing findings in the dependency files, even if
                    # the original file does not exist in the baseline commit.
                    scanned = [Path(t.value) for t in output_extra.core.paths.scanned]
                    scanned.extend(baseline_handler.status.renamed.values())
                    baseline_target_mode_config = TargetModeConfig.pro_diff_scan(
                        frozenset(
                            t for t in scanned if t.exists() and not t.is_symlink()
                        ),
                        0,  # scanning the same set of files in the second run
                    )
                else:
                    baseline_scanning_root_strings = frozenset(
                        Path(t)
                        for t in baseline_targets
                        if t.exists() and not t.is_symlink()
                    )
                baseline_target_manager = TargetManager(
                    scanning_root_strings=baseline_scanning_root_strings,
                    use_semgrepignore_v2=use_semgrepignore_v2,
                    includes=include,
                    excludes=exclude,
                    max_target_bytes=max_target_bytes,
                    # only target the paths that had a match, ignoring symlinks
                    # and non-existent files
                    respect_git_ignore=respect_git_ignore,
                    allow_unknown_extensions=not skip_unknown_extensions,
                    semgrepignore_profiles=semgrepignore_to_semgrepignore_profiles(
                        get_semgrepignore(too_many_entries),
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
                    [rule for rule, matches in rule_matches_by_rule.items() if matches],
                    baseline_target_manager,
                    baseline_target_mode_config,
                    core_runner,
                    output_handler,
                    dump_command_for_core,
                    time_flag,
                    matching_explanations,
                    engine_type,
                    strict,
                    run_secrets,
                    disable_secrets_validation,
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
    return rule_matches_by_rule


##############################################################################
# Join rules
##############################################################################


def adjust_matches_for_join_rules(
    rule_matches_by_rule: RuleMatchMap,
    join_rules: List[Rule],
    target_manager: TargetManager,
    allow_local_builds: bool,
    ptt_enabled: bool,
    output_handler: OutputHandler,
) -> None:
    import semgrep.join_rule as join_rule

    for rule in join_rules:
        join_rule_matches, join_rule_errors = join_rule.run_join_rule(
            rule.raw,
            [scanning_root.path for scanning_root in target_manager.scanning_roots],
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


##############################################################################
# SCA
##############################################################################


# ??
def filter_dependency_aware_rules(
    dependency_aware_rules: List[Rule],
    resolved_deps: Dict[Ecosystem, List[out.ResolvedSubproject]],
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
                deps = list(iter_found_dependencies(sca_project.resolved_dependencies))
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


def resolve_dependencies(
    dependency_aware_rules: List[Rule],
    target_manager: TargetManager,
    output_handler: OutputHandler,
    allow_local_builds: bool,
    ptt_enabled: bool,
    resolve_all_deps_in_diff_scan: bool,
    download_dependency_source_code: bool,
) -> Tuple[
    List[Rule],  # filtered_dependency_aware_rules
    List[DependencyParserError],  # dependency_parser_errors
    List[Path],  # sca_dependency_targets
    List[Union[out.ResolvedSubproject, out.UnresolvedSubproject]],  # all_subprojects
    Dict[Ecosystem, List[out.ResolvedSubproject]],  # resolved_subprojects
]:
    """
    Resolve dependencies and process dependency-related errors.

    Args:
        dependency_aware_rules: Rules that depend on project dependencies
        target_manager: Manager for scan targets
        output_handler: Handler for semgrep errors
        allow_local_builds: Whether to allow local builds
        ptt_enabled: Whether PTT is enabled
        resolve_all_deps_in_diff_scan: Whether to resolve all dependencies in diff scan

    Returns:
        Tuple containing:
        - Filtered dependency-aware rules
        - Dependency parser errors
        - SCA dependency targets
        - All subprojects (resolved and unresolved)
    """
    # Initialize data structures
    filtered_dependency_aware_rules: List[Rule] = []
    dependency_parser_errors: List[DependencyParserError] = []
    sca_dependency_targets: List[Path] = []
    all_subprojects: List[Union[out.ResolvedSubproject, out.UnresolvedSubproject]] = []
    resolved_subprojects: Dict[Ecosystem, List[out.ResolvedSubproject]] = {}

    if not dependency_aware_rules:
        return (
            filtered_dependency_aware_rules,
            dependency_parser_errors,
            sca_dependency_targets,
            all_subprojects,
            resolved_subprojects,
        )

    # Configure dependency resolution
    dependency_resolution_config = DependencyResolutionConfig(
        allow_local_builds=allow_local_builds,
        ptt_enabled=ptt_enabled,
        resolve_untargeted_subprojects=resolve_all_deps_in_diff_scan,
        download_dependency_source_code=download_dependency_source_code,
    )

    # Parse lockfiles to get dependency information
    (
        unresolved_subprojects,
        resolved_subprojects,
        sca_dependency_targets,
    ) = resolve_subprojects(
        target_manager, dependency_aware_rules, dependency_resolution_config
    )

    # Process subprojects and their errors
    all_subprojects.extend(unresolved_subprojects)
    for subprojects in resolved_subprojects.values():
        all_subprojects.extend(subprojects)

    # Handle errors from subprojects
    for subproject in all_subprojects:
        dependency_parser_errors.extend(
            [
                e.value.value
                for e in subproject.errors
                if isinstance(e.value, out.SCAParse)
            ]
        )
        output_handler.handle_semgrep_errors(
            [
                error.DependencyResolutionSemgrepError(
                    type_=e.value.value.type_,
                    dependency_source_file=Path(
                        e.value.value.dependency_source_file.value
                    ),
                )
                for e in subproject.errors
                if isinstance(e.value, out.SCAResol)
            ]
        )

    # Filter rules that match the dependencies
    filtered_dependency_aware_rules = filter_dependency_aware_rules(
        dependency_aware_rules, resolved_subprojects
    )

    return (
        filtered_dependency_aware_rules,
        dependency_parser_errors,
        sca_dependency_targets,
        all_subprojects,
        resolved_subprojects,
    )


def adjust_matches_for_sca_rules(
    rule_matches_by_rule: RuleMatchMap,
    dependency_aware_rules: List[Rule],
    resolved_subprojects: Dict[Ecosystem, List[out.ResolvedSubproject]],
    sca_dependency_targets: List[Path],
    output_handler: OutputHandler,
    output_extra: OutputExtra,
    x_tr: bool = False,
) -> Dict[str, List[out.FoundDependency]]:
    from semgrep.dependency_aware_rule import (
        generate_unreachable_sca_findings,
        generate_reachable_sca_findings,
    )

    for rule in dependency_aware_rules:
        if rule.should_run_on_semgrep_core:
            # If we have a reachability rule (contains a pattern)
            # First we check if each match has a lockfile with the correct
            # vulnerability and turn these into SCA findings
            # Then we generate unreachable findings in all the remaining
            # targeted lockfiles
            # For each rule, we do not want to generate an unreachable
            # finding in a lockfile
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
                x_tr=x_tr,
            )
            rule_matches_by_rule[rule].extend(dep_rule_matches)
            output_handler.handle_semgrep_errors(dep_rule_errors)
        else:
            (
                dep_rule_matches,
                dep_rule_errors,
            ) = generate_unreachable_sca_findings(
                rule, lambda p, d: False, resolved_subprojects, x_tr=False
            )
            rule_matches_by_rule[rule] = dep_rule_matches
            output_handler.handle_semgrep_errors(dep_rule_errors)

    # The caller expects a map from lockfile path to `FoundDependency` items
    # rather than our Subproject representation
    deps_by_lockfile: Dict[str, List[out.FoundDependency]] = {}

    for ecosystem in resolved_subprojects:
        for proj in resolved_subprojects[ecosystem]:
            (
                proj_deps_by_lockfile,
                unknown_lockfile_deps,
            ) = make_dependencies_by_source_path(proj.resolved_dependencies)
            deps_by_lockfile.update(proj_deps_by_lockfile)

            # We don't really expect to have any dependencies with an
            # unknown lockfile, but we can't enforce this with types due to
            # backwards compatibility guarantees on FoundDependency. If we
            # see any dependencies without lockfile path, we assign them to
            # a fake lockfile at the root of each subproject.
            for dep in unknown_lockfile_deps:
                if (
                    str(
                        Path(proj.info.root_dir.value).joinpath(
                            Path("unknown_lockfile")
                        )
                    )
                    not in deps_by_lockfile
                ):
                    deps_by_lockfile[
                        str(
                            Path(proj.info.root_dir.value).joinpath(
                                Path("unknown_lockfile")
                            )
                        )
                    ] = []
                deps_by_lockfile[
                    str(
                        Path(proj.info.root_dir.value).joinpath(
                            Path("unknown_lockfile")
                        )
                    )
                ].append(dep)

    for target in sca_dependency_targets:
        output_extra.all_targets.add(target)

    return deps_by_lockfile


##############################################################################
# Run rules
##############################################################################


# This runs semgrep-core (and also handles SCA and join rules)
@tracing.trace()
def run_rules(
    filtered_rules: List[Rule],
    target_manager: TargetManager,
    target_mode_config: TargetModeConfig,
    core_runner: CoreRunner,
    output_handler: OutputHandler,
    dump_command_for_core: bool,
    time_flag: bool,
    matching_explanations: bool,
    engine_type: EngineType,
    strict: bool,
    # TODO: Use an array of semgrep_output_v1.Product instead of booleans flags
    # for secrets, code, and supply chain
    run_secrets: bool = False,
    disable_secrets_validation: bool = False,
    *,
    with_code_rules: bool = True,
    with_supply_chain: bool = False,
    allow_local_builds: bool = False,
    ptt_enabled: bool = False,
    resolve_all_deps_in_diff_scan: bool = False,
    x_tr: bool = False,
) -> Tuple[
    RuleMatchMap,
    List[SemgrepError],
    OutputExtra,
    Dict[str, List[out.FoundDependency]],
    List[DependencyParserError],
    List[Plan],
    List[Union[out.UnresolvedSubproject, out.ResolvedSubproject]],
]:
    # ---------------------------------------
    # Step1: split the rules (Join, SCA, rest)
    # ---------------------------------------
    join_rules, rest_of_the_rules = partition(
        filtered_rules, lambda rule: rule.mode == JOIN_MODE
    )

    # Get rules that rely on dependencies from the project's lockfile
    dependency_aware_rules: List[Rule] = [
        r for r in rest_of_the_rules if r.project_depends_on
    ]

    (
        filtered_dependency_aware_rules,
        dependency_parser_errors,
        sca_dependency_targets,
        all_subprojects,
        resolved_subprojects,
    ) = resolve_dependencies(
        dependency_aware_rules=dependency_aware_rules,
        target_manager=target_manager,
        output_handler=output_handler,
        allow_local_builds=allow_local_builds,
        ptt_enabled=ptt_enabled,
        resolve_all_deps_in_diff_scan=resolve_all_deps_in_diff_scan,
        download_dependency_source_code=x_tr,
    )

    # compute a set first to avoid O(n^2) complexity
    dependency_aware_rule_ids = set(r.id for r in dependency_aware_rules)
    rest_of_the_rules = [
        r for r in rest_of_the_rules if r.id not in dependency_aware_rule_ids
    ] + filtered_dependency_aware_rules

    # ---------------------------------------
    # Step3: reporting the plan
    # ---------------------------------------

    cli_ux = get_state().get_cli_ux_flavor()
    plans = scan_report.print_scan_status(
        filtered_rules,
        target_manager,
        target_mode_config,
        all_subprojects,
        dependency_parser_errors,
        cli_ux=cli_ux,
        with_code_rules=with_code_rules,
        with_supply_chain=with_supply_chain,
    )

    # ---------------------------------------
    # Step4: Dispatching to semgrep-core!
    # ---------------------------------------

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
        all_subprojects,
    )
    # ---------------------------------------
    # Step5: Adjusting rule_matches_by_rule
    # ---------------------------------------
    if join_rules:
        adjust_matches_for_join_rules(
            rule_matches_by_rule,
            join_rules,
            target_manager,
            allow_local_builds,
            ptt_enabled,
            output_handler,
        )

    if len(dependency_aware_rules) > 0:
        deps_by_lockfile = adjust_matches_for_sca_rules(
            rule_matches_by_rule=rule_matches_by_rule,
            dependency_aware_rules=dependency_aware_rules,
            resolved_subprojects=resolved_subprojects,
            sca_dependency_targets=sca_dependency_targets,
            output_handler=output_handler,
            output_extra=output_extra,
            x_tr=x_tr,
        )
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


##############################################################################
# Entry points
##############################################################################


# semgrep(entrypoint.py) -> main.py -> cli.py -> commands/scan.py -> run_scan()
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
    scanning_roots: Sequence[str],
    historical_secrets: bool = False,
    pattern: Optional[str],
    lang: Optional[str],
    # NOTE: Since the `ci` command reuses this function, we intentionally do
    # not set a default at this level.
    configs: Sequence[str],
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
    force_novcs_project: bool = False,
    force_project_root: Optional[str] = None,
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
    x_tr: bool = False,
    path_sensitive: bool = False,
    capture_core_stderr: bool = True,
    allow_local_builds: bool = False,
    dump_n_rule_partitions: Optional[int] = None,
    dump_rule_partitions_dir: Optional[Path] = None,
    ptt_enabled: bool = False,
    resolve_all_deps_in_diff_scan: bool = False,
    symbol_analysis: bool = False,
    use_semgrepignore_v2: bool = True,
) -> Tuple[
    RuleMatchMap,
    List[SemgrepError],
    Set[Path],
    FileTargetingLog,
    List[Rule],
    ProfileManager,
    OutputExtra,
    Collection[out.MatchSeverity],
    Dict[str, List[out.FoundDependency]],
    List[DependencyParserError],
    int,  # Executed Rule Count
    int,  # Missed Rule Count
    List[Union[out.UnresolvedSubproject, out.ResolvedSubproject]],
]:
    logger.debug(f"semgrep version {__VERSION__}")

    adjust_python_recursion_limit()
    project_url = get_project_url()
    profiler = ProfileManager()

    # ----------------------------
    # Step1: loading the rules
    # ----------------------------
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

    # Metrics send part 1: add environment information
    # Must happen after configs are resolved because it is determined
    # then whether metrics are sent or not
    metrics = get_state().metrics
    add_metrics_part1(
        metrics,
        project_url,
        engine_type,
        configs,
        configs_obj,
        baseline_commit,
        diff_depth,
        run_secrets,
        allow_untrusted_validators,
        disable_secrets_validation,
    )

    # ----------------------------
    # Step1 bis: adjust the rules
    # ----------------------------
    if exclude_rule is None:
        exclude_rule = []

    # TODO: handle de-duplication for pro-rules
    missed_rule_count = configs_obj.missed_rule_count

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
        dump_partitions_and_exit(
            filtered_rules, dump_rule_partitions_dir, dump_n_rule_partitions
        )

    # TODO? should we move this above closer to config_errors or put
    # down so that at least dump_partitions_and_exit is run?
    output_handler.handle_semgrep_errors(config_errors)
    real_config_errors = select_real_errors(config_errors)

    if not pattern:
        log_running_rules(configs_obj, config_errors, filtered_rules)
        sanity_check_resolved_config(real_config_errors, configs_obj)

    # This is after the `not pattern` block, because this error message is less
    # helpful.
    if real_config_errors and strict:
        raise SemgrepError(
            f"Ran with --strict and got {unit_str(len(config_errors), 'error')} while loading configs",
            code=MISSING_CONFIG_EXIT_CODE,
        )

    # ----------------------------
    # Step2: Computing the targets
    # ----------------------------
    # Initialize baseline here to fail early on bad args
    baseline_handler = baseline_handler_opt(
        baseline_commit, baseline_commit_is_mergebase
    )

    respect_git_ignore = not no_git_ignore
    # One of the several properties of the --no-git-ignore option is that
    # it disables the use of git.
    force_novcs_project = force_novcs_project or no_git_ignore
    scanning_root_strings = frozenset(Path(t) for t in scanning_roots)
    too_many_entries = output_handler.settings.max_log_list_entries

    if include is None:
        include = []
    if exclude is None:
        exclude = {}

    try:
        target_manager = TargetManager(
            scanning_root_strings=scanning_root_strings,
            use_semgrepignore_v2=use_semgrepignore_v2,
            includes=include,
            excludes=exclude,
            force_novcs_project=force_novcs_project,
            force_project_root=force_project_root,
            max_target_bytes=max_target_bytes,
            respect_git_ignore=respect_git_ignore,
            respect_rule_paths=respect_rule_paths,
            baseline_handler=baseline_handler,
            allow_unknown_extensions=not skip_unknown_extensions,
            semgrepignore_profiles=semgrepignore_to_semgrepignore_profiles(
                get_semgrepignore(too_many_entries)
            ),
            respect_semgrepignore=respect_semgrepignore,
        )
        # Debugging option --x-ls
        if x_ls or x_ls_long:
            list_targets_and_exit(target_manager, SAST_PRODUCT, long_format=x_ls_long)
    except InvalidScanningRootError as e:
        raise SemgrepError(e)

    target_mode_config = target_mode_conf(
        historical_secrets, baseline_handler, engine_type, diff_depth, target_manager
    )

    # ----------------------------
    # Step3: running the core engine
    # ----------------------------
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
        symbol_analysis=symbol_analysis,
    )
    # TODO? why displayed here? why not closer to log_running_rules?
    log_rules(filtered_rules, too_many_entries)

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
        target_mode_config,
        core_runner,
        output_handler,
        dump_command_for_core,
        time_flag,
        matching_explanations,
        engine_type,
        strict,
        run_secrets,
        disable_secrets_validation,
        with_code_rules=configs_obj.with_code_rules,
        with_supply_chain=configs_obj.with_supply_chain,
        allow_local_builds=allow_local_builds,
        ptt_enabled=ptt_enabled,
        resolve_all_deps_in_diff_scan=resolve_all_deps_in_diff_scan,
        x_tr=x_tr,
    )
    profiler.save("core_time", core_start_time)

    semgrep_errors: List[SemgrepError] = config_errors + scan_errors
    output_handler.handle_semgrep_errors(semgrep_errors)

    # ---------------------------------
    # Step3 bis: optional baseline run
    # ---------------------------------

    # Run baseline if needed
    if baseline_handler:
        rule_matches_by_rule = baseline_run(
            baseline_handler=baseline_handler,
            baseline_commit=baseline_commit,
            rule_matches_by_rule=rule_matches_by_rule,
            all_subprojects=all_subprojects,
            scanning_root_strings=scanning_root_strings,
            target_mode_config=target_mode_config,
            output_extra=output_extra,
            include=include,
            exclude=exclude,
            max_target_bytes=max_target_bytes,
            respect_git_ignore=respect_git_ignore,
            skip_unknown_extensions=skip_unknown_extensions,
            too_many_entries=too_many_entries,
            respect_semgrepignore=respect_semgrepignore,
            core_runner=core_runner,
            output_handler=output_handler,
            dump_command_for_core=dump_command_for_core,
            time_flag=time_flag,
            matching_explanations=matching_explanations,
            engine_type=engine_type,
            strict=strict,
            run_secrets=run_secrets,
            disable_secrets_validation=disable_secrets_validation,
            allow_local_builds=allow_local_builds,
            ptt_enabled=ptt_enabled,
            use_semgrepignore_v2=use_semgrepignore_v2,
        )

    # ---------------------------------
    # Step4: Nosemgrep filtering
    # ---------------------------------
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
    add_metrics_part2(
        metrics,
        filtered_rules,
        output_extra,
        filtered_matches_by_rule,
        semgrep_errors,
        profiler,
        engine_type,
        baseline_handler,
    )

    # ---------------------------------
    # Step5: Autofix
    # ---------------------------------
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
    *,
    config: Path,
    scanning_roots: List[Path],
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
        scanning_roots=[str(t) for t in scanning_roots],
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
