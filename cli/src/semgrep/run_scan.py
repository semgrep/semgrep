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
import json
import time
from io import StringIO
from os import environ
from pathlib import Path
from sys import getrecursionlimit
from sys import setrecursionlimit
from typing import Any
from typing import Collection
from typing import Dict
from typing import List
from typing import Optional
from typing import Sequence
from typing import Set
from typing import Tuple
from typing import Union

from boltons.iterutils import partition

import semgrep.scan_report as scan_report
import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.parse_lockfile import parse_lockfile_path
from semdep.parsers.util import DependencyParserError
from semgrep import __VERSION__
from semgrep.autofix import apply_fixes
from semgrep.config_resolver import get_config
from semgrep.constants import DEFAULT_DIFF_DEPTH
from semgrep.constants import DEFAULT_TIMEOUT
from semgrep.constants import OutputFormat
from semgrep.core_runner import CoreRunner
from semgrep.core_runner import get_contributions
from semgrep.engine import EngineType
from semgrep.error import FilesNotFoundError
from semgrep.error import MISSING_CONFIG_EXIT_CODE
from semgrep.error import SemgrepError
from semgrep.exclude_rules import filter_exclude_rule
from semgrep.git import BaselineHandler
from semgrep.ignores import FileIgnore
from semgrep.ignores import IGNORE_FILE_NAME
from semgrep.ignores import Parser
from semgrep.nosemgrep import process_ignores
from semgrep.output import DEFAULT_SHOWN_SEVERITIES
from semgrep.output import OutputHandler
from semgrep.output import OutputSettings
from semgrep.output_extra import OutputExtra
from semgrep.profile_manager import ProfileManager
from semgrep.project import get_project_url
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatchMap
from semgrep.rule_match import RuleMatchSet
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_types import JOIN_MODE
from semgrep.state import get_state
from semgrep.target_manager import ECOSYSTEM_TO_LOCKFILES
from semgrep.target_manager import FileTargetingLog
from semgrep.target_manager import TargetManager
from semgrep.target_mode import TargetModeConfig
from semgrep.util import unit_str
from semgrep.verbose_logging import getLogger


logger = getLogger(__name__)

##############################################################################
# Helper functions
##############################################################################


def get_file_ignore() -> FileIgnore:
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
                "No .semgrepignore found. Using default .semgrepignore rules. See the docs for the list of default ignores: https://semgrep.dev/docs/cli-usage/#ignoring-files"
            )
            semgrepignore_path = TEMPLATES_DIR / IGNORE_FILE_NAME
        else:
            logger.verbose("using path ignore rules from user provided .semgrepignore")

    with semgrepignore_path.open() as f:
        file_ignore = FileIgnore.from_unprocessed_patterns(
            base_path=workdir,
            patterns=Parser(file_path=semgrepignore_path, base_path=workdir).parse(f),
        )

    return file_ignore


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


# This runs semgrep-core (and also handles SCA and join rules)
def run_rules(
    filtered_rules: List[Rule],
    target_manager: TargetManager,
    core_runner: CoreRunner,
    output_handler: OutputHandler,
    dump_command_for_core: bool,
    time_flag: bool,
    matching_explanations: bool,
    engine_type: EngineType,
    # TODO: Use an array of semgrep_output_v1.Product instead of booleans flags for secrets, code, and supply chain
    run_secrets: bool = False,
    target_mode_config: Optional[TargetModeConfig] = None,
    *,
    with_code_rules: bool = True,
    with_supply_chain: bool = False,
) -> Tuple[
    RuleMatchMap,
    List[SemgrepError],
    OutputExtra,
    Dict[str, List[FoundDependency]],
    List[DependencyParserError],
    int,
]:
    if not target_mode_config:
        target_mode_config = TargetModeConfig.whole_scan()

    cli_ux = get_state().get_cli_ux_flavor()
    num_executed_rules = scan_report.print_scan_status(
        filtered_rules,
        target_manager,
        target_mode_config,
        cli_ux=cli_ux,
        with_code_rules=with_code_rules,
        with_supply_chain=with_supply_chain,
    )

    join_rules, rest_of_the_rules = partition(
        filtered_rules, lambda rule: rule.mode == JOIN_MODE
    )
    dependency_aware_rules = [r for r in rest_of_the_rules if r.project_depends_on]
    dependency_only_rules, rest_of_the_rules = partition(
        rest_of_the_rules, lambda rule: not rule.should_run_on_semgrep_core
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
        run_secrets,
        target_mode_config,
    )

    if join_rules:
        import semgrep.join_rule as join_rule

        for rule in join_rules:
            join_rule_matches, join_rule_errors = join_rule.run_join_rule(
                rule.raw, [target.path for target in target_manager.targets]
            )
            join_rule_matches_set = RuleMatchSet(rule)
            for m in join_rule_matches:
                join_rule_matches_set.add(m)
            join_rule_matches_by_rule = {
                Rule.from_json(rule.raw): list(join_rule_matches_set)
            }
            rule_matches_by_rule.update(join_rule_matches_by_rule)
            output_handler.handle_semgrep_errors(join_rule_errors)

    dependencies = {}
    dependency_parser_errors = []
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
                    target_manager,
                )
                rule_matches_by_rule[rule] = dep_rule_matches
                output_handler.handle_semgrep_errors(dep_rule_errors)
                (
                    dep_rule_matches,
                    dep_rule_errors,
                ) = generate_unreachable_sca_findings(
                    rule, target_manager, already_reachable
                )
                rule_matches_by_rule[rule].extend(dep_rule_matches)
                output_handler.handle_semgrep_errors(dep_rule_errors)
            else:
                (
                    dep_rule_matches,
                    dep_rule_errors,
                ) = generate_unreachable_sca_findings(
                    rule, target_manager, lambda p, d: False
                )
                rule_matches_by_rule[rule] = dep_rule_matches
                output_handler.handle_semgrep_errors(dep_rule_errors)

        # Generate stats per lockfile:
        for ecosystem in ECOSYSTEM_TO_LOCKFILES.keys():
            for lockfile in target_manager.get_lockfiles(ecosystem):
                # Add lockfiles as a target that was scanned
                output_extra.all_targets.add(lockfile)
                # Warning temporal assumption: this is the only place we process
                # parse errors. We silently toss them in other places we call parse_lockfile_path
                # It doesn't really matter where it gets handled as long as we collect the parse errors somewhere
                deps, parse_errors = parse_lockfile_path(lockfile)
                dependencies[str(lockfile)] = deps
                dependency_parser_errors.extend(parse_errors)
    return (
        rule_matches_by_rule,
        semgrep_errors,
        output_extra,
        dependencies,
        dependency_parser_errors,
        num_executed_rules,
    )


##############################################################################
# Entry points
##############################################################################

# cli/bin/semgrep -> main.py -> cli.py -> commands/scan.py -> run_scan()
# old: this used to be called semgrep.semgrep_main.main
def run_scan(
    *,
    diff_depth: int = DEFAULT_DIFF_DEPTH,
    dump_command_for_core: bool = False,
    time_flag: bool = False,
    matching_explanations: bool = False,
    engine_type: EngineType = EngineType.OSS,
    run_secrets: bool = False,
    output_handler: OutputHandler,
    target: Sequence[str],
    pattern: Optional[str],
    lang: Optional[str],
    configs: Sequence[
        str
    ],  # NOTE: Since the `ci` command reuses this function, we intentionally do not set a default at this level.
    no_rewrite_rule_ids: bool = False,
    jobs: Optional[int] = None,
    include: Optional[Sequence[str]] = None,
    exclude: Optional[Sequence[str]] = None,
    exclude_rule: Optional[Sequence[str]] = None,
    strict: bool = False,
    autofix: bool = False,
    replacement: Optional[str] = None,
    dryrun: bool = False,
    disable_nosem: bool = False,
    no_git_ignore: bool = False,
    respect_rule_paths: bool = True,
    timeout: int = DEFAULT_TIMEOUT,
    max_memory: int = 0,
    interfile_timeout: int = 0,
    max_target_bytes: int = 0,
    timeout_threshold: int = 0,
    skip_unknown_extensions: bool = False,
    allow_untrusted_postprocessors: bool = False,
    severity: Optional[Sequence[str]] = None,
    optimizations: str = "none",
    baseline_commit: Optional[str] = None,
    baseline_commit_is_mergebase: bool = False,
    dump_contributions: bool = False,
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
    int,
    out.Contributions,
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
        exclude = []

    if exclude_rule is None:
        exclude_rule = []

    project_url = get_project_url()
    profiler = ProfileManager()

    rule_start_time = time.time()
    configs_obj, config_errors = get_config(
        pattern, lang, configs, replacement=replacement, project_url=project_url
    )
    all_rules = configs_obj.get_rules(no_rewrite_rule_ids)
    profiler.save("config_time", rule_start_time)

    # We determine if SAST / SCA is enabled based on the config str
    with_code_rules = configs_obj.with_code_rules
    with_supply_chain = configs_obj.with_supply_chain

    # Metrics send part 1: add environment information
    # Must happen after configs are resolved because it is determined
    # then whether metrics are sent or not
    metrics = get_state().metrics
    if metrics.is_enabled:
        metrics.add_project_url(project_url)
        metrics.add_integration_name(environ.get("SEMGREP_INTEGRATION_NAME"))
        metrics.add_configs(configs)
        metrics.add_engine_type(engine_type)

    if not severity:
        shown_severities = DEFAULT_SHOWN_SEVERITIES
        filtered_rules = all_rules
    else:
        shown_severities = {out.MatchSeverity.from_json(s) for s in severity}
        filtered_rules = [rule for rule in all_rules if rule.severity.value in severity]
    filtered_rules = filter_exclude_rule(filtered_rules, exclude_rule)

    output_handler.handle_semgrep_errors(config_errors)

    if config_errors and strict:
        raise SemgrepError(
            f"Ran with --strict and got {unit_str(len(config_errors), 'error')} while loading configs",
            code=MISSING_CONFIG_EXIT_CODE,
        )

    if not pattern:
        config_id_if_single = (
            list(configs_obj.valid.keys())[0] if len(configs_obj.valid) == 1 else ""
        )
        invalid_msg = (
            f"({unit_str(len(config_errors), 'invalid config file')})"
            if len(config_errors)
            else ""
        )
        logger.verbose(
            f"running {len(filtered_rules)} rules from {unit_str(len(configs_obj.valid), 'config')} {config_id_if_single} {invalid_msg}".strip()
        )
        if len(config_errors) > 0:
            raise SemgrepError(
                f"invalid configuration file found ({len(config_errors)} configs were invalid)",
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

    # Initialize baseline here to fail early on bad args
    baseline_handler = None
    if baseline_commit:
        try:
            baseline_handler = BaselineHandler(
                baseline_commit, is_mergebase=baseline_commit_is_mergebase
            )
        # TODO better handling
        except Exception as e:
            raise SemgrepError(e)

    respect_git_ignore = not no_git_ignore
    try:
        target_manager = TargetManager(
            includes=include,
            excludes=exclude,
            max_target_bytes=max_target_bytes,
            target_strings=target,
            respect_git_ignore=respect_git_ignore,
            respect_rule_paths=respect_rule_paths,
            baseline_handler=baseline_handler,
            allow_unknown_extensions=not skip_unknown_extensions,
            file_ignore=get_file_ignore(),
        )
    except FilesNotFoundError as e:
        raise SemgrepError(e)

    target_mode_config = TargetModeConfig.whole_scan()
    if baseline_handler is not None:
        if engine_type.is_interfile:
            target_mode_config = TargetModeConfig.pro_diff_scan(
                # `target_manager.get_all_files()` will only return changed files
                # (diff targets) when baseline_handler is set
                target_manager.get_all_files(),
                diff_depth,
            )
        else:
            target_mode_config = TargetModeConfig.diff_scan()

    core_start_time = time.time()
    core_runner = CoreRunner(
        jobs=jobs,
        engine_type=engine_type,
        run_secrets=run_secrets,
        timeout=timeout,
        max_memory=max_memory,
        interfile_timeout=interfile_timeout,
        timeout_threshold=timeout_threshold,
        optimizations=optimizations,
        allow_untrusted_postprocessors=allow_untrusted_postprocessors,
        respect_rule_paths=respect_rule_paths,
    )

    if dump_contributions:
        contributions = get_contributions(engine_type)
    else:
        contributions = out.Contributions([])

    experimental_rules, unexperimental_rules = partition(
        filtered_rules, lambda rule: (isinstance(rule.severity.value, out.Experiment))
    )

    logger.verbose("Rules:")
    for ruleid in sorted(rule.id for rule in unexperimental_rules):
        logger.verbose(f"- {ruleid}")

    if len(experimental_rules) > 0:
        logger.verbose("Experimental Rules:")
        for ruleid in sorted(rule.id for rule in experimental_rules):
            logger.verbose(f"- {ruleid}")

    (
        rule_matches_by_rule,
        semgrep_errors,
        output_extra,
        dependencies,
        dependency_parser_errors,
        num_executed_rules,
    ) = run_rules(
        filtered_rules,
        target_manager,
        core_runner,
        output_handler,
        dump_command_for_core,
        time_flag,
        matching_explanations,
        engine_type,
        run_secrets,
        target_mode_config,
        with_code_rules=with_code_rules,
        with_supply_chain=with_supply_chain,
    )
    profiler.save("core_time", core_start_time)
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
                    baseline_target_strings = target
                    baseline_target_mode_config = target_mode_config
                    if target_mode_config.is_pro_diff_scan:
                        baseline_target_mode_config = TargetModeConfig.pro_diff_scan(
                            frozenset(
                                t
                                for t in target_mode_config.get_diff_targets()
                                if t.exists() and not t.is_symlink()
                            ),
                            target_mode_config.get_diff_depth(),
                        )
                    else:
                        baseline_target_strings = [
                            str(t)
                            for t in baseline_targets
                            if t.exists() and not t.is_symlink()
                        ]
                    baseline_target_manager = TargetManager(
                        includes=include,
                        excludes=exclude,
                        max_target_bytes=max_target_bytes,
                        # only target the paths that had a match, ignoring symlinks and non-existent files
                        target_strings=baseline_target_strings,
                        respect_git_ignore=respect_git_ignore,
                        allow_unknown_extensions=not skip_unknown_extensions,
                        file_ignore=get_file_ignore(),
                    )

                    (
                        baseline_rule_matches_by_rule,
                        baseline_semgrep_errors,
                        _,
                        _,
                        _,
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
                        run_secrets,
                        baseline_target_mode_config,
                    )
                    rule_matches_by_rule = remove_matches_in_baseline(
                        rule_matches_by_rule,
                        baseline_rule_matches_by_rule,
                        baseline_handler.status.renamed,
                    )
                    output_handler.handle_semgrep_errors(baseline_semgrep_errors)
            except Exception as e:
                raise SemgrepError(e)

    ignores_start_time = time.time()
    keep_ignored = disable_nosem or output_handler.formatter.keep_ignores()
    filtered_matches_by_rule, nosem_errors = process_ignores(
        rule_matches_by_rule, keep_ignored=keep_ignored, strict=strict
    )
    profiler.save("ignores_time", ignores_start_time)
    output_handler.handle_semgrep_errors(nosem_errors)

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

    if autofix:
        apply_fixes(filtered_matches_by_rule.kept, dryrun)

    renamed_targets = set(
        baseline_handler.status.renamed.values() if baseline_handler else []
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
        num_executed_rules,
        contributions,
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

    return json.loads(output_handler._build_output())  # type: ignore
