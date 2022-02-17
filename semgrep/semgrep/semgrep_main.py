import json
import time
from io import StringIO
from os import environ
from pathlib import Path
from typing import Any
from typing import Collection
from typing import Dict
from typing import List
from typing import Optional
from typing import Sequence
from typing import Set
from typing import Tuple
from typing import Union

from semgrep.autofix import apply_fixes
from semgrep.config_resolver import get_config
from semgrep.constants import DEFAULT_TIMEOUT
from semgrep.constants import OutputFormat
from semgrep.constants import RuleSeverity
from semgrep.core_runner import CoreRunner
from semgrep.error import FilesNotFoundError
from semgrep.error import MISSING_CONFIG_EXIT_CODE
from semgrep.error import SemgrepError
from semgrep.git import BaselineHandler
from semgrep.ignores import FileIgnore
from semgrep.ignores import IGNORE_FILE_NAME
from semgrep.ignores import Parser
from semgrep.metric_manager import metric_manager
from semgrep.nosemgrep import process_ignores
from semgrep.output import DEFAULT_SHOWN_SEVERITIES
from semgrep.output import OutputHandler
from semgrep.output import OutputSettings
from semgrep.profile_manager import ProfileManager
from semgrep.profiling import ProfilingData
from semgrep.project import get_project_url
from semgrep.rule import Rule
from semgrep.rule_match_map import RuleMatchMap
from semgrep.semgrep_types import JOIN_MODE
from semgrep.target_manager import IgnoreLog
from semgrep.target_manager import TargetManager
from semgrep.util import partition
from semgrep.verbose_logging import getLogger


logger = getLogger(__name__)


def notify_user_of_work(
    filtered_rules: Sequence[Rule],
    include: Sequence[str],
    exclude: Sequence[str],
) -> None:
    """
    Notify user of what semgrep is about to do, including:
    - number of rules
    - which rules? <- not yet, too cluttered
    - which dirs are excluded, etc.
    """
    if include:
        logger.info(f"including files:")
        for inc in include:
            logger.info(f"- {inc}")
    if exclude:
        logger.info(f"excluding files:")
        for exc in exclude:
            logger.info(f"- {exc}")
    logger.info(f"Running {len(filtered_rules)} rules...")
    logger.verbose("rules:")
    for ruleid in sorted([rule.id for rule in filtered_rules]):
        logger.verbose(f"- {ruleid}")


def get_file_ignore() -> FileIgnore:
    TEMPLATES_DIR = Path(__file__).parent / "templates"
    workdir = Path.cwd()

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
        file_ignore = FileIgnore(
            base_path=workdir,
            patterns=Parser(workdir).parse(f),
        )

    return file_ignore


def invoke_semgrep(
    config: Path,
    targets: List[Path],
    output_settings: Optional[OutputSettings] = None,
    **kwargs: Any,
) -> Union[Dict[str, Any], str]:
    """
    Return Semgrep results of 'config' on 'targets' as a dict|str
    Uses default arguments of 'semgrep_main.main' unless overwritten with 'kwargs'
    """
    if output_settings is None:
        output_settings = OutputSettings(output_format=OutputFormat.JSON)

    io_capture = StringIO()
    output_handler = OutputHandler(output_settings)
    (
        filtered_matches_by_rule,
        _,
        _,
        filtered_rules,
        profiler,
        profiling_data,
        shown_severities,
    ) = main(
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
    output_handler.profiling_data = profiling_data
    output_handler.severities = shown_severities

    return json.loads(output_handler._build_output())  # type: ignore


def run_rules(
    filtered_rules: List[Rule],
    target_manager: TargetManager,
    core_runner: CoreRunner,
    output_handler: OutputHandler,
    dump_command_for_core: bool,
    deep: bool,
) -> Tuple[RuleMatchMap, List[SemgrepError], Set[Path], ProfilingData,]:
    join_rules, rest_of_the_rules = partition(
        lambda rule: rule.mode == JOIN_MODE,
        filtered_rules,
    )
    dependency_aware_rules = [
        r for r in rest_of_the_rules if r.project_depends_on is not None
    ]
    filtered_rules = rest_of_the_rules

    (
        rule_matches_by_rule,
        semgrep_errors,
        all_targets,
        profiling_data,
    ) = core_runner.invoke_semgrep(
        target_manager, filtered_rules, dump_command_for_core, deep
    )

    if join_rules:
        import semgrep.join_rule as join_rule

        for rule in join_rules:
            join_rule_matches, join_rule_errors = join_rule.run_join_rule(
                rule.raw, [Path(t) for t in target_manager.targets]
            )
            join_rule_matches_by_rule = {Rule.from_json(rule.raw): join_rule_matches}
            rule_matches_by_rule.update(join_rule_matches_by_rule)
            output_handler.handle_semgrep_errors(join_rule_errors)

    if len(dependency_aware_rules) > 0:
        import semgrep.dependency_aware_rule as dep_aware_rule

        for rule in dependency_aware_rules:
            (
                dep_rule_matches,
                dep_rule_errors,
            ) = dep_aware_rule.run_dependency_aware_rule(
                rule_matches_by_rule.get(rule, []),
                rule,
                [Path(t) for t in target_manager.targets],
            )
            rule_matches_by_rule[rule] = dep_rule_matches
            output_handler.handle_semgrep_errors(dep_rule_errors)

    return (
        rule_matches_by_rule,
        semgrep_errors,
        all_targets,
        profiling_data,
    )


def remove_matches_in_baseline(
    head_matches_by_rule: RuleMatchMap, baseline_matches_by_rule: RuleMatchMap
) -> RuleMatchMap:
    """
    Remove the matches in head_matches_by_rule that also occur in baseline_matches_by_rule
    """
    logger.verbose("Removing matches that exist in baseline scan")
    kept_matches_by_rule: RuleMatchMap = {}

    num_removed = 0

    for rule in head_matches_by_rule:
        kept_matches = []

        head_matches = head_matches_by_rule[rule]

        # Copy so we can destructively modify
        baseline_matches = list(baseline_matches_by_rule.get(rule, []))

        # Note we cannot convert to sets and do set subtraction because
        # the way we consider equality in head vs baseline cannot be used to
        # assert that two matches in head are equal (finding can appear in head
        # more than once with the same syntatic id). We also cannot simply
        # remove elements in head_matches that appear in baseline_matches
        # because the above non-uniqueness of id means we need to remove
        # a match 1:1 (i.e. if match with id X appears 3 times in head but
        # 2 times in baseline) this function needs to return an object with one
        # match with id X.
        for head_match in head_matches:
            for idx in range(len(baseline_matches)):
                if head_match.is_baseline_equivalent(baseline_matches[idx]):
                    baseline_matches.pop(idx)
                    num_removed += 1
                    break
            else:
                kept_matches.append(head_match)

        kept_matches_by_rule[rule] = kept_matches

    logger.verbose(f"Removed {num_removed} matches that were in baseline scan")
    return kept_matches_by_rule


def main(
    *,
    dump_command_for_core: bool = False,
    deep: bool = False,
    output_handler: OutputHandler,
    target: Sequence[str],
    pattern: Optional[str],
    lang: Optional[str],
    configs: Sequence[str],
    no_rewrite_rule_ids: bool = False,
    jobs: int = 1,
    include: Optional[Sequence[str]] = None,
    exclude: Optional[Sequence[str]] = None,
    strict: bool = False,
    autofix: bool = False,
    replacement: Optional[str] = None,
    dryrun: bool = False,
    disable_nosem: bool = False,
    no_git_ignore: bool = False,
    timeout: int = DEFAULT_TIMEOUT,
    max_memory: int = 0,
    max_target_bytes: int = 0,
    timeout_threshold: int = 0,
    skip_unknown_extensions: bool = False,
    severity: Optional[Sequence[str]] = None,
    optimizations: str = "none",
    baseline_commit: Optional[str] = None,
) -> Tuple[
    RuleMatchMap,
    Set[Path],
    IgnoreLog,
    List[Rule],
    ProfileManager,
    ProfilingData,
    Collection[RuleSeverity],
]:
    if include is None:
        include = []

    if exclude is None:
        exclude = []

    project_url = get_project_url()
    profiler = ProfileManager()

    rule_start_time = time.time()
    configs_obj, config_errors = get_config(
        pattern, lang, configs, replacement=replacement, project_url=project_url
    )
    all_rules = configs_obj.get_rules(no_rewrite_rule_ids)
    profiler.save("config_time", rule_start_time)

    if not severity:
        shown_severities = DEFAULT_SHOWN_SEVERITIES
        filtered_rules = all_rules
    else:
        shown_severities = {RuleSeverity(s) for s in severity}
        filtered_rules = [rule for rule in all_rules if rule.severity.value in severity]

    output_handler.handle_semgrep_errors(config_errors)

    if config_errors and strict:
        raise SemgrepError(
            f"run with --strict and there were {len(config_errors)} errors loading configs",
            code=MISSING_CONFIG_EXIT_CODE,
        )

    if not pattern:
        plural = "s" if len(configs_obj.valid) > 1 else ""
        config_id_if_single = (
            list(configs_obj.valid.keys())[0] if len(configs_obj.valid) == 1 else ""
        )
        invalid_msg = (
            f"({len(config_errors)} config files were invalid)"
            if len(config_errors)
            else ""
        )
        logger.verbose(
            f"running {len(filtered_rules)} rules from {len(configs_obj.valid)} config{plural} {config_id_if_single} {invalid_msg}".strip()
        )
        if len(config_errors) > 0:
            raise SemgrepError(
                f"invalid configuration file found ({len(config_errors)} configs were invalid)",
                code=MISSING_CONFIG_EXIT_CODE,
            )
        if len(configs_obj.valid) == 0:
            raise SemgrepError(
                """No config given. Run with `--config auto` or see https://semgrep.dev/docs/running-rules/ for instructions on running with a specific config
""",
                code=MISSING_CONFIG_EXIT_CODE,
            )

        notify_user_of_work(filtered_rules, include, exclude)

    respect_git_ignore = not no_git_ignore
    try:
        target_manager = TargetManager(
            includes=include,
            excludes=exclude,
            max_target_bytes=max_target_bytes,
            targets=target,
            respect_git_ignore=respect_git_ignore,
            skip_unknown_extensions=skip_unknown_extensions,
            file_ignore=get_file_ignore(),
        )
    except FilesNotFoundError as e:
        raise SemgrepError(e)

    # Initialize baseline before running rules to fail early on bad args
    baseline_handler = None
    if baseline_commit:
        try:
            baseline_handler = BaselineHandler(baseline_commit)
        # TODO better handling
        except Exception as e:
            raise SemgrepError(e)

    core_start_time = time.time()
    core_runner = CoreRunner(
        jobs=jobs,
        timeout=timeout,
        max_memory=max_memory,
        timeout_threshold=timeout_threshold,
        optimizations=optimizations,
    )

    (rule_matches_by_rule, semgrep_errors, all_targets, profiling_data,) = run_rules(
        filtered_rules,
        target_manager,
        core_runner,
        output_handler,
        dump_command_for_core,
        deep,
    )
    profiler.save("core_time", core_start_time)
    output_handler.handle_semgrep_errors(semgrep_errors)

    # Run baseline if needed
    if baseline_handler:
        logger.info(f"Running baseline scan with base set to: {baseline_commit}")
        try:
            with baseline_handler.baseline_context():
                # Need to reinstantiate target_manager since
                # filesystem has changed
                try:
                    baseline_target_manager = TargetManager(
                        includes=include,
                        excludes=exclude,
                        max_target_bytes=max_target_bytes,
                        targets=target,
                        respect_git_ignore=respect_git_ignore,
                        skip_unknown_extensions=skip_unknown_extensions,
                        file_ignore=get_file_ignore(),
                    )
                except FilesNotFoundError as e:
                    # This means a file existed in head but not
                    # in baseline context which is fine
                    logger.debug(f"File not found in baseline: {e}")

                (
                    baseline_rule_matches_by_rule,
                    baseline_semgrep_errors,
                    baseline_targets,
                    baseline_profiling_data,
                ) = run_rules(
                    filtered_rules,
                    baseline_target_manager,
                    core_runner,
                    output_handler,
                    dump_command_for_core,
                    deep,
                )
                rule_matches_by_rule = remove_matches_in_baseline(
                    rule_matches_by_rule, baseline_rule_matches_by_rule
                )
                output_handler.handle_semgrep_errors(baseline_semgrep_errors)
        except Exception as e:
            raise SemgrepError(e)

    ignores_start_time = time.time()
    keep_ignored = disable_nosem or output_handler.formatter.keep_ignores()
    filtered_matches_by_rule, nosem_errors, num_ignored_by_nosem = process_ignores(
        rule_matches_by_rule,
        keep_ignored,
        strict=strict,
    )
    profiler.save("ignores_time", ignores_start_time)
    output_handler.handle_semgrep_errors(nosem_errors)

    num_findings = sum(len(v) for v in filtered_matches_by_rule.values())
    profiler.save("total_time", rule_start_time)
    if metric_manager.is_enabled():
        error_types = list(e.semgrep_error_type() for e in semgrep_errors)

        metric_manager.set_project_hash(project_url)
        metric_manager.set_configs_hash(configs)
        metric_manager.set_rules_hash(filtered_rules)
        metric_manager.set_num_rules(len(filtered_rules))
        metric_manager.set_num_targets(len(all_targets))
        metric_manager.set_num_findings(num_findings)
        metric_manager.set_num_ignored(num_ignored_by_nosem)
        metric_manager.set_profiling_times(profiler.dump_stats())
        total_bytes_scanned = sum(t.stat().st_size for t in all_targets)
        metric_manager.set_total_bytes_scanned(total_bytes_scanned)
        metric_manager.set_errors(error_types)
        metric_manager.set_rules_with_findings(filtered_matches_by_rule)
        metric_manager.set_run_timings(
            profiling_data, list(all_targets), filtered_rules
        )

    if autofix:
        apply_fixes(filtered_matches_by_rule, dryrun)

    return (
        filtered_matches_by_rule,
        all_targets,
        target_manager.ignore_log,
        filtered_rules,
        profiler,
        profiling_data,
        shown_severities,
    )
