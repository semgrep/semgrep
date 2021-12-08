import json
import time
from io import StringIO
from pathlib import Path
from typing import Any
from typing import Dict
from typing import List
from typing import Optional
from typing import Sequence
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
from semgrep.ignores import FileIgnore
from semgrep.ignores import IGNORE_FILE_NAME
from semgrep.ignores import Parser
from semgrep.metric_manager import metric_manager
from semgrep.nosemgrep import process_ignores
from semgrep.output import DEFAULT_SHOWN_SEVERITIES
from semgrep.output import OutputHandler
from semgrep.output import OutputSettings
from semgrep.profile_manager import ProfileManager
from semgrep.project import get_project_url
from semgrep.rule import Rule
from semgrep.semgrep_types import JOIN_MODE
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
    for rule in filtered_rules:
        logger.verbose(f"- {rule.id}")


# Currently we do not search for a default .semgrepignore if none is provided
# This is different from the behavior of semgrep-agent, where a default is used
def get_file_ignore() -> Optional[FileIgnore]:
    workdir = Path.cwd()
    semgrepignore_path = Path(workdir / IGNORE_FILE_NAME)
    if semgrepignore_path.is_file():
        logger.verbose("using path ignore rules from .semgrepignore")
        with open(semgrepignore_path) as f:
            file_ignore: Optional[FileIgnore] = FileIgnore(
                base_path=workdir,
                patterns=Parser(workdir).parse(f),
            )
    else:
        logger.verbose("no .semgrepignore found")
        file_ignore = None

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
    output_handler = OutputHandler(output_settings, stdout=io_capture)
    main(
        output_handler=output_handler,
        target=[str(t) for t in targets],
        pattern="",
        lang="",
        configs=[str(config)],
        **kwargs,
    )
    output_handler.close()

    result: Union[Dict[str, Any], str] = (
        json.loads(io_capture.getvalue())
        if output_settings.output_format.is_json()
        else io_capture.getvalue()
    )

    return result


def main(
    *,
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
) -> None:
    if include is None:
        include = []

    if exclude is None:
        exclude = []

    project_url = get_project_url()

    profiler = ProfileManager()

    rule_start_time = time.time()
    configs_obj, errors = get_config(
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

    output_handler.handle_semgrep_errors(errors)

    if errors and strict:
        raise SemgrepError(
            f"run with --strict and there were {len(errors)} errors loading configs",
            code=MISSING_CONFIG_EXIT_CODE,
        )

    if not pattern:
        plural = "s" if len(configs_obj.valid) > 1 else ""
        config_id_if_single = (
            list(configs_obj.valid.keys())[0] if len(configs_obj.valid) == 1 else ""
        )
        invalid_msg = (
            f"({len(errors)} config files were invalid)" if len(errors) else ""
        )
        logger.verbose(
            f"running {len(filtered_rules)} rules from {len(configs_obj.valid)} config{plural} {config_id_if_single} {invalid_msg}".strip()
        )
        if len(errors) > 0:
            raise SemgrepError(
                f"invalid configuration file found ({len(errors)} configs were invalid)",
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
        output_handler.handle_semgrep_error(e)

    join_rules, rest_of_the_rules = partition(
        lambda rule: rule.mode == JOIN_MODE,
        filtered_rules,
    )
    filtered_rules = rest_of_the_rules

    core_start_time = time.time()
    # actually invoke semgrep
    (rule_matches_by_rule, semgrep_errors, all_targets, profiling_data,) = CoreRunner(
        jobs=jobs,
        timeout=timeout,
        max_memory=max_memory,
        timeout_threshold=timeout_threshold,
        optimizations=optimizations,
    ).invoke_semgrep(target_manager, profiler, filtered_rules)

    if join_rules:
        import semgrep.join_rule as join_rule

        for rule in join_rules:
            join_rule_matches, join_rule_errors = join_rule.run_join_rule(
                rule.raw, [Path(t) for t in target_manager.targets]
            )
            join_rule_matches_by_rule = {Rule.from_json(rule.raw): join_rule_matches}
            rule_matches_by_rule.update(join_rule_matches_by_rule)
            output_handler.handle_semgrep_errors(join_rule_errors)

    profiler.save("core_time", core_start_time)

    ignores_start_time = time.time()
    keep_ignored = disable_nosem or output_handler.formatter.keep_ignores()
    filtered_matches_by_rule, nosem_errors, num_ignored_by_nosem = process_ignores(
        rule_matches_by_rule,
        keep_ignored,
        strict=strict,
    )
    profiler.save("ignores_time", ignores_start_time)

    output_handler.handle_semgrep_errors(semgrep_errors)
    output_handler.handle_semgrep_errors(nosem_errors)

    num_findings = sum(len(v) for v in filtered_matches_by_rule.values())
    stats_line = f"ran {len(filtered_rules)} rules on {len(all_targets)} files: {num_findings} findings"
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

    output_handler.handle_semgrep_core_output(
        filtered_matches_by_rule,
        stats_line=stats_line,
        all_targets=all_targets,
        profiler=profiler,
        filtered_rules=filtered_rules,
        profiling_data=profiling_data,
        severities=shown_severities,
    )

    if autofix:
        apply_fixes(filtered_matches_by_rule, dryrun)
