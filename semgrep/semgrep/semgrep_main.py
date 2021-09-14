import json
import subprocess
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
from semgrep.core_runner import CoreRunner
from semgrep.error import MISSING_CONFIG_EXIT_CODE
from semgrep.error import SemgrepError
from semgrep.ignores import process_ignores
from semgrep.metric_manager import metric_manager
from semgrep.output import OutputHandler
from semgrep.output import OutputSettings
from semgrep.profile_manager import ProfileManager
from semgrep.rule import Rule
from semgrep.semgrep_types import JOIN_MODE
from semgrep.target_manager import TargetManager
from semgrep.util import manually_search_file
from semgrep.util import partition
from semgrep.util import sub_check_output
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
    logger.info(f"running {len(filtered_rules)} rules...")
    logger.verbose("rules:")
    for rule in filtered_rules:
        logger.verbose(f"- {rule.id}")


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

    configs_obj, errors = get_config(pattern, lang, configs, replacement)
    all_rules = configs_obj.get_rules(no_rewrite_rule_ids)

    if not severity:
        filtered_rules = all_rules
    else:
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

        if len(configs_obj.valid) == 0:
            if len(errors) > 0:
                raise SemgrepError(
                    f"no valid configuration file found ({len(errors)} configs were invalid)",
                    code=MISSING_CONFIG_EXIT_CODE,
                )
            else:
                raise SemgrepError(
                    """You need to specify a config with --config=<semgrep.dev config name|localfile|localdirectory|url>.
If you're looking for a config to start with, there are thousands at: https://semgrep.dev
The two most popular are:
    --config=p/ci # find logic bugs, and high-confidence security vulnerabilities; recommended for CI
    --config=p/security-audit # find security audit points; noisy, not recommended for CI
""",
                    code=MISSING_CONFIG_EXIT_CODE,
                )

        notify_user_of_work(filtered_rules, include, exclude)

    respect_git_ignore = not no_git_ignore
    target_manager = TargetManager(
        includes=include,
        excludes=exclude,
        max_target_bytes=max_target_bytes,
        targets=target,
        respect_git_ignore=respect_git_ignore,
        output_handler=output_handler,
        skip_unknown_extensions=skip_unknown_extensions,
    )

    profiler = ProfileManager()

    join_rules, rest_of_the_rules = partition(
        lambda rule: rule.mode == JOIN_MODE,
        filtered_rules,
    )
    filtered_rules = rest_of_the_rules

    start_time = time.time()
    # actually invoke semgrep
    (
        rule_matches_by_rule,
        debug_steps_by_rule,
        semgrep_errors,
        all_targets,
        profiling_data,
    ) = CoreRunner(
        jobs=jobs,
        timeout=timeout,
        max_memory=max_memory,
        timeout_threshold=timeout_threshold,
        optimizations=optimizations,
    ).invoke_semgrep(
        target_manager, profiler, filtered_rules
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

    profiler.save("total_time", start_time)

    filtered_matches = process_ignores(
        rule_matches_by_rule, output_handler, strict=strict, disable_nosem=disable_nosem
    )

    output_handler.handle_semgrep_errors(semgrep_errors)
    output_handler.handle_semgrep_errors(filtered_matches.errors)

    num_findings = sum(len(v) for v in filtered_matches.matches.values())
    stats_line = f"ran {len(filtered_rules)} rules on {len(all_targets)} files: {num_findings} findings"

    if metric_manager.is_enabled:
        project_url = None
        try:
            project_url = sub_check_output(
                ["git", "ls-remote", "--get-url"],
                encoding="utf-8",
                stderr=subprocess.DEVNULL,
            )
        except Exception as e:
            logger.debug(f"Failed to get project url from 'git ls-remote': {e}")
            try:
                # add \n to match urls from git ls-remote (backwards compatability)
                project_url = manually_search_file(".git/config", ".com", "\n")
            except Exception as e:
                logger.debug(f"Failed to get project url from .git/config: {e}")

        metric_manager.set_project_hash(project_url)
        metric_manager.set_configs_hash(configs)
        metric_manager.set_rules_hash(filtered_rules)
        metric_manager.set_num_rules(len(filtered_rules))
        metric_manager.set_num_targets(len(all_targets))
        metric_manager.set_num_findings(num_findings)
        metric_manager.set_num_ignored(filtered_matches.num_matches)
        metric_manager.set_run_time(profiler.calls["total_time"][0])
        total_bytes_scanned = sum(t.stat().st_size for t in all_targets)
        metric_manager.set_total_bytes_scanned(total_bytes_scanned)
        metric_manager.set_errors(list(type(e).__name__ for e in semgrep_errors))
        metric_manager.set_run_timings(
            profiling_data, list(all_targets), filtered_rules
        )

    output_handler.handle_semgrep_core_output(
        filtered_matches.matches,
        debug_steps_by_rule,
        stats_line,
        all_targets,
        profiler,
        filtered_rules,
        profiling_data,
    )

    if autofix:
        apply_fixes(filtered_matches.matches, dryrun)
