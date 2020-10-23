import json
import logging
from io import StringIO
from pathlib import Path
from typing import Any
from typing import Dict
from typing import List
from typing import Optional
from typing import Set
from typing import Tuple

import semgrep.config_resolver
from semgrep.autofix import apply_fixes
from semgrep.constants import COMMA_SEPARATED_LIST_RE
from semgrep.constants import DEFAULT_CONFIG_FILE
from semgrep.constants import NOSEM_INLINE_RE
from semgrep.constants import OutputFormat
from semgrep.constants import RULES_KEY
from semgrep.core_runner import CoreRunner
from semgrep.error import InvalidRuleSchemaError
from semgrep.error import MISSING_CONFIG_EXIT_CODE
from semgrep.error import SemgrepError
from semgrep.output import OutputHandler
from semgrep.output import OutputSettings
from semgrep.rule import Rule
from semgrep.rule_lang import YamlMap
from semgrep.rule_lang import YamlTree
from semgrep.rule_match import RuleMatch
from semgrep.target_manager import TargetManager

logger = logging.getLogger(__name__)


def get_config(
    pattern: str, lang: str, config_strs: List[str]
) -> Tuple[semgrep.config_resolver.Config, List[SemgrepError]]:
    # let's check for a pattern
    if pattern:
        # and a language
        if not lang:
            raise SemgrepError("language must be specified when a pattern is passed")

        # TODO for now we generate a manual config. Might want to just call semgrep -e ... -l ...
        config, errors = semgrep.config_resolver.Config.from_pattern_lang(pattern, lang)
    else:
        # else let's get a config. A config is a dict from config_id -> config. Config Id is not well defined at this point.
        config, errors = semgrep.config_resolver.Config.from_config_list(config_strs)

    # if we can't find a config, use default r2c rules
    if not config:
        raise SemgrepError(
            f"No config given and {DEFAULT_CONFIG_FILE} was not found. Try running with --help to debug or if you want to download a default config, try running with --config r2c"
        )

    return config, errors


def notify_user_of_work(
    all_rules: List[Rule], include: List[str], exclude: List[str], verbose: bool = False
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
    logger.info(f"running {len(all_rules)} rules...")
    if verbose:
        logger.info("rules:")
        for rule in all_rules:
            logger.info(f"- {rule.id}")


def rule_match_nosem(rule_match: RuleMatch, strict: bool) -> bool:
    if not rule_match.lines:
        return False

    # Only consider the first line of a match. This will keep consistent
    # behavior on where we expect a 'nosem' comment to exist. If we allow these
    # comments on any line of a match it will get confusing as to what finding
    # the 'nosem' is referring to.
    re_match = NOSEM_INLINE_RE.search(rule_match.lines[0])
    if re_match is None:
        return False

    ids_str = re_match.groupdict()["ids"]
    if ids_str is None:
        logger.debug(
            f"found 'nosem' comment, skipping rule '{rule_match.id}' on line {rule_match.start['line']}"
        )
        return True

    pattern_ids = {
        pattern_id.strip()
        for pattern_id in COMMA_SEPARATED_LIST_RE.split(ids_str)
        if pattern_id.strip()
    }

    result = False
    for pattern_id in pattern_ids:
        if rule_match.id == pattern_id:
            logger.debug(
                f"found 'nosem' comment with id '{pattern_id}', skipping rule '{rule_match.id}' on line {rule_match.start['line']}"
            )
            result = result or True
        else:
            message = f"found 'nosem' comment with id '{pattern_id}', but no corresponding rule trying '{rule_match.id}'"
            if strict:
                raise SemgrepError(message)
            else:
                logger.debug(message)

    return result


def invoke_semgrep(config: Path, targets: List[Path], **kwargs: Any) -> Any:
    """
        Call semgrep with config on targets and return result as a json object

        Uses default arguments of MAIN unless overwritten with a kwarg
    """
    io_capture = StringIO()
    output_handler = OutputHandler(
        OutputSettings(
            output_format=OutputFormat.JSON,
            output_destination=None,
            error_on_findings=False,
            verbose_errors=False,
            strict=False,
        ),
        stdout=io_capture,
    )
    main(
        output_handler=output_handler,
        target=[str(t) for t in targets],
        pattern="",
        lang="",
        configs=[str(config)],
        **kwargs,
    )
    output_handler.close()
    return json.loads(io_capture.getvalue())


def main(
    output_handler: OutputHandler,
    target: List[str],
    pattern: str,
    lang: str,
    configs: List[str],
    no_rewrite_rule_ids: bool = False,
    jobs: int = 1,
    include: Optional[List[str]] = None,
    exclude: Optional[List[str]] = None,
    strict: bool = False,
    autofix: bool = False,
    dryrun: bool = False,
    disable_nosem: bool = False,
    dangerously_allow_arbitrary_code_execution_from_rules: bool = False,
    no_git_ignore: bool = False,
    timeout: int = 0,
    max_memory: int = 0,
    timeout_threshold: int = 0,
    skip_unknown_extensions: bool = False,
    testing: bool = False,
) -> None:
    if include is None:
        include = []

    if exclude is None:
        exclude = []

    configs_obj, errors = get_config(pattern, lang, configs)
    all_rules = configs_obj.get_rules(no_rewrite_rule_ids)

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
        logger.debug(
            f"running {len(all_rules)} rules from {len(configs_obj.valid)} config{plural} {config_id_if_single} {invalid_msg}"
        )

        if len(configs_obj.valid) == 0:
            raise SemgrepError(
                f"no valid configuration file found ({len(errors)} configs were invalid)",
                code=MISSING_CONFIG_EXIT_CODE,
            )

        notify_user_of_work(all_rules, include, exclude)

    respect_git_ignore = not no_git_ignore
    target_manager = TargetManager(
        includes=include,
        excludes=exclude,
        targets=target,
        respect_git_ignore=respect_git_ignore,
        output_handler=output_handler,
        skip_unknown_extensions=skip_unknown_extensions,
    )

    # actually invoke semgrep
    rule_matches_by_rule, debug_steps_by_rule, semgrep_errors, num_targets = CoreRunner(
        allow_exec=dangerously_allow_arbitrary_code_execution_from_rules,
        jobs=jobs,
        timeout=timeout,
        max_memory=max_memory,
        timeout_threshold=timeout_threshold,
        testing=testing,
    ).invoke_semgrep(target_manager, all_rules)

    output_handler.handle_semgrep_errors(semgrep_errors)

    if not disable_nosem:
        rule_matches_by_rule = {
            rule: [
                rule_match
                for rule_match in rule_matches
                if not rule_match_nosem(rule_match, strict)
            ]
            for rule, rule_matches in rule_matches_by_rule.items()
        }

    num_findings = sum(len(v) for v in rule_matches_by_rule.values())
    stats_line = (
        f"ran {len(all_rules)} rules on {num_targets} files: {num_findings} findings"
    )

    output_handler.handle_semgrep_core_output(
        rule_matches_by_rule, debug_steps_by_rule, stats_line
    )

    if autofix:
        apply_fixes(rule_matches_by_rule, dryrun)
