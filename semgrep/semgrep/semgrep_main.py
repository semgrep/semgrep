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
from semgrep.semgrep_types import YAML_ALL_VALID_RULE_KEYS
from semgrep.semgrep_types import YAML_MUST_HAVE_KEYS
from semgrep.target_manager import TargetManager

logger = logging.getLogger(__name__)

MISSING_RULE_ID = "no-rule-id"


def validate_single_rule(
    config_id: str, rule_yaml: YamlTree[YamlMap]
) -> Optional[Rule]:
    """
        Validate that a rule dictionary contains all necessary keys
        and can be correctly parsed.

        Returns Rule object if valid otherwise raises InvalidRuleSchemaError
    """
    rule: YamlMap = rule_yaml.value

    rule_keys = set({k.value for k in rule.keys()})
    extra_keys = rule_keys - YAML_ALL_VALID_RULE_KEYS
    extra_key_spans = sorted([rule.key_tree(k) for k in extra_keys])
    missing_keys = YAML_MUST_HAVE_KEYS - rule_keys

    if missing_keys and extra_keys:
        help_msg = f"Unexpected keys {extra_keys} found. Is one of these a typo of {missing_keys}?"
        raise InvalidRuleSchemaError(
            short_msg="incorrect keys",
            long_msg=f"{config_id} is missing required keys {missing_keys}",
            spans=[rule_yaml.span.truncate(lines=5)]
            + [e.span for e in extra_key_spans],
            help=help_msg,
        )
    elif missing_keys:
        help_msg = f"Add {missing_keys} to your config file."
        raise InvalidRuleSchemaError(
            short_msg="missing keys",
            long_msg=f"{config_id} is missing required keys {missing_keys}",
            spans=[rule_yaml.span.truncate(lines=5)]
            + [e.span for e in extra_key_spans],
            help=help_msg,
        )
    elif extra_keys:
        help_msg = f"Unexpected keys {extra_keys} found. Were you looking for any of these unused, valid keys?\n {sorted(YAML_ALL_VALID_RULE_KEYS - rule_keys)}"
        raise InvalidRuleSchemaError(
            short_msg="invalid keys",
            long_msg=f"{config_id} has extra, un-interpretable keys: {extra_keys}",
            spans=[e.span for e in extra_key_spans],
            help=help_msg,
        )
    # Defaults to search mode if mode is not specified
    return Rule.from_yamltree(rule_yaml)


def validate_configs(
    configs: Dict[str, YamlTree],
) -> Tuple[Dict[str, List[Rule]], List[SemgrepError]]:
    """
        Take configs and separate into valid and list of errors parsing the invalid ones
    """
    errors: List[SemgrepError] = []
    valid: Dict[str, Any] = {}
    for config_id, config_yaml_tree in configs.items():
        config = config_yaml_tree.value
        if not isinstance(config, YamlMap):
            errors.append(SemgrepError(f"{config_id} was not a mapping"))
            continue

        rules = config.get(RULES_KEY)
        if rules is None:
            errors.append(
                InvalidRuleSchemaError(
                    short_msg="missing keys",
                    long_msg=f"{config_id} is missing `{RULES_KEY}` as top-level key",
                    spans=[config_yaml_tree.span.truncate(lines=5)],
                )
            )
            continue
        valid_rules = []
        for rule_dict in rules.value:

            try:
                rule = validate_single_rule(config_id, rule_dict)
            except InvalidRuleSchemaError as ex:
                errors.append(ex)
            else:
                valid_rules.append(rule)

        if valid_rules:
            valid[config_id] = valid_rules
    return valid, errors


def safe_relative_to(a: Path, b: Path) -> Path:
    try:
        return a.relative_to(b)
    except ValueError:
        # paths had no common prefix; not possible to relativize
        return a


def convert_config_id_to_prefix(config_id: str) -> str:
    at_path = Path(config_id)
    at_path = safe_relative_to(at_path, Path.cwd())

    prefix = ".".join(at_path.parts[:-1]).lstrip("./").lstrip(".")
    if len(prefix):
        prefix += "."
    return prefix


def rename_rule_ids(valid_configs: Dict[str, List[Rule]]) -> Dict[str, List[Rule]]:
    transformed = {}
    for config_id, rules in valid_configs.items():
        transformed[config_id] = [
            rule.with_id(
                f"{convert_config_id_to_prefix(config_id)}{rule.id or MISSING_RULE_ID}"
            )
            for rule in rules
        ]
    return transformed


def get_config(
    pattern: str, lang: str, config: str
) -> Tuple[Dict[str, List[Rule]], List[SemgrepError]]:
    # let's check for a pattern
    if pattern:
        # and a language
        if not lang:
            raise SemgrepError("language must be specified when a pattern is passed")

        # TODO for now we generate a manual config. Might want to just call semgrep -e ... -l ...
        configs = semgrep.config_resolver.manual_config(pattern, lang)
    else:
        # else let's get a config. A config is a dict from config_id -> config. Config Id is not well defined at this point.
        try:
            configs = semgrep.config_resolver.resolve_config(config)
        except SemgrepError as e:
            return {}, [e]

    # if we can't find a config, use default r2c rules
    if not configs:
        raise SemgrepError(
            f"No config given and {DEFAULT_CONFIG_FILE} was not found. Try running with --help to debug or if you want to download a default config, try running with --config r2c"
        )

    valid_configs, error = validate_configs(configs)
    return valid_configs, error


def flatten_configs(transformed_configs: Dict[str, List[Rule]]) -> List[Rule]:
    return [rule for rules in transformed_configs.values() for rule in rules]


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
            strict=False,
        ),
        stdout=io_capture,
    )
    main(
        output_handler=output_handler,
        target=[str(t) for t in targets],
        pattern="",
        lang="",
        config=str(config),
        **kwargs,
    )
    output_handler.close()
    return json.loads(io_capture.getvalue())


def main(
    output_handler: OutputHandler,
    target: List[str],
    pattern: str,
    lang: str,
    config: str,
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
) -> None:
    if include is None:
        include = []

    if exclude is None:
        exclude = []

    valid_configs, config_errors = get_config(pattern, lang, config)

    output_handler.handle_semgrep_errors(config_errors)

    if config_errors and strict:
        raise SemgrepError(
            f"run with --strict and there were {len(config_errors)} errors loading configs",
            code=MISSING_CONFIG_EXIT_CODE,
        )

    if not no_rewrite_rule_ids:
        # re-write the configs to have the hierarchical rule ids
        valid_configs = rename_rule_ids(valid_configs)

    # extract just the rules from valid configs
    all_rules = flatten_configs(valid_configs)

    if not pattern:
        plural = "s" if len(valid_configs) > 1 else ""
        config_id_if_single = (
            list(valid_configs.keys())[0] if len(valid_configs) == 1 else ""
        )
        invalid_msg = (
            f"({len(config_errors)} config files were invalid)"
            if len(config_errors)
            else ""
        )
        logger.debug(
            f"running {len(all_rules)} rules from {len(valid_configs)} config{plural} {config_id_if_single} {invalid_msg}"
        )

        notify_user_of_work(all_rules, include, exclude)

        if len(valid_configs) == 0:
            raise SemgrepError(
                f"no valid configuration file found ({len(config_errors)} configs were invalid)",
                code=MISSING_CONFIG_EXIT_CODE,
            )

    respect_git_ignore = not no_git_ignore
    target_manager = TargetManager(
        includes=include,
        excludes=exclude,
        targets=target,
        respect_git_ignore=respect_git_ignore,
        output_handler=output_handler,
    )

    # actually invoke semgrep
    rule_matches_by_rule, debug_steps_by_rule, semgrep_core_errors = CoreRunner(
        allow_exec=dangerously_allow_arbitrary_code_execution_from_rules, jobs=jobs,
    ).invoke_semgrep(target_manager, all_rules)

    semgrep_errors = [e.into_semgrep_error() for e in semgrep_core_errors]
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

    output_handler.handle_semgrep_core_output(rule_matches_by_rule, debug_steps_by_rule)

    if autofix:
        apply_fixes(rule_matches_by_rule, dryrun)
