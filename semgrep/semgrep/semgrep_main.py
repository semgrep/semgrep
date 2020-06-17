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
from semgrep.constants import RULES_KEY
from semgrep.core_runner import CoreRunner
from semgrep.error import ErrorWithSpan
from semgrep.error import INVALID_CODE_EXIT_CODE
from semgrep.error import InvalidPatternNameError
from semgrep.error import MISSING_CONFIG_EXIT_CODE
from semgrep.error import SemgrepError
from semgrep.output import OutputHandler
from semgrep.rule import Rule
from semgrep.rule_lang import YamlMap
from semgrep.rule_lang import YamlTree
from semgrep.rule_match import RuleMatch
from semgrep.semgrep_types import YAML_ALL_VALID_RULE_KEYS
from semgrep.semgrep_types import YAML_MUST_HAVE_KEYS
from semgrep.target_manager import TargetManager
from semgrep.util import debug_print
from semgrep.util import print_error
from semgrep.util import print_msg

MISSING_RULE_ID = "no-rule-id"


def validate_single_rule(
    config_id: str, rule_yaml: YamlTree[YamlMap], output_handler: OutputHandler,
) -> Optional[Rule]:
    """
        Validate that a rule dictionary contains all necessary keys
        and can be correctly parsed
    """
    rule: YamlMap = rule_yaml.value

    rule_keys = set({k.value for k in rule.keys()})
    if not rule_keys.issuperset(YAML_MUST_HAVE_KEYS):
        missing_keys = YAML_MUST_HAVE_KEYS - rule_keys
        # TODO(https://github.com/returntocorp/semgrep/issues/746): return the error messages instead of
        #  immediately printing them so we can emit nice JSON errors to semgrep.live
        output_handler.handle_semgrep_rule_errors(
            ErrorWithSpan(
                short_msg="missing keys",
                long_msg=f"{config_id} is missing required keys {missing_keys}",
                level="error",
                spans=[rule_yaml.span.truncate(lines=5)],
            )
        )
        return None
    if not rule_keys.issubset(YAML_ALL_VALID_RULE_KEYS):
        extra_keys: Set[str] = rule_keys - YAML_ALL_VALID_RULE_KEYS
        extra_key_spans = sorted([rule.key_tree(k) for k in extra_keys])

        output_handler.handle_semgrep_rule_errors(
            ErrorWithSpan(
                short_msg="extra top-level key",
                long_msg=f"{config_id} has an invalid top-level rule key: {sorted([k for k in extra_keys])}",
                help=f"Only {sorted(YAML_ALL_VALID_RULE_KEYS)} are valid keys",
                spans=[k.span.with_context(before=2, after=2) for k in extra_key_spans],
                level="error",
            )
        )
        return None
    try:
        return Rule.from_yamltree(rule_yaml)
    except ErrorWithSpan as ex:
        output_handler.handle_semgrep_rule_errors(ex)
        return None
    except InvalidPatternNameError as ex:
        rule_id = rule.get("id")
        if not rule_id:
            rule_id_str = MISSING_RULE_ID
        else:
            rule_id_str = rule_id.value
        rule_id_err_msg = f"{rule_id_str}"
        output_handler.handle_semgrep_rule_errors(
            SemgrepError(
                f"{config_id}: inside rule id {rule_id_err_msg}, pattern fields can't look like this: {ex}"
            )
        )

        return None


def validate_configs(
    configs: Dict[str, Optional[YamlTree]], output_handler: OutputHandler
) -> Tuple[Dict[str, List[Rule]], Dict[str, Any]]:
    """
        Take configs and separate into valid and invalid ones
    """
    errors: Dict[str, Any] = {}
    valid: Dict[str, Any] = {}
    for config_id, config_yaml_tree in configs.items():
        if not config_yaml_tree:
            errors[config_id] = config_yaml_tree
            continue
        config = config_yaml_tree.value
        if not isinstance(config, YamlMap):
            print_error(f"{config_id} was not a mapping")
            errors[config_id] = config_yaml_tree.unroll()
            continue
        rules = config.get(RULES_KEY)
        if rules is None:
            output_handler.handle_semgrep_rule_errors(
                ErrorWithSpan(
                    short_msg="missing keys",
                    long_msg=f"{config_id} is missing `{RULES_KEY}` as top-level key",
                    level="error",
                    spans=[config_yaml_tree.span.truncate(lines=5)],
                )
            )
            errors[config_id] = config_yaml_tree.unroll()
            continue
        valid_rules = []
        invalid_rules = []
        for rule_dict in rules.value:
            rule = validate_single_rule(
                config_id, rule_dict, output_handler=output_handler
            )
            if rule:
                valid_rules.append(rule)
            else:
                invalid_rules.append(rule_dict)

        if invalid_rules:
            errors[config_id] = {
                **config_yaml_tree.unroll_dict(),
                "rules": invalid_rules,
            }
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
    pattern: str, lang: str, config: str, output_handler: OutputHandler
) -> Tuple[Dict[str, List[Rule]], Dict[str, Any]]:
    # let's check for a pattern
    if pattern:
        # and a language
        if not lang:
            raise SemgrepError("language must be specified when a pattern is passed")

        # TODO for now we generate a manual config. Might want to just call semgrep -e ... -l ...
        configs = semgrep.config_resolver.manual_config(pattern, lang)
    else:
        # else let's get a config. A config is a dict from config_id -> config. Config Id is not well defined at this point.
        configs = semgrep.config_resolver.resolve_config(config)

    # if we can't find a config, use default r2c rules
    if not configs:
        raise SemgrepError(
            f"No config given and {DEFAULT_CONFIG_FILE} was not found. Try running with --help to debug or if you want to download a default config, try running with --config r2c"
        )

    # let's split our configs into valid and invalid configs.
    # It's possible that a config_id exists in both because we check valid rules and invalid rules
    # instead of just hard failing for that config if mal-formed
    valid_configs, invalid_configs = validate_configs(
        configs, output_handler=output_handler
    )
    return valid_configs, invalid_configs


def flatten_configs(transformed_configs: Dict[str, List[Rule]]) -> List[Rule]:
    return [rule for rules in transformed_configs.values() for rule in rules]


def notify_user_of_work(
    all_rules: List[Rule],
    include: List[str],
    include_dir: List[str],
    exclude: List[str],
    exclude_dir: List[str],
    verbose: bool = False,
) -> None:
    """
    Notify user of what semgrep is about to do, including:
    - number of rules
    - which rules? <- not yet, too cluttered
    - which dirs are excluded, etc.
    """
    if include:
        print_msg(f"including files:")
        for inc in include:
            print_msg(f"- {inc}")
    if include_dir:
        print_msg(f"including directories:")
        for inc in include_dir:
            print_msg(f"- {inc}")
    if exclude:
        print_msg(f"excluding files:")
        for exc in exclude:
            print_msg(f"- {exc}")
    if exclude_dir:
        print_msg(f"excluding directories:")
        for exc in exclude_dir:
            print_msg(f"- {exc}")
    print_msg(f"running {len(all_rules)} rules...")
    if verbose:
        print_msg("rules:")
        for rule in all_rules:
            print_msg(f"- {rule.id}")


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
        debug_print(
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
            debug_print(
                f"found 'nosem' comment with id '{pattern_id}', skipping rule '{rule_match.id}' on line {rule_match.start['line']}"
            )
            result = result or True
        else:
            message = f"found 'nosem' comment with id '{pattern_id}', but no corresponding rule trying '{rule_match.id}'"
            if strict:
                raise SemgrepError(message)
            else:
                debug_print(message)

    return result


def main(
    output_handler: OutputHandler,
    target: List[str],
    pattern: str,
    lang: str,
    config: str,
    no_rewrite_rule_ids: bool,
    jobs: int,
    include: List[str],
    include_dir: List[str],
    exclude: List[str],
    exclude_dir: List[str],
    strict: bool,
    autofix: bool,
    disable_nosem: bool,
    dangerously_allow_arbitrary_code_execution_from_rules: bool,
    no_git_ignore: bool,
) -> None:
    include.extend(include_dir)
    exclude.extend(exclude_dir)

    valid_configs, invalid_configs = get_config(pattern, lang, config, output_handler)

    if invalid_configs and strict:
        raise SemgrepError(
            f"run with --strict and there were {len(invalid_configs)} errors loading configs",
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
            f"({len(invalid_configs)} config files were invalid)"
            if len(invalid_configs)
            else ""
        )
        debug_print(
            f"running {len(all_rules)} rules from {len(valid_configs)} config{plural} {config_id_if_single} {invalid_msg}"
        )

        notify_user_of_work(all_rules, include, include_dir, exclude, exclude_dir)

        if len(valid_configs) == 0:
            raise SemgrepError(
                f"no valid configuration file found ({len(invalid_configs)} configs were invalid)",
                code=MISSING_CONFIG_EXIT_CODE,
            )

    respect_git_ignore = not no_git_ignore
    target_manager = TargetManager(
        includes=include,
        excludes=exclude,
        targets=target,
        respect_git_ignore=respect_git_ignore,
    )

    # actually invoke semgrep
    rule_matches_by_rule, debug_steps_by_rule, semgrep_errors = CoreRunner(
        allow_exec=dangerously_allow_arbitrary_code_execution_from_rules, jobs=jobs,
    ).invoke_semgrep(target_manager, all_rules)

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
    output_handler.handle_semgrep_core_errors(semgrep_errors)

    if len(semgrep_errors):
        if strict:
            raise SemgrepError(
                f"run with --strict and {len(semgrep_errors)} errors occurred during semgrep run; exiting",
                code=INVALID_CODE_EXIT_CODE,
            )
        else:
            print_error(
                "Run with --strict to exit with non-zero exit code when errors exist"
            )

    if autofix:
        apply_fixes(rule_matches_by_rule)
