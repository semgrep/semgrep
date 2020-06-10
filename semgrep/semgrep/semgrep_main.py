import json
import sys
from pathlib import Path
from typing import Any
from typing import Dict
from typing import List
from typing import Optional
from typing import Tuple

import semgrep.config_resolver
from semgrep.autofix import apply_fixes
from semgrep.constants import DEFAULT_CONFIG_FILE
from semgrep.constants import OutputFormat
from semgrep.constants import RULES_KEY
from semgrep.core_runner import CoreRunner
from semgrep.error import FINDINGS_EXIT_CODE
from semgrep.error import INVALID_CODE_EXIT_CODE
from semgrep.error import InvalidPatternNameError
from semgrep.error import InvalidRuleSchemaError
from semgrep.error import MISSING_CONFIG_EXIT_CODE
from semgrep.error import SemgrepError
from semgrep.output import build_output
from semgrep.rule import Rule
from semgrep.rule_lang import YamlTree
from semgrep.rule_match import RuleMatch
from semgrep.semgrep_types import YAML_ALL_VALID_RULE_KEYS
from semgrep.semgrep_types import YAML_MUST_HAVE_KEYS
from semgrep.util import debug_print
from semgrep.util import is_url
from semgrep.util import print_error
from semgrep.util import print_msg

MISSING_RULE_ID = "no-rule-id"


def validate_single_rule(config_id: str, rule_yaml: YamlTree) -> Optional[Rule]:
    """
        Validate that a rule dictionary contains all necessary keys
        and can be correctly parsed
    """
    rule: Dict[str, Any] = rule_yaml.value  # type: ignore
    rule_id_err_msg = f'{rule.get("id", MISSING_RULE_ID).unroll()}'
    rule_keys = set(rule.keys())
    if not rule_keys.issuperset(YAML_MUST_HAVE_KEYS):
        missing_keys = YAML_MUST_HAVE_KEYS - rule_keys
        print_error(
            f"{config_id} is missing required keys {missing_keys} at rule id {rule_id_err_msg}"
        )
        return None
    if not rule_keys.issubset(YAML_ALL_VALID_RULE_KEYS):
        extra_keys = rule_keys - YAML_ALL_VALID_RULE_KEYS
        print_error(
            f"{config_id} has an invalid top-level rule key {extra_keys} at rule id {rule_id_err_msg}, can only have: {sorted(YAML_ALL_VALID_RULE_KEYS)}"
        )
        return None
    try:
        return Rule.from_yamltree(rule_yaml)
    except (InvalidPatternNameError, InvalidRuleSchemaError) as ex:
        print_error(
            f"{config_id}: inside rule id {rule_id_err_msg}, pattern fields can't look like this: {ex}"
        )
        return None


def validate_configs(
    configs: Dict[str, Optional[YamlTree]]
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
        if not isinstance(config, dict):
            print_error(f"{config_id} was not a mapping")
            errors[config_id] = config_yaml_tree.unroll()
            continue
        rules = config.get(RULES_KEY)  # type: ignore
        if rules is None:
            print_error(f"{config_id} is missing `{RULES_KEY}` as top-level key")
            errors[config_id] = config_yaml_tree.unroll()
            continue
        valid_rules = []
        invalid_rules = []
        for rule_dict in rules.value:
            rule = validate_single_rule(config_id, rule_dict)
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


### Handle output


def post_output(output_url: str, output: str) -> None:
    import requests  # here for faster startup times

    print_msg(f"posting to {output_url}...")
    try:
        r = requests.post(output_url, data=output, timeout=10)
        debug_print(f"posted to {output_url} and got status_code:{r.status_code}")
    except requests.exceptions.Timeout:
        raise SemgrepError(f"posting output to {output_url} timed out")


def save_output(destination: str, output: str) -> None:
    if is_url(destination):
        post_output(destination, output)
    else:
        if Path(destination).is_absolute():
            save_path = Path(destination)
        else:
            base_path = semgrep.config_resolver.get_base_path()
            save_path = base_path.joinpath(destination)
        # create the folders if not exists
        save_path.parent.mkdir(parents=True, exist_ok=True)
        with save_path.open(mode="w") as fout:
            fout.write(output)


def get_config(
    pattern: str, lang: str, config: str
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
    valid_configs, invalid_configs = validate_configs(configs)
    return valid_configs, invalid_configs


def flatten_configs(transformed_configs: Dict[str, List[Rule]]) -> List[Rule]:
    return [rule for rules in transformed_configs.values() for rule in rules]


def main(
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
    json_format: bool,
    debugging_json: bool,
    sarif: bool,
    output_destination: str,
    quiet: bool,
    strict: bool,
    exit_on_error: bool,
    autofix: bool,
    dangerously_allow_arbitrary_code_execution_from_rules: bool,
) -> str:
    # get the proper paths for targets i.e. handle base path of /home/repo when it exists in docker
    targets = semgrep.config_resolver.resolve_targets(target)
    valid_configs, invalid_configs = get_config(pattern, lang, config)

    output_format = OutputFormat.TEXT
    if json_format:
        output_format = OutputFormat.JSON
    elif debugging_json:
        output_format = OutputFormat.JSON_DEBUG
    elif sarif:
        output_format = OutputFormat.SARIF

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

        if len(valid_configs) == 0:
            raise SemgrepError(
                f"no valid configuration file found ({len(invalid_configs)} configs were invalid)",
                code=MISSING_CONFIG_EXIT_CODE,
            )

    # actually invoke semgrep
    rule_matches_by_rule, debug_steps_by_rule, semgrep_errors = CoreRunner(
        allow_exec=dangerously_allow_arbitrary_code_execution_from_rules,
        jobs=jobs,
        exclude=exclude,
        include=include,
        exclude_dir=exclude_dir,
        include_dir=include_dir,
    ).invoke_semgrep(targets, all_rules)

    if output_format == OutputFormat.TEXT:
        for error in semgrep_errors:
            print_error(pretty_error(error))

    output = handle_output(
        rule_matches_by_rule,
        semgrep_errors,
        output_format,
        debug_steps_by_rule,
        quiet,
        output_destination,
        exit_on_error,
    )
    if autofix:
        apply_fixes(rule_matches_by_rule)

    if strict and len(semgrep_errors):
        raise SemgrepError(
            f"run with --strict and {len(semgrep_errors)} errors occurred during semgrep run; exiting",
            code=INVALID_CODE_EXIT_CODE,
        )

    return output


def pretty_error(error: Dict[str, Any]) -> str:
    try:
        if {"path", "start", "end", "extra"}.difference(error.keys()) == set():
            header = f"{error['extra']['message']}\n--> {error['path']}:{error['start']['line']}"
            line_1 = error["extra"]["line"]
            start_col = error["start"]["col"]
            line_2 = " " * (start_col - 1) + "^"
            line_3 = f"= note: If the code is correct, this could be a semgrep bug -- please help us fix this by filing an an issue at https://semgrep.dev"
            return "\n".join([header, line_1, line_2, line_3])
        else:
            return f"semgrep-core error: {json.dumps(error,indent=2)}"
    except KeyError:
        return f"semgrep-core error: {json.dumps(error, indent=2)}"


def handle_output(
    rule_matches_by_rule: Dict[Rule, List[RuleMatch]],
    semgrep_errors: List[Any],
    output_format: OutputFormat,
    debug_steps_by_rule: Dict[Rule, List[Dict[str, Any]]],
    quiet: bool = False,
    output_destination: Optional[str] = None,
    exit_on_error: bool = False,
) -> str:
    rules = frozenset(rule_matches_by_rule.keys())
    rule_matches = [
        match
        for matches_of_one_rule in rule_matches_by_rule.values()
        for match in matches_of_one_rule
    ]

    output = build_output(
        rule_matches,
        debug_steps_by_rule,
        rules,
        semgrep_errors,
        output_format,
        not output_destination,
    )
    if not quiet:
        if output:
            print(output)

    if output_destination:
        save_output(output_destination, output)

    if exit_on_error and any(match.should_fail_run for match in rule_matches):
        sys.exit(FINDINGS_EXIT_CODE)

    # TODO tests still rely on this but it is unnecessary
    return output
