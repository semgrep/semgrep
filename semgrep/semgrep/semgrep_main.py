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
from semgrep.constants import ID_KEY
from semgrep.constants import OutputFormat
from semgrep.constants import RULES_KEY
from semgrep.core_runner import CoreRunner
from semgrep.output import build_output
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.semgrep_types import InvalidRuleSchema
from semgrep.semgrep_types import YAML_ALL_VALID_RULE_KEYS
from semgrep.semgrep_types import YAML_MUST_HAVE_KEYS
from semgrep.util import debug_print
from semgrep.util import FINDINGS_EXIT_CODE
from semgrep.util import INVALID_CODE_EXIT_CODE
from semgrep.util import is_url
from semgrep.util import MISSING_CONFIG_EXIT_CODE
from semgrep.util import print_error
from semgrep.util import print_error_exit
from semgrep.util import print_msg

MISSING_RULE_ID = "no-rule-id"


def validate_single_rule(config_id: str, rule: Dict[str, Any]) -> bool:
    """
        Validate that a rule dictionary contains all necessary keys
        and can be correctly parsed
    """
    rule_id_err_msg = f'{rule.get("id", MISSING_RULE_ID)}'
    rule_keys = set(rule.keys())
    if not rule_keys.issuperset(YAML_MUST_HAVE_KEYS):
        missing_keys = YAML_MUST_HAVE_KEYS - rule_keys
        print_error(
            f"{config_id} is missing required keys {missing_keys} at rule id {rule_id_err_msg}"
        )
        return False
    if not rule_keys.issubset(YAML_ALL_VALID_RULE_KEYS):
        extra_keys = rule_keys - YAML_ALL_VALID_RULE_KEYS
        print_error(
            f"{config_id} has invalid rule key {extra_keys} at rule id {rule_id_err_msg}, can only have: {YAML_ALL_VALID_RULE_KEYS}"
        )
        return False
    try:
        _ = Rule.from_json(rule).expression
    except InvalidRuleSchema as ex:
        print_error(
            f"{config_id}: inside rule id {rule_id_err_msg}, pattern fields can't look like this: {ex}"
        )
        return False
    try:
        _ = Rule.from_json(rule).globs
    except InvalidRuleSchema as ex:
        print_error(
            f"{config_id}: inside rule id {rule_id_err_msg}, path fields can't look like this: {ex}"
        )
        return False

    return True


def validate_configs(
    configs: Dict[str, Optional[Dict[str, Any]]]
) -> Tuple[Dict[str, Any], Dict[str, Any]]:
    """
        Take configs and separate into valid and invalid ones
    """
    errors = {}
    valid = {}
    for config_id, config in configs.items():
        if not config:
            errors[config_id] = config
            continue
        if RULES_KEY not in config:
            print_error(f"{config_id} is missing `{RULES_KEY}` as top-level key")
            errors[config_id] = config
            continue
        rules = config.get(RULES_KEY)
        valid_rules = []
        invalid_rules = []
        for rule in rules:  # type: ignore
            if validate_single_rule(config_id, rule):
                valid_rules.append(rule)
            else:
                invalid_rules.append(rule)

        if invalid_rules:
            errors[config_id] = {**config, "rules": invalid_rules}
        if valid_rules:
            valid[config_id] = {**config, "rules": valid_rules}
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


def rename_rule_ids(valid_configs: Dict[str, Any]) -> Dict[str, Any]:
    transformed = {}
    for config_id, config in valid_configs.items():
        rules = config.get(RULES_KEY, [])
        transformed_rules = [
            {
                **rule,
                ID_KEY: f"{convert_config_id_to_prefix(config_id)}{rule.get(ID_KEY, MISSING_RULE_ID)}",
            }
            for rule in rules
        ]
        transformed[config_id] = {**config, RULES_KEY: transformed_rules}
    return transformed


### Handle output


def post_output(output_url: str, output: str) -> None:
    import requests  # here for faster startup times

    print_msg(f"posting to {output_url}...")
    try:
        r = requests.post(output_url, data=output, timeout=10)
        debug_print(f"posted to {output_url} and got status_code:{r.status_code}")
    except requests.exceptions.Timeout:
        print_error_exit(f"posting output to {output_url} timed out")


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


def get_config(generate_config: bool, pattern: str, lang: str, config: str) -> Any:
    # first check if user asked to generate a config
    if generate_config:
        semgrep.config_resolver.generate_config()

    # let's check for a pattern
    elif pattern:
        # and a language
        if not lang:
            print_error_exit("language must be specified when a pattern is passed")

        # TODO for now we generate a manual config. Might want to just call semgrep -e ... -l ...
        configs = semgrep.config_resolver.manual_config(pattern, lang)
    else:
        # else let's get a config. A config is a dict from config_id -> config. Config Id is not well defined at this point.
        configs = semgrep.config_resolver.resolve_config(config)

    # if we can't find a config, use default r2c rules
    if not configs:
        print_error_exit(
            f"No config given and {DEFAULT_CONFIG_FILE} was not found. Try running with --help to debug or if you want to download a default config, try running with --config r2c"
        )

    # let's split our configs into valid and invalid configs.
    # It's possible that a config_id exists in both because we check valid rules and invalid rules
    # instead of just hard failing for that config if mal-formed
    valid_configs, invalid_configs = validate_configs(configs)
    return valid_configs, invalid_configs


def flatten_configs(transformed_configs: Dict[str, Any]) -> List[Rule]:
    return [
        Rule.from_json(rule)
        for config in transformed_configs.values()
        for rule in config.get(RULES_KEY, [])
    ]


def should_exclude_this_path(path: Path) -> bool:
    """
        Return true if "test" or "example" are anywhere in path
    """
    return any("test" in p or "example" in p for p in path.parts)


def main(
    target: List[str],
    pattern: str,
    lang: str,
    config: str,
    generate_config: bool,
    no_rewrite_rule_ids: bool,
    jobs: int,
    include: List[str],
    include_dir: List[str],
    exclude: List[str],
    exclude_dir: List[str],
    exclude_tests: bool,
    json_format: bool,
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
    valid_configs, invalid_configs = get_config(generate_config, pattern, lang, config)

    output_format = OutputFormat.TEXT
    if json_format:
        output_format = OutputFormat.JSON
    elif sarif:
        output_format = OutputFormat.SARIF

    if invalid_configs and strict:
        print_error_exit(
            f"run with --strict and there were {len(invalid_configs)} errors loading configs",
            MISSING_CONFIG_EXIT_CODE,
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
            print_error_exit(
                f"no valid configuration file found ({len(invalid_configs)} configs were invalid)",
                MISSING_CONFIG_EXIT_CODE,
            )

    # actually invoke semgrep
    rule_matches_by_rule, semgrep_errors = CoreRunner(
        allow_exec=dangerously_allow_arbitrary_code_execution_from_rules,
        jobs=jobs,
        exclude=exclude,
        include=include,
        exclude_dir=exclude_dir,
        include_dir=include_dir,
    ).invoke_semgrep(targets, all_rules)

    if exclude_tests:
        ignored_in_tests = 0
        filtered_findings_by_rule = {}
        for rule, rule_matches in rule_matches_by_rule.items():
            filtered = []
            for rule_match in rule_matches:
                if should_exclude_this_path(Path(rule_match.path)):
                    ignored_in_tests += 1
                else:
                    filtered.append(rule_match)
            filtered_findings_by_rule[rule] = filtered
        rule_matches_by_rule = filtered_findings_by_rule
        if ignored_in_tests > 0:
            print_error(
                f"warning: ignored {ignored_in_tests} results in tests due to --exclude-tests option"
            )

    for finding in semgrep_errors:
        print_error(f"semgrep: {finding['path']}: {finding['check_id']}")

    if strict and len(semgrep_errors):
        print_error_exit(
            f"run with --strict and {len(semgrep_errors)} errors occurred during semgrep run; exiting",
            INVALID_CODE_EXIT_CODE,
        )

    output = handle_output(
        rule_matches_by_rule,
        semgrep_errors,
        output_format,
        quiet,
        output_destination,
        exit_on_error,
    )
    if autofix:
        apply_fixes(rule_matches_by_rule)

    return output


def handle_output(
    rule_matches_by_rule: Dict[Rule, List[RuleMatch]],
    semgrep_errors: List[Any],
    output_format: OutputFormat,
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
        rule_matches, rules, semgrep_errors, output_format, not output_destination
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
