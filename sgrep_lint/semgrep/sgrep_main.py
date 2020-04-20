import argparse
import sys
from pathlib import Path
from pathlib import PurePath
from typing import Any
from typing import Dict
from typing import List
from typing import Optional
from typing import Tuple

import requests
import semgrep.config_resolver
from semgrep.autofix import apply_fixes
from semgrep.constants import ID_KEY
from semgrep.constants import RULES_KEY
from semgrep.output import build_normal_output
from semgrep.output import build_output_json
from semgrep.rule import Rule
from semgrep.sgrep_bridge import SgrepBridge
from semgrep.sgrep_types import InvalidRuleSchema
from semgrep.sgrep_types import YAML_ALL_VALID_RULE_KEYS
from semgrep.sgrep_types import YAML_MUST_HAVE_KEYS
from semgrep.util import debug_print
from semgrep.util import FINDINGS_EXIT_CODE
from semgrep.util import INVALID_CODE_EXIT_CODE
from semgrep.util import is_url
from semgrep.util import MISSING_CONFIG_EXIT_CODE
from semgrep.util import print_error
from semgrep.util import print_error_exit
from semgrep.util import print_msg

SGREP_RULES_HOME = "https://github.com/returntocorp/sgrep-rules"
MISSING_RULE_ID = "no-rule-id"


def validate_single_rule(config_id: str, rule: Dict[str, Any]) -> bool:
    rule_id_err_msg = f'(rule id: {rule.get("id", MISSING_RULE_ID)})'
    if not set(rule.keys()).issuperset(YAML_MUST_HAVE_KEYS):
        print_error(
            f"{config_id} is missing keys at rule {rule_id_err_msg}, must have: {YAML_MUST_HAVE_KEYS}"
        )
        return False
    if not set(rule.keys()).issubset(YAML_ALL_VALID_RULE_KEYS):
        print_error(
            f"{config_id} has invalid rule key at rule {rule_id_err_msg}, can only have: {YAML_ALL_VALID_RULE_KEYS}"
        )
        return False
    try:
        _ = Rule.from_json(rule).expression
    except InvalidRuleSchema as ex:
        print_error(
            f"{config_id}: inside rule {rule_id_err_msg}, pattern fields can't look like this: {ex}"
        )
        return False

    return True


def validate_configs(
    configs: Dict[str, Optional[Dict[str, Any]]]
) -> Tuple[Dict[str, Any], Dict[str, Any]]:
    """ Take configs and separate into valid and invalid ones"""
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


def post_output(output_url: str, output_data: Dict[str, Any]) -> None:
    print_msg(f"posting to {output_url}...")
    r = requests.post(output_url, json=output_data)
    debug_print(f"posted to {output_url} and got status_code:{r.status_code}")


def r2c_error_format(sgrep_errors_json: Dict[str, Any]) -> Dict[str, Any]:
    # TODO https://docs.r2c.dev/en/latest/api/output.html
    return sgrep_errors_json


def save_output(
    output_str: str, output_data: Dict[str, Any], json: bool = False
) -> None:
    if is_url(output_str):
        post_output(output_str, output_data)
    else:
        if Path(output_str).is_absolute():
            save_path = Path(output_str)
        else:
            base_path = semgrep.config_resolver.get_base_path()
            save_path = base_path.joinpath(output_str)
        # create the folders if not exists
        save_path.parent.mkdir(parents=True, exist_ok=True)
        with save_path.open(mode="w") as fout:
            if json:
                fout.write(build_output_json(output_data))
            else:
                fout.write(
                    "\n".join(build_normal_output(output_data, color_output=False))
                )


def get_config(args: Any) -> Any:
    # first check if user asked to generate a config
    if args.generate_config:
        semgrep.config_resolver.generate_config()

    # let's check for a pattern
    elif args.pattern:
        # and a language
        if not args.lang:
            print_error_exit("language must be specified when a pattern is passed")
        lang = args.lang
        pattern = args.pattern

        # TODO for now we generate a manual config. Might want to just call sgrep -e ... -l ...
        configs = semgrep.config_resolver.manual_config(pattern, lang)
    else:
        # else let's get a config. A config is a dict from config_id -> config. Config Id is not well defined at this point.
        configs = semgrep.config_resolver.resolve_config(args.config)

    # if we can't find a config, use default r2c rules
    if not configs:
        print_error_exit(
            f"No config given. If you want to see some examples, try running with --config r2c"
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


def main(args: argparse.Namespace) -> Dict[str, Any]:
    """ main function that parses args and runs sgrep """
    # get the proper paths for targets i.e. handle base path of /home/repo when it exists in docker
    targets = semgrep.config_resolver.resolve_targets(args.target)
    valid_configs, invalid_configs = get_config(args)

    if invalid_configs and args.strict:
        print_error_exit(
            f"run with --strict and there were {len(invalid_configs)} errors loading configs"
        )

    if not args.no_rewrite_rule_ids:
        # re-write the configs to have the hierarchical rule ids
        valid_configs = rename_rule_ids(valid_configs)

    # extract just the rules from valid configs
    all_rules = flatten_configs(valid_configs)

    if not args.pattern:
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

    # actually invoke sgrep
    findings_by_rule, sgrep_errors = SgrepBridge(
        args.dangerously_allow_arbitrary_code_execution_from_rules, args.exclude_tests
    ).invoke_sgrep(targets, all_rules)

    for finding in sgrep_errors:
        print_error(f"sgrep: {finding['path']}: {finding['check_id']}")

    if args.strict and len(sgrep_errors):
        print_error_exit(
            f"run with --strict and {len(sgrep_errors)} errors occurred during sgrep run; exiting",
            INVALID_CODE_EXIT_CODE,
        )

    output_data = handle_output(findings_by_rule, sgrep_errors, args)

    if args.autofix:
        apply_fixes(findings_by_rule)

    return output_data


def handle_output(
    findings_by_rule: Any, sgrep_errors: Any, args: Any
) -> Dict[str, Any]:
    outputs_after_booleans: List[Dict[str, Any]] = []
    for findings in findings_by_rule.values():
        outputs_after_booleans.extend(findings)

    # output results
    output_data = {
        "results": outputs_after_booleans,
        "errors": r2c_error_format(sgrep_errors),
    }
    if args.exclude:
        exclude_glob_patterns = args.exclude
        debug_print(f"patterns to exclude: {', '.join(exclude_glob_patterns)}")
        filtered_results = [
            output
            for output in outputs_after_booleans
            if not any(
                PurePath(output.get("path", "")).match(pat)
                for pat in exclude_glob_patterns
            )
        ]
        debug_print(
            f"filtered output from {len(outputs_after_booleans)} down to {len(filtered_results)} results"
        )
        output_data["results"] = filtered_results
    if not args.quiet:
        if args.json:
            print(build_output_json(output_data))
        else:
            if outputs_after_booleans:
                print("\n".join(build_normal_output(output_data, color_output=True)))

    if args.output:
        save_output(args.output, output_data, args.json)
    if args.error and outputs_after_booleans:
        sys.exit(FINDINGS_EXIT_CODE)

    return output_data
