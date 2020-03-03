#!/usr/bin/env python3
import argparse
import collections
import itertools
import json
import os
import subprocess
import sys
import tempfile
import time
import traceback
from datetime import datetime
from pathlib import Path
from pathlib import PurePath
from typing import Any
from typing import DefaultDict
from typing import Dict
from typing import Generator
from typing import Iterable
from typing import Iterator
from typing import List
from typing import NewType
from typing import Optional
from typing import Set
from typing import Tuple
from urllib.parse import urlparse

import colorama
import config_resolver
import requests
import yaml
from constants import DEFAULT_CONFIG_FILE
from constants import ID_KEY
from constants import RCE_RULE_FLAG
from constants import RULES_KEY
from evaluation import build_boolean_expression
from evaluation import enumerate_patterns_in_boolean_expression
from evaluation import evaluate_expression
from sgrep_types import BooleanRuleExpression
from sgrep_types import InvalidRuleSchema
from sgrep_types import Operator
from sgrep_types import OPERATORS
from sgrep_types import PatternId
from sgrep_types import Range
from sgrep_types import SgrepRange
from util import debug_print
from util import is_url
from util import print_error
from util import print_error_exit
from util import print_msg

# Constants

SGREP_URL = "https://sgrep.dev/"
SGREP_RULES_HOME = "https://github.com/returntocorp/sgrep-rules"
PLEASE_FILE_ISSUE_TEXT = "An error occurred while invoking the sgrep engine; please help us fix this by filing an an issue at https://sgrep.dev"
MISSING_RULE_ID = "no-rule-id"


# Exit codes
FINDINGS_EXIT_CODE = 1

# These are the only valid top-level keys
MUST_HAVE_KEYS = {"id", "message", "languages", "severity"}
MUST_HAVE_ONLY_ONE_KEY = {"pattern", "patterns"}
ALL_VALID_RULE_KEYS = MUST_HAVE_ONLY_ONE_KEY.union(MUST_HAVE_KEYS)


SGREP_PATH = "sgrep"

# helper functions


def parse_sgrep_output(
    sgrep_findings: List[Dict[str, Any]]
) -> Dict[PatternId, List[SgrepRange]]:
    output: DefaultDict[PatternId, List[SgrepRange]] = collections.defaultdict(list)
    for finding in sgrep_findings:
        check_id = finding["check_id"]
        # restore the pattern id: the check_id was encoded as f"{rule_index}.{pattern_id}"
        pattern_id = PatternId(".".join(check_id.split(".")[1:]))
        output[pattern_id].append(sgrep_finding_to_range(finding))
    return dict(output)


def sgrep_finding_to_range(sgrep_finding: Dict[str, Any]) -> SgrepRange:
    metavars = sgrep_finding["extra"]["metavars"]
    return SgrepRange(
        Range(sgrep_finding["start"]["offset"], sgrep_finding["end"]["offset"]),
        {k: v["abstract_content"] for k, v in metavars.items()},
    )


def group_rule_by_langauges(
    all_rules: List[Dict[str, Any]]
) -> Dict[str, List[Dict[str, Any]]]:
    by_lang: Any = collections.defaultdict(list)
    for rule in all_rules:
        for language in rule["languages"]:
            by_lang[language].append(rule)
    return by_lang


def invoke_sgrep(
    all_rules: List[Dict[str, Any]], targets: List[Path], strict: bool
) -> Dict[str, Any]:
    """Returns parsed json output of sgrep"""

    outputs: List[Any] = []  # multiple invocations per language
    errors: List[Any] = []
    for language, all_rules_for_language in group_rule_by_langauges(all_rules).items():
        with tempfile.NamedTemporaryFile("w") as fout:
            # very important not to sort keys here
            yaml_as_str = yaml.safe_dump(
                {"rules": all_rules_for_language}, sort_keys=False
            )
            fout.write(yaml_as_str)
            fout.flush()
            extra_args = (
                ["-report_parse_errors", "-report_fatal_errors"] if strict else []
            )
            cmd = (
                [SGREP_PATH]
                + extra_args
                + [
                    "-lang",
                    language,
                    f"-rules_file",
                    fout.name,
                    *[str(path) for path in targets],
                ]
            )
            try:
                output = subprocess.check_output(cmd, shell=False)
            except subprocess.CalledProcessError as ex:
                print_error(
                    f"non-zero return code while invoking sgrep with:\n\t{' '.join(cmd)}\n{ex}"
                )
                print_error_exit(f"\n\n{PLEASE_FILE_ISSUE_TEXT}")
            output_json = json.loads((output.decode("utf-8")))

            errors.extend(output_json["errors"])
            outputs.extend(output_json["matches"])
    return {"matches": outputs, "errors": errors}


def fetch_lines_in_file(
    path: Path, start_line_number: int, end_line_number: int
) -> Optional[Iterable[str]]:
    """
    `line_number` is one-indexed! Returns the line if it can be found, returns None if the path doesn't exist
    TODO: cachine
    """
    if not path.exists():
        return None
    with path.open(buffering=1) as fin:  # buffering=1 turns on line-level reads
        return list(itertools.islice(fin, start_line_number - 1, end_line_number))


def rewrite_message_with_metavars(yaml_rule, sgrep_result):
    msg_text = yaml_rule["message"]
    if "metavars" in sgrep_result["extra"]:
        for metavar, contents in sgrep_result["extra"]["metavars"].items():
            msg_text = msg_text.replace(metavar, contents["abstract_content"])
    return msg_text


def transform_to_r2c_output(finding: Dict[str, Any]) -> Dict[str, Any]:
    # https://docs.r2c.dev/en/latest/api/output.html does not support offset at the moment
    if "offset" in finding["start"]:
        del finding["start"]["offset"]
    if "offset" in finding["end"]:
        del finding["end"]["offset"]
    return finding


def should_send_to_sgrep(expression: BooleanRuleExpression) -> bool:
    """
    don't send rules like "and-either" or "and-all" to sgrep
    """
    return (
        expression.pattern_id is not None
        and expression.operand is not None
        and (expression.operator != OPERATORS.WHERE_PYTHON)
    )


def flatten_rule_patterns(all_rules) -> Iterator[Dict[str, Any]]:
    for rule_index, rule in enumerate(all_rules):
        flat_expressions = list(
            enumerate_patterns_in_boolean_expression(
                build_boolean_expression(rule)
            )
        )
        for expr in flat_expressions:
            if not should_send_to_sgrep(expr):
                continue
            # if we don't copy an array (like `languages`), the yaml file will refer to it by reference (with an anchor)
            # which is nice and all but the sgrep YAML parser doesn't support that
            new_check_id = f"{rule_index}.{expr.pattern_id}"
            yield {
                "id": new_check_id,
                "pattern": expr.operand,
                "severity": rule["severity"],
                "languages": rule["languages"].copy(),
                "message": "<internalonly>",
            }


### Config helpers


def validate_single_rule(config_id: str, rule_index: int, rule: Dict[str, Any]) -> bool:
    rule_id_err_msg = f'(rule id: {rule.get("id", MISSING_RULE_ID)})'
    if not set(rule.keys()).issuperset(MUST_HAVE_KEYS):
        print_error(
            f"{config_id} is missing keys at rule {rule_index+1} {rule_id_err_msg}, must have: {MUST_HAVE_KEYS}"
        )
        return False
    if not set(rule.keys()).issubset(ALL_VALID_RULE_KEYS):
        print_error(
            f"{config_id} has invalid rule key at rule {rule_index+1} {rule_id_err_msg}, can only have: {ALL_VALID_RULE_KEYS}"
        )
        return False
    if not "pattern" in rule and not "patterns" in rule:
        print_error(
            f"{config_id} is missing key `pattern` or `patterns` at rule {rule_index+1} {rule_id_err_msg}"
        )
        return False
    if "patterns" in rule and not rule["patterns"]:
        print_error(
            f"{config_id} no patterns found inside rule {rule_index+1} {rule_id_err_msg}"
        )
        return False
    try:
        _ = build_boolean_expression(rule)
    except InvalidRuleSchema as ex:
        print_error(
            f"{config_id}: inside rule {rule_index+1} {rule_id_err_msg}, pattern fields can't look like this: {ex}"
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
        for i, rule in enumerate(rules):  # type: ignore
            if validate_single_rule(config_id, i, rule):
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


def validate_pattern_with_sgrep(pattern: str, language: str) -> bool:
    cmd = [SGREP_PATH, "-lang", language, f"--validate-pattern-stdin"]
    p = subprocess.run(cmd, stdout=subprocess.PIPE, input=pattern, encoding="utf-8")
    return p.returncode == 0


def validate_patterns(valid_configs: Dict[str, Any]) -> List[str]:
    invalid: List[str] = []
    for config_id, config in valid_configs.items():
        rules = config.get(RULES_KEY, [])
        for rule in rules:
            expressions = enumerate_patterns_in_boolean_expression(
                build_boolean_expression(rule)
            )
            for expr in expressions:
                for language in rule["languages"]:
                    # avoid patterns that don't have pattern_ids, like pattern-either
                    if should_send_to_sgrep(expr) and not validate_pattern_with_sgrep(
                        expr.operand, language  # type: ignore
                    ):
                        invalid.append(expr.operand)  # type: ignore
                        print_error(
                            f"in {config_id}, pattern in rule {rule['id']} can't be parsed for language {language}: {expr.operand}"
                        )
    return invalid


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


def flatten_configs(transformed_configs: Dict[str, Any]) -> List[Any]:
    return [
        rule
        for config in transformed_configs.values()
        for rule in config.get(RULES_KEY, [])
    ]


def manual_config(pattern: str, lang: str) -> Dict[str, Any]:
    # TODO remove when using sgrep -e ... -l ... instead of this hacked config
    return {
        "manual": {
            RULES_KEY: [
                {
                    ID_KEY: "-",
                    "pattern": pattern,
                    "message": pattern,
                    "languages": [lang],
                    "severity": "ERROR",
                }
            ]
        }
    }


### Handle output


def post_output(output_url: str, output_data: Dict[str, Any]) -> None:
    print_msg(f"posting to {output_url}...")
    r = requests.post(output_url, json=output_data)
    debug_print(f"posted to {output_url} and got status_code:{r.status_code}")


def build_output_json(output_json: Dict[str, Any]) -> str:
    return json.dumps(output_json)


def color_line(line, line_number, start_line, start_col, end_line, end_col):
    start_color = 0 if line_number > start_line else start_col
    # column offset
    start_color = max(0, start_color - 1)
    end_color = end_col if line_number >= end_line else len(line) + 1 + 1
    end_color = max(end_color - 1, 0)
    line = (
        line[:start_color]
        + colorama.Style.BRIGHT
        + line[start_color:end_color]
        + colorama.Style.RESET_ALL
        + line[end_color:]
    )
    return line


def finding_to_line(finding: Dict[str, Any], color_output: bool) -> Iterator[str]:
    path = finding.get("path")
    start_line = finding.get("start", {}).get("line")
    end_line = finding.get("end", {}).get("line")
    start_col = finding.get("start", {}).get("col")
    end_col = finding.get("end", {}).get("col")
    if path and start_line:
        file_lines = fetch_lines_in_file(Path(path), start_line, end_line)
        if file_lines:
            for i, line in enumerate(file_lines):
                if color_output:
                    yield f"{colorama.Fore.GREEN}{start_line + i}{colorama.Style.RESET_ALL}:{color_line(line.rstrip(), start_line + i, start_line, start_col, end_line, end_col)}"
                else:
                    yield f"{start_line + i}:{line.rstrip()}"


def build_normal_output(
    output_data: Dict[str, Any], color_output: bool
) -> Iterator[str]:
    results = output_data.get("results", [])
    last_file = None
    last_message = None
    for finding in sorted(
        results,
        key=lambda k: (k.get("path", "<no path>"), k.get("check_id", "<no rule id>")),
    ):
        RESET_COLOR = colorama.Style.RESET_ALL if color_output else ""
        GREEN_COLOR = colorama.Fore.GREEN if color_output else ""
        YELLOW_COLOR = colorama.Fore.YELLOW if color_output else ""

        current_file = finding.get("path", "<no path>")
        check_id = finding.get("check_id")
        message = finding.get("extra", {}).get("message")
        if last_file is None or last_file != current_file:
            if last_file is not None:
                yield ""
            yield f"{GREEN_COLOR}{current_file}{RESET_COLOR}"
            last_message = None
        # don't display the rule line if the check is empty
        if (
            check_id
            and check_id != "-"
            and (last_message is None or last_message != message)
        ):
            yield f"{YELLOW_COLOR}rule:{check_id}: {finding.get('extra', {}).get('message')}{RESET_COLOR}"

        last_file = current_file
        last_message = message
        yield from finding_to_line(finding, color_output)


def save_output(output_str: str, output_data: Dict[str, Any], json: bool = False):
    if is_url(output_str):
        post_output(output_str, output_data)
    else:
        if Path(output_str).is_absolute():
            save_path = Path(output_str)
        else:
            base_path = config_resolver.get_base_path()
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


def should_exclude_this_path(path: Path) -> bool:
    return any("test" in p or "example" in p for p in path.parts)


def set_flags(debug: bool, quiet: bool) -> None:
    """Set the global DEBUG and QUIET flags"""
    # TODO move to a proper logging framework
    global DEBUG
    global QUIET
    if debug:
        DEBUG = True  # type: ignore
        debug_print("DEBUG is on")
    if quiet:
        QUIET = True  # type: ignore
        debug_print("QUIET is on")


# entry point
def main(args: argparse.Namespace):
    """ main function that parses args and runs sgrep """

    # set the flags
    set_flags(args.verbose, args.quiet)

    # change cwd if using docker
    config_resolver.adjust_for_docker(args.precommit)

    # get the proper paths for targets i.e. handle base path of /home/repo when it exists in docker
    targets = config_resolver.resolve_targets(args.target)

    # first check if user asked to generate a config
    if args.generate_config:
        config_resolver.generate_config()

    # let's check for a pattern
    elif args.pattern:
        # and a language
        if not args.lang:
            print_error_exit("language must be specified when a pattern is passed")
        lang = args.lang
        pattern = args.pattern

        # TODO for now we generate a manual config. Might want to just call sgrep -e ... -l ...
        configs = config_resolver.manual_config(pattern, lang)
    else:
        # else let's get a config. A config is a dict from config_id -> config. Config Id is not well defined at this point.
        configs = config_resolver.resolve_config(args.config)

    # if we can't find a config, use default r2c rules
    if not configs:
        print_error_exit(
            f"No config given. If you want to see some examples, try running with --config r2c"
        )

    # let's split our configs into valid and invalid configs.
    # It's possible that a config_id exists in both because we check valid rules and invalid rules
    # instead of just hard failing for that config if mal-formed
    valid_configs, errors = validate_configs(configs)

    validate = args.validate
    strict = args.strict

    if errors:
        if strict:
            print_error_exit(
                f"run with --strict and there were {len(errors)} errors loading configs"
            )
        elif validate:
            print_error_exit(
                f"run with --validate and there were {len(errors)} errors loading configs"
            )
    elif validate:  # no errors!
        print_error_exit("Config is valid", exit_code=0)

    if not args.no_rewrite_rule_ids:
        # re-write the configs to have the hierarchical rule ids
        valid_configs = rename_rule_ids(valid_configs)

    # now validate all the patterns inside the configs
    if not args.skip_pattern_validation:
        start_validate_t = time.time()
        invalid_patterns = validate_patterns(valid_configs)
        if len(invalid_patterns):
            print_error_exit(
                f"{len(invalid_patterns)} invalid patterns found inside rules; aborting"
            )
        debug_print(f"debug: validated config in {time.time() - start_validate_t}")

    # extract just the rules from valid configs
    all_rules = flatten_configs(valid_configs)

    if not args.pattern:
        plural = "s" if len(valid_configs) > 1 else ""
        config_id_if_single = (
            list(valid_configs.keys())[0] if len(valid_configs) == 1 else ""
        )
        invalid_msg = (
            f"({len(errors)} config files were invalid)" if len(errors) else ""
        )
        print_msg(
            f"running {len(all_rules)} rules from {len(valid_configs)} config{plural} {config_id_if_single} {invalid_msg}"
        )
    # TODO log valid and invalid configs if verbose

    # a rule can have multiple patterns inside it. Flatten these so we can send sgrep a single yml file list of patterns
    all_patterns = list(flatten_rule_patterns(all_rules))

    # actually invoke sgrep
    start = datetime.now()
    output_json = invoke_sgrep(all_patterns, targets, strict)
    debug_print(f"sgrep ran in {datetime.now() - start}")
    debug_print(str(output_json))

    # group output; we want to see all of the same rule ids on the same file path
    by_rule_index: Dict[int, Dict[str, List[Dict[str, Any]]]] = collections.defaultdict(
        lambda: collections.defaultdict(list)
    )

    for finding in output_json["errors"]:
        print_error(f"sgrep: {finding['path']}: {finding['check_id']}")

    if strict and len(output_json["errors"]):
        print_error_exit(
            f"run with --strict and {len(output_json['errors'])} errors occurred during sgrep run; exiting"
        )

    for finding in output_json["matches"]:
        # decode the rule index from the output check_id
        rule_index = int(finding["check_id"].split(".")[0])
        by_rule_index[rule_index][finding["path"]].append(finding)

    current_path = Path.cwd()
    outputs_after_booleans = []
    ignored_in_tests = 0
    for rule_index, paths in by_rule_index.items():
        expression = build_boolean_expression(all_rules[rule_index])
        debug_print(str(expression))
        # expression = (op, pattern_id) for (op, pattern_id, pattern) in expression_with_patterns]
        for filepath, results in paths.items():
            debug_print(
                f"-------- rule (index {rule_index}) {all_rules[rule_index]['id']}------ filepath: {filepath}"
            )
            check_ids_to_ranges = parse_sgrep_output(results)
            debug_print(str(check_ids_to_ranges))
            valid_ranges_to_output = evaluate_expression(
                expression, check_ids_to_ranges
            )

            # only output matches which are inside these offsets!
            debug_print(f"compiled result {valid_ranges_to_output}")
            debug_print("-" * 80)
            for result in results:
                if sgrep_finding_to_range(result).range in valid_ranges_to_output:
                    path_object = Path(result["path"])
                    if args.exclude_tests and should_exclude_this_path(path_object):
                        ignored_in_tests += 1
                        continue

                    # restore the original rule ID
                    result["check_id"] = all_rules[rule_index]["id"]
                    # rewrite the path to be relative to the current working directory
                    result["path"] = str(safe_relative_to(path_object, current_path))

                    # restore the original message
                    result["extra"]["message"] = rewrite_message_with_metavars(
                        all_rules[rule_index], result
                    )
                    result = transform_to_r2c_output(result)
                    outputs_after_booleans.append(result)

    if ignored_in_tests > 0:
        print_error(
            f"warning: ignored {ignored_in_tests} results in tests due to --exclude-tests option"
        )

    # output results
    output_data = {"results": outputs_after_booleans}
    if not args.quiet:
        if args.json:
            print(build_output_json(output_data))
        else:
            print("\n".join(build_normal_output(output_data, color_output=True)))
    if args.output:
        save_output(args.output, output_data, args.json)
    if args.error and outputs_after_booleans:
        sys.exit(FINDINGS_EXIT_CODE)


# CLI

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description=f"sgrep CLI. For more information about sgrep, go to {SGREP_URL}",
        prog="sgrep",  # we have to lie to the user since they know of this as `sgrep`
    )

    # input
    parser.add_argument(
        "target",
        nargs="*",
        default=["."],
        help="Files to search (by default, entire current working directory searched). Implied argument if piping to sgrep.",
    )

    # config options
    config = parser.add_argument_group("config")
    config_ex = config.add_mutually_exclusive_group()
    config_ex.add_argument(
        "-g",
        "--generate-config",
        help=f"Generte starter {DEFAULT_CONFIG_FILE}",
        action="store_true",
    )

    config_ex.add_argument(
        "-f",
        "--config",
        help=f"Config YAML file or directory of YAML files ending in .yml|.yaml, OR URL of a config file, OR sgrep registry entry name. See the README for sgrep for information on config file format.",
    )

    config_ex.add_argument("-e", "--pattern", help="sgrep pattern")
    config.add_argument(
        "-l",
        "--lang",
        help="Parses pattern and all files in specified language. Must be used with -e/--pattern.",
    )
    config.add_argument(
        "--validate",
        help=f"Validate config file(s). No search is performed.",
        action="store_true",
    )
    config.add_argument(
        "--strict",
        help=f"only invoke sgrep if config(s) are valid",
        action="store_true",
    )

    config.add_argument(
        RCE_RULE_FLAG,
        help=f"DANGEROUS: allow rules to run arbitrary code: ONLY ENABLE IF YOU TRUST THE SOURCE OF ALL RULES IN YOUR CONFIG.",
        action="store_true",
    )

    config.add_argument(
        "--exclude-tests",
        help=f"try to exclude tests, documentation, and examples (based on filename/path)",
        action="store_true",
    )
    config.add_argument("--precommit", help=argparse.SUPPRESS, action="store_true")

    # output options
    output = parser.add_argument_group("output")

    output.add_argument(
        "-q",
        "--quiet",
        help="Do not print anything to stdout. Search results can still be saved to an output file specified by -o/--output. Exit code provides success status.",
        action="store_true",
    )

    output.add_argument(
        "--no-rewrite-rule-ids",
        help="Do not rewrite rule ids when they appear in nested subfolders (by default, rule 'foo' in test/rules.yaml will be renamed 'test.foo')",
        action="store_true",
    )

    output.add_argument(
        "-o",
        "--output",
        help="Save search results to a file or post to URL. Default is to print to stdout.",
    )
    output.add_argument(
        "--json", help="Convert search output to JSON format.", action="store_true"
    )
    output.add_argument(
        "--r2c",
        help="output json in r2c platform format (https://app.r2c.dev)",
        action="store_true",
    )
    output.add_argument(
        "--skip-pattern-validation",
        help="skip using sgrep to validate patterns before running (not recommended)",
        action="store_true",
    )
    output.add_argument(
        "--error",
        help="System Exit 1 if there are findings. Useful for CI and scripts.",
        action="store_true",
    )
    # logging options
    logging = parser.add_argument_group("logging")

    logging.add_argument(
        "-v",
        "--verbose",
        help=f"Sets the logging level to verbose. E.g. statements about which files are being processed will be printed.",
        action="store_true",
    )

    ### Parse and validate
    args = parser.parse_args()
    if args.lang and not args.pattern or (args.pattern and not args.lang):
        parser.error("-e/--pattern and -l/--lang must both be specified")
    try:
        main(args)
    except NotImplementedError as ex:
        print_error_exit(
            f"sgrep encountered an error: {ex}; this is not your fault. {PLEASE_FILE_ISSUE_TEXT}"
        )
