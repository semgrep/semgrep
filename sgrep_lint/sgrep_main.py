import argparse
import collections
import itertools
import json
import subprocess
import sys
import tempfile
import time
from datetime import datetime
from pathlib import Path
from typing import Any
from typing import DefaultDict
from typing import Dict
from typing import Iterable
from typing import Iterator
from typing import List
from typing import Optional
from typing import Set
from typing import Tuple

import colorama
import config_resolver
import requests
import yaml
from constants import ID_KEY
from constants import PLEASE_FILE_ISSUE_TEXT
from constants import RCE_RULE_FLAG
from constants import RULES_KEY
from evaluation import build_boolean_expression
from evaluation import enumerate_patterns_in_boolean_expression
from evaluation import evaluate_expression
from sgrep_types import BooleanRuleExpression
from sgrep_types import InvalidRuleSchema
from sgrep_types import OPERATORS
from sgrep_types import PatternId
from sgrep_types import Range
from sgrep_types import SgrepRange
from sgrep_types import YAML_ALL_VALID_RULE_KEYS
from sgrep_types import YAML_MUST_HAVE_KEYS
from util import debug_print
from util import FINDINGS_EXIT_CODE
from util import INVALID_CODE_EXIT_CODE
from util import INVALID_PATTERN_EXIT_CODE
from util import is_url
from util import MISSING_CONFIG_EXIT_CODE
from util import print_error
from util import print_error_exit
from util import print_msg
from util import UNPARSEABLE_YAML_EXIT_CODE


# Constants

SGREP_RULES_HOME = "https://github.com/returntocorp/sgrep-rules"
MISSING_RULE_ID = "no-rule-id"


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
    by_lang: Dict[str, List[Dict[str, Any]]] = collections.defaultdict(list)
    for rule in all_rules:
        for language in rule["languages"]:
            by_lang[language].append(rule)
    return by_lang


def sgrep_error_json_to_message_then_exit(
    error_json: Dict[str, Any], all_rules: List[Dict[str, Any]]
) -> None:
    """
    See format_output_exception in sgrep O'Caml for details on schema
    """
    error_type = error_json["error"]
    if error_type == "invalid language":
        print_error_exit(f'invalid language {error_json["language"]}')
    elif error_type == "invalid pattern":
        decoded_pattern_index = decode_rule_id_to_index(error_json["pattern_id"])
        rule = all_rules[decoded_pattern_index]
        print_error(
            f'in rule {rule["id"]} for language {error_json["language"]} invalid pattern "{error_json["pattern"]}": {error_json["message"]}'
        )
        exit(INVALID_PATTERN_EXIT_CODE)
    # no special formatting ought to be required for the other types; the sgrep python should be performing
    # validation for them. So if any other type of error occurs, ask the user to file an issue
    else:
        print_error_exit(
            f'an internal error occured while invoking the sgrep engine: {error_type}: {error_json.get("message", "")}.\n\n{PLEASE_FILE_ISSUE_TEXT}'
        )


def yield_targeting_options(args: argparse.Namespace) -> Iterator[str]:
    """Yields include/exclude CLI options to call semgrep with.

    This is based on the arguments given to semgrep-lint.
    """
    for pattern in args.include:
        yield from ["-include", pattern]
    for pattern in args.exclude:
        yield from ["-exclude", pattern]
    for pattern in args.exclude_dir:
        yield from ["-exclude-dir", pattern]


def invoke_sgrep(
    all_patterns: List[Dict[str, Any]],
    targets: List[Path],
    output_mode_json: bool,
    all_rules: List[Dict[str, Any]],
    targeting_options: List[str],
) -> Dict[str, Any]:
    """Returns parsed json output of sgrep"""

    outputs: List[Any] = []  # multiple invocations per language
    errors: List[Any] = []
    for language, all_rules_for_language in group_rule_by_langauges(
        all_patterns
    ).items():
        with tempfile.NamedTemporaryFile("w") as fout:
            # very important not to sort keys here
            yaml_as_str = yaml.safe_dump(
                {"rules": all_rules_for_language}, sort_keys=False
            )
            fout.write(yaml_as_str)
            fout.flush()
            cmd = [
                SGREP_PATH,
                "-lang",
                language,
                "-rules_file",
                fout.name,
                *targeting_options,
                *[str(path) for path in targets],
            ]
            try:
                output = subprocess.check_output(cmd, shell=False)
            except subprocess.CalledProcessError as ex:
                try:
                    # see if sgrep output a JSON error that we can decode
                    output_json = json.loads((ex.output.decode("utf-8", "replace")))
                    if "error" in output_json:
                        sgrep_error_json_to_message_then_exit(output_json, all_rules)
                    else:
                        raise ex  # let our general exception handler take care of this
                except Exception:
                    print_error(
                        f"non-zero return code while invoking sgrep with:\n\t{' '.join(cmd)}\n{ex}"
                    )
                    print_error_exit(f"\n\n{PLEASE_FILE_ISSUE_TEXT}")
            output_json = json.loads((output.decode("utf-8", "replace")))
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


def rewrite_message_with_metavars(
    yaml_rule: Dict[str, Any], sgrep_result: Dict[str, Any]
) -> str:
    msg_text: str = yaml_rule["message"]
    if "metavars" in sgrep_result["extra"]:
        for metavar, contents in sgrep_result["extra"]["metavars"].items():
            msg_text = msg_text.replace(metavar, contents["abstract_content"])
    return msg_text


def generate_fix(
    yaml_rule: Dict[str, Any], sgrep_result: Dict[str, Any]
) -> Optional[Any]:
    fix_str = yaml_rule.get("fix")
    if fix_str is None:
        return None
    if "metavars" in sgrep_result["extra"]:
        for metavar, contents in sgrep_result["extra"]["metavars"].items():
            fix_str = fix_str.replace(metavar, contents["abstract_content"])
    return fix_str


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


def flatten_rule_patterns(all_rules: List[Dict[str, Any]]) -> Iterator[Dict[str, Any]]:
    for rule_index, rule in enumerate(all_rules):
        flat_expressions = list(
            enumerate_patterns_in_boolean_expression(build_boolean_expression(rule))
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
    if not set(rule.keys()).issuperset(YAML_MUST_HAVE_KEYS):
        print_error(
            f"{config_id} is missing keys at rule {rule_index+1} {rule_id_err_msg}, must have: {YAML_MUST_HAVE_KEYS}"
        )
        return False
    if not set(rule.keys()).issubset(YAML_ALL_VALID_RULE_KEYS):
        print_error(
            f"{config_id} has invalid rule key at rule {rule_index+1} {rule_id_err_msg}, can only have: {YAML_ALL_VALID_RULE_KEYS}"
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


def flatten_configs(transformed_configs: Dict[str, Any]) -> List[Dict[str, Any]]:
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
    # wrap errors under "data" entry to be compatible with
    # https://docs.r2c.dev/en/latest/api/output.html#errors
    errors = output_json["errors"]
    if errors:
        output_json["errors"] = {
            "data": output_json["errors"],
            "message": "SgrepRuntimeErrors",
        }
    return json.dumps(output_json)


def color_line(
    line: str,
    line_number: int,
    start_line: int,
    start_col: int,
    end_line: int,
    end_col: int,
) -> str:
    start_color = 0 if line_number > start_line else start_col
    # column offset
    start_color = max(start_color - 1, 0)
    end_color = end_col if line_number >= end_line else len(line) + 1 + 1
    end_color = max(end_color - 1, 0)
    line = (
        line[:start_color]
        + colorama.Style.BRIGHT
        + line[start_color : end_color + 1]  # want the color to include the end_col
        + colorama.Style.RESET_ALL
        + line[end_color + 1 :]
    )
    return line


def finding_to_raw_lines(
    finding: Dict[str, Any], color_output: bool = False
) -> Optional[Iterable[str]]:
    path = finding.get("path")
    start_line = finding.get("start", {}).get("line")
    end_line = finding.get("end", {}).get("line")
    if path and start_line:
        return fetch_lines_in_file(Path(path), start_line, end_line)
    return None


def finding_to_line(
    finding: Dict[str, Any], color_output: bool = False
) -> Iterator[str]:
    path = finding.get("path")
    start_line = finding.get("start", {}).get("line")
    end_line = finding.get("end", {}).get("line")
    start_col = finding.get("start", {}).get("col")
    end_col = finding.get("end", {}).get("col")
    file_lines = finding.get("extra", {}).get("file_lines")
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
        BLUE_COLOR = colorama.Fore.BLUE if color_output else ""

        current_file = finding.get("path", "<no path>")
        check_id = finding.get("check_id")
        extra = finding.get("extra", {})
        message = extra.get("message")
        fix = extra.get("fix")
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
        if fix:
            yield f"{BLUE_COLOR}autofix:{RESET_COLOR} {fix}"


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


def modify_file(filepath: str, finding: Dict[str, Any]) -> None:
    p = Path(filepath)
    SPLIT_CHAR = "\n"
    contents = p.read_text()
    lines = contents.split(SPLIT_CHAR)
    fix = finding.get("extra", {}).get("fix")

    # get the start and end points
    start_obj = finding.get("start", {})
    start_line = start_obj.get("line", 1) - 1  # start_line is 1 indexed
    start_col = start_obj.get("col", 1) - 1  # start_col is 1 indexed
    end_obj = finding.get("end", {})
    end_line = end_obj.get("line", 1) - 1  # end_line is 1 indexed
    end_col = end_obj.get("col", 1) - 1  # end_line is 1 indexed

    # break into before, to modify, after
    before_lines = lines[:start_line]
    before_on_start_line = lines[start_line][:start_col]
    after_on_end_line = lines[end_line][end_col + 1 :]  # next char after end of match
    modified_lines = (before_on_start_line + fix + after_on_end_line).splitlines()
    after_lines = lines[end_line + 1 :]  # next line after end of match
    contents_after_fix = before_lines + modified_lines + after_lines

    contents_after_fix_str = SPLIT_CHAR.join(contents_after_fix)
    p.write_text(contents_after_fix_str)


def should_exclude_this_path(path: Path) -> bool:
    return any("test" in p or "example" in p for p in path.parts)


def dump_parsed_ast(
    to_json: bool, language: str, pattern: Optional[str], targets: List[Path]
) -> None:
    with tempfile.NamedTemporaryFile("w") as fout:
        args = []
        if pattern:
            fout.write(pattern)
            fout.flush()
            args = ["-lang", language, "-dump_pattern", fout.name]
        else:
            if len(targets) != 1:
                print_error_exit("exactly one target file is required with this option")
            target = targets[0]
            args = ["-lang", language, "-dump_ast", str(target)]

        if to_json:
            args = ["-json"] + args

        cmd = [SGREP_PATH] + args
        try:
            output = subprocess.check_output(cmd, shell=False)
        except subprocess.CalledProcessError as ex:
            print_error(f"error invoking sgrep with:\n\t{' '.join(cmd)}\n{ex}")
            print_error_exit(f"\n\n{PLEASE_FILE_ISSUE_TEXT}")
        print(output.decode())


def uniq_id(r: Any) -> Tuple[str, str, int, int, int, int]:
    start = r.get("start", {})
    end = r.get("end", {})
    return (
        r.get("check_id"),
        r.get("path"),
        start.get("line"),
        start.get("col"),
        end.get("line"),
        end.get("col"),
    )


def decode_rule_id_to_index(rule_id: str) -> int:
    # decode the rule index from the output check_id
    return int(rule_id.split(".")[0])


def dedup_output(outputs: List[Any]) -> List[Any]:
    return list({uniq_id(r): r for r in outputs}.values())


def add_finding_line(outputs: List[Any]) -> List[Any]:
    for r in outputs:
        file_lines = finding_to_raw_lines(r)
        if file_lines is not None:
            r["extra"]["file_lines"] = list(file_lines)
        else:
            r["extra"]["file_lines"] = []
    return outputs


def clean_output(outputs: List[Any]) -> List[Any]:
    for r in outputs:
        del r["extra"]["metavars"]
    return outputs


# entry point
def main(args: argparse.Namespace) -> Dict[str, Any]:
    """ main function that parses args and runs sgrep """

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

    if args.dump_ast:
        if not args.lang:
            print_error_exit("language must be specified to dump ASTs")
        dump_parsed_ast(args.json, args.lang, args.pattern, targets)
        sys.exit(0)

    # if we can't find a config, use default r2c rules
    if not configs:
        print_error_exit(
            f"No config given. If you want to see some examples, try running with --config r2c"
        )

    # let's split our configs into valid and invalid configs.
    # It's possible that a config_id exists in both because we check valid rules and invalid rules
    # instead of just hard failing for that config if mal-formed
    valid_configs, invalid_configs = validate_configs(configs)

    validate = args.validate
    strict = args.strict

    if invalid_configs:
        if strict:
            print_error_exit(
                f"run with --strict and there were {len(invalid_configs)} errors loading configs"
            )
        elif validate:
            print_error_exit(
                f"run with --validate and there were {len(invalid_configs)} errors loading configs"
            )
    elif validate:  # no errors!
        print_error_exit("Config is valid", exit_code=0)

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

    # a rule can have multiple patterns inside it. Flatten these so we can send sgrep a single yml file list of patterns
    all_patterns = list(flatten_rule_patterns(all_rules))

    # actually invoke sgrep
    start = datetime.now()
    output_json = invoke_sgrep(
        all_patterns,
        targets,
        args.json,
        all_rules,
        targeting_options=list(yield_targeting_options(args)),
    )
    debug_print(f"sgrep ran in {datetime.now() - start}")
    debug_print(str(output_json))

    # group output; we want to see all of the same rule ids on the same file path
    by_rule_index: Dict[int, Dict[str, List[Dict[str, Any]]]] = collections.defaultdict(
        lambda: collections.defaultdict(list)
    )

    sgrep_errors = output_json["errors"]

    for finding in sgrep_errors:
        print_error(f"sgrep: {finding['path']}: {finding['check_id']}")

    if strict and len(sgrep_errors):
        print_error_exit(
            f"run with --strict and {len(sgrep_errors)} errors occurred during sgrep run; exiting",
            INVALID_CODE_EXIT_CODE,
        )

    for finding in output_json["matches"]:
        rule_index = decode_rule_id_to_index(finding["check_id"])
        by_rule_index[rule_index][finding["path"]].append(finding)

    current_path = Path.cwd()
    outputs_after_booleans = []
    ignored_in_tests = 0
    fixes: List[Tuple[str, Dict[str, Any]]] = []

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
                expression,
                check_ids_to_ranges,
                flags={
                    RCE_RULE_FLAG: args.dangerously_allow_arbitrary_code_execution_from_rules
                },
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

                    # reproduce free-form metadata
                    result["extra"]["metadata"] = all_rules[rule_index].get(
                        "metadata", {}
                    )

                    # restore the original message
                    result["extra"]["message"] = rewrite_message_with_metavars(
                        all_rules[rule_index], result
                    )

                    # try to generate a fix
                    fix = generate_fix(all_rules[rule_index], result)
                    if fix:
                        result["extra"]["fix"] = fix
                        fixes.append((filepath, result))
                    result = transform_to_r2c_output(result)
                    outputs_after_booleans.append(result)

    if ignored_in_tests > 0:
        print_error(
            f"warning: ignored {ignored_in_tests} results in tests due to --exclude-tests option"
        )

    outputs_after_booleans = dedup_output(outputs_after_booleans)
    outputs_after_booleans = add_finding_line(outputs_after_booleans)
    outputs_after_booleans = clean_output(outputs_after_booleans)

    # output results
    output_data = {
        "results": outputs_after_booleans,
        "errors": r2c_error_format(sgrep_errors),
    }
    if not args.quiet:
        if args.json:
            print(build_output_json(output_data))
        else:
            if outputs_after_booleans:
                print("\n".join(build_normal_output(output_data, color_output=True)))
    if args.autofix and fixes:
        modified_files: Set[str] = set()
        for filepath, finding in fixes:
            try:
                modify_file(filepath, finding)
                modified_files.add(filepath)
            except Exception as e:
                print_error_exit(f"unable to modify file: {filepath}: {e}")
        num_modified = len(modified_files)
        print_msg(
            f"Successfully modified {num_modified} file{'s' if num_modified > 1 else ''}."
        )
    if args.output:
        save_output(args.output, output_data, args.json)
    if args.error and outputs_after_booleans:
        sys.exit(FINDINGS_EXIT_CODE)

    return output_data
