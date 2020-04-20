import collections
import json
import subprocess
import tempfile
from datetime import datetime
from pathlib import Path
from typing import Any
from typing import DefaultDict
from typing import Dict
from typing import Iterator
from typing import List
from typing import Tuple

import yaml
from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.constants import RCE_RULE_FLAG
from semgrep.constants import SGREP_PATH
from semgrep.evaluation import enumerate_patterns_in_boolean_expression
from semgrep.evaluation import evaluate_expression
from semgrep.rule import Rule
from semgrep.sgrep_types import BooleanRuleExpression
from semgrep.sgrep_types import OPERATORS
from semgrep.sgrep_types import PatternId
from semgrep.sgrep_types import Range
from semgrep.sgrep_types import SgrepRange
from semgrep.util import debug_print
from semgrep.util import INVALID_PATTERN_EXIT_CODE
from semgrep.util import print_error
from semgrep.util import print_error_exit

"""
Handle executing and parsing output of sgrep binary
"""


# Rename to CoreRunner
class SgrepBridge:
    def __init__(self, allow_exec: bool, exclude_tests: bool):
        self._allow_exec = allow_exec
        self._exclude_tests = exclude_tests

    def decode_rule_id_to_index(self, rule_id: str) -> int:
        # decode the rule index from the output check_id
        return int(rule_id.split(".")[0])

    def _flatten_rule_patterns(self, rules: List[Rule]) -> Iterator[Dict[str, Any]]:
        """
            Convert list of rules to format understandable by sgrep core
        """
        for rule_index, rule in enumerate(rules):
            flat_expressions = list(
                enumerate_patterns_in_boolean_expression(rule.expression)
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
                    "severity": rule.severity,
                    "languages": rule.languages.copy(),
                    "message": "<internalonly>",
                }

    def _group_rule_by_langauges(
        self, rules: List[Rule]
    ) -> Dict[str, List[Dict[str, Any]]]:
        patterns = list(self._flatten_rule_patterns(rules))
        by_lang: Dict[str, List[Dict[str, Any]]] = collections.defaultdict(list)
        for pattern in patterns:
            for language in pattern["languages"]:
                by_lang[language].append(pattern)
        return by_lang

    def _sgrep_error_json_to_message_then_exit(
        self, error_json: Dict[str, Any],
    ) -> None:
        """
        See format_output_exception in sgrep O'Caml for details on schema
        """
        error_type = error_json["error"]
        if error_type == "invalid language":
            print_error_exit(f'invalid language {error_json["language"]}')
        elif error_type == "invalid pattern":
            print_error(
                f'invalid pattern "{error_json["pattern"]}": {error_json["message"]}'
            )
            exit(INVALID_PATTERN_EXIT_CODE)
        # no special formatting ought to be required for the other types; the sgrep python should be performing
        # validation for them. So if any other type of error occurs, ask the user to file an issue
        else:
            print_error_exit(
                f'an internal error occured while invoking the sgrep engine: {error_type}: {error_json.get("message", "")}.\n\n{PLEASE_FILE_ISSUE_TEXT}'
            )

    def run_rules(
        self, rules: List[Rule], targets: List[Path]
    ) -> Tuple[Dict[Rule, Dict[str, List[Dict[str, Any]]]], List[Any]]:
        """
            Run all rules on targets and return list of all places that match patterns, ... todo errors
        """
        outputs: List[Any] = []  # multiple invocations per language
        errors: List[Any] = []

        for language, all_rules_for_language in self._group_rule_by_langauges(
            rules
        ).items():
            with tempfile.NamedTemporaryFile("w") as fout:
                # very important not to sort keys here
                yaml_as_str = yaml.safe_dump(
                    {"rules": all_rules_for_language}, sort_keys=False
                )
                fout.write(yaml_as_str)
                fout.flush()
                cmd = [SGREP_PATH] + [
                    "-lang",
                    language,
                    f"-rules_file",
                    fout.name,
                    *[str(path) for path in targets],
                ]
                try:
                    output = subprocess.check_output(cmd, shell=False)
                except subprocess.CalledProcessError as ex:
                    try:
                        # see if sgrep output a JSON error that we can decode
                        output_json = json.loads((ex.output.decode("utf-8", "replace")))
                        if "error" in output_json:
                            self._sgrep_error_json_to_message_then_exit(output_json)
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

                # group output; we want to see all of the same rule ids on the same file path
        by_rule_index: Dict[
            Rule, Dict[str, List[Dict[str, Any]]]
        ] = collections.defaultdict(lambda: collections.defaultdict(list))

        for finding in outputs:
            rule_index = self.decode_rule_id_to_index(finding["check_id"])
            rule = rules[rule_index]
            finding["check_id"] = ".".join(finding["check_id"].split(".")[1:])
            by_rule_index[rule][finding["path"]].append(finding)

        return by_rule_index, errors

    def resolve_rules(
        self,
        all_rules: List[Rule],
        outputs: Dict[Rule, Dict[str, List[Dict[str, Any]]]],
    ) -> Dict[Rule, List[Dict[str, Any]]]:
        """
            Takes output of all running all patterns and rules and returns Findings
        """
        current_path = Path.cwd()
        findings_by_rule: Dict[Rule, List[Dict[str, Any]]] = {}
        ignored_in_tests = 0

        for rule, paths in outputs.items():
            expression = rule.expression
            debug_print(str(expression))
            # expression = (op, pattern_id) for (op, pattern_id, pattern) in expression_with_patterns]
            for filepath, results in paths.items():
                debug_print(f"-------- rule ({rule.id} ------ filepath: {filepath}")
                check_ids_to_ranges = parse_sgrep_output(
                    results
                )  # TODO should run_rules handle this
                debug_print(str(check_ids_to_ranges))
                valid_ranges_to_output = evaluate_expression(
                    expression,
                    check_ids_to_ranges,
                    flags={RCE_RULE_FLAG: self._allow_exec},
                )

                # only output matches which are inside these offsets!
                debug_print(f"compiled result {valid_ranges_to_output}")
                debug_print("-" * 80)
                for result in results:
                    if sgrep_finding_to_range(result).range in valid_ranges_to_output:
                        path_object = Path(result["path"])
                        if self._exclude_tests and should_exclude_this_path(
                            path_object
                        ):
                            ignored_in_tests += 1
                            continue

                        # restore the original rule ID
                        result["check_id"] = rule.id
                        # rewrite the path to be relative to the current working directory
                        result["path"] = str(
                            safe_relative_to(path_object, current_path)
                        )

                        # restore the original message
                        result["extra"]["message"] = rewrite_message_with_metavars(
                            rule, result
                        )

                        result = transform_to_r2c_output(result)
                        # todo dedup this
                        findings_by_rule.setdefault(rule, []).append(result)

        if ignored_in_tests > 0:
            print_error(
                f"warning: ignored {ignored_in_tests} results in tests due to --exclude-tests option"
            )

        return findings_by_rule

    def invoke_sgrep(
        self, targets: List[Path], output_mode_json: bool, all_rules: List[Rule],
    ) -> Tuple[Dict[Rule, List[Dict[str, Any]]], List[Any]]:
        """
            Takes in rules and targets and retuns object with findings
        """
        # a rule can have multiple patterns inside it. Flatten these so we can send sgrep a single yml file list of patterns
        start = datetime.now()

        outputs, errors = self.run_rules(all_rules, targets)
        findings_by_rule = self.resolve_rules(all_rules, outputs)

        debug_print(f"sgrep ran in {datetime.now() - start}")

        return findings_by_rule, errors


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


def should_send_to_sgrep(expression: BooleanRuleExpression) -> bool:
    """
    don't send rules like "and-either" or "and-all" to sgrep
    """
    return (
        expression.pattern_id is not None
        and expression.operand is not None
        and (expression.operator != OPERATORS.WHERE_PYTHON)
    )


def parse_sgrep_output(
    sgrep_findings: List[Dict[str, Any]]
) -> Dict[PatternId, List[SgrepRange]]:
    output: DefaultDict[PatternId, List[SgrepRange]] = collections.defaultdict(list)
    for finding in sgrep_findings:
        check_id = finding["check_id"]
        pattern_id = PatternId(check_id)
        output[pattern_id].append(sgrep_finding_to_range(finding))
    return dict(output)


def transform_to_r2c_output(finding: Dict[str, Any]) -> Dict[str, Any]:
    # https://docs.r2c.dev/en/latest/api/output.html does not support offset at the moment
    if "offset" in finding["start"]:
        del finding["start"]["offset"]
    if "offset" in finding["end"]:
        del finding["end"]["offset"]
    return finding


def safe_relative_to(a: Path, b: Path) -> Path:
    try:
        return a.relative_to(b)
    except ValueError:
        # paths had no common prefix; not possible to relativize
        return a


def should_exclude_this_path(path: Path) -> bool:
    return any("test" in p or "example" in p for p in path.parts)


def sgrep_finding_to_range(sgrep_finding: Dict[str, Any]) -> SgrepRange:
    metavars = sgrep_finding["extra"]["metavars"]
    return SgrepRange(
        Range(sgrep_finding["start"]["offset"], sgrep_finding["end"]["offset"]),
        {k: v["abstract_content"] for k, v in metavars.items()},
    )


def rewrite_message_with_metavars(rule: Rule, sgrep_result: Dict[str, Any]) -> str:
    msg_text = rule.message
    if "metavars" in sgrep_result["extra"]:
        for metavar, contents in sgrep_result["extra"]["metavars"].items():
            msg_text = msg_text.replace(metavar, contents["abstract_content"])
    return msg_text
