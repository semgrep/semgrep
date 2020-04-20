import collections
import json
import subprocess
import tempfile
from datetime import datetime
from pathlib import Path
from typing import Any
from typing import Dict
from typing import Iterator
from typing import List
from typing import Tuple

import yaml
from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.constants import SGREP_PATH
from semgrep.evaluation import enumerate_patterns_in_boolean_expression
from semgrep.evaluation import evaluate
from semgrep.rule import Rule
from semgrep.sgrep_types import BooleanRuleExpression
from semgrep.sgrep_types import OPERATORS
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

    def _decode_rule_id_to_index(self, rule_id: str) -> int:
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
        # a rule can have multiple patterns inside it. Flatten these so we can send sgrep a single yml file list of patterns
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

    def _run_rules(
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
            rule_index = self._decode_rule_id_to_index(finding["check_id"])
            rule = rules[rule_index]
            finding["check_id"] = ".".join(finding["check_id"].split(".")[1:])
            by_rule_index[rule][finding["path"]].append(finding)

        return by_rule_index, errors

    def _resolve_output(
        self, outputs: Dict[Rule, Dict[str, List[Dict[str, Any]]]],
    ) -> Dict[Rule, List[Dict[str, Any]]]:
        """
            Takes output of all running all patterns and rules and returns Findings
        """
        findings_by_rule: Dict[Rule, List[Dict[str, Any]]] = {}

        for rule, paths in outputs.items():
            findings = []
            for filepath, results in paths.items():
                debug_print(f"-------- rule ({rule.id} ------ filepath: {filepath}")

                findings.extend(evaluate(rule, results, self._allow_exec))

            # todo dedup this
            # Brendon figure this out in the morning
            findings_by_rule[rule] = findings

        # ignored_in_tests = 0
        # if ignored_in_tests > 0:
        #     print_error(
        #         f"warning: ignored {ignored_in_tests} results in tests due to --exclude-tests option"
        #     )

        return findings_by_rule

    def invoke_sgrep(
        self, targets: List[Path], rules: List[Rule],
    ) -> Tuple[Dict[Rule, List[Dict[str, Any]]], List[Any]]:
        """
            Takes in rules and targets and retuns object with findings
        """
        start = datetime.now()

        outputs, errors = self._run_rules(rules, targets)
        findings_by_rule = self._resolve_output(outputs)

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


def should_exclude_this_path(path: Path) -> bool:
    return any("test" in p or "example" in p for p in path.parts)
