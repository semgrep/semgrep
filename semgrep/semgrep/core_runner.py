import collections
import functools
import json
import multiprocessing
import re
import subprocess
import tempfile
from datetime import datetime
from pathlib import Path
from typing import Any
from typing import Dict
from typing import IO
from typing import Iterator
from typing import List
from typing import Optional
from typing import Tuple

from ruamel.yaml import YAML

from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.constants import SEMGREP_PATH
from semgrep.equivalences import Equivalence
from semgrep.error import _UnknownLanguageError
from semgrep.error import InvalidPatternError
from semgrep.error import SemgrepError
from semgrep.error import UnknownLanguageError
from semgrep.evaluation import enumerate_patterns_in_boolean_expression
from semgrep.evaluation import evaluate
from semgrep.pattern import Pattern
from semgrep.pattern_match import PatternMatch
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.semgrep_types import BooleanRuleExpression
from semgrep.semgrep_types import OPERATORS
from semgrep.target_manager import TargetManager
from semgrep.util import debug_print
from semgrep.util import debug_tqdm_write
from semgrep.util import partition
from semgrep.util import progress_bar


def _offset_to_line_no(offset: int, buff: str) -> int:
    """
        Given string buffer return one indexed line number associated with byte offset
    """
    return buff.count("\n", 0, offset) + 1


def _offset_to_col_no(offset: int, buff: str) -> int:
    """
        Return one indexed col number associated with byte offset
    """
    return offset - buff.rfind("\n", 0, offset)


def get_re_matches(patterns_re: List[Tuple], path: Path) -> List[PatternMatch]:
    try:
        contents = path.read_text()
    except UnicodeDecodeError:
        debug_print(f"regex matcher skipping binary file at {path}")
        return []

    return [
        PatternMatch(
            {
                "check_id": pattern_id,
                "path": str(path),
                "start": {
                    "offset": match.start(),
                    "line": _offset_to_line_no(match.start(), contents),
                    "col": _offset_to_col_no(match.start(), contents),
                },
                "end": {
                    "offset": match.end(),
                    "line": _offset_to_line_no(match.end(), contents),
                    "col": _offset_to_col_no(match.end(), contents),
                },
                "extra": {"lines": [contents[match.start() : match.end()]]},
            }
        )
        for pattern_id, pattern in patterns_re
        for match in re.finditer(pattern, contents)
    ]


def get_target_files(
    targets: List[Path], exclude: List[str], include: List[str]
) -> List[Path]:
    if not include:
        # Default to all files
        include = ["*"]

    filepaths = [
        target
        for target in targets
        if target.is_file()
        and any(target.match(i) for i in include)
        and not any(target.match(e) for e in exclude)
    ]
    filepaths.extend(
        path
        for target in targets
        if target.is_dir()
        for path in target.rglob("*")
        if path.is_file()
        and any(path.match(i) for i in include)
        and not any(path.match(e) for e in exclude)
    )

    return filepaths


class CoreRunner:
    """
        Handles interactions between semgrep and semgrep-core

        This includes properly invoking semgrep-core and parsing the output
    """

    def __init__(self, allow_exec: bool, jobs: int):
        self._allow_exec = allow_exec
        self._jobs = jobs

    def _flatten_rule_patterns(self, rules: List[Rule]) -> Iterator[Pattern]:
        """
            Convert list of rules to format understandable by semgrep core
        """
        for rule_index, rule in enumerate(rules):
            flat_expressions = list(
                enumerate_patterns_in_boolean_expression(rule.expression)
            )
            for expr in flat_expressions:
                if not should_send_to_semgrep_core(expr):
                    continue

                span = (
                    rule.pattern_spans.get(expr.pattern_id)
                    if expr.pattern_id is not None
                    else None
                )

                yield Pattern(
                    rule_index, expr, rule.severity, rule.languages, span,
                )

    def _group_patterns_by_language(
        self, rules: List[Rule]
    ) -> Dict[str, List[Pattern]]:
        # a rule can have multiple patterns inside it. Flatten these so we can send semgrep a single yml file list of patterns
        patterns: List[Pattern] = list(self._flatten_rule_patterns(rules))
        by_lang: Dict[str, List[Pattern]] = collections.defaultdict(list)
        for pattern in patterns:
            for language in pattern.languages:
                by_lang[language].append(pattern)
        return by_lang

    def _semgrep_error_json_to_message_then_exit(
        self, error_json: Dict[str, Any], patterns: List[Pattern],
    ) -> None:
        """
        See format_output_exception in semgrep O'Caml for details on schema
        """
        error_type = error_json["error"]
        if error_type == "invalid language":
            raise SemgrepError(
                f'{error_json["language"]} was accepted by semgrep but rejected by semgrep-core. {PLEASE_FILE_ISSUE_TEXT}'
            )
        elif error_type == "invalid pattern":

            matching_pattern = next(
                (p for p in patterns if p._id == error_json["pattern_id"]), None
            )
            if matching_pattern is None or matching_pattern.span is None:
                raise SemgrepError(
                    f"Pattern id from semgrep-core was missing in pattern spans. {PLEASE_FILE_ISSUE_TEXT}"
                )
            matching_span = matching_pattern.span

            raise InvalidPatternError(
                short_msg=error_type,
                long_msg=f"Pattern could not be parsed as {error_json['language']}",
                spans=[matching_span],
                level="error",
                help=None,
            )
        # no special formatting ought to be required for the other types; the semgrep python should be performing
        # validation for them. So if any other type of error occurs, ask the user to file an issue
        else:
            raise SemgrepError(
                f'an internal error occured while invoking semgrep-core:\n\t{error_type}: {error_json.get("message", "no message")}\n{PLEASE_FILE_ISSUE_TEXT}'
            )

    def _flatten_all_equivalences(self, rules: List[Rule]) -> List[Equivalence]:
        """
        Convert all the equivalences defined in the rules into a single rule file
        """

        equivalences = []

        for rule in rules:
            try:
                equivalences.extend(rule.equivalences)
            except Exception as e:
                raise SemgrepError(
                    f"could not get equivalences for rule {rule.id}: {e}"
                )

        return equivalences

    def _write_equivalences_file(self, fp: IO, equivalences: List[Equivalence]) -> None:
        # I don't even know why this is a thing.
        # cf. https://stackoverflow.com/questions/51272814/python-yaml-dumping-pointer-references
        yaml = YAML()
        yaml.representer.ignore_aliases = lambda *data: True
        yaml.dump({"equivalences": [e.to_json() for e in equivalences]}, fp)
        fp.flush()

    def _run_rule(
        self, rule: Rule, target_manager: TargetManager, cache_dir: str
    ) -> Tuple[List[RuleMatch], List[Dict[str, Any]], List[Any]]:
        """
            Run all rules on targets and return list of all places that match patterns, ... todo errors
        """
        outputs: List[PatternMatch] = []  # multiple invocations per language
        errors: List[Any] = []
        equivalences = rule.equivalences

        for language, all_patterns_for_language in self._group_patterns_by_language(
            [rule]
        ).items():
            try:
                targets = target_manager.get_files(
                    language, rule.includes, rule.excludes
                )
            except _UnknownLanguageError as ex:
                raise UnknownLanguageError(
                    short_msg="invalid language",
                    long_msg=f"unsupported language {language}",
                    spans=[rule.languages_span.with_context(before=1, after=1)],
                    level="error",
                ) from ex

            if targets == []:
                continue

            # semgrep-core doesn't know about OPERATORS.REGEX - this is
            # strictly a semgrep Python feature. Regex filtering is
            # performed purely in Python code then compared against
            # semgrep-core's results for other patterns.
            patterns_regex, patterns = partition(
                lambda p: p.expression.operator == OPERATORS.REGEX,
                all_patterns_for_language,
            )
            if patterns_regex:
                patterns_json = [pattern.to_json() for pattern in patterns_regex]

                try:
                    patterns_re = [
                        (pattern["id"], re.compile(pattern["pattern"]))
                        for pattern in patterns_json
                    ]
                except re.error as err:
                    raise SemgrepError(f"invalid regular expression specified: {err}")

                re_fn = functools.partial(get_re_matches, patterns_re)
                with multiprocessing.Pool(self._jobs) as pool:
                    matches = pool.map(re_fn, targets)

                outputs.extend(
                    single_match
                    for file_matches in matches
                    for single_match in file_matches
                )

            patterns_json = [p.to_json() for p in patterns]
            with tempfile.NamedTemporaryFile(
                "w"
            ) as pattern_file, tempfile.NamedTemporaryFile(
                "w"
            ) as target_file, tempfile.NamedTemporaryFile(
                "w"
            ) as equiv_file:
                yaml = YAML()
                yaml.dump({"rules": patterns_json}, pattern_file)
                pattern_file.flush()
                target_file.write("\n".join(str(t) for t in targets))
                target_file.flush()

                cmd = [SEMGREP_PATH] + [
                    "-lang",
                    language,
                    "-rules_file",
                    pattern_file.name,
                    "-j",
                    str(self._jobs),
                    "-target_file",
                    target_file.name,
                    "-use_parsing_cache",
                    cache_dir,
                ]

                if equivalences:
                    self._write_equivalences_file(equiv_file, equivalences)
                    cmd += ["-equivalences", equiv_file.name]

                core_run = subprocess.run(
                    cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE
                )

                debug_print(core_run.stderr.decode("utf-8", "replace"))

                if core_run.returncode != 0:
                    # see if semgrep output a JSON error that we can decode
                    semgrep_output = core_run.stdout.decode("utf-8", "replace")
                    try:
                        output_json = json.loads(semgrep_output)
                    except ValueError:
                        raise SemgrepError(
                            f"unexpected non-json output while invoking semgrep-core:\n{PLEASE_FILE_ISSUE_TEXT}"
                        )

                    if "error" in output_json:
                        self._semgrep_error_json_to_message_then_exit(
                            output_json, patterns
                        )
                    else:
                        raise SemgrepError(
                            f"unexpected json output while invoking semgrep-core:\n{PLEASE_FILE_ISSUE_TEXT}"
                        )

                output_json = json.loads((core_run.stdout.decode("utf-8", "replace")))
                errors.extend(output_json["errors"])
                outputs.extend(PatternMatch(m) for m in output_json["matches"])

        # group output; we want to see all of the same rule ids on the same file path
        by_rule_index: Dict[
            Rule, Dict[Path, List[PatternMatch]]
        ] = collections.defaultdict(lambda: collections.defaultdict(list))

        for pattern_match in outputs:
            by_rule_index[rule][pattern_match.path].append(pattern_match)

        findings = []
        debugging_steps: List[Any] = []
        for rule, paths in by_rule_index.items():
            for filepath, pattern_matches in paths.items():
                debug_print(f"----- rule ({rule.id}) ----- filepath: {filepath}")

                findings_for_rule, debugging_steps = evaluate(
                    rule, pattern_matches, self._allow_exec
                )
                findings.extend(findings_for_rule)

        findings = dedup_output(findings)

        # debugging steps are only tracked for a single file, just overwrite
        return findings, debugging_steps, errors

    def _run_rules(
        self, rules: List[Rule], target_manager: TargetManager
    ) -> Tuple[
        Dict[Rule, List[RuleMatch]], Dict[Rule, List[Dict[str, Any]]], List[Any]
    ]:
        findings_by_rule: Dict[Rule, List[RuleMatch]] = {}
        debugging_steps_by_rule: Dict[Rule, List[Dict[str, Any]]] = {}
        all_errors = []

        # cf. for bar_format: https://tqdm.github.io/docs/tqdm/
        with tempfile.TemporaryDirectory() as semgrep_core_ast_cache_dir:
            for rule in progress_bar(
                rules, bar_format="{l_bar}{bar}|{n_fmt}/{total_fmt}"
            ):
                debug_tqdm_write(f"Running rule {rule._raw.get('id')}")
                rule_matches, debugging_steps, errors = self._run_rule(
                    rule, target_manager, semgrep_core_ast_cache_dir
                )
                findings_by_rule[rule] = rule_matches
                debugging_steps_by_rule[rule] = debugging_steps
                all_errors.extend(errors)

        all_errors = dedup_errors(all_errors)
        return findings_by_rule, debugging_steps_by_rule, all_errors

    def _resolve_output(
        self, outputs: Dict[Rule, Dict[Path, List[PatternMatch]]]
    ) -> Tuple[Dict[Rule, List[RuleMatch]], Dict[Rule, List[Dict[str, Any]]]]:
        """
            Takes output of all running all patterns and rules and returns Findings
        """
        findings_by_rule: Dict[Rule, List[RuleMatch]] = {}
        debugging_steps_by_rule: Dict[Rule, List[Dict[str, Any]]] = {}

        return findings_by_rule, debugging_steps_by_rule

    def invoke_semgrep(
        self, target_manager: TargetManager, rules: List[Rule]
    ) -> Tuple[
        Dict[Rule, List[RuleMatch]], Dict[Rule, List[Dict[str, Any]]], List[Any]
    ]:
        """
            Takes in rules and targets and retuns object with findings
        """
        start = datetime.now()

        findings_by_rule, debug_steps_by_rule, errors = self._run_rules(
            rules, target_manager
        )

        debug_print(f"semgrep ran in {datetime.now() - start}")

        return findings_by_rule, debug_steps_by_rule, errors


def dedup_output(outputs: List[RuleMatch]) -> List[RuleMatch]:
    return list({uniq_id(r): r for r in outputs}.values())


def dedup_errors(errors: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    def uniq_error_id(
        error: Dict[str, Any]
    ) -> Tuple[str, str, Optional[int], Optional[int], Optional[int], Optional[int]]:
        start = error.get("start", {})
        end = error.get("end", {})
        return (
            error.get("check_id"),  # type: ignore
            error.get("path"),
            start.get("line"),
            start.get("col"),
            end.get("line"),
            end.get("col"),
        )

    return list({uniq_error_id(r): r for r in errors}.values())


def uniq_id(
    r: RuleMatch,
) -> Tuple[str, Path, Optional[int], Optional[int], Optional[int], Optional[int]]:
    start = r.start
    end = r.end
    return (
        r.id,
        r.path,
        start.get("line"),
        start.get("col"),
        end.get("line"),
        end.get("col"),
    )


def should_send_to_semgrep_core(expression: BooleanRuleExpression) -> bool:
    """
    don't send rules like "and-either" or "and-all" to semgrep
    """
    return (
        expression.pattern_id is not None
        and expression.operand is not None
        and (expression.operator != OPERATORS.WHERE_PYTHON)
    )
