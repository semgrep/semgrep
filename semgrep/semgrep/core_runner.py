import collections
import functools
import json
import logging
import re
import subprocess
import tempfile
from datetime import datetime
from multiprocessing import pool
from pathlib import Path
from typing import Any
from typing import cast
from typing import Dict
from typing import IO
from typing import Iterator
from typing import List
from typing import Optional
from typing import Pattern as TPattern
from typing import Sequence
from typing import Set
from typing import Tuple

from ruamel.yaml import YAML

from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.core_exception import CoreException
from semgrep.equivalences import Equivalence
from semgrep.error import _UnknownLanguageError
from semgrep.error import InvalidPatternError
from semgrep.error import MatchTimeoutError
from semgrep.error import SemgrepError
from semgrep.error import UnknownLanguageError
from semgrep.evaluation import enumerate_patterns_in_boolean_expression
from semgrep.evaluation import evaluate
from semgrep.output import OutputSettings
from semgrep.pattern import Pattern
from semgrep.pattern_match import PatternMatch
from semgrep.profile_manager import ProfileManager
from semgrep.profiling import ProfilingData
from semgrep.profiling import Times
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.semgrep_types import BooleanRuleExpression
from semgrep.semgrep_types import Language
from semgrep.semgrep_types import OPERATORS
from semgrep.semgrep_types import TAINT_MODE
from semgrep.spacegrep import run_spacegrep
from semgrep.target_manager import TargetManager
from semgrep.target_manager_extensions import all_supported_languages
from semgrep.util import debug_tqdm_write
from semgrep.util import partition
from semgrep.util import progress_bar
from semgrep.util import SEMGREP_PATH
from semgrep.util import sub_run

logger = logging.getLogger(__name__)


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


def get_re_matches(
    patterns_re: Sequence[Tuple[Any, TPattern[Any]]], path: Path
) -> List[PatternMatch]:
    try:
        contents = path.read_text()
    except UnicodeDecodeError:
        logger.debug(f"regex matcher skipping binary file at {path}")
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
                "extra": {},
            }
        )
        for pattern_id, pattern in patterns_re
        for match in re.finditer(pattern, contents)
    ]


class CoreRunner:
    """
    Handles interactions between semgrep and semgrep-core

    This includes properly invoking semgrep-core and parsing the output
    """

    def __init__(
        self,
        output_settings: OutputSettings,
        allow_exec: bool,
        jobs: int,
        timeout: int,
        max_memory: int,
        timeout_threshold: int,
        report_time: bool,
    ):
        self._output_settings = output_settings
        self._allow_exec = allow_exec
        self._jobs = jobs
        self._timeout = timeout
        self._max_memory = max_memory
        self._timeout_threshold = timeout_threshold
        self._report_time = report_time

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

                for lang in rule.languages:
                    yield Pattern(rule_index, expr, rule.severity, lang, span)

    def _group_patterns_by_language(self, rule: Rule) -> Dict[Language, List[Pattern]]:

        by_lang: Dict[Language, List[Pattern]] = collections.defaultdict(list)
        if rule.mode == TAINT_MODE:
            for lang in rule.languages:
                by_lang[lang] = []
        else:
            # a rule can have multiple patterns inside it. Flatten these so we can send semgrep a single yml file list of patterns
            patterns: List[Pattern] = list(self._flatten_rule_patterns([rule]))
            for pattern in patterns:
                by_lang[pattern.language].append(pattern)
        return by_lang

    def _raise_semgrep_error_from_json(
        self,
        error_json: Dict[str, Any],
        patterns: List[Pattern],
        rule: Rule,
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
                long_msg=f"Pattern could not be parsed as a {error_json['language']} semgrep pattern",
                spans=[matching_span],
                help=None,
            )
        # no special formatting ought to be required for the other types; the semgrep python should be performing
        # validation for them. So if any other type of error occurs, ask the user to file an issue
        else:
            raise SemgrepError(
                f"an internal error occured while invoking semgrep-core while running rule '{rule.id}'. Consider skipping this rule and reporting this issue.\n\t{error_type}: {error_json.get('message', 'no message')}\n{PLEASE_FILE_ISSUE_TEXT}"
            )

    def _write_equivalences_file(self, fp: IO, equivalences: List[Equivalence]) -> None:
        # I don't even know why this is a thing.
        # cf. https://stackoverflow.com/questions/51272814/python-yaml-dumping-pointer-references
        yaml = YAML()
        yaml.representer.ignore_aliases = lambda *data: True
        yaml.dump({"equivalences": [e.to_json() for e in equivalences]}, fp)
        fp.flush()

    def _run_core_command(
        self,
        patterns_json: List[Any],
        patterns: List[Pattern],
        targets: List[Path],
        language: Language,
        rule: Rule,
        rules_file_flag: str,
        cache_dir: str,
    ) -> dict:
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
                language.value,
                "-json",
                rules_file_flag,
                pattern_file.name,
                "-j",
                str(self._jobs),
                "-target_file",
                target_file.name,
                "-use_parsing_cache",
                cache_dir,
                "-timeout",
                str(self._timeout),
                "-max_memory",
                str(self._max_memory),
            ]

            equivalences = rule.equivalences
            if equivalences:
                self._write_equivalences_file(equiv_file, equivalences)
                cmd += ["-equivalences", equiv_file.name]

            if self._report_time:
                cmd += ["-json_time"]

            if self._output_settings.debug:
                cmd += ["-debug"]

            core_run = sub_run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            output_json = self._extract_core_output(rule, patterns, core_run)
            return output_json

    def _extract_core_output(
        self, rule: Rule, patterns: List[Pattern], core_run: subprocess.CompletedProcess
    ) -> Dict[str, Any]:
        semgrep_output = core_run.stdout.decode("utf-8", errors="replace")
        semgrep_error_output = core_run.stderr.decode("utf-8", errors="replace")

        # By default, we print semgrep-core's error output, which includes
        # semgrep-core's logging if it was requested via --debug.
        #
        # If semgrep-core prints anything on stderr when running with default
        # flags, it's a bug that should be fixed in semgrep-core.
        #
        if semgrep_error_output != "":
            name = f"[rule '{rule.id}']"
            logger.info(
                f"--- semgrep-core stderr {name} ---\n"
                f"{semgrep_error_output}"
                f"--- end semgrep-core stderr {name} ---"
            )

        returncode = core_run.returncode
        if returncode != 0:
            output_json = self._parse_core_output(
                rule, core_run, semgrep_output, semgrep_error_output, returncode
            )

            if "error" in output_json:
                self._raise_semgrep_error_from_json(output_json, patterns, rule)
            else:
                self._fail(
                    'non-zero exit status with missing "error" field in json response',
                    rule,
                    core_run,
                    returncode,
                    semgrep_output,
                    semgrep_error_output,
                )

        output_json = self._parse_core_output(
            rule, core_run, semgrep_output, semgrep_error_output, returncode
        )
        return output_json

    def _parse_core_output(
        self,
        rule: Rule,
        core_run: subprocess.CompletedProcess,
        semgrep_output: str,
        semgrep_error_output: str,
        returncode: int,
    ) -> Dict[str, Any]:
        # See if semgrep output contains a JSON error that we can decode.
        try:
            return cast(Dict[str, Any], json.loads(semgrep_output))
        except ValueError:
            self._fail(
                "unparseable json output",
                rule,
                core_run,
                returncode,
                semgrep_output,
                semgrep_error_output,
            )
            return {}  # never reached

    def _fail(
        self,
        reason: str,
        rule: Rule,
        core_run: subprocess.CompletedProcess,
        returncode: int,
        semgrep_output: str,
        semgrep_error_output: str,
    ) -> None:
        # Once we require python >= 3.8, switch to using shlex.join instead
        # for proper quoting of the command line.
        shell_command = " ".join(core_run.args)
        raise SemgrepError(
            f"semgrep-core failed: {reason}\n"
            f"rule ID: '{rule.id}'\n"
            f"semgrep-core exit code: {returncode}\n"
            f"semgrep-core command: {shell_command}\n"
            f"unexpected non-json output while invoking semgrep-core:\n"
            "--- semgrep-core stdout ---\n"
            f"{semgrep_output}"
            "--- end semgrep-core stdout ---\n"
            "--- semgrep-core stderr ---\n"
            f"{semgrep_error_output}"
            "--- end semgrep-core stderr ---\n"
            f"{PLEASE_FILE_ISSUE_TEXT}"
        )

    def _add_match_times(
        self,
        rule: Rule,
        profiling_data: ProfilingData,
        output_time_json: Dict[str, Any],
    ) -> None:
        """Collect the match times reported by semgrep-core (or spacegrep)."""
        if "targets" in output_time_json:
            for target in output_time_json["targets"]:
                if "match_time" in target and "path" in target:
                    profiling_data.set_run_times(
                        rule.id,
                        target["path"],
                        Times(
                            parse_time=target["parse_time"],
                            match_time=target["match_time"],
                            run_time=target["run_time"],
                        ),
                    )
        if "rule_parse_time" in output_time_json:
            profiling_data.set_parse_time(rule.id, output_time_json["rule_parse_time"])

    def _run_rule(
        self,
        rule: Rule,
        target_manager: TargetManager,
        cache_dir: str,
        max_timeout_files: List[Path],
        profiler: ProfileManager,
        profiling_data: ProfilingData,
    ) -> Tuple[List[RuleMatch], List[Dict[str, Any]], List[SemgrepError], Set[Path]]:
        """
        Run all rules on targets and return list of all places that match patterns, ... todo errors
        """
        outputs: List[PatternMatch] = []  # multiple invocations per language
        errors: List[SemgrepError] = []
        all_targets: Set[Path] = set()

        for language, all_patterns_for_language in self._group_patterns_by_language(
            rule
        ).items():

            targets = self.get_files_for_language(language, rule, target_manager)
            targets = [target for target in targets if target not in max_timeout_files]
            all_targets = all_targets.union(targets)
            if not targets:
                continue

            if rule.mode == TAINT_MODE:
                pattern_json = rule._raw.copy()
                del pattern_json["mode"]
                pattern = Pattern(
                    0, rule.expression, rule.severity, language, rule._yaml.span
                )

                output_json = profiler.track(
                    rule.id,
                    self._run_core_command,
                    [pattern_json],
                    [pattern],
                    targets,
                    language,
                    rule,
                    "-tainting_rules_file",
                    cache_dir,
                )
            else:
                # semgrep-core doesn't know about OPERATORS.REGEX - this is
                # strictly a semgrep Python feature. Regex filtering is
                # performed purely in Python code then compared against
                # semgrep-core's results for other patterns.
                patterns_regex, patterns = partition(
                    lambda p: p.expression.operator == OPERATORS.REGEX
                    or p.expression.operator == OPERATORS.NOT_REGEX,
                    all_patterns_for_language,
                )
                if patterns_regex:
                    self.handle_regex_patterns(outputs, patterns_regex, targets)

                # regex-only rules only support OPERATORS.REGEX.
                # Skip passing this rule to semgrep-core.
                if language == Language.REGEX:
                    continue

                # semgrep-core doesn't know about the following operators -
                # they are strictly semgrep Python features:
                #   - OPERATORS.METAVARIABLE_REGEX
                #   - OPERATORS.METAVARIABLE_COMPARISON
                patterns = [
                    pattern
                    for pattern in patterns
                    if pattern.expression.operator
                    not in [
                        OPERATORS.METAVARIABLE_REGEX,
                        OPERATORS.METAVARIABLE_COMPARISON,
                    ]
                ]

                patterns_json = [p.to_json() for p in patterns]

                if language == Language.GENERIC:
                    output_json = profiler.track(
                        rule.id,
                        run_spacegrep,
                        rule.id,
                        patterns,
                        targets,
                        timeout=self._timeout,
                        report_time=self._report_time,
                    )
                else:  # Run semgrep-core
                    output_json = profiler.track(
                        rule.id,
                        self._run_core_command,
                        patterns_json,
                        patterns,
                        targets,
                        language,
                        rule,
                        "-rules_file",
                        cache_dir,
                    )

            errors.extend(
                CoreException.from_json(e, language.value, rule.id).into_semgrep_error()
                for e in output_json["errors"]
            )
            outputs.extend(PatternMatch(m) for m in output_json["matches"])
            if "time" in output_json:
                self._add_match_times(rule, profiling_data, output_json["time"])

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
                logger.debug(
                    f"--> rule ({rule.id}) has findings on filepath: {filepath}"
                )

                findings_for_rule, debugging_steps = evaluate(
                    rule, pattern_matches, self._allow_exec
                )
                findings.extend(findings_for_rule)

        findings = dedup_output(findings)
        logger.debug(f"...ran on {len(all_targets)} files")

        # debugging steps are only tracked for a single file, just overwrite
        return findings, debugging_steps, errors, all_targets

    def handle_regex_patterns(
        self,
        outputs: List[PatternMatch],
        patterns_regex: List[Any],
        targets: List[Path],
    ) -> None:
        patterns_json = [pattern.to_json() for pattern in patterns_regex]
        try:
            patterns_re = [
                (pattern["id"], re.compile(pattern["pattern"]))
                for pattern in patterns_json
            ]
        except re.error as err:
            raise SemgrepError(f"invalid regular expression specified: {err}")

        re_fn = functools.partial(get_re_matches, patterns_re)
        with pool.ThreadPool(self._jobs) as tpool:
            matches = tpool.map(re_fn, targets)

        outputs.extend(
            single_match for file_matches in matches for single_match in file_matches
        )

    @staticmethod
    def get_files_for_language(
        language: Language, rule: Rule, target_manager: TargetManager
    ) -> List[Path]:
        try:
            targets = target_manager.get_files(language, rule.includes, rule.excludes)
        except _UnknownLanguageError as ex:
            raise UnknownLanguageError(
                short_msg=f"invalid language: {language}",
                long_msg=f"unsupported language: {language}. supported languages are: {', '.join(all_supported_languages())}",
                spans=[rule.languages_span.with_context(before=1, after=1)],
            ) from ex
        return targets

    def _run_rules(
        self, rules: List[Rule], target_manager: TargetManager, profiler: ProfileManager
    ) -> Tuple[
        Dict[Rule, List[RuleMatch]],
        Dict[Rule, List[Dict[str, Any]]],
        List[SemgrepError],
        Set[Path],  # targets
        ProfilingData,  # match time for each (rule, target)
    ]:
        findings_by_rule: Dict[Rule, List[RuleMatch]] = {}
        debugging_steps_by_rule: Dict[Rule, List[Dict[str, Any]]] = {}
        all_errors: List[SemgrepError] = []
        file_timeouts: Dict[Path, int] = collections.defaultdict(lambda: 0)
        max_timeout_files: List[Path] = []
        all_targets: Set[Path] = set()
        profiling_data: ProfilingData = ProfilingData()

        # cf. for bar_format: https://tqdm.github.io/docs/tqdm/
        with tempfile.TemporaryDirectory() as semgrep_core_ast_cache_dir:
            for rule in progress_bar(
                rules, bar_format="{l_bar}{bar}|{n_fmt}/{total_fmt}"
            ):
                debug_tqdm_write(f"Running rule {rule._raw.get('id')}...")
                rule_matches, debugging_steps, errors, rule_targets = self._run_rule(
                    rule,
                    target_manager,
                    semgrep_core_ast_cache_dir,
                    max_timeout_files,
                    profiler,
                    profiling_data,
                )
                all_targets = all_targets.union(rule_targets)
                findings_by_rule[rule] = rule_matches
                debugging_steps_by_rule[rule] = debugging_steps
                all_errors.extend(errors)
                for err in errors:
                    if isinstance(err, MatchTimeoutError):
                        file_timeouts[err.path] += 1
                        if (
                            self._timeout_threshold != 0
                            and file_timeouts[err.path] >= self._timeout_threshold
                        ):
                            max_timeout_files.append(err.path)

        all_errors = dedup_errors(all_errors)
        return (
            findings_by_rule,
            debugging_steps_by_rule,
            all_errors,
            all_targets,
            profiling_data,
        )

    def _run_rules_direct_to_semgrep_core(
        self,
        rules: List[Rule],
        target_manager: TargetManager,
        profiler: ProfileManager,
    ) -> Tuple[
        Dict[Rule, List[RuleMatch]],
        Dict[Rule, List[Any]],
        List[SemgrepError],
        Set[Path],
        ProfilingData,
    ]:
        from itertools import chain
        from collections import defaultdict

        logger.debug(f"Passing whole rules directly to semgrep_core")

        outputs: Dict[Rule, List[RuleMatch]] = defaultdict(list)
        errors: List[SemgrepError] = []
        all_targets: Set[Path] = set()
        profiling_data: ProfilingData = ProfilingData()
        # cf. for bar_format: https://tqdm.github.io/docs/tqdm/
        with tempfile.TemporaryDirectory() as semgrep_core_ast_cache_dir:
            for rule, language in tuple(
                chain(
                    *(
                        [(rule, language) for language in rule.languages]
                        for rule in rules
                    )
                )
            ):
                debug_tqdm_write(f"Running rule {rule._raw.get('id')}...")
                with tempfile.NamedTemporaryFile(
                    "w", suffix=".yaml"
                ) as rule_file, tempfile.NamedTemporaryFile("w") as target_file:
                    targets = self.get_files_for_language(
                        language, rule, target_manager
                    )
                    # opti: no need to call semgrep-core if no target files
                    if not targets:
                        continue
                    all_targets = all_targets.union(targets)

                    target_file.write("\n".join(map(lambda p: str(p), targets)))
                    target_file.flush()
                    yaml = YAML()
                    yaml.dump({"rules": [rule._raw]}, rule_file)
                    rule_file.flush()

                    cmd = [SEMGREP_PATH] + [
                        "-lang",
                        language.value,
                        "-fast",
                        "-json",
                        "-config",
                        rule_file.name,
                        "-j",
                        str(self._jobs),
                        "-target_file",
                        target_file.name,
                        "-use_parsing_cache",
                        semgrep_core_ast_cache_dir,
                        "-timeout",
                        str(self._timeout),
                        "-max_memory",
                        str(self._max_memory),
                    ]

                    if self._report_time:
                        cmd += ["-json_time"]

                    if self._output_settings.debug:
                        cmd += ["-debug"]

                    core_run = sub_run(
                        cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE
                    )
                    output_json = self._extract_core_output(rule, [], core_run)

                    if "time" in output_json:
                        self._add_match_times(rule, profiling_data, output_json["time"])

                # end with tempfile.NamedTemporaryFile(...) ...
                findings = [
                    RuleMatch.from_pattern_match(
                        rule.id,
                        PatternMatch(pattern_match),
                        message=rule.message,
                        metadata=rule.metadata,
                        severity=rule.severity,
                        fix=rule.fix,
                        fix_regex=rule.fix_regex,
                    )
                    for pattern_match in output_json["matches"]
                ]
                # TODO: we should do that in Semgrep_generic.ml instead
                findings = dedup_output(findings)
                outputs[rule].extend(findings)
                errors.extend(
                    CoreException.from_json(
                        e, language.value, rule.id
                    ).into_semgrep_error()
                    for e in output_json["errors"]
                )
        # end for rule, language ...

        return outputs, {}, errors, all_targets, profiling_data

    # end _run_rules_direct_to_semgrep_core

    def invoke_semgrep(
        self,
        target_manager: TargetManager,
        profiler: ProfileManager,
        rules: List[Rule],
        optimizations: str,
    ) -> Tuple[
        Dict[Rule, List[RuleMatch]],
        Dict[Rule, List[Dict[str, Any]]],
        List[SemgrepError],
        Set[Path],
        ProfilingData,
    ]:
        """
        Takes in rules and targets and retuns object with findings
        """
        start = datetime.now()

        experimental = optimizations == "all"
        runner_fxn = (
            self._run_rules_direct_to_semgrep_core if experimental else self._run_rules
        )
        (
            findings_by_rule,
            debug_steps_by_rule,
            errors,
            all_targets,
            profiling_data,
        ) = runner_fxn(rules, target_manager, profiler)

        logger.debug(
            f"semgrep ran in {datetime.now() - start} on {len(all_targets)} files"
        )
        by_severity = collections.defaultdict(list)
        for rule, findings in findings_by_rule.items():
            by_severity[rule.severity.lower()].extend(findings)

        by_sev_strings = [
            f"{len(findings)} {sev}" for sev, findings in by_severity.items()
        ]
        logger.debug(f'findings summary: {", ".join(by_sev_strings)}')

        return (
            findings_by_rule,
            debug_steps_by_rule,
            errors,
            all_targets,
            profiling_data,
        )


# Note that this may remove matches that have the same range but different
# metavariable bindings, and arbitrarily choose the first one in the list.
# TODO: changes that? Do like in semgrep-core and return all the matches?
# Also now Semgrep.ml Semgrep_generic.ml internally do some dedup, so we
# may not need anymore to do it here.
def dedup_output(outputs: List[RuleMatch]) -> List[RuleMatch]:
    return list({uniq_id(r): r for r in outputs}.values())


def dedup_errors(errors: List[SemgrepError]) -> List[SemgrepError]:
    return list(set(errors))


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
