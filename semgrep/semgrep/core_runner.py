import collections
import json
import subprocess
import tempfile
from datetime import datetime
from pathlib import Path
from typing import Any
from typing import cast
from typing import Dict
from typing import List
from typing import Optional
from typing import Set
from typing import Tuple

from ruamel.yaml import YAML

from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.core_exception import CoreException
from semgrep.error import _UnknownLanguageError
from semgrep.error import InvalidPatternError
from semgrep.error import MatchTimeoutError
from semgrep.error import SemgrepError
from semgrep.error import UnknownLanguageError
from semgrep.evaluation import create_output
from semgrep.pattern_match import PatternMatch
from semgrep.profile_manager import ProfileManager
from semgrep.profiling import ProfilingData
from semgrep.profiling import Times
from semgrep.rule import Rule
from semgrep.rule_lang import Span
from semgrep.rule_match import RuleMatch
from semgrep.semgrep_types import Language
from semgrep.target_manager import TargetManager
from semgrep.target_manager_extensions import all_supported_languages
from semgrep.util import debug_tqdm_write
from semgrep.util import is_debug
from semgrep.util import progress_bar
from semgrep.util import SEMGREP_PATH
from semgrep.util import sub_run
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


class CoreRunner:
    """
    Handles interactions between semgrep and semgrep-core

    This includes properly invoking semgrep-core and parsing the output
    """

    def __init__(
        self,
        jobs: int,
        timeout: int,
        max_memory: int,
        timeout_threshold: int,
        optimizations: str,
    ):
        self._jobs = jobs
        self._timeout = timeout
        self._max_memory = max_memory
        self._timeout_threshold = timeout_threshold
        self._optimizations = optimizations

    def _raise_semgrep_error_from_json(
        self,
        error_json: Dict[str, Any],
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
        elif error_type == "invalid regexp in rule":
            raise SemgrepError(f'Invalid regexp in rule: {error_json["message"]}')
        elif error_type == "invalid pattern":
            range = error_json["range"]
            # If pattern is empty treat as <no pattern>
            s = error_json.get("pattern", "<no pattern>") or "<no pattern>"
            matching_span = Span.from_string_token(
                s=s,
                line=range.get("line", 0),
                col=range.get("col", 0),
                path=range.get("path", []),
                filename="semgrep temp file",
            )
            if error_json["message"] == "Parsing.Parse_error":
                long_msg = f"Pattern `{s.strip()}` could not be parsed as a {error_json['language']} semgrep pattern"
            else:
                long_msg = f"Error parsing {error_json['language']} pattern: {error_json['message']}"

            raise InvalidPatternError(
                short_msg=error_type,
                long_msg=long_msg,
                spans=[matching_span],
                help=None,
            )
        # no special formatting ought to be required for the other types; the semgrep python should be performing
        # validation for them. So if any other type of error occurs, ask the user to file an issue
        else:
            raise SemgrepError(
                f"an internal error occured while invoking semgrep-core while running rule '{rule.id}'. Consider skipping this rule and reporting this issue.\n\t{error_type}: {error_json.get('message', 'no message')}\n{PLEASE_FILE_ISSUE_TEXT}"
            )

    def _extract_core_output(
        self, rule: Rule, core_run: subprocess.CompletedProcess
    ) -> Dict[str, Any]:
        semgrep_output = core_run.stdout.decode("utf-8", errors="replace")

        stderr = core_run.stderr
        if stderr is None:
            semgrep_error_output = (
                "<semgrep-core stderr not captured, should be printed above>\n"
            )
        else:
            semgrep_error_output = stderr.decode("utf-8", errors="replace")

        # By default, we print semgrep-core's error output, which includes
        # semgrep-core's logging if it was requested via --debug.
        #
        # If semgrep-core prints anything on stderr when running with default
        # flags, it's a bug that should be fixed in semgrep-core.
        #
        name = f"[rule '{rule.id}']"
        logger.debug(
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
                self._raise_semgrep_error_from_json(output_json, rule)
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
                        rule,
                        Path(target["path"]),
                        Times(
                            parse_time=target["parse_time"],
                            match_time=target["match_time"],
                            run_time=target["run_time"],
                        ),
                    )
        if "rule_parse_time" in output_time_json:
            profiling_data.set_rule_parse_time(
                rule, output_time_json["rule_parse_time"]
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
        return list(targets)

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
        logger.debug(f"Passing whole rules directly to semgrep_core")

        outputs: Dict[Rule, List[RuleMatch]] = collections.defaultdict(list)
        errors: List[SemgrepError] = []
        all_targets: Set[Path] = set()
        file_timeouts: Dict[Path, int] = collections.defaultdict(lambda: 0)
        max_timeout_files: Set[Path] = set()

        profiling_data: ProfilingData = ProfilingData()
        # cf. for bar_format: https://tqdm.github.io/docs/tqdm/
        with tempfile.TemporaryDirectory() as semgrep_core_ast_cache_dir:
            for rule in progress_bar(
                rules, bar_format="{l_bar}{bar}|{n_fmt}/{total_fmt}"
            ):
                for language in rule.languages:
                    debug_tqdm_write(f"Running rule {rule.id}...")
                    with tempfile.NamedTemporaryFile(
                        "w", suffix=".yaml"
                    ) as rule_file, tempfile.NamedTemporaryFile(
                        "w"
                    ) as target_file, tempfile.NamedTemporaryFile(
                        "w"
                    ) as equiv_file:
                        targets = self.get_files_for_language(
                            language, rule, target_manager
                        )

                        targets = [
                            target
                            for target in targets
                            if target not in max_timeout_files
                        ]

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
                            "-json_time",
                        ]

                        if self._optimizations != "none":
                            cmd.append("-fast")

                        stderr: Optional[int] = subprocess.PIPE
                        if is_debug():
                            cmd += ["-debug"]
                            stderr = None

                        core_run = sub_run(cmd, stdout=subprocess.PIPE, stderr=stderr)
                        output_json = self._extract_core_output(rule, core_run)

                        if "time" in output_json:
                            self._add_match_times(
                                rule, profiling_data, output_json["time"]
                            )

                    # end with tempfile.NamedTemporaryFile(...) ...
                    pattern_matches = [
                        PatternMatch(match) for match in output_json["matches"]
                    ]
                    findings = create_output(rule, pattern_matches)

                    findings = dedup_output(findings)
                    outputs[rule].extend(findings)
                    parsed_errors = [
                        CoreException.from_json(
                            e, language.value, rule.id
                        ).into_semgrep_error()
                        for e in output_json["errors"]
                    ]
                    for err in parsed_errors:
                        if isinstance(err, MatchTimeoutError):
                            file_timeouts[err.path] += 1
                            if (
                                self._timeout_threshold != 0
                                and file_timeouts[err.path] >= self._timeout_threshold
                            ):
                                max_timeout_files.add(err.path)
                    errors.extend(parsed_errors)
            # end for language ...
        # end for rule ...

        return outputs, {}, errors, all_targets, profiling_data

    # end _run_rules_direct_to_semgrep_core

    def invoke_semgrep(
        self,
        target_manager: TargetManager,
        profiler: ProfileManager,
        rules: List[Rule],
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

        (
            findings_by_rule,
            debug_steps_by_rule,
            errors,
            all_targets,
            profiling_data,
        ) = self._run_rules_direct_to_semgrep_core(rules, target_manager, profiler)

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


# This will remove matches that have the same range but different
# metavariable bindings, choosing the last one in the list. We want the
# last because if there multiple possible bindings, they will be returned
# by semgrep-core from largest range to smallest. For an example, see
# tests/e2e/test_message_interpolation.py::test_message_interpolation;
# specifically, the multi-pattern-inside test
#
# Another option is to not dedup, since Semgrep.ml now does its own deduping
# otherwise, and surface both matches
def dedup_output(outputs: List[RuleMatch]) -> List[RuleMatch]:
    return list({uniq_id(r): r for r in reversed(outputs)}.values())[::-1]


def uniq_id(
    r: RuleMatch,
) -> Tuple[str, Path, Optional[int], Optional[int], Optional[int], Optional[int], str]:
    start = r.start
    end = r.end
    return (
        r.id,
        r.path,
        start.get("line"),
        start.get("col"),
        end.get("line"),
        end.get("col"),
        r.message,
    )
