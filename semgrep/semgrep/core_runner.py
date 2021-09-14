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
from semgrep.core_output import CoreOutput
from semgrep.core_output import RuleId
from semgrep.error import _UnknownLanguageError
from semgrep.error import SemgrepError
from semgrep.error import UnknownLanguageError
from semgrep.profile_manager import ProfileManager
from semgrep.profiling import ProfilingData
from semgrep.profiling import Times
from semgrep.progress_bar import debug_tqdm_write
from semgrep.progress_bar import progress_bar
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.semgrep_core import SemgrepCore
from semgrep.semgrep_types import Language
from semgrep.target_manager import TargetManager
from semgrep.target_manager_extensions import all_supported_languages
from semgrep.util import is_debug
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

            if "errors" in output_json:
                parsed_output = CoreOutput.parse(output_json, RuleId(rule.id))
                errors = parsed_output.errors
                if len(errors) < 1:
                    self._fail(
                        "non-zero exit status errors array is empty in json response",
                        rule,
                        core_run,
                        returncode,
                        semgrep_output,
                        semgrep_error_output,
                    )
                raise errors[0].to_semgrep_error(RuleId(rule.id))
            else:
                self._fail(
                    'non-zero exit status with missing "errors" field in json response',
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
                    ) as rule_file, tempfile.NamedTemporaryFile("w") as target_file:
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

                        cmd = [SemgrepCore.path()] + [
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
                        core_output = CoreOutput.parse(output_json, RuleId(rule.id))

                        if "time" in output_json:
                            self._add_match_times(
                                rule, profiling_data, output_json["time"]
                            )

                    # end with tempfile.NamedTemporaryFile(...) ...
                    outputs[rule].extend(core_output.rule_matches(rule))
                    parsed_errors = [
                        e.to_semgrep_error(RuleId(rule.id)) for e in core_output.errors
                    ]
                    for err in core_output.errors:
                        if err.is_timeout():
                            assert err.path is not None

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
            by_severity[rule.severity.value.lower()].extend(findings)

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
