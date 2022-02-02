import collections
import json
import resource
import subprocess
import sys
import tempfile
from datetime import datetime
from pathlib import Path
from typing import Any
from typing import cast
from typing import Dict
from typing import List
from typing import Optional
from typing import Sequence
from typing import Set
from typing import Tuple

from ruamel.yaml import YAML

from semgrep.config_resolver import Config
from semgrep.constants import Colors
from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.core_output import CoreOutput
from semgrep.core_output import CoreTiming
from semgrep.core_output import RuleId
from semgrep.error import _UnknownLanguageError
from semgrep.error import SemgrepCoreError
from semgrep.error import SemgrepError
from semgrep.error import UnknownLanguageError
from semgrep.error import with_color
from semgrep.profile_manager import ProfileManager
from semgrep.profiling import ProfilingData
from semgrep.profiling import Times
from semgrep.rule import Rule
from semgrep.rule_match import CoreLocation
from semgrep.rule_match import RuleMatch
from semgrep.semgrep_core import SemgrepCore
from semgrep.semgrep_types import LANGUAGE
from semgrep.semgrep_types import Language
from semgrep.target_manager import TargetManager
from semgrep.util import is_debug
from semgrep.util import sub_run
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

Target = Tuple[Path, Language]


def setrlimits_preexec_fn() -> None:
    """
    Sets stack limit of current running process to the maximum possible
    of the following as allowed by the OS:
    - 5120000
    - stack hard limit / 3
    - stack hard limit / 4
    - current existing soft limit

    Note this is intended to run as a preexec_fn before semgrep-core in a subprocess
    so all code here runs in a child fork before os switches to semgrep-core binary
    """
    # Get current soft and hard stack limits
    old_soft_limit, hard_limit = resource.getrlimit(resource.RLIMIT_STACK)
    logger.info(f"Existing stack limits: Soft: {old_soft_limit}, Hard: {hard_limit}")

    # Have candidates in case os unable to set certain limit
    potential_soft_limits = [
        int(
            hard_limit / 3
        ),  # Larger fractions cause "current limit exceeds maximum limit" for unknown reason
        int(hard_limit / 4),
        5120000,  # Magic number that seems to work for most cases
        old_soft_limit,
    ]

    # Reverse sort so maximum possible soft limit is set
    potential_soft_limits.sort(reverse=True)
    for soft_limit in potential_soft_limits:
        try:
            logger.info(f"Trying to set soft limit to {soft_limit}")
            resource.setrlimit(resource.RLIMIT_STACK, (soft_limit, hard_limit))
            logger.info(f"Set stack limit to {soft_limit}, {hard_limit}")
            return
        except Exception as e:
            logger.info(f"Failed to set stack limit to {soft_limit}, {hard_limit}")
            logger.verbose(str(e))

    logger.info("Failed to change stack limits")


def dedup_errors(errors: List[SemgrepCoreError]) -> List[SemgrepCoreError]:
    return list({uniq_error_id(e): e for e in errors}.values())


def uniq_error_id(
    error: SemgrepCoreError,
) -> Tuple[int, Path, CoreLocation, CoreLocation, str]:
    return (error.code, error.path, error.start, error.end, error.message)


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
        self, rules: List[Rule], core_run: subprocess.CompletedProcess
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
        logger.debug(
            f"--- semgrep-core stderr ---\n"
            f"{semgrep_error_output}"
            f"--- end semgrep-core stderr ---"
        )

        returncode = core_run.returncode
        if returncode != 0:
            output_json = self._parse_core_output(
                core_run, semgrep_output, semgrep_error_output, returncode
            )

            if "errors" in output_json:
                parsed_output = CoreOutput.parse(rules, output_json)
                errors = parsed_output.errors
                if len(errors) < 1:
                    self._fail(
                        "non-zero exit status errors array is empty in json response",
                        core_run,
                        returncode,
                        semgrep_output,
                        semgrep_error_output,
                    )
                raise errors[0].to_semgrep_error()
            else:
                self._fail(
                    'non-zero exit status with missing "errors" field in json response',
                    core_run,
                    returncode,
                    semgrep_output,
                    semgrep_error_output,
                )

        output_json = self._parse_core_output(
            core_run, semgrep_output, semgrep_error_output, returncode
        )
        return output_json

    def _parse_core_output(
        self,
        core_run: subprocess.CompletedProcess,
        semgrep_output: str,
        semgrep_error_output: str,
        returncode: int,
    ) -> Dict[str, Any]:
        # See if semgrep output contains a JSON error that we can decode.
        try:
            return cast(Dict[str, Any], json.loads(semgrep_output))
        except ValueError:
            if returncode == -11:
                # Killed by signal 11 (segmentation fault), this could be a
                # stack overflow that was not intercepted by the OCaml runtime.
                soft_limit, _hard_limit = resource.getrlimit(resource.RLIMIT_STACK)
                tip = f" This may be a stack overflow. Current stack limit is {soft_limit}, try increasing it via `ulimit -s {2*soft_limit}`."
            else:
                tip = ""
            self._fail(
                f"Semgrep encountered an internal error.{tip}",
                core_run,
                returncode,
                semgrep_output,
                semgrep_error_output,
            )
            return {}  # never reached

    def _fail(
        self,
        reason: str,
        core_run: subprocess.CompletedProcess,
        returncode: int,
        semgrep_output: str,
        semgrep_error_output: str,
    ) -> None:
        # Once we require python >= 3.8, switch to using shlex.join instead
        # for proper quoting of the command line.
        shell_command = " ".join(core_run.args)
        details = with_color(
            Colors.white,
            f"semgrep-core exit code: {returncode}\n"
            f"semgrep-core command: {shell_command}\n"
            f"unexpected non-json output while invoking semgrep-core:\n"
            "--- semgrep-core stdout ---\n"
            f"{semgrep_output}"
            "--- end semgrep-core stdout ---\n"
            "--- semgrep-core stderr ---\n"
            f"{semgrep_error_output}"
            "--- end semgrep-core stderr ---\n",
        )
        raise SemgrepError(
            f"Error while matching: {reason}\n{details}" f"{PLEASE_FILE_ISSUE_TEXT}"
        )

    def _add_match_times(
        self,
        profiling_data: ProfilingData,
        timing: CoreTiming,
    ) -> None:
        rules = timing.rules
        targets = [t.target for t in timing.target_timings]

        profiling_data.init_empty(rules, targets)
        profiling_data.set_rules_parse_time(timing.rules_parse_time)

        for t in timing.target_timings:
            rule_timings = {
                rt.rule: Times(rt.parse_time, rt.match_time)
                for rt in t.per_rule_timings
            }
            profiling_data.set_file_times(t.target, rule_timings, t.run_time)

    @staticmethod
    def get_files_for_language(
        language: Language, rule: Rule, target_manager: TargetManager
    ) -> List[Path]:
        try:
            targets = target_manager.get_files(language, rule.includes, rule.excludes)
        except _UnknownLanguageError as ex:
            raise UnknownLanguageError(
                short_msg=f"invalid language: {language}",
                long_msg=f"unsupported language: {language}. supported languages are: {', '.join(LANGUAGE.all_language_keys)}",
                spans=[rule.languages_span.with_context(before=1, after=1)],
            ) from ex
        return list(targets)

    def _get_targets(
        self, rules: List[Rule], target_manager: TargetManager, all_targets: Set[Path]
    ) -> List[Dict[str, Any]]:
        target_info: Dict[Tuple[Path, Language], List[RuleId]] = {}

        for rule in rules:
            for language in rule.languages:
                targets = self.get_files_for_language(language, rule, target_manager)

                for target in targets:
                    all_targets.add(target)
                    t = (target, language)
                    if t in target_info:
                        target_info[t].append(RuleId(rule.orig_id))
                    else:
                        target_info[t] = [RuleId(rule.orig_id)]

        targets_json = []
        for target_and_lang in target_info:
            rule_ids = target_info[target_and_lang]
            targets_json.append(
                {
                    "path": str(target_and_lang[0]),
                    "language": target_and_lang[1],
                    "rule_ids": rule_ids,
                }
            )

        return targets_json

    def _dump_semgrep_core_command(
        self, rules: List[Rule], target_manager: TargetManager, profiler: ProfileManager
    ) -> None:
        rules_file = "semgrep_rules.yaml"
        targets_file = "semgrep_targets.txt"

        with tempfile.TemporaryDirectory() as semgrep_core_ast_cache_dir:
            with open(rules_file, "w+") as rule_file, open(
                targets_file, "w+"
            ) as target_file:
                targets_with_rules = self._get_targets(rules, target_manager, set())
                target_file.write(json.dumps(targets_with_rules))
                target_file.flush()

                yaml = YAML()
                yaml.dump({"rules": [rule._raw for rule in rules]}, rule_file)
                rule_file.flush()

                # Run semgrep
                cmd = [SemgrepCore.path()] + [
                    "-json",
                    "-rules",
                    rule_file.name,
                    "-j",
                    str(self._jobs),
                    "-targets",
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

                if is_debug():
                    cmd += ["-debug"]

                print(" ".join(cmd))

    def _run_rules_direct_to_semgrep_core(
        self,
        rules: List[Rule],
        target_manager: TargetManager,
        profiler: ProfileManager,
    ) -> Tuple[
        Dict[Rule, List[RuleMatch]],
        List[SemgrepError],
        Set[Path],
        ProfilingData,
    ]:
        logger.debug(f"Passing whole rules directly to semgrep_core")

        errors: List[SemgrepError] = []
        all_targets: Set[Path] = set()
        file_timeouts: Dict[Path, int] = collections.defaultdict(lambda: 0)
        max_timeout_files: Set[Path] = set()

        profiling_data: ProfilingData = ProfilingData()

        with tempfile.TemporaryDirectory() as semgrep_core_ast_cache_dir:
            with tempfile.NamedTemporaryFile(
                "w", suffix=".yaml"
            ) as rule_file, tempfile.NamedTemporaryFile("w") as target_file:
                targets_with_rules = self._get_targets(
                    rules, target_manager, all_targets
                )
                target_file.write(json.dumps(targets_with_rules))
                target_file.flush()

                yaml = YAML()
                yaml.dump({"rules": [rule._raw for rule in rules]}, rule_file)
                rule_file.flush()

                # Run semgrep
                cmd = [SemgrepCore.path()] + [
                    "-json",
                    "-rules",
                    rule_file.name,
                    "-j",
                    str(self._jobs),
                    "-targets",
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

                core_run = sub_run(
                    cmd,
                    stdout=subprocess.PIPE,
                    stderr=stderr,
                    preexec_fn=setrlimits_preexec_fn,
                )

                # Process output
                output_json = self._extract_core_output(rules, core_run)
                core_output = CoreOutput.parse(rules, output_json)

                if "time" in output_json:
                    self._add_match_times(profiling_data, core_output.timing)

                # end with tempfile.NamedTemporaryFile(...) ...
                outputs = core_output.rule_matches(rules)
                parsed_errors = [e.to_semgrep_error() for e in core_output.errors]
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

        return outputs, errors, all_targets, profiling_data

    # end _run_rules_direct_to_semgrep_core

    def invoke_semgrep(
        self,
        dump_command_for_core: bool,
        target_manager: TargetManager,
        profiler: ProfileManager,
        rules: List[Rule],
    ) -> Tuple[
        Dict[Rule, List[RuleMatch]],
        List[SemgrepError],
        Set[Path],
        ProfilingData,
    ]:
        if dump_command_for_core:
            self._dump_semgrep_core_command(rules, target_manager, profiler)
            sys.exit(0)

        """
        Takes in rules and targets and retuns object with findings
        """
        start = datetime.now()

        (
            findings_by_rule,
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
            errors,
            all_targets,
            profiling_data,
        )

    def validate_configs(self, configs: Tuple[str, ...]) -> Sequence[SemgrepError]:
        metachecks = Config.from_config_list(["p/semgrep-rule-lints"], None)[
            0
        ].get_rules(True)

        parsed_errors = []

        with tempfile.NamedTemporaryFile("w", suffix=".yaml") as rule_file:

            yaml = YAML()
            yaml.dump(
                {"rules": [metacheck._raw for metacheck in metachecks]}, rule_file
            )
            rule_file.flush()

            cmd = (
                [SemgrepCore.path()]
                + [
                    "-json",
                    "-check_rules",
                    rule_file.name,
                ]
                + list(configs)
            )

            stderr: Optional[int] = subprocess.PIPE

            core_run = sub_run(
                cmd,
                stdout=subprocess.PIPE,
                stderr=stderr,
            )
            output_json = self._extract_core_output(metachecks, core_run)
            core_output = CoreOutput.parse(metachecks, output_json)

            parsed_errors += [e.to_semgrep_error() for e in core_output.errors]

        return dedup_errors(parsed_errors)
