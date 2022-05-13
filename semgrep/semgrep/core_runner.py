import asyncio
import collections
import json
import os
import resource
import shlex
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

from attr import asdict
from attr import field
from attr import frozen
from ruamel.yaml import YAML
from tqdm import tqdm

import semgrep.output_from_core as core
from semgrep.config_resolver import Config
from semgrep.constants import Colors
from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.constants import USER_DATA_FOLDER
from semgrep.core_output import core_error_to_semgrep_error
from semgrep.core_output import core_matches_to_rule_matches
from semgrep.core_output import parse_core_output
from semgrep.error import _UnknownLanguageError
from semgrep.error import SemgrepCoreError
from semgrep.error import SemgrepError
from semgrep.error import UnknownLanguageError
from semgrep.error import with_color
from semgrep.profiling import ProfilingData
from semgrep.profiling import Times
from semgrep.rule import Rule
from semgrep.rule_match import OrderedRuleMatchList
from semgrep.rule_match import RuleMatchMap
from semgrep.semgrep_core import SemgrepCore
from semgrep.semgrep_types import LANGUAGE
from semgrep.semgrep_types import Language
from semgrep.state import get_state
from semgrep.target_manager import TargetManager
from semgrep.util import sub_check_output
from semgrep.util import unit_str
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

RULE_SAVE_FILE = str(USER_DATA_FOLDER / Path("semgrep_rules.yaml"))
TARGET_SAVE_FILE = str(USER_DATA_FOLDER / Path("semgrep_targets.txt"))


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
        old_soft_limit * 100,
        old_soft_limit * 10,
        old_soft_limit * 5,
        1000000000,
        512000000,
        51200000,
        5120000,  # Magic numbers that seems to work for most cases
        old_soft_limit,
    ]

    # Reverse sort so maximum possible soft limit is set
    potential_soft_limits.sort(reverse=True)
    for soft_limit in potential_soft_limits:
        try:
            logger.info(f"Trying to set soft limit to {soft_limit}")
            resource.setrlimit(resource.RLIMIT_STACK, (soft_limit, hard_limit))
            logger.info(f"Successfully set stack limit to {soft_limit}, {hard_limit}")
            return
        except Exception as e:
            logger.info(
                f"Failed to set stack limit to {soft_limit}, {hard_limit}. Trying again."
            )
            logger.verbose(str(e))

    logger.info("Failed to change stack limits")


# This is used only to dedup errors from validate_configs(). For dedupping errors
# from _invoke_semgrep(), see output.py and the management of self.error_set
def dedup_errors(errors: List[SemgrepCoreError]) -> List[SemgrepCoreError]:
    return list({uniq_error_id(e): e for e in errors}.values())


def uniq_error_id(
    error: SemgrepCoreError,
) -> Tuple[int, Path, core.Position, core.Position, str]:
    return (
        error.code,
        Path(error.core.location.path),
        error.core.location.start,
        error.core.location.end,
        error.core.message,
    )


class StreamingSemgrepCore:
    """
    Handles running semgrep-core in a streaming fashion

    This behavior is assumed to be that semgrep-core:
    - prints a "." on a newline for every file it finishes scanning
    - prints a single json blob of all results

    Exposes the subprocess.CompletedProcess properties for
    expediency in integrating
    """

    def __init__(self, cmd: List[str], total: int) -> None:
        """
        cmd: semgrep-core command to run
        total: how many rules to run / how many "." we expect to see
               used to display progress_bar
        """
        self._cmd = cmd
        self._total = total
        self._stdout = ""
        self._stderr = ""
        self._progress_bar: Optional[tqdm] = None  # type: ignore

    @property
    def stdout(self) -> str:
        # stdout of semgrep-core sans "."
        return self._stdout

    @property
    def stderr(self) -> str:
        # stderr of semgrep-core command
        return self._stderr

    async def _core_stdout_processor(
        self, stream: Optional[asyncio.StreamReader]
    ) -> None:
        """
        Asynchronously process stdout of semgrep-core

        Updates progress bar one increment for every "." it sees from semgrep-core
        stdout

        When it sees non-"." output it saves it to self._stdout
        """
        stdout_lines: List[bytes] = []

        # appease mypy. stream is only None if call to create_subproccess_exec
        # sets stdout/stderr stream to None
        assert stream

        # Start out reading two bytes at a time (".\n")
        bytes_to_read = 2
        while True:
            # blocking read if buffer doesnt contain any lines or EOF
            line_bytes = await stream.read(n=bytes_to_read)

            # read returns empty when EOF
            if not line_bytes:
                self._stdout = b"".join(stdout_lines).decode("utf-8", "replace")
                break

            if line_bytes == b".\n":
                if self._progress_bar:
                    self._progress_bar.update()
            else:
                stdout_lines.append(line_bytes)
                # Once we see a non-"." char it means we are reading a large json blob
                # so increase the buffer read size (kept below subprocess buffer limit below)
                bytes_to_read = 1024 * 1024 * 512

    async def _core_stderr_processor(
        self, stream: Optional[asyncio.StreamReader]
    ) -> None:
        """
        Asynchronously process stderr of semgrep-core

        Basically works synchronously and combines output to
        stderr to self._stderr
        """
        stderr_lines: List[str] = []

        if stream is None:
            raise RuntimeError("subprocess was created without a stream")

        while True:
            # blocking read if buffer doesnt contain any lines or EOF
            line_bytes = await stream.readline()

            # readline returns empty when EOF
            if not line_bytes:
                self._stderr = "".join(stderr_lines)
                break

            line = line_bytes.decode("utf-8", "replace")
            stderr_lines.append(line)

    async def _stream_subprocess(self) -> int:
        process = await asyncio.create_subprocess_exec(
            *self._cmd,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
            limit=1024 * 1024 * 1024,  # buffer limit to read in bytes
            preexec_fn=setrlimits_preexec_fn,
        )

        # Raise any exceptions from processing stdout/err
        results = await asyncio.gather(
            self._core_stdout_processor(process.stdout),
            self._core_stderr_processor(process.stderr),
            return_exceptions=True,
        )
        for r in results:
            if isinstance(r, Exception):
                raise SemgrepError(f"Error while running rules: {r}")

        # Return exit code of cmd. process should already be done
        return await process.wait()

    def execute(self) -> int:
        """
        Run semgrep-core and listen to stdout to update
        progress_bar as necessary

        Blocks til completion and returns exit code
        """
        terminal = get_state().terminal
        if (
            sys.stderr.isatty()
            and self._total > 1
            and not terminal.is_quiet
            and not terminal.is_debug
        ):
            # cf. for bar_format: https://tqdm.github.io/docs/tqdm/
            self._progress_bar = tqdm(  # typing: ignore
                total=self._total,
                file=sys.stderr,
                bar_format="  {l_bar}{bar}|{n_fmt}/{total_fmt} tasks",
            )

        rc = asyncio.run(self._stream_subprocess())
        if self._progress_bar:
            self._progress_bar.close()

        return rc


@frozen
class Task:
    path: str = field(converter=str)
    language: Language
    rule_ids: Sequence[str]


class Plan(List[Task]):
    @property
    def rule_count(self) -> int:
        return len({rule for task in self for rule in task.rule_ids})

    @property
    def file_count(self) -> int:
        return len(self)

    def split_by_lang_label(self) -> Dict[str, "Plan"]:
        result: Dict[str, Plan] = collections.defaultdict(Plan)
        for task in self:
            label = (
                "<multilang>"
                if task.language in {Language("regex"), Language("generic")}
                else task.language
            )
            result[label].append(task)
        return result

    def to_json(self) -> List[Dict[str, Any]]:
        return [asdict(task) for task in self]

    def log(self) -> None:
        if self.rule_count == 0:
            logger.info("Nothing to scan.")
            return

        if self.rule_count == 1:
            logger.info(f"Scanning {unit_str(len(self), 'file')}.")
            return

        plans_by_language = sorted(
            self.split_by_lang_label().items(),
            key=lambda x: (x[1].file_count, x[1].rule_count),
            reverse=True,
        )
        if len(plans_by_language) == 1:
            logger.info(
                f"Scanning {unit_str(self.file_count, 'file')} with {unit_str(self.rule_count, f'{plans_by_language[0][0]} rule')}."
            )
            return

        logger.info("\nScanning across multiple languages:")
        for language, plan in plans_by_language:
            lang_chars = max(len(lang) for lang, _ in plans_by_language)
            rules_chars = max(
                len(str(plan.rule_count)) for _, plan in plans_by_language
            ) + len(" rules")
            files_chars = max(
                len(str(plan.file_count)) for _, plan in plans_by_language
            ) + len(" files")

            lang_field = language.rjust(lang_chars)
            rules_field = unit_str(plan.rule_count, "rule", pad=True).rjust(rules_chars)
            files_field = unit_str(plan.file_count, "file", pad=True).rjust(files_chars)

            logger.info(f"    {lang_field} | {rules_field} × {files_field}")
        logger.info("")

    def __str__(self) -> str:
        return f"<Plan of {len(self)} tasks for {list(self.split_by_lang_label())}>"


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
        core_opts_str: Optional[str],
    ):
        self._jobs = jobs
        self._timeout = timeout
        self._max_memory = max_memory
        self._timeout_threshold = timeout_threshold
        self._optimizations = optimizations
        self._core_opts = shlex.split(core_opts_str) if core_opts_str else []

    def _extract_core_output(
        self,
        rules: List[Rule],
        returncode: int,
        shell_command: str,
        core_stdout: str,
        core_stderr: str,
    ) -> Dict[str, Any]:
        if not core_stderr:
            core_stderr = (
                "<semgrep-core stderr not captured, should be printed above>\n"
            )

        # By default, we print semgrep-core's error output, which includes
        # semgrep-core's logging if it was requested via --debug.
        #
        # If semgrep-core prints anything on stderr when running with default
        # flags, it's a bug that should be fixed in semgrep-core.
        #
        logger.debug(
            f"--- semgrep-core stderr ---\n"
            f"{core_stderr}"
            f"--- end semgrep-core stderr ---"
        )

        if returncode != 0:
            output_json = self._parse_core_output(
                shell_command, core_stdout, core_stderr, returncode
            )

            if "errors" in output_json:
                parsed_output = parse_core_output(output_json)
                errors = parsed_output.errors
                if len(errors) < 1:
                    self._fail(
                        "non-zero exit status errors array is empty in json response",
                        shell_command,
                        returncode,
                        core_stdout,
                        core_stderr,
                    )
                raise core_error_to_semgrep_error(errors[0])
            else:
                self._fail(
                    'non-zero exit status with missing "errors" field in json response',
                    shell_command,
                    returncode,
                    core_stdout,
                    core_stderr,
                )

        output_json = self._parse_core_output(
            shell_command, core_stdout, core_stderr, returncode
        )
        return output_json

    def _parse_core_output(
        self,
        shell_command: str,
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
                shell_command,
                returncode,
                semgrep_output,
                semgrep_error_output,
            )
            return {}  # never reached

    def _fail(
        self,
        reason: str,
        shell_command: str,
        returncode: int,
        semgrep_output: str,
        semgrep_error_output: str,
    ) -> None:
        # Once we require python >= 3.8, switch to using shlex.join instead
        # for proper quoting of the command line.
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
        timing: core.CoreTiming,
    ) -> None:
        targets = [Path(t.path) for t in timing.targets]

        profiling_data.init_empty(timing.rules, targets)
        if timing.rules_parse_time:
            profiling_data.set_rules_parse_time(timing.rules_parse_time)

        for t in timing.targets:
            rule_timings = {
                rt.rule_id: Times(rt.parse_time, rt.match_time) for rt in t.rule_times
            }
            profiling_data.set_file_times(Path(t.path), rule_timings, t.run_time)

    def get_files_for_language(
        self, language: Language, rule: Rule, target_manager: TargetManager
    ) -> List[Path]:
        try:
            targets = target_manager.get_files_for_rule(
                language, rule.includes, rule.excludes, rule.id
            )
        except _UnknownLanguageError as ex:
            raise UnknownLanguageError(
                short_msg=f"invalid language: {language}",
                long_msg=f"unsupported language: {language}. {LANGUAGE.show_suppported_languages_message()}",
                spans=[rule.languages_span.with_context(before=1, after=1)],
            ) from ex
        return list(targets)

    def _plan_core_run(
        self, rules: List[Rule], target_manager: TargetManager, all_targets: Set[Path]
    ) -> Plan:
        """
        Gets the targets to run for each rule

        Returns this information as a list of targets with language and rule ids,
        which semgrep-core requires to know what rules to run for each file
        Also updates all_targets, used by core_runner

        Note: this is a list because a target can appear twice (e.g. Java + Generic)
        """
        target_info: Dict[
            Tuple[Path, Language], List[str]  # TODO: List[core.RuleId]
        ] = collections.defaultdict(list)

        for rule in rules:
            for language in rule.languages:
                targets = self.get_files_for_language(language, rule, target_manager)

                for target in targets:
                    all_targets.add(target)
                    target_info[target, language].append(rule.id)  # TODO: core.RuleId

        return Plan(
            [
                Task(
                    path=target,
                    language=language,
                    rule_ids=target_info[target, language],
                )
                for target, language in target_info
            ]
        )

    def _run_rules_direct_to_semgrep_core(
        self,
        rules: List[Rule],
        target_manager: TargetManager,
        dump_command_for_core: bool,
        deep: bool,
    ) -> Tuple[RuleMatchMap, List[SemgrepError], Set[Path], ProfilingData,]:
        logger.debug(f"Passing whole rules directly to semgrep_core")

        outputs: RuleMatchMap = collections.defaultdict(OrderedRuleMatchList)
        errors: List[SemgrepError] = []
        all_targets: Set[Path] = set()
        file_timeouts: Dict[Path, int] = collections.defaultdict(lambda: 0)
        max_timeout_files: Set[Path] = set()

        profiling_data: ProfilingData = ProfilingData()

        rule_file_name = (
            RULE_SAVE_FILE
            if dump_command_for_core
            else tempfile.NamedTemporaryFile("w", suffix=".yaml").name
        )
        target_file_name = (
            TARGET_SAVE_FILE
            if dump_command_for_core
            else tempfile.NamedTemporaryFile("w").name
        )

        with open(rule_file_name, "w+") as rule_file, open(
            target_file_name, "w+"
        ) as target_file:

            plan = self._plan_core_run(rules, target_manager, all_targets)
            plan.log()
            target_file.write(json.dumps(plan.to_json()))
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
                "-timeout",
                str(self._timeout),
                "-timeout_threshold",
                str(self._timeout_threshold),
                "-max_memory",
                str(self._max_memory),
                "-json_time",
            ]

            if self._core_opts:
                logger.info(
                    f"Running with user defined core options: {self._core_opts}"
                )
                cmd.extend(self._core_opts)

            if self._optimizations != "none":
                cmd.append("-fast")

            # TODO: use exact same command-line arguments so just
            # need to replace the SemgrepCore.path() part.
            if deep:
                print("!!!This is a proprietary extension of semgrep.!!!")
                print("!!!You must be logged in to access this extension!!!")
                targets = target_manager.targets
                if len(targets) == 1 and targets[0].path.is_dir():
                    root = str(targets[0].path)
                else:
                    raise SemgrepError("deep mode needs a single target (root) dir")

                deep_path = SemgrepCore.deep_path()
                if deep_path is None:
                    raise SemgrepError(
                        "Could not run deep analysis: DeepSemgrep not installed. Run `semgrep install-deep-semgrep`"
                    )

                logger.info(f"Using DeepSemgrep installed in {deep_path}")
                version = sub_check_output(
                    [deep_path, "--version"],
                    timeout=10,
                    encoding="utf-8",
                    stderr=subprocess.DEVNULL,
                ).rstrip()
                logger.info(f"DeepSemgrep Version Info: ({version})")

                cmd = [deep_path] + [
                    "--json",
                    "--rules",
                    rule_file.name,
                    # "-j",
                    # str(self._jobs),
                    "--targets",
                    target_file.name,
                    "--root",
                    root,
                    # "--timeout",
                    # str(self._timeout),
                    # "--timeout_threshold",
                    # str(self._timeout_threshold),
                    # "--max_memory",
                    # str(self._max_memory),
                ]

            stderr: Optional[int] = subprocess.PIPE
            terminal = get_state().terminal
            if terminal.is_debug:
                cmd += ["--debug"]

            logger.debug("Running semgrep-core with command:")
            logger.debug(" ".join(cmd))

            if dump_command_for_core:
                print(" ".join(cmd))
                sys.exit(0)

            runner = StreamingSemgrepCore(cmd, len(plan))
            returncode = runner.execute()

            # Process output
            output_json = self._extract_core_output(
                rules,
                returncode,
                " ".join(cmd),
                runner.stdout,
                runner.stderr,
            )
            core_output = parse_core_output(output_json)

            if ("time" in output_json) and core_output.time:
                self._add_match_times(profiling_data, core_output.time)

            # end with tempfile.NamedTemporaryFile(...) ...
            outputs = core_matches_to_rule_matches(rules, core_output)
            parsed_errors = [core_error_to_semgrep_error(e) for e in core_output.errors]
            for err in core_output.errors:
                if isinstance(err.error_type.value, core.Timeout):
                    assert err.location.path is not None

                    file_timeouts[Path(err.location.path)] += 1
                    if (
                        self._timeout_threshold != 0
                        and file_timeouts[Path(err.location.path)]
                        >= self._timeout_threshold
                    ):
                        max_timeout_files.add(Path(err.location.path))
            errors.extend(parsed_errors)

        os.remove(rule_file_name)
        os.remove(target_file_name)

        return outputs, errors, all_targets, profiling_data

    # end _run_rules_direct_to_semgrep_core

    def invoke_semgrep(
        self,
        target_manager: TargetManager,
        rules: List[Rule],
        dump_command_for_core: bool,
        deep: bool,
    ) -> Tuple[RuleMatchMap, List[SemgrepError], Set[Path], ProfilingData,]:
        """
        Takes in rules and targets and retuns object with findings
        """
        start = datetime.now()

        (
            findings_by_rule,
            errors,
            all_targets,
            profiling_data,
        ) = self._run_rules_direct_to_semgrep_core(
            rules, target_manager, dump_command_for_core, deep
        )

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

            runner = StreamingSemgrepCore(cmd, 1)  # only scanning combined rules
            returncode = runner.execute()

            # Process output
            output_json = self._extract_core_output(
                metachecks,
                returncode,
                " ".join(cmd),
                runner.stdout,
                runner.stderr,
            )
            core_output = parse_core_output(output_json)

            parsed_errors += [
                core_error_to_semgrep_error(e) for e in core_output.errors
            ]

        return dedup_errors(parsed_errors)
