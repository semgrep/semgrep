import asyncio
import collections
import contextlib
import json
import multiprocessing
import resource
import shlex
import subprocess
import sys
import tempfile
from datetime import datetime
from pathlib import Path
from typing import Any
from typing import Callable
from typing import cast
from typing import Coroutine
from typing import Dict
from typing import List
from typing import Optional
from typing import Sequence
from typing import Set
from typing import Tuple
from typing import TYPE_CHECKING

from attr import asdict
from attr import field
from attr import frozen
from ruamel.yaml import YAML
from tqdm import tqdm

import semgrep.fork_subprocess as fork_subprocess
import semgrep.output_from_core as core
from semgrep.app import auth
from semgrep.config_resolver import Config
from semgrep.constants import Colors
from semgrep.constants import EngineType
from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.core_output import core_error_to_semgrep_error
from semgrep.core_output import core_matches_to_rule_matches
from semgrep.core_output import parse_core_output
from semgrep.error import SemgrepCoreError
from semgrep.error import SemgrepError
from semgrep.error import with_color
from semgrep.output_extra import OutputExtra
from semgrep.parsing_data import ParsingData
from semgrep.profiling import ProfilingData
from semgrep.profiling import Times
from semgrep.rule import Rule
from semgrep.rule_match import OrderedRuleMatchList
from semgrep.rule_match import RuleMatchMap
from semgrep.semgrep_core import SemgrepCore
from semgrep.semgrep_types import Language
from semgrep.state import get_state
from semgrep.target_manager import TargetManager
from semgrep.util import sub_check_output
from semgrep.util import unit_str
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


# Size in bytes of the input buffer for reading analysis outputs.
INPUT_BUFFER_LIMIT: int = 1024 * 1024 * 1024

# Number of bytes to read at once when reading the stdout produced by
# semgrep-core.
#
# This must be less than INPUT_BUFFER_LIMIT, otherwise a deadlock can
# result where the parent is waiting for this many bytes but the child
# has filled its buffer, so it blocks.
#
# test/e2e/test_performance.py is one test that exercises this risk.
LARGE_READ_SIZE: int = 1024 * 1024 * 512


def get_cpu_count() -> int:
    try:
        return multiprocessing.cpu_count()
    except NotImplementedError:
        return 1  # CPU count is not implemented on Windows


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


def open_and_ignore(fname: str) -> None:
    """
    Attempt to open 'fname' simply so a record of having done so will
    be seen by 'strace'.
    """
    try:
        with open(fname, "rb") as in_file:
            pass  # Not expected, but not a problem.
    except BaseException:
        pass  # Expected outcome


class StreamingSemgrepCore:
    """
    Handles running semgrep-core in a streaming fashion

    This behavior is assumed to be that semgrep-core:
    - prints a "." on a newline for every file it finishes scanning
    - prints a number on a newline for any extra targets produced during a scan
    - prints a single json blob of all results

    Exposes the subprocess.CompletedProcess properties for
    expediency in integrating
    """

    def __init__(self, cmd: List[str], total: int) -> None:
        """
        cmd: semgrep-core command to run
        total: how many rules to run / how many "." we expect to see a priori
               used to display progress_bar
        """
        self._cmd = cmd
        self._total = total
        self._stdout = ""
        self._stderr = ""
        self._progress_bar: Optional[tqdm] = None  # type: ignore

        # Map from file name to contents, to be checked before the real
        # file system when servicing requests from semgrep-core.
        self.vfs_map: Dict[str, bytes] = {}

    @property
    def stdout(self) -> str:
        # stdout of semgrep-core sans "." and extra target counts
        return self._stdout

    @property
    def stderr(self) -> str:
        # stderr of semgrep-core command
        return self._stderr

    async def _core_stdout_processor(self, stream: asyncio.StreamReader) -> None:
        """
        Asynchronously process stdout of semgrep-core

        Updates progress bar one increment for every "." it sees from semgrep-core
        stdout

        Increases the progress bar total for any number reported from semgrep-core
        stdout

        When it sees neither output it saves it to self._stdout
        """
        stdout_lines: List[bytes] = []
        num_total_targets: int = self._total
        num_scanned_targets: int = 0

        # Start out reading two bytes at a time (".\n")
        get_input: Callable[
            [asyncio.StreamReader], Coroutine[Any, Any, bytes]
        ] = lambda s: s.readexactly(2)
        reading_json = False
        # Read ".\n" repeatedly until we reach the JSON output.
        # TODO: read progress from one channel and JSON data from another.
        # or at least write/read progress as a stream of JSON objects so that
        # we don't have to hack a parser together.
        while True:
            # blocking read if buffer doesnt contain any lines or EOF
            try:
                line_bytes = await get_input(stream)
            except asyncio.IncompleteReadError:
                # happens if the data that follows a sequence of zero
                # or more ".\n" has fewer than two bytes, such as:
                # "", "3", ".\n.\n3", ".\n.\n.\n.", etc.
                raise SemgrepError(
                    "semgrep-core exited successfully but produced unexpected output"
                )

            # read returns empty when EOF
            if not line_bytes:
                self._stdout = b"".join(stdout_lines).decode("utf-8", "replace")
                break

            if line_bytes == b".\n" and not reading_json:
                num_scanned_targets += 1
                if self._progress_bar:
                    self._progress_bar.update()
            elif chr(line_bytes[0]).isdigit() and not reading_json:
                if not line_bytes.endswith(b"\n"):
                    line_bytes = line_bytes + await stream.readline()
                extra_targets = int(line_bytes)
                num_total_targets += extra_targets
                if self._progress_bar:
                    self._progress_bar.total += extra_targets
            else:
                stdout_lines.append(line_bytes)
                # Once we see a non-"." char it means we are reading a large json blob
                # so increase the buffer read size.
                reading_json = True
                get_input = lambda s: s.read(n=LARGE_READ_SIZE)

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

    def _handle_read_file(self, fname: str) -> Tuple[bytes, int]:
        """
        Handler for semgrep_analyze 'read_file' callback.
        """
        try:
            if fname in self.vfs_map:
                contents = self.vfs_map[fname]
                logger.debug(f"read_file: in memory {fname}: {len(contents)} bytes")
                return (contents, 0)
            with open(fname, "rb") as in_file:
                contents = in_file.read()
                logger.debug(f"read_file: disk read {fname}: {len(contents)} bytes")
                return (contents, 0)
        except BaseException as e:
            logger.debug(f"read_file: reading {fname}: exn: {e!r}")
            exnClass = type(e).__name__
            return (f"{fname}: {exnClass}: {e}".encode(), 1)

    async def _handle_process_outputs(
        self, stdout: asyncio.StreamReader, stderr: asyncio.StreamReader
    ) -> None:
        """
        Wait for both output streams to reach EOF, processing and
        accumulating the results in the meantime.
        """
        results = await asyncio.gather(
            self._core_stdout_processor(stdout),
            self._core_stderr_processor(stderr),
            return_exceptions=True,
        )

        # Raise any exceptions from processing stdout/err
        for r in results:
            if isinstance(r, Exception):
                raise SemgrepError(f"Error while running rules: {r}")

    async def _stream_exec_subprocess(self) -> int:
        """
        Run semgrep-core via fork/exec, consuming its output
        asynchronously.

        Return its exit code when it terminates.
        """
        process = await asyncio.create_subprocess_exec(
            *self._cmd,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
            limit=INPUT_BUFFER_LIMIT,
            preexec_fn=setrlimits_preexec_fn,
        )

        # Ensured by passing stdout/err named parameters above.
        assert process.stdout
        assert process.stderr

        await self._handle_process_outputs(process.stdout, process.stderr)

        # Return exit code of cmd. process should already be done
        return await process.wait()

    def _run_forked_analysis_in_child(self) -> None:
        """
        Run semgrep_analyze from within the child process.
        """
        # Like in the exec case, try to expand limits in the child.
        setrlimits_preexec_fn()

        # TYPE_CHECKING is always false at run time, but by doing this,
        # mypy will know which module 'semgrep_bridge_python' refers to,
        # and hence check the calls to it.
        if TYPE_CHECKING:
            import semgrep_bridge_python
        else:
            semgrep_bridge_python = SemgrepCore.get_bridge_module()

        # Currently, we delay initializing the OCaml runtime until we
        # are in the forked child.  Later, we may want to hoist this to
        # the parent somewhere so multiple queries can be more
        # efficiently performed in the context of a Snowflake UDF.
        semgrep_bridge_python.startup()

        # Invoke the semgrep_bridge_python module.
        err = semgrep_bridge_python.semgrep_analyze(self._cmd, self._handle_read_file)

        if err != None:
            # Convey an error back to the parent.
            print(err, file=sys.stderr)
            sys.exit(2)

        semgrep_bridge_python.shutdown()

    async def _stream_fork_subprocess(self) -> int:
        """
        Run semgrep_bridge_python.so in a forked (but not exec'd) child
        process, consuming its output asynchronously.

        Return its exit code when it terminates.
        """
        process = await fork_subprocess.start_fork_subprocess(
            lambda: self._run_forked_analysis_in_child(), limit=INPUT_BUFFER_LIMIT
        )

        await self._handle_process_outputs(process.stdout, process.stderr)

        return process.wait()

    def execute(self) -> int:
        """
        Run semgrep-core and listen to stdout to update
        progress_bar as necessary

        Blocks til completion and returns exit code
        """
        open_and_ignore("/tmp/core-runner-semgrep-BEGIN")

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

        if SemgrepCore.using_bridge_module():
            rc = asyncio.run(self._stream_fork_subprocess())
        else:
            rc = asyncio.run(self._stream_exec_subprocess())

        if self._progress_bar:
            self._progress_bar.close()

        open_and_ignore("/tmp/core-runner-semgrep-END")
        return rc


@frozen
class Task:
    path: str = field(converter=str)
    language: Language
    rule_nums: Sequence[int]


class TargetMappings(List[Task]):
    @property
    def rule_count(self) -> int:
        return len({rule for task in self for rule in task.rule_nums})

    @property
    def file_count(self) -> int:
        return len(self)


class Plan:
    """
    Saves and displays knowledge of what will be run

    to_json: creates the json passed to semgrep_core - see Input_to_core.atd
    log: outputs a summary of how many files will be scanned for each file
    """

    def __init__(self, mappings: List[Task], rule_ids: List[str]):
        self.target_mappings = TargetMappings(mappings)
        # important: this is a list of rule_ids, not a set
        # target_mappings relies on the index of each rule_id in rule_ids
        self.rule_ids = rule_ids

    def split_by_lang_label(self) -> Dict[str, "TargetMappings"]:
        result: Dict[str, TargetMappings] = collections.defaultdict(TargetMappings)
        for task in self.target_mappings:
            label = (
                "<multilang>"
                if task.language in {Language("regex"), Language("generic")}
                else task.language
            )
            result[label].append(task)
        return result

    def to_json(self) -> Dict[str, Any]:
        return {
            "target_mappings": [asdict(task) for task in self.target_mappings],
            "rule_ids": self.rule_ids,
        }

    @property
    def num_targets(self) -> int:
        return len(self.target_mappings)

    def log(self) -> None:
        metrics = get_state().metrics

        if self.target_mappings.rule_count == 0:
            logger.info("Nothing to scan.")
            return

        if self.target_mappings.rule_count == 1:
            logger.info(f"Scanning {unit_str(len(self.target_mappings), 'file')}.")
            return

        plans_by_language = sorted(
            self.split_by_lang_label().items(),
            key=lambda x: (x[1].file_count, x[1].rule_count),
            reverse=True,
        )
        if len(plans_by_language) == 1:
            logger.info(
                f"Scanning {unit_str(self.target_mappings.file_count, 'file')} with {unit_str(self.target_mappings.rule_count, f'{plans_by_language[0][0]} rule')}."
            )
            return

        logger.info("\nScanning across multiple languages:")
        for language, plan in plans_by_language:
            metrics.add_feature("language", language)

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
        return f"<Plan of {len(self.target_mappings)} tasks for {list(self.split_by_lang_label())}>"


class CoreRunner:
    """
    Handles interactions between semgrep and semgrep-core

    This includes properly invoking semgrep-core and parsing the output
    """

    def __init__(
        self,
        jobs: Optional[int],
        engine: EngineType,
        timeout: int,
        max_memory: int,
        timeout_threshold: int,
        interfile_timeout: int,
        optimizations: str,
        core_opts_str: Optional[str],
    ):
        if jobs is None:
            if engine is EngineType.PRO_INTERFILE:
                # In some cases inter-file analysis consumes too much memory, and
                # not using multi-core seems to help containing the problem.
                jobs = 1
            else:
                jobs = get_cpu_count()
        self._jobs = jobs
        self._timeout = timeout
        self._max_memory = max_memory
        self._timeout_threshold = timeout_threshold
        self._interfile_timeout = interfile_timeout
        self._optimizations = optimizations
        self._core_opts = shlex.split(core_opts_str) if core_opts_str else []

        self._binary_path = SemgrepCore.path()

        if engine.is_pro:
            deep_path = (
                SemgrepCore.deep_path()
            )  # DEPRECATED: To be removed by Feb 2023 launch
            pro_path = SemgrepCore.pro_path()

            if pro_path is None:
                if deep_path is not None:
                    logger.error(
                        f"""You have an old DeepSemgrep binary installed in {deep_path}
  DeepSemgrep is now Semgrep Pro, run `semgrep install-semgrep-pro` to install it,
  then please delete {deep_path} manually.
  """
                    )
                raise SemgrepError(
                    "Could not use Pro features: Semgrep Pro Engine not installed. Run `semgrep install-semgrep-pro`"
                )

            self._binary_path = pro_path

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
        except ValueError as exn:
            if returncode == -11 or returncode == -9:
                # Killed by signal 11 (segmentation fault), this could be a
                # stack overflow that was not intercepted by the OCaml runtime.
                soft_limit, _hard_limit = resource.getrlimit(resource.RLIMIT_STACK)
                tip = f"""
                Semgrep exceeded system resources. This may be caused by
                    1. Stack overflow. Try increasing the stack limit to
                       `{soft_limit}` by running `ulimit -s {soft_limit}`
                       before running Semgrep.
                    2. Out of memory. Try increasing the memory available to
                       your container (if running in CI). If that is not
                       possible, run `semgrep` with `--max-memory
                       $YOUR_MEMORY_LIMIT`.
                    3. Some extremely niche compiler/c-bindings bug. (We've
                       never seen this, but it's always possible.)
                    You can also try reducing the number of processes Semgrep
                    uses by running `semgrep` with `--jobs 1` (or some other
                    number of jobs). If you are running in CI, please try
                    running the same command locally.
                """
            else:
                tip = f"Semgrep encountered an internal error: {exn}."
            self._fail(
                f"{tip}",
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
        if timing.rules_parse_time:
            profiling_data.set_rules_parse_time(timing.rules_parse_time)

        for t in timing.targets:
            rule_timings = {
                rt.rule_id: Times(rt.parse_time, rt.match_time) for rt in t.rule_times
            }
            profiling_data.set_file_times(Path(t.path), rule_timings, t.run_time)

    def _add_max_memory_bytes(
        self, profiling_data: ProfilingData, max_memory_bytes: int
    ) -> None:
        """
        This represents the maximum amount of memory used by the OCaml side of
        Semgrep during its execution.

        This is useful for telemetry purposes.
        """
        profiling_data.set_max_memory_bytes(max_memory_bytes)

    def _plan_core_run(
        self, rules: List[Rule], target_manager: TargetManager, all_targets: Set[Path]
    ) -> Plan:
        """
        Gets the targets to run for each rule

        Returns this information as a list of rule ids and a list of targets with
        language + index of the rule ids for the rules to run each target on.
        Semgrep-core will use this to determine what to run (see Input_to_core.atd).
        Also updates all_targets, used by core_runner

        Note: this is a list because a target can appear twice (e.g. Java + Generic)
        """
        target_info: Dict[Tuple[Path, Language], List[int]] = collections.defaultdict(
            list
        )

        for i, rule in enumerate(rules):
            for language in rule.languages:
                targets = list(
                    target_manager.get_files_for_rule(
                        language, rule.includes, rule.excludes, rule.id
                    )
                )

                for target in targets:
                    all_targets.add(target)
                    target_info[target, language].append(i)

        return Plan(
            [
                Task(
                    path=target,
                    language=language,
                    rule_nums=target_info[target, language],
                )
                for target, language in target_info
            ],
            [rule.id for rule in rules],
        )

    def _run_rules_direct_to_semgrep_core_helper(
        self,
        rules: List[Rule],
        target_manager: TargetManager,
        dump_command_for_core: bool,
        engine: EngineType,
    ) -> Tuple[RuleMatchMap, List[SemgrepError], OutputExtra,]:
        state = get_state()
        logger.debug(f"Passing whole rules directly to semgrep_core")

        outputs: RuleMatchMap = collections.defaultdict(OrderedRuleMatchList)
        errors: List[SemgrepError] = []
        all_targets: Set[Path] = set()
        file_timeouts: Dict[Path, int] = collections.defaultdict(lambda: 0)
        max_timeout_files: Set[Path] = set()
        # TODO this is a quick fix, refactor this logic

        profiling_data: ProfilingData = ProfilingData()
        parsing_data: ParsingData = ParsingData()

        # Create an exit stack context manager to properly handle closing
        # either the temp files for an actual run or else the dump files for
        # a future direct run of semgrep-core. This method of file management
        # is OS-agnostic and should be portable across POSIX and Windows
        # systems. It also ensures that NamedTemporaryFile objects will delete
        # their corresponding temp files after closing streams to them.
        exit_stack = contextlib.ExitStack()
        rule_file = exit_stack.enter_context(
            (state.env.user_data_folder / "semgrep_rules.json").open("w+")
            if dump_command_for_core
            else tempfile.NamedTemporaryFile("w+", suffix=".json")
        )
        target_file = exit_stack.enter_context(
            (state.env.user_data_folder / "semgrep_targets.txt").open("w+")
            if dump_command_for_core
            else tempfile.NamedTemporaryFile("w+")
        )

        with exit_stack:

            plan = self._plan_core_run(rules, target_manager, all_targets)
            plan.log()
            parsing_data.add_targets(plan)
            target_file_contents = json.dumps(plan.to_json())
            target_file.write(target_file_contents)
            target_file.flush()

            rule_file_contents = json.dumps(
                {"rules": [rule._raw for rule in rules]}, indent=2, sort_keys=True
            )
            rule_file.write(rule_file_contents)
            rule_file.flush()

            # Create a map to feed to semgrep-core as an alternative to
            # having it actually read the files.
            vfs_map: Dict[str, bytes] = {
                target_file.name: target_file_contents.encode("UTF-8"),
                rule_file.name: rule_file_contents.encode("UTF-8"),
            }

            # Run semgrep
            cmd = [
                self._binary_path,
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
            if engine.is_pro:
                if auth.get_token() is None:
                    logger.error("!!!This is a proprietary extension of semgrep.!!!")
                    logger.error("!!!You must be logged in to access this extension!!!")
                else:
                    logger.error(
                        "You are using the Semgrep Pro Engine, our advanced analysis system uniquely designed to refine and enhance your results."
                    )
                    if engine is EngineType.PRO_INTERFILE:
                        logger.error(
                            "You can expect to see longer scan times - we're taking our time to make sure everything is just right for you. With <3, the Semgrep team."
                        )

                logger.info(f"Using Semgrep Pro installed in {self._binary_path}")
                version = sub_check_output(
                    [self._binary_path, "-pro_version"],
                    timeout=10,
                    encoding="utf-8",
                    stderr=subprocess.STDOUT,
                ).rstrip()
                logger.info(f"Semgrep Pro Version Info: ({version})")

                if engine is EngineType.PRO_INTERFILE:
                    targets = target_manager.targets
                    if len(targets) == 1 and targets[0].path.is_dir():
                        root = str(targets[0].path)
                    else:
                        # TODO: This is no longer true...
                        raise SemgrepError("deep mode needs a single target (root) dir")
                    cmd += ["-deep_inter_file"]
                    cmd += [
                        "-timeout_for_interfile_analysis",
                        str(self._interfile_timeout),
                    ]
                    cmd += [root]
                elif engine is EngineType.PRO_INTRAFILE:
                    cmd += ["-deep_intra_file"]

            stderr: Optional[int] = subprocess.PIPE
            if state.terminal.is_debug:
                cmd += ["--debug"]

            logger.debug("Running Semgrep engine with command:")
            logger.debug(" ".join(cmd))

            if dump_command_for_core:
                # Even if using the bridge, print the command as if
                # using the executable since presumably the user wants
                # to copy+paste it to a shell.  (The real command is
                # still visible in the log message above.)
                printed_cmd = cmd.copy()
                printed_cmd[0] = self._binary_path
                print(" ".join(printed_cmd))
                sys.exit(0)

            runner = StreamingSemgrepCore(cmd, plan.num_targets)
            runner.vfs_map = vfs_map
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
                self._add_max_memory_bytes(
                    profiling_data, core_output.time.max_memory_bytes
                )

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
                if isinstance(
                    err.error_type.value,
                    (
                        core.LexicalError,
                        core.ParseError,
                        core.PartialParsing,
                        core.SpecifiedParseError,
                        core.AstBuilderError,
                    ),
                ):
                    parsing_data.add_error(err)
            errors.extend(parsed_errors)

        output_extra = OutputExtra(
            all_targets, profiling_data, parsing_data, core_output.explanations
        )

        return (
            outputs,
            errors,
            output_extra,
        )

    def _run_rules_direct_to_semgrep_core(
        self,
        rules: List[Rule],
        target_manager: TargetManager,
        dump_command_for_core: bool,
        engine: EngineType,
    ) -> Tuple[RuleMatchMap, List[SemgrepError], OutputExtra,]:
        """
        Sometimes we may run into synchronicity issues with the latest DeepSemgrep binary.
        These issues may possibly cause a failure if a user, for instance, updates their
        version of Semgrep, but does not update to the latest version of DeepSemgrep.

        A short bandaid solution for now is to suggest that a user updates to the latest
        version, if the DeepSemgrep binary crashes for any reason.
        """
        try:
            return self._run_rules_direct_to_semgrep_core_helper(
                rules, target_manager, dump_command_for_core, engine
            )
        except Exception as e:
            if engine.is_pro:
                logger.error(
                    f"""

Semgrep Pro crashed during execution (unknown reason).
This can sometimes happen because either Semgrep Pro or Semgrep is out of date.

Try updating your version of Semgrep Pro (`semgrep install-semgrep-pro`) or your version of Semgrep (`pip install semgrep/brew install semgrep`).
If both are up-to-date and the crash persists, please contact support to report an issue!
When reporting the issue, please re-run the semgrep command with the
`--debug` flag so as to print more details about what happened, if you can.

Exception raised: `{e}`
                    """
                )
                sys.exit(2)
            raise e

    # end _run_rules_direct_to_semgrep_core

    def invoke_semgrep(
        self,
        target_manager: TargetManager,
        rules: List[Rule],
        dump_command_for_core: bool,
        engine: EngineType,
    ) -> Tuple[RuleMatchMap, List[SemgrepError], OutputExtra,]:
        """
        Takes in rules and targets and retuns object with findings
        """
        start = datetime.now()

        (
            findings_by_rule,
            errors,
            output_extra,
        ) = self._run_rules_direct_to_semgrep_core(
            rules, target_manager, dump_command_for_core, engine
        )

        logger.debug(
            f"semgrep ran in {datetime.now() - start} on {len(output_extra.all_targets)} files"
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
            output_extra,
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
                [self._binary_path]
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
