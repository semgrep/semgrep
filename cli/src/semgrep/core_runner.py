import asyncio
import collections
import contextlib
import json
import resource
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

from attr import evolve
from rich.progress import BarColumn
from rich.progress import Progress
from rich.progress import TaskID
from rich.progress import TaskProgressColumn
from rich.progress import TextColumn
from rich.progress import TimeElapsedColumn
from ruamel.yaml import YAML

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.app import auth
from semgrep.config_resolver import Config
from semgrep.console import console
from semgrep.constants import Colors
from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.core_output import core_error_to_semgrep_error
from semgrep.core_output import core_matches_to_rule_matches
from semgrep.core_targets_plan import Plan
from semgrep.core_targets_plan import Task
from semgrep.engine import EngineType
from semgrep.error import SemgrepCoreError
from semgrep.error import SemgrepError
from semgrep.error import with_color
from semgrep.output_extra import OutputExtra
from semgrep.parsing_data import ParsingData
from semgrep.rule import Rule
from semgrep.rule_match import OrderedRuleMatchList
from semgrep.rule_match import RuleMatchMap
from semgrep.semgrep_types import Language
from semgrep.state import get_state
from semgrep.target_manager import TargetManager
from semgrep.target_mode import TargetModeConfig
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


def get_contributions(engine_type: EngineType) -> out.Contributions:
    binary_path = engine_type.get_binary_path()
    start = datetime.now()
    if binary_path is None:  # should never happen, doing this for mypy
        raise SemgrepError("semgrep engine not found.")
    cmd = [
        str(binary_path),
        "-json",
        "-dump_contributions",
    ]
    env = get_state().env

    try:
        # nosemgrep: python.lang.security.audit.dangerous-subprocess-use.dangerous-subprocess-use
        raw_output = subprocess.run(
            cmd,
            timeout=env.git_command_timeout,
            capture_output=True,
            encoding="utf-8",
            check=True,
        ).stdout
        contributions = out.Contributions.from_json_string(raw_output)
    except subprocess.CalledProcessError:
        logger.warning("Failed to collect contributions. Continuing with scan...")
        contributions = out.Contributions([])

    logger.debug(f"semgrep contributions ran in {datetime.now() - start}")
    return contributions


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
    # since this logging is inside the child core processes,
    # which have their own output requirements so that CLI can parse its stdout,
    # we use a different logger than the usual "semgrep" one
    core_logger = getLogger("semgrep_core")

    # Get current soft and hard stack limits
    old_soft_limit, hard_limit = resource.getrlimit(resource.RLIMIT_STACK)
    core_logger.info(
        f"Existing stack limits: Soft: {old_soft_limit}, Hard: {hard_limit}"
    )

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
            core_logger.info(f"Trying to set soft limit to {soft_limit}")
            resource.setrlimit(resource.RLIMIT_STACK, (soft_limit, hard_limit))
            core_logger.info(
                f"Successfully set stack limit to {soft_limit}, {hard_limit}"
            )
            return
        except Exception as e:
            core_logger.info(
                f"Failed to set stack limit to {soft_limit}, {hard_limit}. Trying again."
            )
            core_logger.verbose(str(e))

    core_logger.info("Failed to change stack limits")


# This is used only to dedup errors from validate_configs(). For dedupping errors
# from _invoke_semgrep(), see output.py and the management of self.error_set
def dedup_errors(errors: List[SemgrepCoreError]) -> List[SemgrepCoreError]:
    return list({uniq_error_id(e): e for e in errors}.values())


def uniq_error_id(
    error: SemgrepCoreError,
) -> Tuple[int, Path, out.Position, out.Position, str]:
    return (
        error.code,
        Path(error.core.location.path.value),
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

    def __init__(self, cmd: List[str], total: int, engine_type: EngineType) -> None:
        """
        cmd: semgrep-core command to run
        total: how many rules to run / how many "." we expect to see a priori
               used to display progress_bar
        """
        self._cmd = cmd
        self._total = total
        self._stdout = ""
        self._stderr = ""
        self._progress_bar: Optional[Progress] = None
        self._progress_bar_task_id: Optional[TaskID] = None
        self._engine_type: EngineType = engine_type

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

        # Start out reading two bytes at a time (".\n")
        get_input: Callable[
            [asyncio.StreamReader], Coroutine[Any, Any, bytes]
        ] = lambda s: s.readexactly(2)
        reading_json = False
        # Read ".\n" repeatedly until we reach the JSON output.
        # TODO: read progress from one channel and JSON data from another.
        # or at least write/read progress as a stream of JSON objects so that
        # we don't have to hack a parser together.
        has_started = False
        while True:
            # blocking read if buffer doesnt contain any lines or EOF
            try:
                line_bytes = await get_input(stream)
            except asyncio.IncompleteReadError:
                logger.debug(self._stderr)
                # happens if the data that follows a sequence of zero
                # or more ".\n" has fewer than two bytes, such as:
                # "", "3", ".\n.\n3", ".\n.\n.\n.", etc.

                # Hack: the exact wording of parts this message may be used in metrics queries
                # that are looking for it. Make sure `semgrep-core exited with unexpected output`
                # and `interfile analysis` are both in the message, or talk to Emma.
                raise SemgrepError(
                    f"""
                    You are seeing this because the engine was killed.

                    The most common reason this happens is because it used too much memory.
                    If your repo is large (~10k files or more), you have three options:
                    1. Increase the amount of memory available to semgrep
                    2. Reduce the number of jobs semgrep runs with via `-j <jobs>`. We
                        recommend using 1 job if you are running out of memory.
                    3. Scan the repo in parts (contact us for help)

                    Otherwise, it is likely that semgrep is hitting the limit on only some
                    files. In this case, you can try to set the limit on the amount of memory
                    semgrep can use on each file with `--max-memory <memory>`. We recommend
                    lowering this to a limit 70% of the available memory. For CI runs with
                    interfile analysis, the default max-memory is 5000MB. Without, the default
                    is unlimited.

                    The last thing you can try if none of these work is to raise the stack
                    limit with `ulimit -s <limit>`.

                    If you have tried all these steps and still are seeing this error, please
                    contact us.

                       Error: semgrep-core exited with unexpected output
                    """
                )

            if (
                not has_started
                and self._progress_bar
                and self._progress_bar_task_id is not None
            ):
                has_started = True
                self._progress_bar.start_task(self._progress_bar_task_id)

            # read returns empty when EOF
            if not line_bytes:
                self._stdout = b"".join(stdout_lines).decode("utf-8", "replace")
                break

            if line_bytes == b".\n" and not reading_json:
                # We expect to see 3 dots for each target, when running interfile analysis:
                # - once when finishing phase 4, name resolution, on that target
                # - once when finishing phase 5, taint configs, on that target
                # - once when finishing analysis on that target as usual
                #
                # However, for regular OSS Semgrep, we only print one dot per
                # target, that being the last bullet point listed above.
                #
                # So a dot counts as 1 progress if running Pro, but 3 progress if
                # running the OSS engine.
                advanced_targets = 1 if self._engine_type.is_interfile else 3

                if self._progress_bar and self._progress_bar_task_id is not None:
                    self._progress_bar.update(
                        self._progress_bar_task_id, advance=advanced_targets
                    )
            elif chr(line_bytes[0]).isdigit() and not reading_json:
                if not line_bytes.endswith(b"\n"):
                    line_bytes = line_bytes + await stream.readline()
                extra_targets = int(line_bytes)
                num_total_targets += extra_targets
                if self._progress_bar and self._progress_bar_task_id is not None:
                    self._progress_bar.update(
                        self._progress_bar_task_id, total=num_total_targets
                    )
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

    def execute(self) -> int:
        """
        Run semgrep-core and listen to stdout to update
        progress_bar as necessary

        Blocks til completion and returns exit code
        """
        open_and_ignore("/tmp/core-runner-semgrep-BEGIN")

        terminal = get_state().terminal
        with Progress(
            # align progress bar to output by indenting 2 spaces
            # (the +1 space comes from column gap)
            TextColumn(" "),
            BarColumn(),
            TaskProgressColumn(),
            TimeElapsedColumn(),
            console=console,
            disable=(
                not sys.stderr.isatty()
                or self._total <= 1
                or terminal.is_quiet
                or terminal.is_debug
            ),
        ) as progress_bar:
            self._progress_bar = progress_bar
            self._progress_bar_task_id = self._progress_bar.add_task(
                "", total=self._total, start=False
            )

            rc = asyncio.run(self._stream_exec_subprocess())

        open_and_ignore("/tmp/core-runner-semgrep-END")
        return rc


class CoreRunner:
    """
    Handles interactions between semgrep and semgrep-core

    This includes properly invoking semgrep-core and parsing the output
    """

    def __init__(
        self,
        jobs: Optional[int],
        engine_type: EngineType,
        run_secrets: bool,
        timeout: int,
        max_memory: int,
        timeout_threshold: int,
        interfile_timeout: int,
        optimizations: str,
        allow_untrusted_postprocessors: bool,
        respect_rule_paths: bool = True,
    ):
        self._binary_path = engine_type.get_binary_path()
        self._jobs = jobs or engine_type.default_jobs
        self._engine_type = engine_type
        self._run_secrets = engine_type
        self._timeout = timeout
        self._max_memory = max_memory
        self._timeout_threshold = timeout_threshold
        self._interfile_timeout = interfile_timeout
        self._optimizations = optimizations
        self._allow_untrusted_postprocessors = allow_untrusted_postprocessors
        self._respect_rule_paths = respect_rule_paths

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
                parsed_output = out.CoreOutput.from_json(output_json)
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

    @staticmethod
    def plan_core_run(
        rules: List[Rule],
        target_manager: TargetManager,
        all_targets: Optional[Set[Path]] = None,
    ) -> Plan:
        """
        Gets the targets to run for each rule

        Returns this information as a list of rule ids and a list of targets with
        language + index of the rule ids for the rules to run each target on.
        Semgrep-core will use this to determine what to run (see Input_to_core.atd).
        Also updates all_targets if set, used by core_runner

        Note: this is a list because a target can appear twice (e.g. Java + Generic)
        """
        target_info: Dict[Tuple[Path, Language], List[int]] = collections.defaultdict(
            list
        )

        lockfiles = target_manager.get_all_lockfiles()

        rules = [
            rule
            for rule in rules
            # filter out SCA rules with no relevant lockfiles
            if not (isinstance(rule.product.value, out.SCA))
            or any(lockfiles[ecosystem] for ecosystem in rule.ecosystems)
        ]

        for rule_num, rule in enumerate(rules):
            for language in rule.languages:
                targets = list(
                    target_manager.get_files_for_rule(
                        language,
                        rule.includes,
                        rule.excludes,
                        rule.id,
                    )
                )

                for target in targets:
                    if all_targets is not None:
                        all_targets.add(target)
                    target_info[target, language].append(rule_num)

        return Plan(
            [
                Task(
                    path=target,
                    analyzer=language,
                    # tuple conversion makes rule_nums hashable, so usable as cache key
                    rule_nums=tuple(target_info[target, language]),
                )
                for target, language in target_info
            ],
            rules,
            lockfiles_by_ecosystem=lockfiles,
        )

    def _run_rules_direct_to_semgrep_core_helper(
        self,
        rules: List[Rule],
        target_manager: TargetManager,
        dump_command_for_core: bool,
        time_flag: bool,
        matching_explanations: bool,
        engine: EngineType,
        run_secrets: bool,
        target_mode_config: TargetModeConfig,
    ) -> Tuple[RuleMatchMap, List[SemgrepError], OutputExtra,]:
        state = get_state()
        logger.debug(f"Passing whole rules directly to semgrep_core")

        outputs: RuleMatchMap = collections.defaultdict(OrderedRuleMatchList)
        errors: List[SemgrepError] = []
        all_targets: Set[Path] = set()
        file_timeouts: Dict[Path, int] = collections.defaultdict(lambda: 0)
        max_timeout_files: Set[Path] = set()
        # TODO this is a quick fix, refactor this logic

        profiling_data: Optional[out.Profile] = None
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
        if target_mode_config.is_pro_diff_scan:
            diff_target_file = exit_stack.enter_context(
                (state.env.user_data_folder / "semgrep_diff_targets.txt").open("w+")
                if dump_command_for_core
                else tempfile.NamedTemporaryFile("w+")
            )

        with exit_stack:
            cmd = [
                str(self._binary_path),
                "-json",
            ]

            # adding rules option
            rule_file_contents = json.dumps(
                {"rules": [rule._raw for rule in rules]}, indent=2, sort_keys=True
            )
            rule_file.write(rule_file_contents)
            rule_file.flush()
            cmd.extend(["-rules", rule_file.name])

            # adding multi-core option
            cmd.extend(["-j", str(self._jobs)])

            # adding targets option
            if target_mode_config.is_pro_diff_scan:
                diff_targets = target_mode_config.get_diff_targets()
                diff_target_file_contents = "\n".join(
                    [str(path) for path in diff_targets]
                )
                diff_target_file.write(diff_target_file_contents)
                diff_target_file.flush()
                cmd.extend(["-diff_targets", diff_target_file.name])
                cmd.extend(["-diff_depth", str(target_mode_config.get_diff_depth())])

                # For the pro diff scan, it's necessary to consider all input files as
                # "targets" and the files that have changed between the head and baseline
                # commits as "diff targets". To compile a comprehensive list of all input files
                # for `plan`, the `baseline_handler` is disabled within the `target_manager`
                # when executing `plan_core_run`.
                plan = self.plan_core_run(
                    rules, evolve(target_manager, baseline_handler=None), all_targets
                )

            else:
                plan = self.plan_core_run(rules, target_manager, all_targets)

            plan.record_metrics()
            parsing_data.add_targets(plan)
            target_file_contents = json.dumps(plan.to_json())
            target_file.write(target_file_contents)
            target_file.flush()
            cmd.extend(["-targets", target_file.name])

            # adding limits
            cmd.extend(
                [
                    "-timeout",
                    str(self._timeout),
                    "-timeout_threshold",
                    str(self._timeout_threshold),
                    "-max_memory",
                    str(self._max_memory),
                ]
            )
            if matching_explanations:
                cmd.append("-matching_explanations")
            if time_flag:
                cmd.append("-json_time")
            if not self._respect_rule_paths:
                cmd.append("-disable_rule_paths")

            # Create a map to feed to semgrep-core as an alternative to
            # having it actually read the files.
            vfs_map: Dict[str, bytes] = {
                target_file.name: target_file_contents.encode("UTF-8"),
                rule_file.name: rule_file_contents.encode("UTF-8"),
            }

            if self._optimizations != "none":
                cmd.append("-fast")

            if run_secrets:
                cmd += ["-secrets"]
                if not engine.is_pro:
                    # This should be impossible, but the types don't rule it out so...
                    raise SemgrepError(
                        "Secrets post processors tried to run without the pro-engine."
                    )

            if self._allow_untrusted_postprocessors:
                cmd.append("-allow-untrusted-postprocessors")

            # TODO: use exact same command-line arguments so just
            # need to replace the SemgrepCore.path() part.
            if engine.is_pro:
                if auth.get_token() is None:
                    logger.error("!!!This is a proprietary extension of semgrep.!!!")
                    logger.error("!!!You must be logged in to access this extension!!!")
                else:
                    if engine is EngineType.PRO_INTERFILE:
                        logger.error(
                            "Semgrep Pro Engine may be slower and show different results than Semgrep OSS."
                        )

                if engine is EngineType.PRO_INTERFILE:
                    targets = target_manager.targets
                    if len(targets) == 1:
                        root = str(targets[0].path)
                    else:
                        raise SemgrepError(
                            "Inter-file analysis can only take a single target (for multiple files pass a directory)"
                        )
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
                printed_cmd[0] = str(self._binary_path)
                print(" ".join(printed_cmd))
                sys.exit(0)

            # Multiplied by three, because we have three places in Pro Engine to
            # report progress, versus one for OSS Engine.
            runner = StreamingSemgrepCore(cmd, plan.num_targets * 3, engine)
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
            core_output = out.CoreOutput.from_json(output_json)
            if core_output.paths.skipped:
                for skip in core_output.paths.skipped:
                    if skip.rule_id:
                        rule_info = f"rule {skip.rule_id}"
                    else:
                        rule_info = "all rules"
                        logger.verbose(
                            f"skipped '{skip.path}' [{rule_info}]: {skip.reason}: {skip.details}"
                        )

            # end with tempfile.NamedTemporaryFile(...) ...
            outputs = core_matches_to_rule_matches(rules, core_output)
            parsed_errors = [core_error_to_semgrep_error(e) for e in core_output.errors]
            for err in core_output.errors:
                if isinstance(err.error_type.value, out.Timeout):
                    assert err.location.path is not None

                    file_timeouts[Path(err.location.path.value)] += 1
                    if (
                        self._timeout_threshold != 0
                        and file_timeouts[Path(err.location.path.value)]
                        >= self._timeout_threshold
                    ):
                        max_timeout_files.add(Path(err.location.path.value))
                if isinstance(
                    err.error_type.value,
                    (
                        out.LexicalError,
                        out.ParseError,
                        out.PartialParsing,
                        out.SpecifiedParseError,
                        out.AstBuilderError,
                    ),
                ):
                    parsing_data.add_error(err)
            errors.extend(parsed_errors)

        output_extra = OutputExtra(
            core_output,
            all_targets,
            parsing_data,
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
        time_flag: bool,
        matching_explanations: bool,
        engine: EngineType,
        run_secrets: bool,
        target_mode_config: TargetModeConfig,
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
                rules,
                target_manager,
                dump_command_for_core,
                time_flag,
                matching_explanations,
                engine,
                run_secrets,
                target_mode_config,
            )
        except SemgrepError as e:
            # Handle Semgrep errors normally
            raise e
        except Exception as e:
            # Unexpected error, output a warning that the engine might be out of date
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

    def invoke_semgrep_core(
        self,
        target_manager: TargetManager,
        rules: List[Rule],
        dump_command_for_core: bool,
        time_flag: bool,
        matching_explanations: bool,
        engine: EngineType,
        run_secrets: bool,
        target_mode_config: TargetModeConfig,
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
            rules,
            target_manager,
            dump_command_for_core,
            time_flag,
            matching_explanations,
            engine,
            run_secrets,
            target_mode_config,
        )

        logger.debug(
            f"semgrep ran in {datetime.now() - start} on {len(output_extra.all_targets)} files"
        )
        by_severity = collections.defaultdict(list)
        for rule, findings in findings_by_rule.items():
            by_severity[rule.severity.to_json().lower()].extend(findings)

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
        if self._binary_path is None:  # should never happen, doing this for mypy
            raise SemgrepError("semgrep engine not found.")

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

            cmd = [
                str(self._binary_path),
                "-json",
                "-check_rules",
                rule_file.name,
                *configs,
            ]

            # only scanning combined rules
            runner = StreamingSemgrepCore(cmd, 1, self._engine_type)
            returncode = runner.execute()

            # Process output
            output_json = self._extract_core_output(
                metachecks, returncode, " ".join(cmd), runner.stdout, runner.stderr
            )
            core_output = out.CoreOutput.from_json(output_json)

            parsed_errors += [
                core_error_to_semgrep_error(e) for e in core_output.errors
            ]

        return dedup_errors(parsed_errors)
