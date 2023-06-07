import asyncio
import collections
import contextlib
import json
import resource
import shlex
import subprocess
import sys
import tempfile
from datetime import datetime
from functools import lru_cache
from pathlib import Path
from typing import Any
from typing import Callable
from typing import cast
from typing import Coroutine
from typing import DefaultDict
from typing import Dict
from typing import FrozenSet
from typing import List
from typing import Mapping
from typing import Optional
from typing import Sequence
from typing import Set
from typing import Tuple

from attr import asdict
from attr import define
from attr import field
from attr import frozen
from boltons.iterutils import get_path
from rich import box
from rich.columns import Columns
from rich.padding import Padding
from rich.progress import BarColumn
from rich.progress import Progress
from rich.progress import TaskID
from rich.progress import TaskProgressColumn
from rich.progress import TextColumn
from rich.progress import TimeElapsedColumn
from rich.table import Table
from ruamel.yaml import YAML

import semgrep.output_from_core as core
from semgrep.app import auth
from semgrep.config_resolver import Config
from semgrep.console import console
from semgrep.constants import Colors
from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.core_output import core_error_to_semgrep_error
from semgrep.core_output import core_matches_to_rule_matches
from semgrep.core_output import parse_core_output
from semgrep.engine import EngineType
from semgrep.error import SemgrepCoreError
from semgrep.error import SemgrepError
from semgrep.error import with_color
from semgrep.output_extra import OutputExtra
from semgrep.parsing_data import ParsingData
from semgrep.profiling import ProfilingData
from semgrep.profiling import Times
from semgrep.rule import Rule
from semgrep.rule import RuleProduct
from semgrep.rule_match import OrderedRuleMatchList
from semgrep.rule_match import RuleMatchMap
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_types import Language
from semgrep.state import get_state
from semgrep.target_manager import TargetManager
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
) -> Tuple[int, Path, core.Position, core.Position, str]:
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


@frozen
class Task:
    path: str = field(converter=str)
    language: Language  # Xlang; see Xlang.mli
    # a rule_num is the rule's index in the rule ID list
    rule_nums: Tuple[int, ...]

    @property
    def language_label(self) -> str:
        return (
            "<multilang>"
            if not self.language.definition.is_target_language
            else self.language.definition.id
        )


class TargetMappings(List[Task]):
    @property
    def rule_count(self) -> int:
        return len({rule_num for task in self for rule_num in task.rule_nums})

    @property
    def file_count(self) -> int:
        return len(self)


@define
class TaskCounts:
    files: int = 0
    rules: int = 0


class Plan:
    """
    Saves and displays knowledge of what will be run

    to_json: creates the json passed to semgrep_core - see Input_to_core.atd
    log: outputs a summary of how many files will be scanned for each file
    """

    def __init__(
        self,
        mappings: List[Task],
        rules: List[Rule],
        *,
        lockfiles_by_ecosystem: Optional[Dict[Ecosystem, FrozenSet[Path]]] = None,
    ):
        self.target_mappings = TargetMappings(mappings)
        # important: this is a list of rule_ids, not a set
        # target_mappings relies on the index of each rule_id in rule_ids
        self.rules = rules
        self.lockfiles_by_ecosystem = lockfiles_by_ecosystem

    # TODO: make this counts_by_lang_label, returning TaskCounts
    def split_by_lang_label(self) -> Dict[str, "TargetMappings"]:
        result: Dict[str, TargetMappings] = collections.defaultdict(TargetMappings)
        for task in self.target_mappings:
            result[task.language_label].append(task)
        return result

    @lru_cache(maxsize=1000)  # caching this saves 60+ seconds on mid-sized repos
    def ecosystems_by_rule_nums(self, rule_nums: Tuple[int]) -> Set[Ecosystem]:
        return {
            ecosystem
            for rule_num in rule_nums
            for ecosystem in self.rules[rule_num].ecosystems
        }

    def counts_by_ecosystem(
        self,
    ) -> Mapping[Ecosystem, TaskCounts]:
        result: DefaultDict[Ecosystem, TaskCounts] = collections.defaultdict(TaskCounts)

        # if a pypi rule does reachability analysis on *.json files,
        # when the user has no .json files, then there is no task for it,
        # but we should still print it as a reachability rule we used
        # so we get rule counts by looking at all rules
        for rule in self.rules:
            for ecosystem in rule.ecosystems:
                result[ecosystem].rules += 1

        # one .json file could determine the reachability of libraries from pypi and npm at the same time
        # so one task might need increase counts for multiple ecosystems (unlike when splitting by lang)
        for task in self.target_mappings:
            for ecosystem in self.ecosystems_by_rule_nums(task.rule_nums):
                result[ecosystem].files += 1

        # if a rule scans npm and maven, but we only have npm lockfiles,
        # then we skip mentioning maven in debug info by deleting maven's counts
        if self.lockfiles_by_ecosystem is not None:
            unused_ecosystems = {
                ecosystem
                for ecosystem in result
                if not self.lockfiles_by_ecosystem.get(ecosystem)
            }
            for ecosystem in unused_ecosystems:
                del result[ecosystem]

        return result

    def to_json(self) -> Dict[str, Any]:
        return {
            "target_mappings": [asdict(task) for task in self.target_mappings],
            "rule_ids": [rule.id for rule in self.rules],
        }

    @property
    def num_targets(self) -> int:
        return len(self.target_mappings)

    def table_by_language(self) -> Table:
        table = Table(box=box.SIMPLE_HEAD, show_edge=False)
        table.add_column("Language")
        table.add_column("Rules", justify="right")
        table.add_column("Files", justify="right")

        plans_by_language = sorted(
            self.split_by_lang_label().items(),
            key=lambda x: (x[1].file_count, x[1].rule_count),
            reverse=True,
        )
        for language, plan in plans_by_language:
            table.add_row(language, str(plan.rule_count), str(plan.file_count))

        return table

    def table_by_ecosystem(self) -> Table:
        table = Table(box=box.SIMPLE_HEAD, show_edge=False)
        table.add_column("Ecosystem")
        table.add_column("Rules", justify="right")
        table.add_column("Files", justify="right")
        table.add_column("Lockfiles")

        counts_by_ecosystem = self.counts_by_ecosystem()

        for ecosystem, plan in sorted(
            counts_by_ecosystem.items(),
            key=lambda x: (x[1].files, x[1].rules),
            reverse=True,
        ):
            if self.lockfiles_by_ecosystem is not None:
                lockfile_paths = ", ".join(
                    str(lockfile)
                    for lockfile in self.lockfiles_by_ecosystem.get(ecosystem, [])
                )
            else:
                lockfile_paths = "N/A"

            table.add_row(
                ecosystem.kind,
                str(plan.rules),
                str(plan.files),
                lockfile_paths,
            )

        return table

    def table_by_origin(self) -> Table:
        table = Table(box=box.SIMPLE_HEAD, show_edge=False)
        table.add_column("Origin")
        table.add_column("Rules", justify="right")

        origin_counts = collections.Counter(
            get_path(
                rule.metadata, ("semgrep.dev", "rule", "origin"), default="unknown"
            )
            for rule in self.rules
            if rule.product == RuleProduct.sast
        )

        for origin, count in sorted(
            origin_counts.items(), key=lambda x: x[1], reverse=True
        ):
            origin_name = origin.replace("_", " ").capitalize()

            table.add_row(origin_name, str(count))

        return table

    def table_by_sca_analysis(self) -> Table:
        table = Table(box=box.SIMPLE_HEAD, show_edge=False)
        table.add_column("Analysis")
        table.add_column("Rules", justify="right")

        SCA_ANALYSIS_NAMES = {
            "reachable": "Reachability",
            "legacy": "Basic",
            "malicious": "Basic",
            "upgrade-only": "Basic",
        }

        sca_analysis_counts = collections.Counter(
            SCA_ANALYSIS_NAMES.get(rule.metadata.get("sca-kind", ""), "Unknown")
            for rule in self.rules
            if rule.product == RuleProduct.sca
        )

        for sca_analysis, count in sorted(
            sca_analysis_counts.items(), key=lambda x: x[1], reverse=True
        ):
            sca_analysis_name = sca_analysis.replace("_", " ").title()

            table.add_row(sca_analysis_name, str(count))

        return table

    def record_metrics(self) -> None:
        metrics = get_state().metrics

        for language in self.split_by_lang_label():
            metrics.add_feature("language", language)

    def print(self, *, with_tables_for: RuleProduct) -> None:
        if self.target_mappings.rule_count == 0:
            console.print("Nothing to scan.")
            return

        if self.target_mappings.rule_count == 1:
            console.print(f"Scanning {unit_str(len(self.target_mappings), 'file')}.")
            return

        if len(self.split_by_lang_label()) == 1:
            console.print(
                f"Scanning {unit_str(self.target_mappings.file_count, 'file')} with {unit_str(self.target_mappings.rule_count, f'{list(self.split_by_lang_label())[0]} rule')}."
            )
            return

        if with_tables_for == RuleProduct.sast:
            tables = [
                self.table_by_language(),
                self.table_by_origin(),
            ]
        elif with_tables_for == RuleProduct.sca:
            tables = [
                self.table_by_ecosystem(),
                self.table_by_sca_analysis(),
            ]
        else:
            tables = []

        columns = Columns(tables, padding=(1, 8))

        # rich tables are 2 spaces indented by default
        # deindent only by 1 to align the content, instead of the invisible table border
        console.print(Padding(columns, (1, 0)), deindent=1)

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
        engine_type: EngineType,
        timeout: int,
        max_memory: int,
        timeout_threshold: int,
        interfile_timeout: int,
        optimizations: str,
        core_opts_str: Optional[str],
    ):
        self._binary_path = engine_type.get_binary_path()
        self._jobs = jobs or engine_type.default_jobs
        self._engine_type = engine_type
        self._timeout = timeout
        self._max_memory = max_memory
        self._timeout_threshold = timeout_threshold
        self._interfile_timeout = interfile_timeout
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
            profiling_data.set_file_times(Path(t.path.value), rule_timings, t.run_time)

    def _add_max_memory_bytes(
        self, profiling_data: ProfilingData, max_memory_bytes: int
    ) -> None:
        """
        This represents the maximum amount of memory used by the OCaml side of
        Semgrep during its execution.

        This is useful for telemetry purposes.
        """
        profiling_data.set_max_memory_bytes(max_memory_bytes)

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
            if rule.product != RuleProduct.sca
            or any(lockfiles[ecosystem] for ecosystem in rule.ecosystems)
        ]

        for rule_num, rule in enumerate(rules):
            for language in rule.languages:
                targets = list(
                    target_manager.get_files_for_rule(
                        language, rule.includes, rule.excludes, rule.id
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
                    language=language,
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
            plan = self.plan_core_run(rules, target_manager, all_targets)
            plan.record_metrics()

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
                str(self._binary_path),
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
            all_targets,
            profiling_data,
            parsing_data,
            core_output.explanations,
            core_output.rules_by_engine,
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
            core_output = parse_core_output(output_json)

            parsed_errors += [
                core_error_to_semgrep_error(e) for e in core_output.errors
            ]

        return dedup_errors(parsed_errors)
