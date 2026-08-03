#
# Copyright (c) 2024-2025 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
##############################################################################
# Prelude
##############################################################################
# Allows function calls from Python into OCaml, to allow us to incrementally
# migrate pysemgrep functionality to osemgrep piece by piece.
#
# See `src/rpc/README.txt` from the repository root for more details.
# coupling: src/rpc/RPC.handle_call()
# coupling: semgrep_output_v1.atd which defines the CallXxx and RetXxx
from __future__ import annotations

import logging
import signal
import subprocess
import sys
import threading
from dataclasses import dataclass
from dataclasses import field
from datetime import datetime
from types import TracebackType
from typing import IO
from typing import List
from typing import Optional
from typing import Type
from typing import TypeVar

from opentelemetry import trace as otrace

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep import simple_profiling as simple_profiling_module
from semgrep import telemetry
from semgrep.semgrep_core import SemgrepCore
from semgrep.simple_profiling import import_simple_profiling
from semgrep.simple_profiling import simple_profiling
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

##############################################################################
# Constants
##############################################################################

# This is a typical system default, but let's not leave it up to chance.
ENCODING = "utf-8"

# After a call, how long we wait, in seconds, for the subprocess to exit cleanly
# before killing it. There's no real downside to having this be low, except that
# we are more likely to get an error in the log even if there's no real issue
# except that the system is overloaded or something like that.
#
# We do want to log if we hit this timeout, though, since it *could* be
# indicative of a real problem.
SUBPROC_TIMEOUT_S = 1

# Maximum amount of the RPC subprocess's stderr we retain and surface when it
# dies without responding. We keep only the *tail* (the most recent output,
# where a crash message lands), so this also bounds memory: see _StderrTail.
STDERR_TAIL_BYTES = 4096

##############################################################################
# Helpers
##############################################################################


class _StderrTail:
    """Drains a subprocess's stderr in a daemon thread, keeping only the last
    STDERR_TAIL_BYTES. Bounds memory (no temp file), and the dedicated reader
    is what keeps stderr=PIPE from deadlocking while we block on stdout.

    If `tee` is given, each chunk is also written through to it. Under debug that
    keeps semgrep-core's stderr streaming live (not just surfaced on a crash),
    while we still retain a bounded tail for the death diagnostic.
    """

    def __init__(self, stream: IO[bytes], tee: Optional[IO[bytes]] = None) -> None:
        self._buf = bytearray()
        self._trimmed = False
        self._tee = tee
        self._lock = threading.Lock()
        self._thread = threading.Thread(target=self._drain, args=(stream,), daemon=True)
        self._thread.start()

    def _drain(self, stream: IO[bytes]) -> None:
        try:
            for chunk in iter(lambda: stream.read(4096), b""):
                with self._lock:
                    self._buf += chunk
                    if len(self._buf) > STDERR_TAIL_BYTES:
                        del self._buf[:-STDERR_TAIL_BYTES]
                        self._trimmed = True
                # Tee outside the lock: no blocking I/O while holding it.
                self._tee_chunk(chunk)
        except (OSError, ValueError):
            # Stream closed underneath us; nothing more to capture.
            pass

    def _tee_chunk(self, chunk: bytes) -> None:
        if self._tee is None:
            return
        try:
            self._tee.write(chunk)
            self._tee.flush()
        except (OSError, ValueError):
            # Live passthrough is best-effort; a failure must not stop draining.
            self._tee = None

    def tail(self) -> str:
        # Let the child's final stderr flush before we snapshot.
        self._thread.join(timeout=0.5)
        with self._lock:
            data = bytes(self._buf)
            trimmed = self._trimmed
        text = data.decode(ENCODING, errors="replace")
        # If we trimmed, the first line is a partial fragment; drop it.
        if trimmed and "\n" in text:
            text = text.split("\n", 1)[1]
        return text.strip()


def _stderr_capture_enabled() -> bool:
    """Whether to capture semgrep-core stderr for the crash diagnostic. Only
    under debug logging: that stderr can carry request-derived data (matched
    source, etc.), so it must stay out of default logs."""
    from semgrep.state import get_state

    return get_state().terminal.log_level == logging.DEBUG


def _capture_stderr(
    stream: Optional[IO[bytes]], enabled: bool
) -> Optional[_StderrTail]:
    """Attach a stderr drainer when capture is enabled (see
    _stderr_capture_enabled). Capture only happens under debug, so we also tee
    the stream to our stderr to keep semgrep-core's logs streaming live."""
    if not (enabled and stream is not None):
        return None
    return _StderrTail(stream, tee=getattr(sys.stderr, "buffer", None))


def _describe_exit(returncode: Optional[int]) -> str:
    """Describe a subprocess exit status. States the fact only -- no guesses
    about the cause (e.g. "likely OOM"), which tend to anchor debugging on the
    wrong thing. The signal name plus the captured stderr are enough to go on.
    """
    if returncode is None:
        return "did not exit (still running)"
    if returncode < 0:
        signum = -returncode
        try:
            name = signal.Signals(signum).name
        except ValueError:
            name = f"signal {signum}"
        return f"was killed by signal {signum} ({name})"
    if returncode == 0:
        return "exited cleanly (code 0)"
    return f"exited with code {returncode}"


def _stderr_suffix(stderr_tail: Optional[_StderrTail]) -> str:
    """The captured-stderr block to append to a failure message, or "" if none."""
    tail = stderr_tail.tail() if stderr_tail is not None else ""
    if not tail:
        return ""
    return f"\n--- semgrep-core stderr (last {STDERR_TAIL_BYTES} bytes) ---\n{tail}"


def _diagnose_death(
    returncode: Optional[int], stderr_tail: Optional[_StderrTail]
) -> None:
    """Log a single, correlated ERROR explaining why the RPC subprocess failed
    to return a response. Call only when the process exited on its own (a
    process we killed ourselves is not a 'death' to diagnose -- say so plainly
    at the kill site instead)."""
    logger.error(
        f"RPC subprocess failed: semgrep-core {_describe_exit(returncode)}."
        + _stderr_suffix(stderr_tail)
    )


# Read `size` bytes from `io`. Returns fewer bytes if we hit EOF.
def _really_read(io: IO[bytes], size: int) -> str:
    # Operate on bytes, not str.
    out: bytes = b""
    while len(out) < size:
        # `read` may return fewer bytes than requested. In practice this is
        # unlikely, but we need to handle it.
        #
        # We could have used a BufferedReader which does this for us, but to do
        # so we'd need access to the `buffer` property of the input stream.
        # While it's documented in the (terrible) Python API docs for at least
        # some kinds of input streams, MyPy doesn't recognize it and it's not
        # clear to me (nmote) whether it is guaranteed to be present on the
        # streams provided by subprocess.Popen. So, to be on the safe side,
        # we'll just do this ourselves.
        new: bytes = io.read(size)
        # This happens if we hit EOF. In that case, repeatedly reading will lead
        # to an infinite loop.
        if len(new) == 0:
            logger.error(f"0 bytes read from RPC input stream")
            break
        out = out + new
    # When we read the RPC call for file targeting, we could encounter files
    # with non-utf8 characters, in that case we replace them with <?>
    # i.e abc.txt -> ab<?>.txt
    return out.decode(ENCODING, errors="replace")


def _read_packet(io: IO[bytes]) -> Optional[str]:
    # Unlike `read`, `readline` is guaranteed to return a full line unless there
    # is an EOF
    raw = io.readline()
    if raw == b"":
        # EOF: subprocess closed stdout without responding, almost always
        # because it died. Silent here; the caller diagnoses via _diagnose_death.
        return None
    # errors="replace" so non-UTF-8 garbage on stdout reaches the isdigit check
    # below (a clean protocol error) rather than raising UnicodeDecodeError.
    size_str = raw.decode(ENCODING, errors="replace").strip()
    if not size_str.isdigit():
        # Non-digit line: something wrote non-packet data to semgrep-core's
        # stdout. That's a protocol violation distinct from the subprocess
        # dying, so report it as its own error.
        got = f"'{size_str[:50]}'" if size_str else "a blank line"
        logger.error(
            "RPC protocol error: expected a numeric length header from "
            f"semgrep-core, got {got} (something wrote non-packet data to its "
            "stdout)"
        )
        return None
    size = int(size_str)
    return _really_read(io, size)


def _write_packet(io: IO[bytes], packet: str) -> None:
    # Size in bytes
    size: int = len(packet.encode(ENCODING))
    size_str = str(size) + "\n"
    io.write(size_str.encode(ENCODING))
    io.write(packet.encode(ENCODING))
    io.flush()


def _wrap_call_with_trace_context(call: out.FunctionCall) -> out.RpcCall:
    """Wrap a function call with the current OpenTelemetry span context."""
    from semgrep.state import get_state

    state = get_state()
    ctx = state.telemetry._get_current_context()
    span_id = otrace.format_span_id(ctx.span_id) if ctx.is_valid else None
    return out.RpcCall(call=call, parent_span_id=span_id)


def _parse_function_result(packet: str) -> Optional[out.FunctionReturn]:
    try:
        res = out.FunctionResult.from_json_string(packet)
        import_simple_profiling(res.profiling_results)
        return res.function_return
    # There are at least two kinds of exceptions that can be raised during
    # deserialization. Instead of enumerating them and hoping that we stay up to
    # date, let's just use a catch-all. In the end it doesn't really matter
    # exactly what went wrong, we still want to catch it and log.
    except Exception as e:
        truncated = packet[:50]
        logger.error(f"Error while deserializing RPC response '{truncated}': {e}")
        return None


##############################################################################
# Entry point
##############################################################################
T = TypeVar("T")


def _cmd() -> List[str]:
    """
    Return the base command to run an RPC call or start an RPC server.
    """
    from semgrep.state import get_state

    # We always use the pro binary if it's available. It's up to the caller to
    # appropriately handle the case where the pro function is not available and
    # to ensure that pro RPC methods are only called during a pro scan.
    semgrep_core_path = SemgrepCore.pro_path() or SemgrepCore.executable_path()
    cmd: List[str] = []

    cmd.append(str(semgrep_core_path))
    cmd.append("-rpc")

    if simple_profiling_module.enabled_simple_profiling:
        cmd.append("-simple_profiling")

    state = get_state()
    if state.terminal.log_level is logging.DEBUG:
        cmd.append("-debug")

    # Parallelism and memory profile flags need to be inherited.
    # THINK: what else needs to be?  timeout? max_memory_mb?

    num_jobs = state.jobs()
    if num_jobs is not None:
        cmd.extend(["-j", str(num_jobs)])

    mem_policy = state.memory_policy()
    if mem_policy is not None:
        cmd.extend(["-x-mem-policy", mem_policy.cli_value])

    return cmd


@simple_profiling
def rpc_call(call: out.FunctionCall, cls: Type[T]) -> Optional[T]:
    from semgrep.state import get_state

    start = datetime.now()

    cmd = _cmd()

    state = get_state()
    capture_stderr = _stderr_capture_enabled()
    if state.telemetry.enabled:
        cmd.append("-trace")
        if state.telemetry.trace_endpoint is not None:
            cmd.extend(["-trace_endpoint", state.telemetry.trace_endpoint])
        state.telemetry.inject()

    with subprocess.Popen(
        cmd,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE if capture_stderr else subprocess.DEVNULL,
        text=False,
    ) as proc:
        stderr_tail = _capture_stderr(proc.stderr, capture_stderr)
        # Whether the subprocess sent us a well-formed response packet. If it
        # did not, the finally block diagnoses why (crash/signal + stderr).
        got_response = False
        try:
            # These need to be local variables because otherwise mypy doesn't
            # trust the results of the None checks.
            proc_stdin = proc.stdin
            proc_stdout = proc.stdout
            if proc_stdin is None or proc_stdout is None:
                # This can't happen, since we set stdin and stdout args to PIPE
                # above. But mypy doesn't understand that. But log just in case
                # it actually can happen.
                logger.error(f"RPC subprocess missing stdout or stdin channel")
                return None
            call_str = _wrap_call_with_trace_context(call).to_json_string().strip()
            try:
                _write_packet(proc_stdin, call_str)
                proc_stdin.close()
            except BrokenPipeError:
                # The subprocess died before reading its request. Degrade to
                # None; the finally block diagnoses the death.
                return None

            ret_str = _read_packet(proc_stdout)
            if ret_str is None:
                # No need to log here; the finally block diagnoses the failure.
                return None
            ret = _parse_function_result(ret_str)
            if ret is None:
                # No need to log here, it's handled in the error case of
                # _parse_function_return
                return None
            # We got a well-formed response packet -- whatever it contains, the
            # subprocess responded and did not die on us.
            got_response = True
            # Any request can return an error
            if isinstance(ret.value, out.RetError):
                err: str = ret.value.value
                logger.error(f"RPC response indicated an error: {err}")
                return None
            # Check that we got the correct kind of response
            if isinstance(ret.value, cls):
                secs = (datetime.now() - start).total_seconds()
                logger.debug(f"RPC completed in: {secs}s")
                return ret.value
            else:
                logger.error(f"Received an incorrect kind of RPC response")
                return None
        finally:
            try:
                proc.wait(timeout=SUBPROC_TIMEOUT_S)
            except subprocess.TimeoutExpired:
                # We killed it, so don't frame the resulting signal exit code as
                # an external death below -- say plainly that we timed it out.
                proc.kill()
                proc.wait()
                logger.error(
                    f"RPC subprocess did not exit within {SUBPROC_TIMEOUT_S}s; "
                    "killed it." + _stderr_suffix(stderr_tail)
                )
            else:
                # The process exited on its own.
                if not got_response:
                    _diagnose_death(proc.returncode, stderr_tail)
                elif proc.returncode not in (0, None):
                    logger.warning(
                        f"RPC subprocess {_describe_exit(proc.returncode)} after "
                        "returning a response."
                    )


##############################################################################
# Process Management
##############################################################################

# There is some duplication between here and rpc_call(). For some
# reason, switching all RPC calls to the new
# multiple-requests-per-process style caused massive slowdowns in CI,
# so, until we can track down and fix the problem, it's easier to have
# two separate versions of the logic: rpc_call() for running a single
# request in a process and stopping, and RpcSession for managing a
# server process that can handle any number of requests.


@dataclass(frozen=True)
class RpcSession:
    """
    An RPC process that can be used to run multiple RPC calls,
    blocking on each call.

    You can start an OCaml process with RpcSession.start(), which can
    also be used as a context manager:

    .. code-block:: python

        with RpcSession.start() as rpc:
            contributors = rpc.call(out.FunctionCall(out.CallContributions()), out.RetContributions)
            formatter_args = out.CallFormatter((formatter, ctx, output))
            format = rpc.call(out.FunctionCall(formatter_args), out.RetFormatter)

    :param process: The semgrep process to send RPC calls to.
    :param stderr_tail: Bounded capture of the process's stderr, used to
        diagnose a crash. None when stderr streams live (debug mode).
    """

    process: subprocess.Popen
    stderr_tail: Optional[_StderrTail]
    # One-shot latch (a mutable cell on a frozen dataclass): a dead session hit
    # by many calls is diagnosed once, not once per call. compare=False keeps it
    # out of equality/hashing.
    _diagnosed: List[bool] = field(default_factory=list, compare=False, repr=False)

    def _diagnose_death_once(self) -> None:
        if self._diagnosed:
            return
        self._diagnosed.append(True)
        _diagnose_death(self.process.returncode, self.stderr_tail)

    @staticmethod
    def start() -> RpcSession:
        """Start a new Semgrep OCaml RPC process.
        This defaults to using the pro executable if available.
        """
        from semgrep.state import get_state

        state = get_state()

        capture_stderr = _stderr_capture_enabled()

        cmd = _cmd()
        if state.telemetry.enabled:
            cmd.append("-trace")
            if state.telemetry.trace_endpoint is not None:
                cmd.extend(["-trace_endpoint", state.telemetry.trace_endpoint])
            state.telemetry.inject()

        server = subprocess.Popen(
            cmd,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE if capture_stderr else subprocess.DEVNULL,
            text=False,
        )
        stderr_tail = _capture_stderr(server.stderr, capture_stderr)
        return RpcSession(process=server, stderr_tail=stderr_tail)

    def __enter__(self) -> RpcSession:
        return self

    def __exit__(
        self,
        type: Optional[Type[BaseException]],
        value: Optional[BaseException],
        traceback: Optional[TracebackType],
    ) -> None:
        try:
            process_stdin = self.process.stdin
            if process_stdin:
                process_stdin.close()
            try:
                self.process.wait(timeout=SUBPROC_TIMEOUT_S)
            except subprocess.TimeoutExpired:
                self.process.kill()
                self.process.wait()
                logger.error(
                    f"RPC subprocess did not exit within {SUBPROC_TIMEOUT_S}s; "
                    "killed it."
                )
            else:
                # Exited on its own: report an abnormal exit (with stderr, in
                # debug) that would otherwise pass silently. The latch avoids
                # repeating a death a call already diagnosed.
                if self.process.returncode not in (0, None):
                    self._diagnose_death_once()
        finally:
            # Popen.__exit__ closes the streams, which ends the drain thread.
            self.process.__exit__(type, value, traceback)

    @telemetry.trace()
    @simple_profiling
    def call(self, call: out.FunctionCall, expected_type: Type[T]) -> Optional[T]:
        """Call an RPC function. Block until we get a response.

        If we get an error response from the RPC call, we log the
        error and return None.

        :param call: The parameters for the RPC call.
        :param expected_type: The type of response we expect from the
            specific RPC call. This is not checked statically.

        :return: The output of the RPC call or None if we encountered
                 an error during execution.
        """
        # These need to be local variables because otherwise mypy doesn't
        # trust the results of the None checks.
        proc_stdin = self.process.stdin
        proc_stdout = self.process.stdout
        if proc_stdin is None or proc_stdout is None:
            # This shouldn't happen, since we set stdin and stdout
            # args to PIPE in _start_semgrep(), but there's no
            # static guarantee.
            logger.error(f"RPC subprocess missing stdout or stdin channel")
            return None

        # If the server already died (e.g. a crash on a previous call), don't
        # write to a closed pipe -- diagnose the death instead.
        if self.process.poll() is not None:
            self._diagnose_death_once()
            return None

        call_str = _wrap_call_with_trace_context(call).to_json_string().strip()
        try:
            _write_packet(proc_stdin, call_str)
        except BrokenPipeError:
            # The server died around write time. Reap it first so we can name
            # the signal rather than reporting "still running".
            try:
                self.process.wait(timeout=SUBPROC_TIMEOUT_S)
            except subprocess.TimeoutExpired:
                pass
            self._diagnose_death_once()
            return None

        ret_str = _read_packet(proc_stdout)
        if ret_str is None:
            # A None read is either the server dying (EOF) or a protocol error
            # already logged by _read_packet. On EOF the process may not be
            # reapable yet (stdout close and exit aren't atomic), so wait briefly
            # -- as the BrokenPipe path does -- before deciding. A live process
            # (protocol error) times out and falls through to the verbose line.
            try:
                self.process.wait(timeout=SUBPROC_TIMEOUT_S)
            except subprocess.TimeoutExpired:
                pass
            if self.process.poll() is not None:
                self._diagnose_death_once()
            else:
                logger.verbose("Unable to read RPC response")
            return None
        ret = _parse_function_result(ret_str)
        if ret is None:
            # No need to log here, it's handled in the error case of
            # _parse_function_return
            return None

        # Any request can return an error
        if isinstance(ret.value, out.RetError):
            err: str = ret.value.value
            logger.error(f"RPC response indicated an error: {err}")
            return None

        # Check that we got the correct kind of response
        if isinstance(ret.value, expected_type):
            return ret.value
        else:
            logger.error(
                f"Received an incorrect kind of RPC response. Expected {expected_type}, got {type(ret.value)}"
            )
            return None
