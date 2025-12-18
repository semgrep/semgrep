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
import subprocess
from dataclasses import dataclass
from datetime import datetime
from types import TracebackType
from typing import IO
from typing import List
from typing import Optional
from typing import Type
from typing import TypeVar

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep import simple_profiling as simple_profiling_module
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

##############################################################################
# Helpers
##############################################################################


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
    size_str = io.readline().decode(ENCODING).strip()
    if not size_str.isdigit():
        # Avoid horrific log spew if we somehow got a really long line
        truncated = size_str[:50]
        logger.error(f"RPC input error: Expected a number, got '{truncated}'")
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

    return cmd


@simple_profiling
def rpc_call(call: out.FunctionCall, cls: Type[T]) -> Optional[T]:
    from semgrep.state import get_state

    start = datetime.now()

    cmd = _cmd()

    state = get_state()
    if state.traces.enabled:
        cmd.append("-trace")
        if state.traces.trace_endpoint is not None:
            cmd.extend(["-trace_endpoint", state.traces.trace_endpoint])
        state.traces.inject()

    with subprocess.Popen(
        cmd,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        text=False,
    ) as proc:
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
            call_str = call.to_json_string().strip()
            _write_packet(proc_stdin, call_str)
            proc_stdin.close()

            ret_str = _read_packet(proc_stdout)
            if ret_str is None:
                # No need to log here. _read_packet logs anyway if if returns
                # None.
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
                if proc.returncode != 0:
                    logger.error(f"RPC subprocess exited with code {proc.returncode}")
            except subprocess.TimeoutExpired:
                logger.error(f"RPC subprocess did not exit cleanly. Killing it.")
                proc.kill()


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
    """

    process: subprocess.Popen

    @staticmethod
    def start() -> RpcSession:
        """Start a new Semgrep OCaml RPC process.
        This defaults to using the pro executable if available.
        """
        server = subprocess.Popen(
            _cmd(),
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            text=False,
        )
        return RpcSession(server)

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
        finally:
            self.process.kill()

        self.process.__exit__(type, value, traceback)

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

        call_str = call.to_json_string().strip()
        _write_packet(proc_stdin, call_str)

        ret_str = _read_packet(proc_stdout)
        if ret_str is None:
            logger.error(f"Unable to read RPC response")
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
