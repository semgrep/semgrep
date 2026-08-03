#
# Copyright (c) 2026 Semgrep Inc.
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
import io
import logging
import subprocess
import sys
import types
from typing import Any
from typing import cast
from typing import List
from typing import Optional

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.error import SemgrepError
from semgrep.rpc import _describe_exit
from semgrep.rpc import _diagnose_death
from semgrep.rpc import _read_packet
from semgrep.rpc import _StderrTail
from semgrep.rpc import rpc_call
from semgrep.rpc import RpcSession
from semgrep.rpc import STDERR_TAIL_BYTES
from semgrep.rpc_call import get_targets


##############################################################################
# Test fakes / helpers
##############################################################################


class _BrokenPipe:
    """A stdin whose write raises BrokenPipeError, to simulate a dead child."""

    def write(self, _data: bytes) -> int:
        raise BrokenPipeError()

    def flush(self) -> None:
        pass

    def close(self) -> None:
        pass


class _FakePopen:
    """Stand-in for subprocess.Popen to drive rpc_call / RpcSession.call without
    spawning a real process.

    - ``poll_results``: values poll() returns in order (the last one sticks);
      use for RpcSession, which polls to detect a dead server.
    - ``wait_result`` / ``wait_raises``: what wait() does; use for rpc_call,
      which reaps the one-shot child in its finally block.
    """

    def __init__(
        self,
        *,
        stdout: bytes = b"",
        stderr: bytes = b"",
        poll_results: Optional[List[Optional[int]]] = None,
        wait_result: int = 0,
        wait_raises: bool = False,
        stdin_broken: bool = False,
    ) -> None:
        # Any: stdin is either a BytesIO or a _BrokenPipe stub.
        self.stdin: Any = _BrokenPipe() if stdin_broken else io.BytesIO()
        self.stdout = io.BytesIO(stdout)
        self.stderr = io.BytesIO(stderr)
        self.returncode: Optional[int] = None
        self.killed = False
        self._poll_results = list(poll_results or [])
        self._wait_result = wait_result
        self._wait_raises = wait_raises

    def __enter__(self) -> "_FakePopen":
        return self

    def __exit__(self, *_exc: object) -> None:
        self.stdin.close()
        self.stdout.close()
        self.stderr.close()

    def poll(self) -> Optional[int]:
        if self._poll_results:
            self.returncode = self._poll_results.pop(0)
        return self.returncode

    def wait(self, timeout: Optional[float] = None) -> int:
        if self._wait_raises:
            self._wait_raises = False  # a subsequent wait (post-kill) succeeds
            raise subprocess.TimeoutExpired(cmd="semgrep-core", timeout=timeout or 0.0)
        self.returncode = self._wait_result
        return self._wait_result

    def kill(self) -> None:
        self.killed = True
        self._wait_result = -9


def _packet(function_return: out.FunctionReturn) -> bytes:
    """Serialize a FunctionResult into the length-prefixed RPC wire format."""
    result = out.FunctionResult(function_return=function_return, profiling_results=[])
    payload = result.to_json_string().encode("utf-8")
    return f"{len(payload)}\n".encode("utf-8") + payload


def _patch_rpc_call(monkeypatch, fake: _FakePopen) -> None:
    # Replace only the `subprocess` name inside semgrep.rpc (not the global
    # subprocess module), so Popen returns our fake while subprocess.run used
    # elsewhere (e.g. get_state) keeps working.
    shim = types.SimpleNamespace(
        Popen=lambda *a, **k: fake,
        PIPE=subprocess.PIPE,
        DEVNULL=subprocess.DEVNULL,
        TimeoutExpired=subprocess.TimeoutExpired,
    )
    monkeypatch.setattr("semgrep.rpc.subprocess", shim)
    monkeypatch.setattr("semgrep.rpc._cmd", lambda: ["semgrep-core"])
    monkeypatch.setattr(
        "semgrep.rpc._wrap_call_with_trace_context",
        lambda call: out.RpcCall(call=call, parent_span_id=None),
    )


def _errors(caplog) -> List[str]:
    return [r.getMessage() for r in caplog.records if r.levelno >= logging.ERROR]


_CALL = out.FunctionCall(out.CallContributions())


class TestGetTargets:
    @pytest.mark.quick
    def test_raises_on_rpc_failure(self, monkeypatch):
        """When rpc_call returns None (semgrep-core crash/failure),
        get_targets should raise SemgrepError instead of silently
        returning an empty result."""
        monkeypatch.setattr("semgrep.rpc_call.rpc_call", lambda *args, **kwargs: None)

        scanning_roots = out.ScanningRoots(
            root_paths=[],
            targeting_conf=out.TargetingConf(
                exclude=[],
                max_target_bytes=0,
                respect_gitignore=True,
                respect_semgrepignore_files=True,
                always_select_explicit_targets=False,
                explicit_targets=[],
                force_novcs_project=False,
                exclude_minified_files=False,
                # include_binary_files is optional (defaults to false); omitted here.
            ),
        )

        with pytest.raises(SemgrepError, match="Failed to obtain target files"):
            get_targets(scanning_roots)


class TestDescribeExit:
    @pytest.mark.quick
    def test_sigkill(self):
        msg = _describe_exit(-9)
        assert "signal 9" in msg and "SIGKILL" in msg
        # No causal guesses: those anchor debugging on the wrong thing.
        for banned in ("likely", "oom", "memory", "stack overflow"):
            assert banned not in msg.lower()

    @pytest.mark.quick
    def test_sigsegv(self):
        msg = _describe_exit(-11)
        assert "signal 11" in msg and "SIGSEGV" in msg
        for banned in ("likely", "crash", "stack overflow"):
            assert banned not in msg.lower()

    @pytest.mark.quick
    def test_sigabrt(self):
        assert "SIGABRT" in _describe_exit(-6)

    @pytest.mark.quick
    def test_unknown_signal(self):
        # 200 is not a real signal on any supported platform.
        msg = _describe_exit(-200)
        assert "signal 200" in msg
        assert "SIG" not in msg  # no name invented for an unknown signal

    @pytest.mark.quick
    def test_still_running(self):
        assert "did not exit" in _describe_exit(None)

    @pytest.mark.quick
    def test_clean_exit(self):
        msg = _describe_exit(0)
        assert "exited cleanly" in msg and "code 0" in msg
        # The "no response" nuance belongs to the caller, not this describer.
        assert "without sending a response" not in msg

    @pytest.mark.quick
    def test_nonzero_exit(self):
        assert "exited with code 5" in _describe_exit(5)


class TestReadPacket:
    @pytest.mark.quick
    def test_eof_is_silent(self, caplog):
        """An empty readline (EOF) returns None and logs NOTHING -- not even the
        VERBOSE line the pre-fix code emitted. The caller diagnoses instead."""
        caplog.set_level(logging.DEBUG, logger="semgrep.rpc")
        assert _read_packet(io.BytesIO(b"")) is None
        assert not caplog.records

    @pytest.mark.quick
    def test_malformed_header_logs_protocol_error(self, caplog):
        caplog.set_level(logging.DEBUG, logger="semgrep.rpc")
        assert _read_packet(io.BytesIO(b"not-a-number\n")) is None
        errors = _errors(caplog)
        assert any("protocol error" in m for m in errors)
        assert any("non-packet data" in m for m in errors)

    @pytest.mark.quick
    def test_non_utf8_header_is_protocol_error_not_crash(self, caplog):
        """Non-UTF-8 garbage on stdout must produce the protocol error, not a
        propagating UnicodeDecodeError."""
        caplog.set_level(logging.DEBUG, logger="semgrep.rpc")
        assert _read_packet(io.BytesIO(b"\xff\xfe\n")) is None
        assert any("protocol error" in m for m in _errors(caplog))

    @pytest.mark.quick
    def test_blank_header_line_reads_nicely(self, caplog):
        """A non-empty but whitespace-only line (e.g. a stray newline) should
        read as 'a blank line', not the awkward "got ''"."""
        caplog.set_level(logging.DEBUG, logger="semgrep.rpc")
        assert _read_packet(io.BytesIO(b"\n")) is None
        errors = _errors(caplog)
        assert any("a blank line" in m for m in errors)
        assert not any("got ''" in m for m in errors)


class TestStderrTail:
    @pytest.mark.quick
    def test_bounded_and_keeps_tail(self):
        """After draining >2x the cap, the buffer settles to <= cap and the most
        recent output is retained. (Peak during drain can briefly exceed the cap
        by one read chunk; this pins the settled size and tail retention.)"""
        payload = (b"filler\n" * 2000) + b"LAST_LINE_MARKER\n"
        assert len(payload) > 2 * STDERR_TAIL_BYTES
        tail = _StderrTail(io.BytesIO(payload))
        result = tail.tail()
        assert len(tail._buf) <= STDERR_TAIL_BYTES
        assert len(result.encode("utf-8")) <= STDERR_TAIL_BYTES
        assert "LAST_LINE_MARKER" in result

    @pytest.mark.quick
    def test_drops_partial_leading_line(self):
        """When trimmed mid-line, the dangling partial first line is dropped."""
        payload = (b"P" * (STDERR_TAIL_BYTES * 2)) + b"\nclean line\n"
        result = _StderrTail(io.BytesIO(payload)).tail()
        assert result == "clean line"

    @pytest.mark.quick
    def test_keeps_complete_line_at_exactly_cap(self):
        """A complete first line must be kept when the total is exactly the cap
        and no trim happened -- the partial-line drop must not fire here."""
        line = b"x" * (STDERR_TAIL_BYTES - 1) + b"\n"
        assert len(line) == STDERR_TAIL_BYTES
        result = _StderrTail(io.BytesIO(line)).tail()
        assert result == "x" * (STDERR_TAIL_BYTES - 1)

    @pytest.mark.quick
    def test_empty(self):
        assert _StderrTail(io.BytesIO(b"")).tail() == ""

    @pytest.mark.quick
    def test_tees_full_stream_while_retaining_bounded_tail(self):
        """With a tee (debug passthrough), the whole stream streams through live
        while only the bounded tail is retained for the death diagnostic."""
        src = b"first\n" + b"Z" * (STDERR_TAIL_BYTES * 2) + b"\nlast\n"
        sink = io.BytesIO()
        tail = _StderrTail(io.BytesIO(src), tee=sink)
        result = tail.tail()
        assert sink.getvalue() == src  # tee gets the full, untruncated stream
        assert len(tail._buf) <= STDERR_TAIL_BYTES  # retained tail stays bounded
        assert "last" in result


class TestDiagnoseDeath:
    @pytest.mark.quick
    def test_reports_signal_and_stderr(self, caplog):
        caplog.set_level(logging.DEBUG, logger="semgrep.rpc")
        tail = _StderrTail(io.BytesIO(b"Fatal error: an ocaml backtrace here\n"))
        _diagnose_death(-9, tail)
        errors = _errors(caplog)
        assert len(errors) == 1
        assert "SIGKILL" in errors[0]
        assert "an ocaml backtrace here" in errors[0]

    @pytest.mark.quick
    def test_no_stderr_still_reports(self, caplog):
        caplog.set_level(logging.DEBUG, logger="semgrep.rpc")
        _diagnose_death(-11, None)
        errors = _errors(caplog)
        assert len(errors) == 1
        assert "SIGSEGV" in errors[0]


class TestRpcCallFinally:
    """The one-shot rpc_call path: how it decides to diagnose a death."""

    @pytest.mark.quick
    def test_signal_death_reports_signal_not_expected_a_number(
        self, monkeypatch, caplog
    ):
        """Regression: a signal-killed subprocess yields a signal-named error,
        and the old misleading 'Expected a number' text is gone."""
        caplog.set_level(logging.DEBUG, logger="semgrep.rpc")
        fake = _FakePopen(stdout=b"", stderr=b"boom\n", wait_result=-9)
        _patch_rpc_call(monkeypatch, fake)

        assert rpc_call(_CALL, out.RetContributions) is None
        errors = _errors(caplog)
        assert any("SIGKILL" in m for m in errors)
        assert not any("Expected a number" in r.getMessage() for r in caplog.records)

    @pytest.mark.quick
    def test_error_response_is_not_diagnosed_as_death(self, monkeypatch, caplog):
        """A well-formed RetError means the subprocess responded -- it must not
        be misdiagnosed as a death in the finally block."""
        caplog.set_level(logging.DEBUG, logger="semgrep.rpc")
        fake = _FakePopen(
            stdout=_packet(out.FunctionReturn(out.RetError("boom"))), wait_result=0
        )
        _patch_rpc_call(monkeypatch, fake)

        assert rpc_call(_CALL, out.RetContributions) is None
        messages = [r.getMessage() for r in caplog.records]
        assert any("RPC response indicated an error: boom" in m for m in messages)
        assert not any("RPC subprocess failed" in m for m in messages)

    @pytest.mark.quick
    def test_timeout_kill_is_not_framed_as_signal_death(self, monkeypatch, caplog):
        """When we time out and kill the child, say so -- do not report the
        self-inflicted SIGKILL as an external death."""
        caplog.set_level(logging.DEBUG, logger="semgrep.rpc")
        fake = _FakePopen(stdout=b"", wait_raises=True)
        _patch_rpc_call(monkeypatch, fake)

        assert rpc_call(_CALL, out.RetContributions) is None
        assert fake.killed
        messages = [r.getMessage() for r in caplog.records]
        assert any("did not exit within" in m and "killed it" in m for m in messages)
        assert not any("RPC subprocess failed" in m for m in messages)

    @pytest.mark.quick
    def test_stderr_not_surfaced_in_default_mode(self, monkeypatch, caplog):
        """In default (non-debug) mode the signal is named but the subprocess
        stderr must NOT be copied into the log (it can carry customer data)."""
        caplog.set_level(logging.DEBUG, logger="semgrep.rpc")
        fake = _FakePopen(stdout=b"", stderr=b"SENSITIVE_STDERR\n", wait_result=-9)
        _patch_rpc_call(monkeypatch, fake)
        monkeypatch.setattr("semgrep.rpc._stderr_capture_enabled", lambda: False)

        assert rpc_call(_CALL, out.RetContributions) is None
        errors = _errors(caplog)
        assert any("SIGKILL" in m for m in errors)
        assert not any("SENSITIVE_STDERR" in m for m in errors)

    @pytest.mark.quick
    def test_broken_pipe_on_write_degrades_to_none(self, monkeypatch, caplog):
        """If the child dies before reading its request, the write's
        BrokenPipeError must become None + a diagnostic, not a propagating
        exception."""
        caplog.set_level(logging.DEBUG, logger="semgrep.rpc")
        fake = _FakePopen(stdin_broken=True, wait_result=-9)
        _patch_rpc_call(monkeypatch, fake)

        assert rpc_call(_CALL, out.RetContributions) is None
        assert any("SIGKILL" in m for m in _errors(caplog))


class TestRpcSessionCall:
    """The long-lived RpcSession path: dead-server detection + diagnose-once."""

    def _session(self, fake: _FakePopen, stderr: bytes = b"boom\n") -> RpcSession:
        return RpcSession(
            process=cast(subprocess.Popen, fake),
            stderr_tail=_StderrTail(io.BytesIO(stderr)),
        )

    @pytest.mark.quick
    def test_dead_server_diagnoses_and_skips_write(self, caplog):
        caplog.set_level(logging.DEBUG, logger="semgrep.rpc")
        fake = _FakePopen(poll_results=[-11])
        session = self._session(fake)

        assert session.call(_CALL, out.RetContributions) is None
        errors = _errors(caplog)
        assert len(errors) == 1
        assert "SIGSEGV" in errors[0] and "boom" in errors[0]
        # Nothing was written to the dead pipe.
        assert fake.stdin.getvalue() == b""

    @pytest.mark.quick
    def test_eof_read_on_dead_server_is_diagnosed(self, monkeypatch, caplog):
        caplog.set_level(logging.DEBUG, logger="semgrep.rpc")
        monkeypatch.setattr(
            "semgrep.rpc._wrap_call_with_trace_context",
            lambda call: out.RpcCall(call=call, parent_span_id=None),
        )
        # Alive at the write guard; then EOF read and the bounded wait reaps it.
        fake = _FakePopen(stdout=b"", poll_results=[None], wait_result=-6)
        session = self._session(fake)

        assert session.call(_CALL, out.RetContributions) is None
        assert any("SIGABRT" in m for m in _errors(caplog))

    @pytest.mark.quick
    def test_protocol_error_while_alive_is_not_a_death(self, monkeypatch, caplog):
        caplog.set_level(logging.DEBUG, logger="semgrep.rpc")
        monkeypatch.setattr(
            "semgrep.rpc._wrap_call_with_trace_context",
            lambda call: out.RpcCall(call=call, parent_span_id=None),
        )
        # Alive throughout; stdout has non-packet data (a protocol violation).
        # wait_raises=True so the bounded wait times out, as it would for a
        # process that is still running.
        fake = _FakePopen(
            stdout=b"garbage\n", poll_results=[None, None], wait_raises=True
        )
        session = self._session(fake)

        assert session.call(_CALL, out.RetContributions) is None
        errors = _errors(caplog)
        # _read_packet logs the protocol error; no death diagnostic is added.
        assert any("protocol error" in m for m in errors)
        assert not any("RPC subprocess failed" in m for m in errors)

    @pytest.mark.quick
    def test_dead_server_diagnosed_only_once(self, caplog):
        """Repeated calls to a dead session log one error, not one per call."""
        caplog.set_level(logging.DEBUG, logger="semgrep.rpc")
        fake = _FakePopen(poll_results=[-11])
        session = self._session(fake)

        assert session.call(_CALL, out.RetContributions) is None
        assert session.call(_CALL, out.RetContributions) is None
        assert len([m for m in _errors(caplog) if "RPC subprocess failed" in m]) == 1


# A stand-in semgrep-core: read the request, flood stderr with `argv[1]` bytes of
# filler plus an END_MARKER, then die by SIGSEGV without responding. Exercises the
# real stderr PIPE + drain thread, not an in-memory BytesIO.
_STDERR_CRASH_STUB = (
    "import os, sys, signal\n"
    "sys.stdin.read(int(sys.stdin.readline().strip()))\n"
    "sys.stderr.write('X' * int(sys.argv[1]))\n"
    "sys.stderr.write('\\nEND_MARKER\\n')\n"
    "sys.stderr.flush()\n"
    "os.kill(os.getpid(), signal.SIGSEGV)\n"
)


class TestStderrCaptureIntegration:
    """End-to-end stderr capture through a real subprocess (not a fake)."""

    def _patch(self, monkeypatch, filler: int) -> None:
        # stderr capture is gated on debug logging, so enable it.
        monkeypatch.setattr("semgrep.rpc._stderr_capture_enabled", lambda: True)
        # Real subprocess.Popen; only the command and trace wrapper are stubbed.
        monkeypatch.setattr(
            "semgrep.rpc._cmd",
            lambda: [sys.executable, "-c", _STDERR_CRASH_STUB, str(filler)],
        )
        monkeypatch.setattr(
            "semgrep.rpc._wrap_call_with_trace_context",
            lambda call: out.RpcCall(call=call, parent_span_id=None),
        )

    @pytest.mark.kinda_slow
    def test_captures_stderr_from_a_real_crashing_subprocess(self, monkeypatch, caplog):
        caplog.set_level(logging.DEBUG, logger="semgrep.rpc")
        self._patch(monkeypatch, filler=0)
        assert rpc_call(_CALL, out.RetContributions) is None
        errors = _errors(caplog)
        assert any("SIGSEGV" in m for m in errors)
        assert any("END_MARKER" in m for m in errors)

    @pytest.mark.kinda_slow
    @pytest.mark.timeout(30)
    def test_large_stderr_does_not_deadlock_and_is_bounded(self, monkeypatch, caplog):
        """The drain thread's raison d'etre: a subprocess flooding stderr past
        the pipe buffer while we block on stdout must not deadlock, and the
        captured tail stays bounded. Without the thread this would hang (guarded
        by the timeout)."""
        caplog.set_level(logging.DEBUG, logger="semgrep.rpc")
        self._patch(monkeypatch, filler=300_000)  # well over any OS pipe buffer
        assert rpc_call(_CALL, out.RetContributions) is None
        errors = _errors(caplog)
        # The most recent stderr (END_MARKER) survives...
        assert any("END_MARKER" in m for m in errors)
        # ...but the 300 KB of filler is not dumped wholesale.
        assert all(("X" * 5000) not in m for m in errors)
