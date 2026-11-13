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
# MCP metrics and logs are sent off the machine, so they must not carry scan
# output: error messages can quote scanned source code, and tool failures can
# carry semgrep's stderr.
import logging

import click
import pytest

from semgrep.mcp.models import SemgrepScanResult
from semgrep.mcp.utilities.tracing import is_tracing_disabled
from semgrep.mcp.utilities.tracing import with_hook_span
from semgrep.metrics import mcp_error_type
from semgrep.metrics import Metrics
from semgrep.state import get_state

SECRET = "hunter2"


@pytest.mark.quick
@pytest.mark.parametrize(
    ("env", "disabled"),
    [
        ({}, False),
        ({"SEMGREP_SEND_METRICS": "on"}, False),
        ({"SEMGREP_SEND_METRICS": "auto"}, False),
        ({"SEMGREP_SEND_METRICS": "off"}, True),
        ({"SEMGREP_SEND_METRICS": "OFF"}, True),
        ({"SEMGREP_SEND_METRICS": "0"}, True),
        ({"SEMGREP_SEND_METRICS": "false"}, True),
        ({"SEMGREP_MCP_DISABLE_TRACING": "true"}, True),
    ],
)
def test_metrics_off_disables_tracing(monkeypatch, env, disabled):
    monkeypatch.delenv("SEMGREP_SEND_METRICS", raising=False)
    monkeypatch.delenv("SEMGREP_MCP_DISABLE_TRACING", raising=False)
    for var, value in env.items():
        monkeypatch.setenv(var, value)
    assert is_tracing_disabled() == disabled


@pytest.mark.quick
@pytest.mark.parametrize(
    ("error", "expected"),
    [
        ({"type": "SemgrepWarning", "message": SECRET}, "SemgrepWarning"),
        ({"type": ["PartialParsing", [{"path": "a.py"}]]}, "PartialParsing"),
        ({"message": SECRET}, "unknown"),
    ],
)
def test_mcp_error_type(error, expected):
    assert mcp_error_type(error) == expected


@pytest.mark.quick
def test_scan_metrics_record_error_types_not_messages():
    metrics = Metrics()
    metrics.add_mcp(
        deployment_id=None, session_id="s", deployment_name=None, tool_name="t"
    )
    results = SemgrepScanResult(
        version="0.0.0",
        results=[],
        errors=[
            {
                "type": "SemgrepWarning",
                "message": f"Syntax error at line a.py:1:\n `{SECRET}` was unexpected",
            }
        ],
        paths={"scanned": []},
    )
    metrics.add_mcp_scan_metrics(results, num_lines_scanned=0)
    assert metrics.payload.mcp.errors == ["SemgrepWarning"]


@pytest.mark.quick
async def test_hook_failure_reports_only_exception_type(caplog):
    @with_hook_span(send_metrics=False)
    async def failing_hook(_top_level_span):
        raise RuntimeError(f"Error running semgrep: (2) {SECRET}")

    # get_state() is per click context; without one, each call makes a new state.
    with click.Context(click.Command("test")).scope():
        get_state().metrics.add_mcp(
            deployment_id=None, session_id="s", deployment_name=None, tool_name="t"
        )
        with caplog.at_level(logging.INFO), pytest.raises(RuntimeError):
            await failing_hook(None)

        assert get_state().metrics.payload.mcp.errors == ["RuntimeError"]
    assert SECRET not in caplog.text


@pytest.fixture
def span_exporter(monkeypatch):
    """Route the MCP tracer to an in-memory exporter, with tracing enabled."""
    from opentelemetry.sdk.trace import TracerProvider
    from opentelemetry.sdk.trace.export import SimpleSpanProcessor
    from opentelemetry.sdk.trace.export.in_memory_span_exporter import (
        InMemorySpanExporter,
    )

    import semgrep.mcp.utilities.tracing as tracing

    exporter = InMemorySpanExporter()
    provider = TracerProvider()
    provider.add_span_processor(SimpleSpanProcessor(exporter))
    monkeypatch.setattr(tracing, "TRACER", provider.get_tracer("test"))
    monkeypatch.delenv("SEMGREP_SEND_METRICS", raising=False)
    monkeypatch.delenv("SEMGREP_MCP_DISABLE_TRACING", raising=False)
    return exporter, provider


def assert_span_has_only_exception_type(span, exc_type: str) -> None:
    from opentelemetry.trace import StatusCode

    assert span.status.status_code == StatusCode.ERROR
    assert span.status.description == exc_type
    for event in span.events:
        assert SECRET not in str(dict(event.attributes or {}))
    assert [e.attributes["exception.type"] for e in span.events] == [exc_type]


@pytest.mark.quick
async def test_hook_failure_span_records_only_exception_type(span_exporter):
    exporter, provider = span_exporter
    err = RuntimeError(f"Error running semgrep: (2) {SECRET}")

    @with_hook_span(send_metrics=False)
    async def failing_hook(_top_level_span):
        raise err

    parent = provider.get_tracer("test").start_span("parent")
    with click.Context(click.Command("test")).scope(), pytest.raises(RuntimeError):
        await failing_hook(parent)

    (span,) = [s for s in exporter.get_finished_spans() if s.name == "failing_hook"]
    assert_span_has_only_exception_type(span, "RuntimeError")


@pytest.mark.quick
def test_top_level_span_records_only_exception_type(span_exporter, mocker):
    from semgrep.mcp.utilities.tracing import start_tracing

    exporter, _provider = span_exporter
    err = RuntimeError(SECRET)
    with click.Context(click.Command("test")).scope():
        # Don't configure a real exporter to the trace endpoint.
        mocker.patch("semgrep.telemetry.Telemetry.configure")
        with pytest.raises(RuntimeError), start_tracing("mcp-hook"):
            raise err

    (span,) = [s for s in exporter.get_finished_spans() if s.name == "mcp-hook"]
    assert_span_has_only_exception_type(span, "RuntimeError")
