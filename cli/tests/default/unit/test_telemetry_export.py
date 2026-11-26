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
# End-to-end tests of the OpenTelemetry export in src/semgrep/telemetry.py.
#
# OpenTelemetry tracer/logger providers and instrumentors are process-global
# and cannot be reset, so each scenario runs in a fresh Python interpreter.
# That interpreter configures semgrep's telemetry to export to a fake OTLP/HTTP
# collector served from this process, and the tests assert on the decoded
# protobuf payloads that the collector received.
import gzip
import json
import os
import socket
import subprocess
import sys
import textwrap
import threading
import time
from collections import Counter
from dataclasses import dataclass
from dataclasses import field
from http.server import BaseHTTPRequestHandler
from http.server import ThreadingHTTPServer
from typing import Any

import pytest
from opentelemetry.proto.collector.logs.v1.logs_service_pb2 import (
    ExportLogsServiceRequest,
)
from opentelemetry.proto.collector.logs.v1.logs_service_pb2 import (
    ExportLogsServiceResponse,
)
from opentelemetry.proto.collector.trace.v1.trace_service_pb2 import (
    ExportTraceServiceRequest,
)
from opentelemetry.proto.collector.trace.v1.trace_service_pb2 import (
    ExportTraceServiceResponse,
)
from opentelemetry.proto.common.v1.common_pb2 import AnyValue
from opentelemetry.proto.common.v1.common_pb2 import KeyValue
from opentelemetry.proto.trace.v1.trace_pb2 import Span as ProtoSpan

from semgrep import __VERSION__

LOGGER_NAME = "semgrep.test_telemetry_export"
PARENT_TRACE_ID = "0af7651916cd43dd8448eb211c80319c"
PARENT_SPAN_ID = "b7ad6b7169203331"


################################################################################
# Fake OTLP/HTTP collector
################################################################################


@dataclass(frozen=True)
class ExportedSpan:
    name: str
    trace_id: str
    span_id: str
    parent_span_id: str
    kind: int
    attributes: dict[str, Any]
    resource: dict[str, Any]


@dataclass(frozen=True)
class ExportedLog:
    scope: str
    body: str
    severity_text: str
    severity_number: int
    trace_id: str
    span_id: str
    attributes: dict[str, Any]
    resource: dict[str, Any]


@dataclass
class Exported:
    spans: list[ExportedSpan] = field(default_factory=list)
    logs: list[ExportedLog] = field(default_factory=list)
    # Headers of requests to any path other than the OTLP endpoints.
    other_requests: list[dict[str, str]] = field(default_factory=list)
    # JSON object that the scenario script stored in RESULT.
    result: dict[str, Any] = field(default_factory=dict)

    def span(self, name: str) -> ExportedSpan:
        matches = [s for s in self.spans if s.name == name]
        assert len(matches) == 1, f"expected one {name!r} span in {self.spans}"
        return matches[0]

    def logs_from(self, scope: str) -> list[ExportedLog]:
        return [log for log in self.logs if log.scope == scope]


def _decode_value(value: AnyValue) -> Any:
    kind = value.WhichOneof("value")
    if kind is None:
        return None
    if kind == "array_value":
        return [_decode_value(v) for v in value.array_value.values]
    if kind == "kvlist_value":
        return _decode_attrs(value.kvlist_value.values)
    return getattr(value, kind)


def _decode_attrs(kvs: Any) -> dict[str, Any]:
    out: dict[str, Any] = {}
    kv: KeyValue
    for kv in kvs:
        out[kv.key] = _decode_value(kv.value)
    return out


class _Collector:
    def __init__(self) -> None:
        self.exported = Exported()
        self._lock = threading.Lock()
        collector = self

        class Handler(BaseHTTPRequestHandler):
            def do_POST(self) -> None:
                body = self.rfile.read(int(self.headers.get("Content-Length", 0)))
                if self.headers.get("Content-Encoding") == "gzip":
                    body = gzip.decompress(body)
                if self.path == "/v1/traces":
                    collector._add_traces(ExportTraceServiceRequest.FromString(body))
                    response = ExportTraceServiceResponse().SerializeToString()
                elif self.path == "/v1/logs":
                    collector._add_logs(ExportLogsServiceRequest.FromString(body))
                    response = ExportLogsServiceResponse().SerializeToString()
                else:
                    self.send_error(404)
                    return
                self.send_response(200)
                self.send_header("Content-Type", "application/x-protobuf")
                self.send_header("Content-Length", str(len(response)))
                self.end_headers()
                self.wfile.write(response)

            def do_GET(self) -> None:
                with collector._lock:
                    collector.exported.other_requests.append(dict(self.headers))
                self.send_response(200)
                self.send_header("Content-Length", "2")
                self.end_headers()
                self.wfile.write(b"ok")

            def log_message(self, format: str, *args: Any) -> None:
                pass

        self._server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        self.url = f"http://127.0.0.1:{self._server.server_address[1]}"

    def _add_traces(self, request: ExportTraceServiceRequest) -> None:
        with self._lock:
            for resource_spans in request.resource_spans:
                resource = _decode_attrs(resource_spans.resource.attributes)
                for scope_spans in resource_spans.scope_spans:
                    for span in scope_spans.spans:
                        self.exported.spans.append(
                            ExportedSpan(
                                name=span.name,
                                trace_id=span.trace_id.hex(),
                                span_id=span.span_id.hex(),
                                parent_span_id=span.parent_span_id.hex(),
                                kind=span.kind,
                                attributes=_decode_attrs(span.attributes),
                                resource=resource,
                            )
                        )

    def _add_logs(self, request: ExportLogsServiceRequest) -> None:
        with self._lock:
            for resource_logs in request.resource_logs:
                resource = _decode_attrs(resource_logs.resource.attributes)
                for scope_logs in resource_logs.scope_logs:
                    for record in scope_logs.log_records:
                        self.exported.logs.append(
                            ExportedLog(
                                scope=scope_logs.scope.name,
                                body=_decode_value(record.body),
                                severity_text=record.severity_text,
                                severity_number=record.severity_number,
                                trace_id=record.trace_id.hex(),
                                span_id=record.span_id.hex(),
                                attributes=_decode_attrs(record.attributes),
                                resource=resource,
                            )
                        )

    def __enter__(self) -> "_Collector":
        threading.Thread(target=self._server.serve_forever, daemon=True).start()
        return self

    def __exit__(self, *args: Any) -> None:
        self._server.shutdown()
        self._server.server_close()


################################################################################
# Scenario runner
################################################################################

# Mirrors the CLI: semgrep.terminal sets the root logger to DEBUG and lets each
# handler pick its own level.
_PRELUDE = """
import json, logging, os, threading
import requests
from opentelemetry import trace as otrace
from opentelemetry._logs import get_logger_provider
from semgrep.telemetry import Telemetry, TRACER, trace
logging.getLogger().setLevel(logging.DEBUG)
URL = os.environ["TEST_COLLECTOR_URL"]
RESULT = {}
"""

_FLUSH = """
for provider in (otrace.get_tracer_provider(), get_logger_provider()):
    flush = getattr(provider, "force_flush", None)
    if flush is not None:
        assert flush(), f"failed to flush {provider}"
"""

_EPILOGUE = """
print(json.dumps(RESULT))
"""

# Variables that would leak the developer's or CI's tracing setup into a
# scenario, or change the deployment environment derived from the app URL.
_SCRUBBED_ENV_PREFIXES = (
    "OTEL_",
    "DOCKER_OTEL_",
    "SEMGREP_TRACE_PARENT_",
    "SEMGREP_URL",
    "SEMGREP_APP_URL",
)


def run_scenario(
    script: str, env: dict[str, str] | None = None, flush: bool = True
) -> Exported:
    """Run the script in a fresh interpreter that exports to a fake collector.

    With flush=False, telemetry is only exported by the exit handler that
    telemetry.py registers, as in the real CLI.
    """
    with _Collector() as collector:
        child_env = {
            k: v
            for k, v in os.environ.items()
            if not k.startswith(_SCRUBBED_ENV_PREFIXES)
        }
        # Keep any HTTP proxy configured for the test run away from the
        # collector.
        for var in ("NO_PROXY", "no_proxy"):
            child_env[var] = ",".join(
                filter(None, [child_env.get(var), "127.0.0.1", "localhost"])
            )
        child_env["TEST_COLLECTOR_URL"] = collector.url
        child_env.update(env or {})
        proc = subprocess.run(
            [
                sys.executable,
                "-c",
                _PRELUDE
                + textwrap.dedent(script)
                + (_FLUSH if flush else "")
                + _EPILOGUE,
            ],
            env=child_env,
            capture_output=True,
            text=True,
            timeout=60,
        )
        assert proc.returncode == 0, proc.stderr
        # Telemetry must never write to the user's terminal.
        assert proc.stderr == ""
        collector.exported.result = json.loads(proc.stdout.splitlines()[-1])
        return collector.exported


################################################################################
# Scenario: telemetry enabled, typical CLI usage
################################################################################


@pytest.fixture(scope="module")
def exported() -> Exported:
    return run_scenario(
        f"""
        tel = Telemetry()
        tel.configure(
            enabled=True,
            trace_endpoint=URL,
            attributes={{"test.configure_attr": "from-configure"}},
        )
        log = logging.getLogger({LOGGER_NAME!r})

        def in_thread():
            with TRACER.start_as_current_span("in-thread"):
                pass

        @trace()
        def traced_function():
            with TRACER.start_as_current_span("child") as child:
                RESULT["child_span_id"] = otrace.format_span_id(
                    child.get_span_context().span_id
                )
                log.info("info message %s", "with arg")
                log.debug("debug message")
                try:
                    raise ValueError("boom")
                except ValueError:
                    log.exception("handled error")
                requests.get(URL + "/ping", timeout=30)
                worker = threading.Thread(target=in_thread)
                worker.start()
                worker.join()
                tel.add_resource_attrs({{"test.late_attr": "added-later"}})
                tel.inject()
                RESULT["injected"] = {{
                    k: os.environ[k]
                    for k in (
                        "SEMGREP_TRACE_PARENT_TRACE_ID",
                        "SEMGREP_TRACE_PARENT_SPAN_ID",
                        "OTEL_RESOURCE_ATTRIBUTES",
                    )
                }}

        traced_function()
        """
    )


@pytest.mark.kinda_slow
def test_each_span_exported_once(exported: Exported) -> None:
    counts = Counter(span.span_id for span in exported.spans)
    assert counts and all(n == 1 for n in counts.values()), exported.spans


@pytest.mark.kinda_slow
def test_span_tree(exported: Exported) -> None:
    top = exported.span("__main__.traced_function")
    child = exported.span("child")
    in_thread = exported.span("in-thread")

    assert top.parent_span_id == ""
    assert top.attributes["span.owner"] == "team-engine"
    assert child.parent_span_id == top.span_id
    # Context crosses threads via ThreadingInstrumentor.
    assert in_thread.parent_span_id == child.span_id
    assert {span.trace_id for span in exported.spans} == {top.trace_id}
    assert child.span_id == exported.result["child_span_id"]


@pytest.mark.kinda_slow
def test_resource_attributes(exported: Exported) -> None:
    resources = [span.resource for span in exported.spans] + [
        log.resource for log in exported.logs
    ]
    assert resources
    for resource in resources:
        assert resource["service.name"] == "semgrep-cli"
        assert resource["service.version"] == __VERSION__
        assert resource["deployment.environment.name"] == "prod"
        assert resource["experiment.name"] == "unset"
        assert resource["test.configure_attr"] == "from-configure"
        assert isinstance(resource["process.pid"], int)
        # Added after configure() through MutableResource.
        assert resource["test.late_attr"] == "added-later"


@pytest.mark.kinda_slow
def test_requests_are_traced_and_propagate_context(exported: Exported) -> None:
    child = exported.span("child")
    http_spans = [s for s in exported.spans if s.kind == ProtoSpan.SPAN_KIND_CLIENT]
    assert len(http_spans) == 1, exported.spans
    http_span = http_spans[0]

    assert http_span.parent_span_id == child.span_id
    url = http_span.attributes.get("url.full") or http_span.attributes["http.url"]
    assert url.endswith("/ping")

    [ping] = exported.other_requests
    assert ping["traceparent"].split("-")[1:3] == [
        http_span.trace_id,
        http_span.span_id,
    ]


@pytest.mark.kinda_slow
def test_exporter_requests_are_not_traced(exported: Exported) -> None:
    for span in exported.spans:
        url = str(span.attributes.get("url.full") or span.attributes.get("http.url"))
        assert "/v1/" not in url, span


@pytest.mark.kinda_slow
def test_log_records(exported: Exported) -> None:
    child = exported.span("child")
    logs = {log.body: log for log in exported.logs_from(LOGGER_NAME)}
    # DEBUG is below the export handler's level.
    assert set(logs) == {"info message with arg", "handled error"}

    info = logs["info message with arg"]
    assert (info.severity_text, info.severity_number) == ("INFO", 9)
    assert (info.trace_id, info.span_id) == (child.trace_id, child.span_id)
    assert info.attributes["code.function.name"] == "traced_function"
    assert isinstance(info.attributes["code.line.number"], int)
    assert "code.file.path" in info.attributes

    error = logs["handled error"]
    assert (error.severity_text, error.severity_number) == ("ERROR", 17)
    assert error.attributes["exception.type"] == "ValueError"
    assert error.attributes["exception.message"] == "boom"
    assert "ValueError: boom" in error.attributes["exception.stacktrace"]


@pytest.mark.kinda_slow
def test_no_otel_warnings_exported(exported: Exported) -> None:
    noise = [
        log
        for log in exported.logs
        if log.scope != LOGGER_NAME and log.severity_number >= 13  # WARN
    ]
    assert noise == []


@pytest.mark.kinda_slow
def test_inject_passes_context_to_semgrep_core(exported: Exported) -> None:
    child = exported.span("child")
    injected = exported.result["injected"]
    assert injected["SEMGREP_TRACE_PARENT_TRACE_ID"] == child.trace_id
    assert injected["SEMGREP_TRACE_PARENT_SPAN_ID"] == child.span_id

    resource_attrs = dict(
        kv.split("=", 1) for kv in injected["OTEL_RESOURCE_ATTRIBUTES"].split(",")
    )
    assert resource_attrs["test.configure_attr"] == "from-configure"
    assert resource_attrs["test.late_attr"] == "added-later"
    # Process- and service-specific attributes are left for semgrep-core to set.
    assert "service.name" not in resource_attrs
    assert "process.pid" not in resource_attrs


################################################################################
# Scenario: invoked with a parent trace and resource attributes in the env
################################################################################


@pytest.mark.kinda_slow
def test_parent_context_and_env_resource_attributes() -> None:
    exported = run_scenario(
        """
        tel = Telemetry()
        tel.configure(enabled=True, trace_endpoint=URL)
        tel.add_resource_attrs(
            {"experiment.name": "late-override", "test.late_attr": "late"}
        )
        with TRACER.start_as_current_span("top"):
            pass
        """,
        env={
            "SEMGREP_TRACE_PARENT_TRACE_ID": PARENT_TRACE_ID,
            "SEMGREP_TRACE_PARENT_SPAN_ID": PARENT_SPAN_ID,
            "OTEL_RESOURCE_ATTRIBUTES": "experiment.name=from-env,test.env_attr=yes",
            "DOCKER_OTEL_RESOURCE_ATTRIBUTES": "test.docker_attr=docker%20value",
        },
    )
    top = exported.span("top")
    assert (top.trace_id, top.parent_span_id) == (PARENT_TRACE_ID, PARENT_SPAN_ID)
    # Attributes from the environment win over later add_resource_attrs calls.
    assert top.resource["experiment.name"] == "from-env"
    assert top.resource["test.env_attr"] == "yes"
    assert top.resource["test.docker_attr"] == "docker value"
    assert top.resource["test.late_attr"] == "late"


################################################################################
# Scenario: repeated configure() calls, as the MCP server does per tool call
################################################################################


@pytest.mark.kinda_slow
def test_repeated_configure_exports_once() -> None:
    exported = run_scenario(
        f"""
        tel = Telemetry()
        for _ in range(3):
            tel.configure(enabled=True, trace_endpoint=URL)
        with TRACER.start_as_current_span("top"):
            logging.getLogger({LOGGER_NAME!r}).info("hello")
        RESULT["handlers"] = [h.get_name() for h in logging.getLogger().handlers]
        """
    )
    assert [s.name for s in exported.spans] == ["top"]
    assert [log.body for log in exported.logs_from(LOGGER_NAME)] == ["hello"]
    assert exported.result["handlers"].count("otel-logging-handler") == 1


################################################################################
# Scenario: exporting at exit
################################################################################


@pytest.mark.kinda_slow
def test_exit_exports_pending_telemetry() -> None:
    exported = run_scenario(
        f"""
        tel = Telemetry()
        tel.configure(enabled=True, trace_endpoint=URL)
        with TRACER.start_as_current_span("top"):
            logging.getLogger({LOGGER_NAME!r}).info("hello")
        """,
        flush=False,
    )
    assert [s.name for s in exported.spans] == ["top"]
    assert [log.body for log in exported.logs_from(LOGGER_NAME)] == ["hello"]


def _refused_url() -> str:
    with socket.socket() as sock:
        sock.bind(("127.0.0.1", 0))
        port = sock.getsockname()[1]
    # Nothing listens on the port anymore, so connections are refused.
    return f"http://127.0.0.1:{port}"


@pytest.mark.slow
def test_unreachable_collector_does_not_delay_exit() -> None:
    # The exporter retries connection errors for up to 10s; exit must give up
    # after telemetry.py's 2s deadline instead.
    start = time.monotonic()
    exported = run_scenario(
        f"""
        tel = Telemetry()
        tel.configure(enabled=True, trace_endpoint=URL)
        with TRACER.start_as_current_span("top"):
            logging.getLogger({LOGGER_NAME!r}).info("hello")
        """,
        env={"TEST_COLLECTOR_URL": _refused_url()},
        flush=False,
    )
    elapsed = time.monotonic() - start
    assert exported.spans == []
    assert elapsed < 7, f"exit took {elapsed:.1f}s"


################################################################################
# Scenario: telemetry disabled
################################################################################


@pytest.mark.kinda_slow
def test_disabled_exports_nothing() -> None:
    exported = run_scenario(
        f"""
        tel = Telemetry()
        tel.configure(enabled=False, trace_endpoint=URL)
        with TRACER.start_as_current_span("top"):
            logging.getLogger({LOGGER_NAME!r}).info("hello")
        RESULT["handlers"] = [h.get_name() for h in logging.getLogger().handlers]
        RESULT["service_name"] = tel.resource.attributes["service.name"]
        """
    )
    assert exported.spans == []
    assert exported.logs == []
    assert "otel-logging-handler" not in exported.result["handlers"]
    # The resource is still built so pyro-caml can use it.
    assert exported.result["service_name"] == "semgrep-cli"
