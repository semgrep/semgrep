# Copyright (c) 2026 Semgrep Inc.
"""Exercise real core shutdown with a healthy or stalled local OTLP collector."""
import json
import os
import subprocess
import threading
from http.server import BaseHTTPRequestHandler
from http.server import ThreadingHTTPServer
from pathlib import Path

import pytest

from semgrep.semgrep_core import SemgrepCore


@pytest.mark.slow
@pytest.mark.parametrize("collector", ["off", "healthy", "headers", "body"])
def test_tracing_shutdown(tmp_path: Path, collector: str) -> None:
    """A collector must not prevent scan results or keep core alive at exit."""
    release = threading.Event()
    received = threading.Event()

    class Handler(BaseHTTPRequestHandler):
        def do_POST(self) -> None:
            self.rfile.read(int(self.headers.get("Content-Length", "0")))
            received.set()
            if collector == "headers":
                release.wait()
                return
            self.send_response(200)
            self.send_header("Content-Length", "1" if collector == "body" else "0")
            self.end_headers()
            self.wfile.flush()
            if collector == "body":
                release.wait()

        def log_message(self, format: str, *args: object) -> None:
            pass

    rule = tmp_path / "rule.yaml"
    target = tmp_path / "target.py"
    rule.write_text(
        "rules:\n- id: shutdown\n  languages: [python]\n"
        "  message: shutdown\n  severity: WARNING\n  pattern: print(...)\n"
    )
    target.write_text('print("hello")\n')
    with ThreadingHTTPServer(("127.0.0.1", 0), Handler) as server:
        thread = threading.Thread(target=server.serve_forever, daemon=True)
        thread.start()
        command = [
            str(SemgrepCore.path()),
            "-json_nodots",
            "-j",
            "1",
            "-lang",
            "python",
            "-rules",
            str(rule),
            str(target),
        ]
        if collector != "off":
            command += [
                "-trace",
                "-trace_endpoint",
                f"http://127.0.0.1:{server.server_port}",
            ]
        env = {
            key: value
            for key, value in os.environ.items()
            if not key.startswith("OTEL_")
        }
        env["OTEL_EXPORTER_OTLP_TIMEOUT"] = "1000"
        try:
            result = subprocess.run(
                command,
                env=env,
                capture_output=True,
                text=True,
                timeout=10,
            )
            assert result.returncode == 0, result.stderr
            report = json.loads(result.stdout)
            assert len(report["results"]) == 1
            assert report["results"][0]["check_id"] == "shutdown"
            assert not report["errors"]
            assert received.is_set() == (collector != "off")
        finally:
            release.set()
            server.shutdown()
            thread.join()
