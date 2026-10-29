# Copyright (c) 2026 Semgrep Inc.
# SPDX-License-Identifier: LGPL-2.1-or-later
import json
import logging

import click
import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.constants import OutputFormat
from semgrep.error import SemgrepError
from semgrep.output import OutputHandler
from semgrep.output import OutputSettings
from semgrep.state import get_state
from semgrep.structured_logging import configure_json_logging
from semgrep.structured_logging import emit_event
from semgrep.structured_logging import HANDLER_NAME
from semgrep.structured_logging import log_phase
from semgrep.terminal import Terminal


@pytest.fixture
def log_file(tmp_path, monkeypatch):
    path = tmp_path / "events.jsonl"
    monkeypatch.setenv("SEMGREP_LOG_JSON_FILE", str(path))
    monkeypatch.setenv("SEMGREP_LOG_FILE", str(tmp_path / "text.log"))
    root = logging.getLogger()
    old_handlers = root.handlers[:]
    old_level = root.level
    root.handlers = []
    with click.Context(click.Command("semgrep")):
        yield path
    for handler in root.handlers:
        handler.close()
    root.handlers = old_handlers
    root.setLevel(old_level)


@pytest.mark.quick
def test_dual_output_preserves_records_across_reconfiguration(log_file, capsys):
    terminal = Terminal()
    terminal.configure(quiet=False)
    logger = logging.getLogger("semgrep.test")
    logger.info("first\nsecond")
    terminal.configure(quiet=False)
    logger.warning("last", extra={"private_extra": "must not be serialized"})
    records = [json.loads(line) for line in log_file.read_text().splitlines()]
    assert [r["message"] for r in records] == ["first\nsecond", "last"]
    assert [r["level"] for r in records] == ["info", "warning"]
    assert all(
        r["schema_version"] == 1 and r["timestamp"].endswith("Z") for r in records
    )
    assert all(r["source"] == "semgrep.test" and r["attributes"] == {} for r in records)
    assert capsys.readouterr().err == "first\nsecond\nlast\n"
    assert "private_extra" not in log_file.read_text()


@pytest.mark.quick
def test_phase_events_are_structured_only_and_propagate_failure(log_file, capsys):
    Terminal().configure(quiet=False)
    with log_phase("scan"):
        emit_event(
            "scan.summary", "Scan analysis summary.", findings=0, files_scanned=12
        )
    with pytest.raises(ValueError, match="original failure"):
        with log_phase("analysis"):
            raise ValueError("original failure")
    records = [json.loads(line) for line in log_file.read_text().splitlines()]
    assert [r["event"] for r in records] == [
        "phase.started",
        "scan.summary",
        "phase.completed",
        "phase.started",
        "phase.failed",
    ]
    assert records[1]["attributes"] == {"findings": 0, "files_scanned": 12}
    assert records[0]["attributes"]["phase_id"] == records[2]["attributes"]["phase_id"]
    assert records[3]["attributes"]["phase_id"] == records[4]["attributes"]["phase_id"]
    assert records[4]["attributes"]["error_type"] == "ValueError"
    assert records[4]["attributes"]["duration_ms"] >= 0
    assert capsys.readouterr().err == ""


@pytest.mark.quick
def test_unwritable_destination_and_serialization_failures_do_not_fail_scan(
    log_file, monkeypatch
):
    Terminal().configure(quiet=False)
    emit_event("bad", "Invalid event attributes", invalid=object())
    handler = next(
        h for h in logging.getLogger().handlers if h.get_name() == HANDLER_NAME
    )

    assert isinstance(handler, logging.FileHandler)

    def fail_write(message):
        raise OSError("disk full")

    with monkeypatch.context() as patch:
        patch.setattr(handler.stream, "write", fail_write)
        emit_event("unwritten", "A full disk must not fail the scan.")
    emit_event("good", "Valid record", count=0)
    assert json.loads(log_file.read_text())["event"] == "good"
    configure_json_logging(logging.INFO, str(log_file / "not-a-directory"))
    logging.getLogger("semgrep.test").info("Text output still works")
    assert not any(h.get_name() == HANDLER_NAME for h in logging.getLogger().handlers)


@pytest.mark.quick
def test_existing_jsonl_is_appended_and_quiet_filters_logs(log_file):
    log_file.write_text('{"existing":true}\n')
    Terminal().configure(quiet=True)
    logging.getLogger("semgrep.test").info("hidden")
    assert log_file.read_text() == '{"existing":true}\n'


@pytest.mark.quick
def test_typed_errors_are_available_with_json_results(log_file):
    get_state().terminal.configure(quiet=False)
    output = OutputHandler(OutputSettings(output_format=OutputFormat.JSON))
    warning = SemgrepError(
        "Warning message", code=0, level=out.ErrorSeverity(out.Warning_())
    )
    error = SemgrepError("Error message", code=2)
    output.handle_semgrep_errors([warning, error, warning])
    records = [json.loads(line) for line in log_file.read_text().splitlines()]
    typed = [
        record
        for record in records
        if record["event"] in {"scan.warning", "scan.error"}
    ]
    assert [record["event"] for record in typed] == ["scan.warning", "scan.error"]
    assert [record["level"] for record in typed] == ["warning", "error"]
    assert [record["attributes"]["error_code"] for record in typed] == [0, 2]
