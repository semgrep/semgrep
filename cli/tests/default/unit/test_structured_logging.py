# Copyright (c) 2026 Semgrep Inc.
# SPDX-License-Identifier: LGPL-2.1-or-later
import json
import logging

import click
import pytest
from structlog.contextvars import bind_contextvars
from structlog.contextvars import bound_contextvars
from structlog.contextvars import get_contextvars

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.commands.wrapper import handle_command_errors
from semgrep.constants import OutputFormat
from semgrep.error import SemgrepError
from semgrep.metrics import MetricsState
from semgrep.output import OutputHandler
from semgrep.output import OutputSettings
from semgrep.state import get_state
from semgrep.structured_logging import best_effort_events
from semgrep.structured_logging import configure_json_logging
from semgrep.structured_logging import HANDLER_NAME
from semgrep.structured_logging import log
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
    monkeypatch.delenv("SEMGREP_LOG_JSON_FILE", raising=False)
    configure_json_logging(logging.INFO)
    for handler in root.handlers:
        handler.close()
    root.handlers = old_handlers
    root.setLevel(old_level)


@pytest.mark.quick
def test_dual_output_preserves_records_across_reconfiguration(log_file, capsys):
    terminal = Terminal()
    terminal.configure(quiet=False)
    logger = logging.getLogger("semgrep.test")
    log.info("scan started", **{"file.path": "Example.py"})
    logger.info("first\nsecond")
    terminal.configure(quiet=False)
    try:
        raise ValueError("original failure")
    except ValueError:
        logger.warning(
            "last", exc_info=True, extra={"private_extra": "must not be serialized"}
        )
    records = [json.loads(line) for line in log_file.read_text().splitlines()]
    assert [r["message"] for r in records] == ["scan started", "last"]
    assert [r["level"] for r in records] == ["info", "warning"]
    assert all(
        r["schema.version"] == 1 and r["timestamp"].endswith("Z") for r in records
    )
    assert records[0]["attributes"] == {"file.path": "Example.py"}
    assert records[1]["attributes"] == {}
    assert all("event" not in record for record in records)
    text = capsys.readouterr().err
    assert text.startswith("first\nsecond\nlast\nTraceback (most recent call last):")
    assert text.endswith("ValueError: original failure\n")
    assert "private_extra" not in log_file.read_text()


@pytest.mark.quick
def test_phase_events_are_structured_only_and_propagate_failure(log_file, capsys):
    Terminal().configure(quiet=False)
    with log_phase("scan"):
        log.info(
            "scan analysis summary",
            **{"scan.findings.count": 0, "scan.targets.count": 12},
        )
    with pytest.raises(ValueError, match="original failure"):
        with log_phase("analysis"):
            raise ValueError("original failure")
    records = [json.loads(line) for line in log_file.read_text().splitlines()]
    assert [r["message"] for r in records] == [
        "scan started",
        "scan analysis summary",
        "scan completed",
        "analysis started",
        "analysis failed",
    ]
    assert records[1]["attributes"] == {
        "scan.findings.count": 0,
        "scan.targets.count": 12,
    }
    assert records[0]["attributes"]["phase.id"] == records[2]["attributes"]["phase.id"]
    assert records[3]["attributes"]["phase.id"] == records[4]["attributes"]["phase.id"]
    assert records[4]["attributes"]["error.type"] == "ValueError"
    assert records[4]["attributes"]["duration.ms"] >= 0
    assert capsys.readouterr().err == ""


@pytest.mark.quick
def test_unwritable_destination_and_serialization_failures_do_not_fail_scan(
    log_file, monkeypatch
):
    Terminal().configure(quiet=False)
    log.info("Invalid event attributes", invalid=object())
    handler = next(
        h for h in logging.getLogger().handlers if h.get_name() == HANDLER_NAME
    )

    assert isinstance(handler, logging.FileHandler)

    def fail_write(message):
        raise OSError("disk full")

    with monkeypatch.context() as patch:
        patch.setattr(handler.stream, "write", fail_write)
        log.info("A full disk must not fail the scan.")
    log.info("Valid record", count=0)
    assert json.loads(log_file.read_text())["message"] == "Valid record"
    configure_json_logging(logging.INFO, str(log_file / "not-a-directory"))
    logging.getLogger("semgrep.test").info("Text output still works")
    assert not any(h.get_name() == HANDLER_NAME for h in logging.getLogger().handlers)


@pytest.mark.quick
def test_existing_jsonl_is_appended_and_quiet_filters_logs(
    log_file, monkeypatch, capsys
):
    log_file.write_text('{"existing":true}\n')
    Terminal().configure(quiet=True)
    logging.getLogger("semgrep.test").info("hidden")
    assert log_file.read_text() == '{"existing":true}\n'
    log.info("command completed", **{"exit.code": 0})
    assert (
        json.loads(log_file.read_text().splitlines()[-1])["message"]
        == "command completed"
    )
    contents = log_file.read_text()
    monkeypatch.delenv("SEMGREP_LOG_JSON_FILE")
    configure_json_logging(logging.INFO)
    log.error("disabled event", invalid=object())
    assert log_file.read_text() == contents
    assert capsys.readouterr().err == ""


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
        if record["message"] in {"scan warning", "scan error"}
    ]
    assert [record["message"] for record in typed] == ["scan warning", "scan error"]
    assert [record["level"] for record in typed] == ["warning", "error"]
    assert [record["attributes"]["error.code"] for record in typed] == [0, 2]


@pytest.mark.quick
@pytest.mark.parametrize("unexpected", [False, True])
def test_command_context_preserves_values_and_restores_scope(log_file, unexpected):
    Terminal().configure(quiet=False)
    get_state().metrics.configure(MetricsState.OFF)

    @handle_command_errors
    def scan():
        bind_contextvars(**{"scan.id": 12})
        with bound_contextvars(**{"scan.phase": "baseline"}):
            log.info(
                "finding reported",
                **{"file.path": "Src/Example.py", "rule.id": "MixedCase.Rule"},
            )
        log.info("scan completed", **{"scan.id": 13})
        logging.getLogger("semgrep.test").warning("original diagnostic")
        if unexpected:
            raise ValueError("original failure")
        raise SystemExit(1)

    with bound_contextvars(**{"caller.id": "existing"}):
        with pytest.raises(SystemExit) as result:
            scan()
        assert result.value.code == (2 if unexpected else 1)
        assert get_contextvars() == {"caller.id": "existing"}
    log.info("after command")
    records = [json.loads(line) for line in log_file.read_text().splitlines()]
    if unexpected:
        error = next(
            record for record in records if record["message"] == "command failed"
        )
        assert error["level"] == "error"
        assert error["attributes"]["error.type"] == "ValueError"
        assert error["attributes"]["error.stack"].endswith(
            "ValueError: original failure\n"
        )
        records.remove(error)
    common = {
        "caller.id": "existing",
        "command.name": "scan",
        "command.id": records[0]["attributes"]["command.id"],
    }
    assert records[1]["attributes"] == {
        **common,
        "scan.id": 12,
        "scan.phase": "baseline",
        "file.path": "Src/Example.py",
        "rule.id": "MixedCase.Rule",
    }
    assert records[2]["attributes"] == {
        **common,
        "scan.id": 13,
        "scan.phase": "current",
    }
    assert records[3]["attributes"] == {
        **common,
        "scan.id": 12,
        "scan.phase": "current",
    }
    assert records[4]["message"] == "command completed"
    assert records[4]["attributes"]["scan.id"] == 12
    assert records[5]["attributes"] == {}


@pytest.mark.quick
def test_optional_metadata_failure_reports_warning_without_interrupting(log_file):
    Terminal().configure(quiet=False)

    @best_effort_events
    def report_metadata():
        raise TypeError("sensitive metadata must not be copied into fallback")

    report_metadata()
    log.info("scan completed")
    records = [json.loads(line) for line in log_file.read_text().splitlines()]
    assert records[0]["message"] == "structured event unavailable"
    assert records[0]["level"] == "warning"
    assert records[0]["attributes"] == {"error.type": "TypeError"}
    assert records[1]["message"] == "scan completed"
    assert "sensitive" not in log_file.read_text()


@pytest.mark.quick
def test_error_event_preserves_rule_diagnostic_and_registered_source(log_file):
    from semgrep.error import InvalidRuleSchemaError
    from semgrep.error_location import Span

    source = "rules:\n  - id: Example.Rule\n    pattern: example()\n"
    error = InvalidRuleSchemaError(
        short_msg="Invalid rule schema",
        long_msg="One of these properties is missing: message",
        spans=[Span.from_string(source, filename="Rules.yaml")],
        help="Add the required message field",
    )
    configure_json_logging(logging.INFO)
    handler = OutputHandler(OutputSettings(output_format=OutputFormat.JSON))
    handler.handle_semgrep_errors([error])

    [record] = [json.loads(line) for line in log_file.read_text().splitlines()]
    assert record["message"] == "scan error"
    assert record["level"] == "error"
    attributes = record["attributes"]
    assert attributes["error.message"] == "Invalid rule schema"
    assert attributes["error.detail"] == "One of these properties is missing: message"
    assert attributes["error.help"] == "Add the required message field"
    [span] = attributes["error.spans"]
    assert span["file.path"] == "Rules.yaml"
    assert span["file.start.line"] == span["context.start.line"] == 1
    assert span["file.end.line"] == span["context.end.line"] == 3
    assert span["source.text"] == source.rstrip("\n")
    assert "Example.Rule" in span["source.text"]
