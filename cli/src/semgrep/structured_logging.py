# Copyright (c) 2026 Semgrep Inc.
# SPDX-License-Identifier: LGPL-2.1-or-later
"""Optional execution events, independent of scan-result and terminal output."""
import json
import logging
import os
import time
import uuid
from contextlib import contextmanager
from datetime import datetime
from datetime import timezone
from pathlib import Path
from typing import Any
from typing import Iterator

import click

HANDLER_NAME = "semgrep-json-handler"


class JsonLogFormatter(logging.Formatter):
    def format(self, record: logging.LogRecord) -> str:
        event = getattr(record, "event", "log")
        return json.dumps(
            {
                "schema_version": 1,
                "timestamp": datetime.fromtimestamp(record.created, timezone.utc)
                .isoformat(timespec="milliseconds")
                .replace("+00:00", "Z"),
                "level": record.levelname.lower(),
                "source": record.name,
                "event": event,
                "message": click.unstyle(record.getMessage()),
                "attributes": getattr(record, "event_attributes", {}),
            },
            ensure_ascii=False,
            allow_nan=False,
        )


class JsonLogHandler(logging.FileHandler):
    def handleError(self, record: logging.LogRecord) -> None:
        # Logging must not change scan behavior, including when a disk fills up.
        pass


def configure_json_logging(level: int, path: str | None = None) -> None:
    """Keep one append-only destination across repeated terminal configuration."""
    path = path or os.getenv("SEMGREP_LOG_JSON_FILE")
    destination = str(Path(path).absolute()) if path else None
    root = logging.getLogger()
    for handler in list(root.handlers):
        if handler.get_name() == HANDLER_NAME:
            if (
                isinstance(handler, JsonLogHandler)
                and handler.baseFilename == destination
            ):
                handler.setLevel(level)
                return
            root.removeHandler(handler)
            handler.close()
    if destination is None:
        return
    try:
        Path(destination).parent.mkdir(parents=True, exist_ok=True)
        handler = JsonLogHandler(destination, mode="a", encoding="utf-8")
    except (OSError, ValueError):
        logging.getLogger("semgrep").warning("Could not open the structured log file.")
        return
    handler.set_name(HANDLER_NAME)
    handler.setLevel(level)
    handler.setFormatter(JsonLogFormatter())
    root.addHandler(handler)


def emit_event(
    event: str, message: str, *, level: int = logging.INFO, **attributes: Any
) -> None:
    """Write a typed event without adding a line to existing text output."""
    record = logging.LogRecord("semgrep", level, "", 0, message, (), None)
    record.event = event
    record.event_attributes = attributes
    for handler in logging.getLogger().handlers:
        if handler.get_name() == HANDLER_NAME and level >= handler.level:
            handler.handle(record)


@contextmanager
def log_phase(phase: str) -> Iterator[None]:
    """Pair phase events, including nested analysis runs and failed phases."""
    phase_id = str(uuid.uuid4())
    started = time.monotonic()
    emit_event("phase.started", f"Started {phase}.", phase=phase, phase_id=phase_id)
    try:
        yield
    except Exception as error:
        emit_event(
            "phase.failed",
            f"Failed {phase}.",
            level=logging.ERROR,
            phase=phase,
            phase_id=phase_id,
            duration_ms=round((time.monotonic() - started) * 1000),
            error_type=type(error).__name__,
        )
        raise
    else:
        emit_event(
            "phase.completed",
            f"Completed {phase}.",
            phase=phase,
            phase_id=phase_id,
            duration_ms=round((time.monotonic() - started) * 1000),
        )
