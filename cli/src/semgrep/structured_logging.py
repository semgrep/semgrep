# Copyright (c) 2026 Semgrep Inc.
# SPDX-License-Identifier: LGPL-2.1-or-later
"""Optional execution events, independent of scan-result and terminal output."""
import logging
import os
import time
import uuid
from contextlib import contextmanager
from functools import wraps
from pathlib import Path
from typing import Any
from typing import Callable
from typing import Iterator
from typing import ParamSpec

import click
import structlog
from structlog.contextvars import merge_contextvars
from structlog.stdlib import BoundLogger
from structlog.stdlib import ProcessorFormatter
from structlog.typing import EventDict

HANDLER_NAME = "semgrep-json-handler"
_EVENT_LOGGER = logging.getLogger("semgrep.execution")
_EVENT_LOGGER.propagate = False
_EVENT_LOGGER.disabled = True
log: BoundLogger = structlog.wrap_logger(
    _EVENT_LOGGER,
    processors=[
        structlog.stdlib.filter_by_level,
        merge_contextvars,
        structlog.processors.format_exc_info,
        ProcessorFormatter.wrap_for_formatter,
    ],
    wrapper_class=BoundLogger,
)


_P = ParamSpec("_P")


def best_effort_events(func: Callable[_P, None]) -> Callable[_P, None]:
    """Keep optional metadata extraction failures out of scan execution."""

    @wraps(func)
    def wrapped(*args: _P.args, **kwargs: _P.kwargs) -> None:
        if not log.is_enabled_for(logging.INFO):
            return
        try:
            func(*args, **kwargs)
        except Exception as error:
            log.warning(
                "structured event unavailable",
                **{"error.type": type(error).__name__},
            )

    return wrapped


def _event_envelope(logger: Any, method: str, event: EventDict) -> EventDict:
    record = event.pop("_record")
    typed = event.pop("_from_structlog")
    timestamp = event.pop("timestamp")
    level = event.pop("level")
    message = click.unstyle(event.pop("event"))
    if typed:
        if "exception" in event:
            event["error.stack"] = event.pop("exception") + "\n"
    else:
        event.pop("exc_info", None)
        event.pop("stack_info", None)
    return {
        "schema.version": 1,
        "timestamp": timestamp,
        "level": level,
        "source": "semgrep" if typed else record.name,
        "message": message,
        "attributes": event,
    }


class JsonLogHandler(logging.FileHandler):
    def filter(self, record: logging.LogRecord) -> bool:
        return (
            not getattr(record, "structured_skip", False)
            and super().filter(record)
            and (record.name == _EVENT_LOGGER.name or record.levelno >= logging.WARNING)
        )

    def handleError(self, record: logging.LogRecord) -> None:
        # Logging must not change scan behavior, including when a disk fills up.
        pass


def configure_json_logging(level: int, path: str | None = None) -> None:
    """Keep one append-only destination across repeated terminal configuration."""
    # Terminal quiet mode must not leave an opted-in execution stream unfinished.
    level = min(level, logging.INFO)
    path = path or os.getenv("SEMGREP_LOG_JSON_FILE")
    destination = str(Path(path).absolute()) if path else None
    root = logging.getLogger()
    _EVENT_LOGGER.disabled = True
    _EVENT_LOGGER.setLevel(level)
    for handler in list(root.handlers):
        if handler.get_name() == HANDLER_NAME:
            if (
                isinstance(handler, JsonLogHandler)
                and handler.baseFilename == destination
            ):
                handler.setLevel(level)
                _EVENT_LOGGER.handlers = [handler]
                _EVENT_LOGGER.disabled = False
                return
            root.removeHandler(handler)
            _EVENT_LOGGER.removeHandler(handler)
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
    handler.setFormatter(
        ProcessorFormatter(
            foreign_pre_chain=[merge_contextvars],
            processors=[
                structlog.stdlib.add_log_level,
                structlog.processors.TimeStamper(fmt="iso", utc=True),
                _event_envelope,
                structlog.processors.JSONRenderer(
                    ensure_ascii=False, allow_nan=False, default=None
                ),
            ],
        )
    )
    root.addHandler(handler)
    _EVENT_LOGGER.handlers = [handler]
    _EVENT_LOGGER.disabled = False


@best_effort_events
def emit_error(error: Exception, *, message: str | None = None) -> None:
    """Preserve diagnostic details and registered source spans without terminal markup."""
    import semgrep.semgrep_interfaces.semgrep_output_v1 as out
    from semgrep.error import ErrorWithSpan
    from semgrep.error import SemgrepCoreError
    from semgrep.error import SemgrepError
    from semgrep.error_location import SourceFileHash
    from semgrep.error_location import SourceTracker

    warning = isinstance(error, SemgrepError) and isinstance(
        error.level.value, out.Warning_
    )
    attributes: dict[str, Any] = {
        "error.type": type(error).__name__,
        "error.message": str(error),
    }
    if isinstance(error, SemgrepError):
        attributes["error.code"] = error.code
    if isinstance(error, ErrorWithSpan):
        attributes.update({"error.detail": error.long_msg, "error.help": error.help})
    if isinstance(error, SemgrepCoreError):
        if error.core.location:
            location = error.core.location
            attributes.update(
                {
                    "file.path": location.path.value,
                    "file.start.line": location.start.line,
                    "file.start.column": location.start.col,
                    "file.end.line": location.end.line,
                    "file.end.column": location.end.col,
                }
            )
        if error.core.rule_id:
            attributes["rule.id"] = error.core.rule_id.value
    if isinstance(error, (ErrorWithSpan, SemgrepCoreError)) and error.spans:
        spans = []
        for span in error.spans:
            file_path = (
                span.file.value if isinstance(span.file, out.Fpath) else span.file
            )
            context_start = span.context_start or span.start
            context_end = span.context_end or span.end
            details: dict[str, Any] = {
                "file.path": file_path,
                "file.start.line": span.start.line,
                "file.start.column": span.start.col,
                "file.end.line": span.end.line,
                "file.end.column": span.end.col,
                "context.start.line": context_start.line,
                "context.start.column": context_start.col,
                "context.end.line": context_end.line,
                "context.end.column": context_end.col,
            }
            if span.config_path:
                details["config.path"] = span.config_path
            if span.source_hash:
                try:
                    source = SourceTracker.source(SourceFileHash(span.source_hash))
                except KeyError:
                    pass
                else:
                    details["source.text"] = "\n".join(
                        source[context_start.line - 1 : context_end.line]
                    )
            spans.append(details)
        attributes["error.spans"] = spans
    log.log(
        logging.WARNING if warning else logging.ERROR,
        message or ("scan warning" if warning else "scan error"),
        **attributes,
    )


@contextmanager
def log_phase(phase: str) -> Iterator[None]:
    """Pair phase events, including nested analysis runs and failed phases."""
    phase_log = log.bind(**{"phase.name": phase, "phase.id": str(uuid.uuid4())})
    started = time.monotonic()
    phase_log.info(f"{phase} started")
    try:
        yield
    except Exception as error:
        phase_log.error(
            f"{phase} failed",
            **{
                "duration.ms": round((time.monotonic() - started) * 1000),
                "error.type": type(error).__name__,
            },
        )
        raise
    else:
        phase_log.info(
            f"{phase} completed",
            **{
                "duration.ms": round((time.monotonic() - started) * 1000),
            },
        )
