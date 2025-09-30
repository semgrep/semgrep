#
# Copyright (c) 2025 Semgrep Inc.
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
import functools
import os
from collections.abc import Awaitable
from collections.abc import Callable
from collections.abc import Generator
from contextlib import contextmanager
from typing import Any
from typing import Concatenate
from typing import ParamSpec
from typing import TypeVar

from mcp.server.fastmcp.server import Context
from opentelemetry import trace
from ruamel.yaml import YAML

from semgrep.mcp.models import SemgrepScanResult
from semgrep.mcp.utilities.utils import get_anonymous_user_id
from semgrep.mcp.utilities.utils import get_deployment_id_from_token
from semgrep.mcp.utilities.utils import get_git_info
from semgrep.mcp.utilities.utils import get_semgrep_app_token
from semgrep.mcp.utilities.utils import is_hosted
from semgrep.state import get_state
from semgrep.tracing import _DEFAULT_ENDPOINT
from semgrep.tracing import _DEV_ENDPOINT
from semgrep.tracing import _LOCAL_ENDPOINT
from semgrep.tracing import TRACER
from semgrep.verbose_logging import getLogger


logger = getLogger(__name__)

DEPLOYMENT_ROUTE = "/api/agent/deployments/current"

MCP_SERVICE_NAME = "mcp"

yaml = YAML()

################################################################################
# Metrics Helpers #
################################################################################


def attach_git_info(span: trace.Span | None, workspace_dir: str | None) -> None:
    if span is None:
        return
    git_info = get_git_info(workspace_dir)
    span.set_attribute("metrics.git_info.username", git_info["username"])
    span.set_attribute("metrics.git_info.repo", git_info["repo"])
    span.set_attribute("metrics.git_info.branch", git_info["branch"])


def attach_metrics(
    span: trace.Span | None,
    version: str,
    skipped_rules: list[str],
    paths: list[Any],
    findings: list[dict[str, Any]],
    errors: list[dict[str, Any]],
    workspace_dir: str | None,
) -> None:
    if span is None:
        return
    span.set_attribute("metrics.semgrep_version", version)
    span.set_attribute("metrics.num_skipped_rules", len(skipped_rules))
    span.set_attribute("metrics.num_scanned_files", len(paths))
    span.set_attribute("metrics.num_findings", len(findings))
    span.set_attribute("metrics.num_errors", len(errors))
    attach_git_info(span, workspace_dir)
    span.set_attribute("metrics.anonymous_user_id", get_anonymous_user_id())
    # TODO: the actual findings and errors (not just the number). This might require
    # us setting up Datadog metrics and not just tracing.


def attach_scan_metrics(
    span: trace.Span | None,
    results: SemgrepScanResult,
    workspace_dir: str | None,
) -> None:
    if span is None:
        return
    attach_metrics(
        span,
        results.version,
        results.skipped_rules,
        results.paths["scanned"],
        results.results,
        results.errors,
        workspace_dir,
    )


################################################################################
# Tracing Helpers #
################################################################################


def get_trace_endpoint() -> tuple[str, str]:
    """Get the appropriate trace endpoint based on environment."""
    env = os.environ.get("SEMGREP_OTEL_ENDPOINT", "semgrep-prod").lower()

    if env == "semgrep-dev":
        return (_DEV_ENDPOINT, "semgrep-dev")
    elif env == "semgrep-local":
        return (_LOCAL_ENDPOINT, "semgrep-local")
    else:
        return (_DEFAULT_ENDPOINT, "semgrep-prod")


def is_tracing_disabled() -> bool:
    return os.environ.get("SEMGREP_MCP_DISABLE_TRACING", "").lower() == "true"


################################################################################
# Tracing #
################################################################################


@contextmanager
def start_tracing(name: str) -> Generator[trace.Span | None, None, None]:
    """Initialize OpenTelemetry tracing."""
    if is_tracing_disabled():
        yield None
    else:
        (endpoint, env) = get_trace_endpoint()

        token = os.environ.get("SEMGREP_APP_TOKEN", get_semgrep_app_token())

        state = get_state()
        state.traces.configure(
            True,
            endpoint,
            MCP_SERVICE_NAME,
            {
                "metrics.is_hosted": is_hosted(),
                "metrics.deployment_id": get_deployment_id_from_token(token),
                "metrics.anonymous_user_id": get_anonymous_user_id(),
            },
        )

        with TRACER.start_as_current_span(name) as span:
            trace_id = trace.format_trace_id(span.get_span_context().trace_id)
            # Get a link to the trace in Datadog
            link = (
                f"(https://app.datadoghq.com/apm/trace/{trace_id})"
                if env != "semgrep-local"
                else ""
            )

            logger.info("Tracing initialized")
            logger.info(f"Tracing initialized with trace ID: {trace_id} {link}")

            yield span


@contextmanager
def with_span(
    parent_span: trace.Span | None,
    name: str,
) -> Generator[trace.Span | None, None, None]:
    if is_tracing_disabled() or parent_span is None:
        yield None
    else:
        context = trace.set_span_in_context(parent_span)
        with TRACER.start_as_current_span(name, context=context) as span:
            yield span


R = TypeVar("R")
P = ParamSpec("P")


def with_tool_span(
    span_name: str | None = None,
) -> Callable[
    [Callable[Concatenate[Context, P], Awaitable[R]]],
    Callable[Concatenate[Context, P], Awaitable[R]],
]:
    """
    Decorator to wrap MCP tools with a tracing span.

    All tools decorated by @with_tool_span must have an Context parameter.

    Args:
        span_name: Optional name for the span. If not provided, uses the function name.
    """

    def decorator(
        func: Callable[Concatenate[Context, P], Awaitable[R]],
    ) -> Callable[Concatenate[Context, P], Awaitable[R]]:
        @functools.wraps(func)
        async def wrapper(ctx: Context, *args: P.args, **kwargs: P.kwargs) -> R:
            context = ctx.request_context.lifespan_context
            name = span_name or func.__name__

            with with_span(context.top_level_span, name):
                return await func(ctx, *args, **kwargs)

        return wrapper

    return decorator
