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
from collections.abc import Coroutine
from collections.abc import Generator
from contextlib import contextmanager
from typing import Any
from typing import Concatenate
from typing import ParamSpec
from typing import TypeVar

from mcp.server.fastmcp.server import Context
from opentelemetry import trace
from ruamel.yaml import YAML

from semgrep.mcp.models import CodeFile
from semgrep.mcp.models import SemgrepScanResult
from semgrep.mcp.utilities.utils import get_anonymous_user_id
from semgrep.mcp.utilities.utils import get_deployment_id_from_token
from semgrep.mcp.utilities.utils import get_deployment_name_from_token
from semgrep.mcp.utilities.utils import get_git_info
from semgrep.mcp.utilities.utils import get_semgrep_app_token
from semgrep.mcp.utilities.utils import is_hosted
from semgrep.metrics import Finding
from semgrep.metrics import MetricsState
from semgrep.state import get_state
from semgrep.tracing import _DEFAULT_OTEL_ENDPOINT
from semgrep.tracing import _DEV_OTEL_ENDPOINT
from semgrep.tracing import _LOCAL_DEV_OTEL_ENDPOINT
from semgrep.tracing import TRACER
from semgrep.verbose_logging import getLogger


logger = getLogger(__name__)

DEPLOYMENT_ROUTE = "/api/agent/deployments/current"

MCP_SERVICE_NAME = "mcp"

NON_SCAN_MCP_TOOL_TAG = "(non-scan-mcp-tool)"

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
    state = get_state()
    state.metrics.add_mcp_git_info(git_info)


def attach_metrics(
    span: trace.Span | None,
    version: str,
    skipped_rules: list[str],
    paths: list[Any],
    findings: list[dict[str, Any]],
    errors: list[dict[str, Any]],
    workspace_dir: str | None,
    num_lines_scanned: int,
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
    span.set_attribute("metrics.num_lines_scanned", num_lines_scanned)
    # TODO: the actual findings and errors (not just the number). This might require
    # us setting up Datadog metrics and not just tracing.


def attach_scan_metrics(
    span: trace.Span | None,
    results: SemgrepScanResult,
    workspace_dir: str | None,
    code_files: list[CodeFile],
) -> None:
    if span is None:
        return
    num_lines_scanned = sum(len(file.content.splitlines()) for file in code_files)
    attach_metrics(
        span,
        results.version,
        results.skipped_rules,
        results.paths["scanned"],
        results.results,
        results.errors,
        workspace_dir,
        num_lines_scanned,
    )
    state = get_state()
    state.metrics.add_mcp_scan_metrics(results, num_lines_scanned)


def attach_findings_metrics(
    span: trace.Span | None,
    tps: list[tuple[str, Finding]],
    fps: list[tuple[str, Finding]],
    skips: list[tuple[str, Finding]],
) -> None:
    if span is None:
        return
    span.set_attribute("metrics.num_tps", len(tps))
    span.set_attribute("metrics.num_fps", len(fps))
    span.set_attribute("metrics.num_skips", len(skips))
    state = get_state()
    state.metrics.add_mcp_findings_metrics(tps, fps, skips)


################################################################################
# Tracing Helpers #
################################################################################


def get_trace_endpoint() -> tuple[str, str]:
    """Get the appropriate trace endpoint based on environment."""
    env = os.environ.get("SEMGREP_OTEL_ENDPOINT", "semgrep-prod").lower()

    if env == "semgrep-dev":
        return (_DEV_OTEL_ENDPOINT, "semgrep-dev")
    elif env == "semgrep-local":
        return (_LOCAL_DEV_OTEL_ENDPOINT, "semgrep-local")
    else:
        return (_DEFAULT_OTEL_ENDPOINT, "semgrep-prod")


def is_tracing_disabled() -> bool:
    return os.environ.get("SEMGREP_MCP_DISABLE_TRACING", "").lower() == "true"


################################################################################
# Tracing #
################################################################################


@contextmanager
def start_tracing(name: str) -> Generator[trace.Span | None, None, None]:
    """Initialize OpenTelemetry tracing."""
    state = get_state()

    if is_tracing_disabled():
        state.metrics.configure(MetricsState.OFF)
        yield None
    else:
        (endpoint, env) = get_trace_endpoint()

        deployment_id = get_deployment_id_from_token(get_semgrep_app_token())

        state.traces.configure(
            True,
            endpoint,
            MCP_SERVICE_NAME,
            {
                "metrics.is_hosted": is_hosted(),
                "metrics.deployment_id": str(deployment_id) if deployment_id else "",
                "metrics.deployment_name": get_deployment_name_from_token(
                    get_semgrep_app_token()
                )
                or "",
                "metrics.anonymous_user_id": get_anonymous_user_id(),
            },
        )

        state.metrics.configure(MetricsState.ON)

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


def tag_and_send_metrics(send_metrics: bool, is_semgrep_scan: bool) -> None:
    state = get_state()
    if send_metrics:
        if not is_semgrep_scan:
            state.app_session.user_agent.tags.add(NON_SCAN_MCP_TOOL_TAG)
        state.metrics.send()
        state.app_session.user_agent.tags.discard(NON_SCAN_MCP_TOOL_TAG)


def with_tool_span(
    span_name: str | None = None,
    send_metrics: bool = True,
    is_semgrep_scan: bool = True,
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

            state = get_state()
            if send_metrics:
                # Clear the metrics set by the previous tool call
                state.metrics.clear_mcp()
                state.metrics.add_mcp(
                    deployment_id=get_deployment_id_from_token(get_semgrep_app_token()),
                    deployment_name=get_deployment_name_from_token(
                        get_semgrep_app_token()
                    )
                    or "",
                    session_id=context.session_id,
                    tool_name=name,
                )

            with with_span(context.top_level_span, name):
                try:
                    result = await func(ctx, *args, **kwargs)
                    logger.info(f"{name} succeeded")
                    tag_and_send_metrics(send_metrics, is_semgrep_scan)
                    return result
                except Exception as e:
                    logger.info(f"{name} failed: {e}")
                    state.metrics.add_mcp_error(str(e))
                    tag_and_send_metrics(send_metrics, is_semgrep_scan)
                    raise e

        return wrapper

    return decorator


def with_hook_span(
    span_name: str | None = None,
    send_metrics: bool = True,
    is_semgrep_scan: bool = True,
) -> Callable[
    [Callable[Concatenate[trace.Span | None, P], Coroutine[Any, Any, R]]],
    Callable[Concatenate[trace.Span | None, P], Coroutine[Any, Any, R]],
]:
    """
    Decorator to wrap hooks with a tracing span.

    All hooks decorated by @with_hook_span must have an top_level_span parameter.

    Args:
        span_name: Optional name for the span. If not provided, uses the function name.
        send_metrics: Whether to send metrics for the hook.
        is_semgrep_scan: Whether the hook is a scan.
    """

    def decorator(
        func: Callable[Concatenate[trace.Span | None, P], Coroutine[Any, Any, R]],
    ) -> Callable[Concatenate[trace.Span | None, P], Coroutine[Any, Any, R]]:
        @functools.wraps(func)
        async def wrapper(
            top_level_span: trace.Span | None, *args: P.args, **kwargs: P.kwargs
        ) -> R:
            name = span_name or func.__name__

            state = get_state()
            if send_metrics:
                state.metrics.clear_mcp()
                state.metrics.add_mcp(
                    deployment_id=get_deployment_id_from_token(get_semgrep_app_token()),
                    deployment_name=get_deployment_name_from_token(
                        get_semgrep_app_token()
                    )
                    or "",
                    session_id="hook",  # TODO: No session id for hooks yet, using a placeholder
                    tool_name=name,
                )

            with with_span(top_level_span, name):
                result = await func(top_level_span, *args, **kwargs)
                tag_and_send_metrics(send_metrics, is_semgrep_scan)
                return result

        return wrapper

    return decorator
