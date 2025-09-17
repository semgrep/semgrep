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
from collections.abc import Mapping
from contextlib import contextmanager
from typing import Any
from typing import Concatenate
from typing import ParamSpec
from typing import TypeVar

import requests
from mcp.server.fastmcp.server import Context
from opentelemetry import trace
from opentelemetry.exporter.otlp.proto.http.trace_exporter import OTLPSpanExporter
from opentelemetry.sdk.resources import DEPLOYMENT_ENVIRONMENT
from opentelemetry.sdk.resources import Resource
from opentelemetry.sdk.resources import SERVICE_NAME
from opentelemetry.sdk.trace import TracerProvider
from opentelemetry.sdk.trace.export import BatchSpanProcessor
from ruamel.yaml import YAML

from semgrep import __VERSION__
from semgrep.mcp.models import SemgrepScanResult
from semgrep.mcp.utilities.utils import get_user_settings_file
from semgrep.mcp.utilities.utils import is_hosted
from semgrep.semgrep_interfaces.semgrep_output_v1 import CliOutput
from semgrep.state import get_state
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

# coupling: these need to be kept in sync with semgrep-proprietary/tracing.py
DEFAULT_TRACE_ENDPOINT = "https://telemetry.semgrep.dev/v1/traces"
DEFAULT_DEV_ENDPOINT = "https://telemetry.dev2.semgrep.dev/v1/traces"
DEFAULT_LOCAL_ENDPOINT = "http://localhost:4318/v1/traces"

DEPLOYMENT_ROUTE = "/api/agent/deployments/current"

MCP_SERVICE_NAME = "mcp"

yaml = YAML()

################################################################################
# Metrics Helpers #
################################################################################


def get_deployment_id_from_token(token: str) -> str:
    """
    Returns the deployment ID the token is for, if token is valid
    """
    if not token:
        return ""

    state = get_state()

    resp = requests.get(
        f"{state.env.semgrep_url}{DEPLOYMENT_ROUTE}",
        headers={"Authorization": f"Bearer {token}"},
        timeout=(2, 30),
    )
    if resp.status_code == 200:
        deployment = resp.json().get("deployment")
        return str(deployment.get("id")) if deployment else ""
    else:
        return ""


def get_token_from_user_settings() -> str:
    settings_file = get_user_settings_file()
    if not os.access(settings_file, os.R_OK) or not settings_file.is_file():
        return ""
    with settings_file.open() as fd:
        yaml_contents = yaml.load(fd)

    if not isinstance(yaml_contents, Mapping):
        return ""

    return yaml_contents.get("api_token", "")


def attach_metrics(
    span: trace.Span | None,
    version: str,
    skipped_rules: list[str],
    paths: list[Any],
    findings: list[dict[str, Any]],
    errors: list[dict[str, Any]],
    config: str | None,
) -> None:
    if span is None:
        return
    span.set_attribute("metrics.semgrep_version", version)
    span.set_attribute("metrics.num_skipped_rules", len(skipped_rules))
    span.set_attribute("metrics.rule_config", config if config else "default")
    span.set_attribute("metrics.num_scanned_files", len(paths))
    span.set_attribute("metrics.num_findings", len(findings))
    span.set_attribute("metrics.num_errors", len(errors))
    # TODO: the actual findings and errors (not just the number). This might require
    # us setting up Datadog metrics and not just tracing.


def attach_scan_metrics(
    span: trace.Span | None, results: SemgrepScanResult, config: str | None
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
        config,
    )


def attach_rpc_scan_metrics(span: trace.Span | None, results: CliOutput) -> None:
    if span is None:
        return
    span.set_attribute(
        "metrics.semgrep_version",
        results.version.value if results.version else "unknown",
    )
    span.set_attribute("metrics.num_skipped_rules", len(results.skipped_rules))
    # Rules for RPC scans are cached by pulling the user's rules.
    span.set_attribute("metrics.rule_config", "cached")
    span.set_attribute("metrics.num_scanned_files", len(results.paths.scanned))
    span.set_attribute("metrics.num_findings", len(results.results))
    span.set_attribute("metrics.num_errors", len(results.errors))


################################################################################
# Tracing Helpers #
################################################################################


def get_trace_endpoint() -> tuple[str, str]:
    """Get the appropriate trace endpoint based on environment."""
    env = os.environ.get("SEMGREP_OTEL_ENDPOINT", "semgrep-dev").lower()

    if env == "semgrep-prod":
        return (DEFAULT_TRACE_ENDPOINT, "semgrep-prod")
    elif env == "semgrep-local":
        return (DEFAULT_LOCAL_ENDPOINT, "semgrep-local")
    else:
        return (DEFAULT_DEV_ENDPOINT, "semgrep-dev")


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

        token = os.environ.get("SEMGREP_APP_TOKEN", get_token_from_user_settings())

        # Create resource with basic attributes
        resource = Resource.create(
            {
                SERVICE_NAME: MCP_SERVICE_NAME,
                DEPLOYMENT_ENVIRONMENT: env,
                "metrics.semgrep_version": __VERSION__,
                "metrics.is_hosted": is_hosted(),
                "metrics.deployment_id": get_deployment_id_from_token(token),
            }
        )

        # Create tracer provider
        provider = TracerProvider(resource=resource)

        # Create OTLP exporter
        exporter = OTLPSpanExporter(endpoint=endpoint)

        # Create span processor
        processor = BatchSpanProcessor(exporter)
        provider.add_span_processor(processor)

        # Set the global tracer provider
        trace.set_tracer_provider(provider)

        # Get tracer instance
        tracer = trace.get_tracer(MCP_SERVICE_NAME)

        with tracer.start_as_current_span(name) as span:
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
        tracer = trace.get_tracer(MCP_SERVICE_NAME)

        context = trace.set_span_in_context(parent_span)
        with tracer.start_as_current_span(name, context=context) as span:
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
