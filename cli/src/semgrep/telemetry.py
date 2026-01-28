#
# Copyright (c) 2024-2025 Semgrep Inc.
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
# Library to use Opentelemetry traces and logs from the python side of Semgrep
#
# Enables sending traces and logs, currently to Datadog, Jaeger, both with 15
# days retention policy
#
# Communicates with OCaml tracing defined in ../../../libs/tracing/unix/Tracing.ml
# For more info, see https://www.notion.so/semgrep/How-to-add-tracing-b0e1eaa1531e408cbb074663d1f840a6
import functools
import json
import logging
import os
from enum import Enum
from typing import Callable
from typing import TypeVar
from urllib import parse

from attr import define
from opentelemetry import context
from opentelemetry import propagate
from opentelemetry import trace as otrace
from opentelemetry._logs import set_logger_provider
from opentelemetry.attributes import BoundedAttributes
from opentelemetry.exporter.otlp.proto.http._log_exporter import OTLPLogExporter
from opentelemetry.exporter.otlp.proto.http.trace_exporter import OTLPSpanExporter
from opentelemetry.instrumentation.requests import RequestsInstrumentor
from opentelemetry.instrumentation.threading import ThreadingInstrumentor
from opentelemetry.sdk import resources
from opentelemetry.sdk._logs import LoggerProvider
from opentelemetry.sdk._logs import LoggingHandler
from opentelemetry.sdk._logs.export import BatchLogRecordProcessor
from opentelemetry.sdk.environment_variables import OTEL_RESOURCE_ATTRIBUTES
from opentelemetry.sdk.resources import get_aggregated_resources
from opentelemetry.sdk.resources import OTELResourceDetector
from opentelemetry.sdk.resources import ProcessResourceDetector
from opentelemetry.sdk.resources import Resource
from opentelemetry.sdk.resources import ResourceDetector
from opentelemetry.sdk.trace import TracerProvider
from opentelemetry.sdk.trace.export import BatchSpanProcessor
from opentelemetry.trace import SpanContext
from opentelemetry.trace import SpanKind
from opentelemetry.util.types import Attributes
from opentelemetry.util.types import AttributeValue
from typing_extensions import ParamSpec

from semgrep import __VERSION__
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScanInfo

TRACER = otrace.get_tracer(__name__)
TOP_LEVEL_SPAN_KIND = SpanKind.CLIENT
ENGINE_KIND_ATTR = "scan.engine_type"
# Coupling: these constants need to be kept in sync with Tracing.ml

_PYRO_CAML_TAGS = "PYRO_CAML_TAGS"
_PYRO_CAML_SERVICE_NAME = "PYRO_CAML_SERVICE_NAME"
_PYRO_CAML_SERVER_ADDRESS = "PYRO_CAML_SERVER_ADDRESS"
_SEMGREP_TRACE_PARENT_TRACE_ID = "SEMGREP_TRACE_PARENT_TRACE_ID"
_SEMGREP_TRACE_PARENT_SPAN_ID = "SEMGREP_TRACE_PARENT_SPAN_ID"

_DEFAULT_OTEL_ENDPOINT = "https://telemetry.semgrep.dev"
_DEV_OTEL_ENDPOINT = "https://telemetry.dev2.semgrep.dev"
_LOCAL_DEV_OTEL_ENDPOINT = "http://localhost:4318"

_OTEL_ENDPOINT_ALIASES = {
    "semgrep-prod": _DEFAULT_OTEL_ENDPOINT,
    "semgrep-dev": _DEV_OTEL_ENDPOINT,
    "semgrep-local": _LOCAL_DEV_OTEL_ENDPOINT,
}

_DEFAULT_PYROSCOPE_ENDPOINT = "https://pyroscope-receive.private.semgrep.dev"
_DEV_PYROSCOPE_ENDPOINT = "https://pyroscope-receive.dev2.semgrep.dev"
_LOCAL_DEV_PYROSCOPE_ENDPOINT = "http://localhost:4040"
_PYROSCOPE_ENDPOINT_ALIASES = {
    "semgrep-prod": _DEFAULT_PYROSCOPE_ENDPOINT,
    "semgrep-dev": _DEV_PYROSCOPE_ENDPOINT,
    "semgrep-local": _LOCAL_DEV_PYROSCOPE_ENDPOINT,
}


_ENV_ALIASES = {
    "semgrep-prod": "prod",
    "semgrep-dev": "dev2",
    "semgrep-local": "local",
}

# Filter out these attrs when injecting additional context
INJECT_ATTR_FILTER = [
    resources.PROCESS_PID,
    resources.PROCESS_PARENT_PID,
    resources.PROCESS_EXECUTABLE_NAME,
    resources.PROCESS_EXECUTABLE_PATH,
    resources.PROCESS_COMMAND,
    resources.PROCESS_COMMAND_LINE,
    resources.PROCESS_COMMAND_ARGS,
    resources.PROCESS_OWNER,
    resources.PROCESS_RUNTIME_NAME,
    resources.PROCESS_RUNTIME_VERSION,
    resources.PROCESS_RUNTIME_DESCRIPTION,
    resources.SERVICE_NAME,
    resources.SERVICE_VERSION,
    resources.TELEMETRY_SDK_NAME,
    resources.TELEMETRY_SDK_VERSION,
    resources.TELEMETRY_AUTO_VERSION,
    resources.TELEMETRY_SDK_LANGUAGE,
]


def cli_args_to_attrs(locals: dict[str, object]) -> Attributes:
    # coupling: ci.py, scan.py
    args_to_include = [
        "autofix",
        "code",
        "debug",
        "dry_run",
        "exclude",
        "include",
        "jobs",
        "max_memory",
        "max_target_bytes",
        "supply_chain",
        "timeout_threshold",
        "timeout",
        "interfile_timeout",
        "verbose",
    ]
    # prefix all with scan.args
    info: dict[str, str | int] = {}
    for arg in args_to_include:
        value = locals.get(arg)
        if isinstance(value, int) or isinstance(value, str):
            attr_value = value
        elif value is not None:
            attr_value = json.dumps(value)
        else:
            attr_value = "unset"
        info[f"scan.args.{arg}"] = attr_value
    return info


def scan_info_to_attrs(scan_info: ScanInfo) -> Attributes:
    info: Attributes = {
        "scan.deployment_id": scan_info.deployment_id,
        "scan.deployment_name": scan_info.deployment_name,
        "scan.id": scan_info.id or "unset",
    }
    return info


def attrs_to_kv_strs(d: Attributes) -> list[str]:
    if not d:
        return []
    return [f"{k}={parse.quote(str(d[k]).strip())}" for k in d]


def filter_attrs_for_inject(d: Attributes) -> Attributes:
    if not d:
        return d
    new_attrs: Attributes = dict()
    for k in d:
        if k not in INJECT_ATTR_FILTER:
            new_attrs[k] = d[k]  # type: ignore
    return new_attrs


# Normally Opentelemetry resources are supposed to be immutable. But pysemgrep
# does a lot of things in a weird order, such as getting if something is a diff
# scan far after we start tracing, even though really that's something we want
# as a resource attribute
#
# So we create this class that allows us to mutate the resource afterwards
class MutableResource(Resource):
    _mutable_attributes: BoundedAttributes

    def __init__(self, base_resource: Resource) -> None:
        self._mutable_attributes = BoundedAttributes(
            attributes=base_resource.attributes, immutable=False
        )
        super().__init__(
            attributes=self._mutable_attributes, schema_url=base_resource.schema_url
        )

    # Ignore the type here because mypy says the return type is incompatible and
    # then says they're the same type
    @property
    def attributes(self) -> Attributes:  # type: ignore
        return self._mutable_attributes

    def update_attributes(self, new_attrs: Attributes) -> None:
        if not new_attrs:
            return
        for k in new_attrs:
            self._mutable_attributes[k] = new_attrs[k]


# See https://github.com/docker/cli/issues/4958 for why we don't use just OTEL_RESOURCE_ATTRIBUTES
class DockerOTELResourceDetector(ResourceDetector):
    def detect(self) -> "Resource":
        env_resources_items = os.environ.get("DOCKER_OTEL_RESOURCE_ATTRIBUTES")
        env_resource_map: dict[str, AttributeValue] = {}

        if env_resources_items:
            for item in env_resources_items.split(","):
                try:
                    key, value = item.split("=", maxsplit=1)
                except ValueError:
                    continue
                value_url_decoded = parse.unquote(value.strip())
                env_resource_map[key.strip()] = value_url_decoded

        return Resource(env_resource_map)


@define
class Telemetry:
    enabled: bool = False
    resource: MutableResource | None = None
    trace_endpoint: str | None = None

    def configure(
        self,
        enabled: bool,
        trace_endpoint: str | None,
        service_name: str = "semgrep-cli",
        attributes: Attributes = None,  # for adding extra attributes to the resource
    ) -> None:
        self.enabled = enabled

        self.trace_endpoint = trace_endpoint

        env_name = _ENV_ALIASES.get(
            _DEFAULT_OTEL_ENDPOINT
            if self.trace_endpoint is None
            else self.trace_endpoint
        )
        # Note that resource here is immutable, so if we want to blanket attach
        # attributes to Otel info after tracing is setup, we can't do it here.
        # Instead we have to do it in the corresponding kind of processor
        self.resource = MutableResource(
            get_aggregated_resources(
                detectors=[
                    ProcessResourceDetector(),
                    OTELResourceDetector(),
                    DockerOTELResourceDetector(),
                ],
                initial_resource=Resource(
                    attributes={
                        resources.SERVICE_NAME: service_name,
                        resources.SERVICE_VERSION: __VERSION__,
                        "deployment.environment.name": env_name if env_name else "prod",
                        **(dict(attributes) if attributes else {}),
                    },
                ),
            )
        )
        # We set up the resource even if we're disabled so things like pyro caml
        # can use it
        if not self.enabled:
            return

        tracer_provider = TracerProvider(resource=self.resource)
        logger_provider = LoggerProvider(resource=self.resource)

        set_logger_provider(logger_provider)
        otrace.set_tracer_provider(tracer_provider)

        otel_endpoint = (
            _OTEL_ENDPOINT_ALIASES.get(self.trace_endpoint, self.trace_endpoint)
            if self.trace_endpoint
            else _DEFAULT_OTEL_ENDPOINT
        )
        # See https://opentelemetry.io/docs/languages/sdk-configuration/otlp-exporter/#otel_exporter_otlp_endpoint
        # for specs on this
        exporter_spans = OTLPSpanExporter(otel_endpoint + "/v1/traces")
        exporter_logs = OTLPLogExporter(otel_endpoint + "/v1/logs")

        span_processor = BatchSpanProcessor(exporter_spans)
        log_processor = BatchLogRecordProcessor(exporter_logs)

        tracer_provider.add_span_processor(span_processor)
        logger_provider.add_log_record_processor(log_processor)
        tracer_provider.add_span_processor(span_processor)

        # add logging handler to root logger only so we can send logs to Otel and therefore datadog.
        # child loggers will propagate to root logger by default
        logging_handler = LoggingHandler(
            # COUPLING: we do something similar in Tracing.ml. If we want to
            # enable sending debug logs here we probably want to send them from
            # semgrep-core too!
            level=logging.INFO,
            logger_provider=logger_provider,
        )
        logging_handler.set_name("otel-logging-handler")
        # only add handler if it's not already present. it is possible for us to call configure multiple times
        # from the MCP. in that case, we would add the handler multiple times without this check.
        if not any(
            handler.get_name() == "otel-logging-handler"
            for handler in logging.getLogger().handlers
        ):
            logging.getLogger().addHandler(logging_handler)

        RequestsInstrumentor().instrument()
        ThreadingInstrumentor().instrument()
        self.extract()

    def extract(self) -> None:
        trace_id = os.getenv(_SEMGREP_TRACE_PARENT_TRACE_ID)
        span_id = os.getenv(_SEMGREP_TRACE_PARENT_SPAN_ID)
        if trace_id and span_id:
            trace_flags = 1
            # W3C Trace Context spec: https://www.w3.org/TR/trace-context/#version-format
            carrier = {"traceparent": f"00-{trace_id}-{span_id}-{trace_flags:02x}"}
            extracted_context = propagate.extract(carrier, context.get_current())
            context.attach(extracted_context)

    def setup_pyro_caml(self) -> None:
        if not self.resource:
            return
        # Set pyro caml related info
        os.environ[_PYRO_CAML_TAGS] = ",".join(
            [f"version={__VERSION__}"]
            + attrs_to_kv_strs(filter_attrs_for_inject(self.resource.attributes))
        )
        os.environ[_PYRO_CAML_SERVICE_NAME] = "semgrep-core"
        os.environ[_PYRO_CAML_SERVER_ADDRESS] = (
            _PYROSCOPE_ENDPOINT_ALIASES.get(self.trace_endpoint, self.trace_endpoint)
            if self.trace_endpoint
            and self.trace_endpoint in _PYROSCOPE_ENDPOINT_ALIASES.keys()
            else _DEFAULT_PYROSCOPE_ENDPOINT
        )

    def inject(self) -> None:
        if not self.resource:
            return

        # pass along resource attrs we care about
        os.environ[OTEL_RESOURCE_ATTRIBUTES] = ",".join(
            attrs_to_kv_strs(filter_attrs_for_inject(self.resource.attributes))
        )
        # Set current context info for semgrep-core
        current_context = self._get_current_context()
        os.environ[_SEMGREP_TRACE_PARENT_TRACE_ID] = otrace.format_trace_id(
            current_context.trace_id
        )
        os.environ[_SEMGREP_TRACE_PARENT_SPAN_ID] = otrace.format_span_id(
            current_context.span_id
        )

    def _get_current_context(self) -> SpanContext:
        current_span = otrace.get_current_span()
        return current_span.get_span_context()

    def get_trace_id(self) -> int:
        return self._get_current_context().trace_id

    def add_resource_attrs(self, attrs: Attributes) -> None:
        if not self.resource:
            return
        self.resource.update_attributes(attrs)


P = ParamSpec("P")
R = TypeVar("R")


# Different teams that might "own" traces, e.g. should be alerted if there are
# errors, perf regressions, etc.
class TraceOwner(Enum):
    # These are datadog alert aliases
    SAF = "team-saf"  # Semgrep Analysis Foundations
    SSC = "team-ssc"  # Semgrep Supply Chain

    # If you add a new owner here, thank you! Please remember to talk with SAF
    # about updating tracing dashboards/alerts though


def trace(
    owner: TraceOwner = TraceOwner.SAF,
) -> Callable[[Callable[P, R]], Callable[P, R]]:
    def outer(f: Callable[P, R]) -> Callable[P, R]:
        span_name = f"{f.__module__}.{f.__name__}"

        @functools.wraps(f)
        def inner(*args: P.args, **kwargs: P.kwargs) -> R:
            with TRACER.start_as_current_span(
                span_name, attributes={"span.owner": owner.value}
            ):
                return f(*args, **kwargs)

        return inner

    return outer


def get_current_span() -> otrace.Span:
    return otrace.get_current_span()
