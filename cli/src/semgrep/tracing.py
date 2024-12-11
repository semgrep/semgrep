# Library to use Opentelemetry traces and logs from the python side of Semgrep
#
# Enables sending traces and logs, currently to Datadog, Jaeger, both with 15
# days retention policy
#
# Communicates with OCaml tracing defined in ../../../libs/tracing/unix/Tracing.ml
# For more info, see https://www.notion.so/semgrep/How-to-add-tracing-b0e1eaa1531e408cbb074663d1f840a6
import functools
import logging
import os
from typing import Callable
from typing import Optional
from typing import TypeVar

from attr import define
from opentelemetry import context
from opentelemetry import context as context_api
from opentelemetry import propagate
from opentelemetry import trace as otrace
from opentelemetry._logs import set_logger_provider
from opentelemetry.attributes import BoundedAttributes  # type: ignore
from opentelemetry.exporter.otlp.proto.http._log_exporter import OTLPLogExporter
from opentelemetry.exporter.otlp.proto.http.trace_exporter import OTLPSpanExporter
from opentelemetry.instrumentation.requests import RequestsInstrumentor
from opentelemetry.sdk._logs import LogData
from opentelemetry.sdk._logs import LoggerProvider
from opentelemetry.sdk._logs import LoggingHandler
from opentelemetry.sdk._logs import LogRecordProcessor
from opentelemetry.sdk._logs.export import BatchLogRecordProcessor
from opentelemetry.sdk.environment_variables import OTEL_RESOURCE_ATTRIBUTES
from opentelemetry.sdk.resources import get_aggregated_resources
from opentelemetry.sdk.resources import OTELResourceDetector
from opentelemetry.sdk.resources import ProcessResourceDetector
from opentelemetry.sdk.resources import Resource
from opentelemetry.sdk.resources import SERVICE_NAME
from opentelemetry.sdk.resources import SERVICE_VERSION
from opentelemetry.sdk.trace import Span
from opentelemetry.sdk.trace import SpanProcessor
from opentelemetry.sdk.trace import TracerProvider
from opentelemetry.sdk.trace.export import BatchSpanProcessor
from typing_extensions import ParamSpec

from semgrep import __VERSION__
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScanInfo

TRACER = otrace.get_tracer(__name__)

# Coupling: these constants need to be kept in sync with Tracing.ml

_SEMGREP_TRACE_PARENT_TRACE_ID = "SEMGREP_TRACE_PARENT_TRACE_ID"
_SEMGREP_TRACE_PARENT_SPAN_ID = "SEMGREP_TRACE_PARENT_SPAN_ID"

_DEFAULT_ENDPOINT = "https://telemetry.semgrep.dev"
_DEV_ENDPOINT = "https://telemetry.dev2.semgrep.dev"
_LOCAL_ENDPOINT = "http://localhost:4318"

_ENDPOINT_ALIASES = {
    "semgrep-prod": _DEFAULT_ENDPOINT,
    "semgrep-dev": _DEV_ENDPOINT,
    "semgrep-local": _LOCAL_ENDPOINT,
}

_ENV_ALIASES = {
    "semgrep-prod": "prod",
    "semgrep-dev": "dev2",
    "semgrep-local": "local",
}


def scan_info_to_dict(scan_info: ScanInfo) -> dict:
    info = {
        "scan.deployment_id": scan_info.deployment_id,
        "scan.deployment_name": scan_info.deployment_name,
    }
    if scan_info.id:
        info["scan.id"] = scan_info.id
    return info


# Useful for attaching scan info to trace spans
class ScanInfoSpanProcessor(SpanProcessor):
    def __init__(self: "ScanInfoSpanProcessor") -> None:
        self.scan_info: Optional[ScanInfo] = None

    # let's just set the attributes on the span when it starts
    def on_start(
        self: "ScanInfoSpanProcessor",
        span: "Span",
        parent_context: Optional[context_api.Context] = None,
    ) -> None:
        if self.scan_info:
            scan_info_dict = scan_info_to_dict(self.scan_info)
            for k, v in scan_info_dict.items():
                span.set_attribute(k, v)


# Useful for attaching scan info to log records
class ScanInfoLogProcessor(LogRecordProcessor):
    # We use a base processor here, since we only see the log record once
    # they're emitted, so we can't attach the scan info to the log record and
    # guaruntee other processors will emit before/after that happens, unlike
    # with span processors
    def __init__(
        self: "ScanInfoLogProcessor", base_processor: LogRecordProcessor
    ) -> None:
        self.base_processor: LogRecordProcessor = base_processor
        self.scan_info: Optional[ScanInfo] = None

    def emit(self: "ScanInfoLogProcessor", log_data: LogData) -> None:
        if self.scan_info:
            scan_info_dict = scan_info_to_dict(self.scan_info)
            log_record = log_data.log_record
            mut_attrs = dict(log_record.attributes)  # type: ignore
            for k, v in scan_info_dict.items():
                mut_attrs[k] = v
            # a bit hacky but the only way we can set the log_record attrs
            attrs = BoundedAttributes(attributes=mut_attrs)
            log_record.attributes = attrs

        self.base_processor.emit(log_data)

    def shutdown(self: "ScanInfoLogProcessor") -> None:
        self.base_processor.shutdown()  # type: ignore

    def force_flush(self: "ScanInfoLogProcessor", timeout_millis: int = 30000) -> bool:
        return self.base_processor.force_flush(timeout_millis)  # type: ignore


@define
class Traces:
    enabled: bool = False
    scan_info_span_processor = ScanInfoSpanProcessor()
    scan_info_log_processor: Optional[ScanInfoLogProcessor] = None

    def configure(self, enabled: bool, trace_endpoint: Optional[str]) -> None:
        self.enabled = enabled

        if not self.enabled:
            return

        env_name = _ENV_ALIASES.get(
            _DEFAULT_ENDPOINT if trace_endpoint is None else trace_endpoint
        )
        # Note that resource here is immutable, so if we want to blanket attach
        # attributes to Otel info after tracing is setup, we can't do it here.
        # Instead we have to do it in the corresponding kind of processor
        resource = get_aggregated_resources(
            detectors=[ProcessResourceDetector(), OTELResourceDetector()],  # type: ignore
            initial_resource=Resource(
                attributes={
                    SERVICE_NAME: "semgrep-cli",
                    SERVICE_VERSION: __VERSION__,
                    "deployment.environment.name": env_name if env_name else "prod",
                },
            ),
        )

        tracer_provider = TracerProvider(resource=resource)
        logger_provider = LoggerProvider(resource=resource)

        set_logger_provider(logger_provider)
        otrace.set_tracer_provider(tracer_provider)

        endpoint = (
            _ENDPOINT_ALIASES.get(trace_endpoint, trace_endpoint)
            if trace_endpoint
            else _DEFAULT_ENDPOINT
        )
        # See https://opentelemetry.io/docs/languages/sdk-configuration/otlp-exporter/#otel_exporter_otlp_endpoint
        # for specs on this
        exporter_spans = OTLPSpanExporter(endpoint + "/v1/traces")
        exporter_logs = OTLPLogExporter(endpoint + "/v1/logs")

        span_processor = BatchSpanProcessor(exporter_spans)
        log_processor = ScanInfoLogProcessor(BatchLogRecordProcessor(exporter_logs))
        self.scan_info_log_processor = log_processor

        tracer_provider.add_span_processor(span_processor)
        logger_provider.add_log_record_processor(log_processor)
        tracer_provider.add_span_processor(self.scan_info_span_processor)

        # add logging handler so we can send logs to Otel and therefore datadog
        logging_handler = LoggingHandler(
            # COUPLING: we do something similar in Tracing.ml. If we want to
            # enable sending debug logs here we probably want to send them from
            # semgrep-core too!
            level=logging.INFO,
            logger_provider=logger_provider,
        )
        logging.getLogger().addHandler(logging_handler)
        # get all existing loggers and add the handler to them, since at this
        # point we will have already set up loggers most/all places NOTE: we
        # don't set this up beforehand because we need to parse which
        # environment we're in and then set the resource attributes before we
        # can setup the logging handler
        for logger in logging.Logger.manager.loggerDict.values():
            if isinstance(logger, logging.Logger):
                logger.addHandler(logging_handler)

        RequestsInstrumentor().instrument()
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

    def inject(self) -> None:
        if not self.enabled:
            return

        # Inject relevant resource attributes for semgrep-core
        base_resource_attributes = os.environ.get(OTEL_RESOURCE_ATTRIBUTES, "")
        scan_info_dict: dict = (
            scan_info_to_dict(self.scan_info_span_processor.scan_info)
            if self.scan_info_span_processor.scan_info
            else dict()
        )
        # Let's inject info about the scan the format for these is
        # "<key>=<value>" concatenated by commas. Note that if there is a comma
        # or = in the key or value name, even if escaped, ocaml's otel sdk will
        # not parse it correctly, and just drop all of these
        scan_info_kv = [f"{k}={str(v)}" for k, v in scan_info_dict.items()]

        resource_attributes = ",".join(
            scan_info_kv
            + ([base_resource_attributes] if base_resource_attributes else [])
        )
        os.environ[OTEL_RESOURCE_ATTRIBUTES] = resource_attributes

        # Set current context info for semgrep-core
        current_span = otrace.get_current_span()
        current_context = current_span.get_span_context()
        os.environ[_SEMGREP_TRACE_PARENT_TRACE_ID] = otrace.format_trace_id(
            current_context.trace_id
        )
        os.environ[_SEMGREP_TRACE_PARENT_SPAN_ID] = otrace.format_span_id(
            current_context.span_id
        )

    def set_scan_info(self, scan_info: ScanInfo) -> None:
        self.scan_info_span_processor.scan_info = scan_info
        if self.scan_info_log_processor:
            self.scan_info_log_processor.scan_info = scan_info


P = ParamSpec("P")
R = TypeVar("R")


def trace() -> Callable[[Callable[P, R]], Callable[P, R]]:
    def outer(f: Callable[P, R]) -> Callable[P, R]:
        span_name = f"{f.__module__}.{f.__name__}"

        @functools.wraps(f)
        def inner(*args: P.args, **kwargs: P.kwargs) -> R:
            with TRACER.start_as_current_span(span_name):
                return f(*args, **kwargs)

        return inner

    return outer
