"""
Parser for go.mod files
Based on https://go.dev/ref/mod#go-mod-file
"""
from pathlib import Path
from typing import Dict
from typing import List
from typing import Optional
from typing import Tuple
from typing import TypeVar

from semdep.external.parsy import alt
from semdep.external.parsy import Parser
from semdep.external.parsy import regex
from semdep.external.parsy import string
from semdep.parsers.util import mark_line
from semdep.parsers.util import pair
from semdep.parsers.util import safe_path_parse
from semgrep.semgrep_interfaces.semgrep_output_v1 import Direct
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Gomod
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitive
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity

A = TypeVar("A")
B = TypeVar("B")


consume_line = regex(r"[^\n)]*").result(None)

comment = regex(r" *//([^\n]*)", flags=0, group=1)


def multi_spec(spec: "Parser[A]") -> "Parser[List[Tuple[A,Optional[str]]]]":
    return (
        regex(r"[ \t]*\(\n")
        >> (
            regex(r"[ \t]*") >> pair(spec, comment.optional(None)) << string("\n")
        ).many()
        << string(")")
    ) | (regex(r"[ \t]*") >> pair(spec, comment.optional()).map(lambda x: [x]))


def make_directive(
    dir: "Parser[A]", spec: "Parser[B]"
) -> "Parser[Tuple[A,List[Tuple[B,Optional[str]]]]]":
    return pair(dir, multi_spec(spec))


dep_spec = regex(r"([^ \n]+) ([^ \n]+)", flags=0, group=(1, 2))

specs: Dict[str, "Parser[Optional[Tuple[str,...]]]"] = {
    "module": consume_line,
    "go": consume_line,
    "require": dep_spec,
    "exclude": dep_spec,
    "replace": consume_line,
    "retract": consume_line,
}

directive = alt(
    *(make_directive(string(dir), mark_line(spec)) for dir, spec in specs.items())
)

go_mod = (
    directive.sep_by((comment.optional() >> string("\n")).at_least(1))
    << (comment.optional() >> string("\n")).many()
)


def parse_go_mod(
    lockfile_path: Path, manifest_path: Optional[Path]
) -> List[FoundDependency]:
    specs = safe_path_parse(lockfile_path, go_mod)
    if not specs:
        return []
    exclude = set()
    output = []
    for dir, data in specs:
        if dir == "exclude":
            for ((_, dep), _) in data:
                if dep:
                    package, version = dep
                    exclude.add((package, version))
        if dir == "require":
            for ((line_number, dep), comment) in data:
                if dep:
                    package, version = dep
                    output.append(
                        FoundDependency(
                            package=package,
                            version=version,
                            ecosystem=Ecosystem(Gomod()),
                            allowed_hashes={},
                            transitivity=Transitivity(
                                Transitive() if comment == " indirect" else Direct()
                            ),
                            line_number=line_number,
                            resolved_url=package,  # Go package names are URLs
                        )
                    )
    return [d for d in output if (d.package, d.version) not in exclude]


text1 = """\
require (
	github.com/go-chi/chi/v5 v5.0.7 // indirect
	github.com/go-chi/render v1.0.2
	github.com/lucasb-eyer/go-colorful v1.2.0
	go.opentelemetry.io/otel/sdk v1.11.1
)"""

text = """\
module github.com/nikolaydubina/go-instrument-example

go 1.18

retract (
    v1.0.0
    [v1.0.0, v1.9.9]
)

require (
	github.com/go-chi/chi/v5 v5.0.7
	github.com/go-chi/render v1.0.2
	github.com/lucasb-eyer/go-colorful v1.2.0
	go.opentelemetry.io/otel/sdk v1.11.1
)

require (
	github.com/ajg/form v1.5.1 // indirect
	github.com/cenkalti/backoff/v4 v4.1.3 // indirect
	github.com/felixge/httpsnoop v1.0.2 // indirect
	github.com/go-logr/logr v1.2.3 // indirect
	github.com/go-logr/stdr v1.2.2 // indirect
	github.com/golang/protobuf v1.5.2 // indirect
	github.com/grpc-ecosystem/grpc-gateway/v2 v2.7.0 // indirect
	go.opentelemetry.io/contrib v1.0.0 // indirect
	go.opentelemetry.io/otel/exporters/otlp/internal/retry v1.11.1 // indirect
	go.opentelemetry.io/otel/trace v1.11.1 // indirect
	go.opentelemetry.io/proto/otlp v0.19.0 // indirect
	golang.org/x/net v0.0.0-20220722155237-a158d28d115b // indirect
	golang.org/x/text v0.3.8 // indirect
	google.golang.org/genproto v0.0.0-20211118181313-81c1377c94b1 // indirect
	google.golang.org/grpc v1.50.1 // indirect
)

require (
	github.com/riandyrn/otelchi v0.5.0
	go.opentelemetry.io/otel v1.11.1
	go.opentelemetry.io/otel/exporters/otlp/otlptrace v1.11.1
	go.opentelemetry.io/otel/exporters/otlp/otlptrace/otlptracehttp v1.11.1
	golang.org/x/sys v0.2.0 // indirect
	golang.org/x/tools v0.1.12
	google.golang.org/protobuf v1.28.1 // indirect
)"""
