"""
Parser for gradle.lock and build.gradle files
Based on
https://docs.gradle.org/current/userguide/dependency_locking.html
https://docs.gradle.org/current/userguide/dependency_management_for_java_projects.html
"""
from pathlib import Path
from typing import List
from typing import Optional

from parsy import any_char
from parsy import Parser
from parsy import string
from parsy import success

from semdep.parsers.util import consume_line
from semdep.parsers.util import consume_word
from semdep.parsers.util import mark_line
from semdep.parsers.util import safe_path_parse
from semdep.parsers.util import transitivity
from semdep.parsers.util import upto
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Maven
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

# Examples:
# ch.qos.logback.contrib:logback-json-classic:0.1.5=productionRuntimeClasspath,runtimeClasspath,testRuntimeClasspath
dep = mark_line(
    upto(":", consume_other=True)
    >> upto(":", consume_other=True).bind(
        lambda package: upto("=").bind(
            lambda version: consume_line >> success((package, version))
        )
    )
)

PREFIX = """\
# This is a Gradle generated file for dependency locking.
# Manual edits can break the build and are not advised.
# This file is expected to be part of source control.
"""


# If we hit a line that isn't simple, like this:
#     implementation fileTree(dir: "libs", include: ["*.jar"])
# just ignore it
# Examples:
#   implementation "com.mx.path-core:http"
#   testImplementation "org.mockito:mockito-inline:[4.0,5.0["
manifest_line: "Parser[Optional[str]]" = (
    (string("\t") | string("  "))
    >> consume_word
    >> string(" ")
    >> any_char.bind(
        lambda next: (success(None) << consume_line)
        if next not in ['"', "'"]
        else upto(":", consume_other=True)
        >> upto(":", "'", '"').bind(lambda package: success(package) << consume_line)  # type: ignore
    )
)

# Ignore everything before and after the dependencies data
manifest = (
    any_char.until(string("dependencies {\n"), consume_other=True)
    >> (manifest_line | success(None))
    .sep_by(string("\n"), min=1)
    .map(lambda xs: {x for x in xs if x})
    << any_char.many()
)

gradle = string(PREFIX) >> (dep | (string("empty=") >> consume_line)).sep_by(
    string("\n")
).map(lambda xs: [x for x in xs if x])


def parse_gradle(
    lockfile_path: Path, manifest_path: Optional[Path]
) -> List[FoundDependency]:
    deps = safe_path_parse(lockfile_path, gradle)
    if not deps:
        return []
    manifest_deps = safe_path_parse(manifest_path, manifest)
    output = []
    for line_number, (package, version) in deps:
        output.append(
            FoundDependency(
                package=package,
                version=version,
                ecosystem=Ecosystem(Maven()),
                resolved_url=None,
                allowed_hashes={},
                transitivity=transitivity(manifest_deps, [package]),
                line_number=line_number,
            )
        )
    return output
