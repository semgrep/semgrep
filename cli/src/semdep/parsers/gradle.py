from typing import List
from typing import Optional

from packaging.version import InvalidVersion
from packaging.version import Version
from parsy import any_char
from parsy import string
from parsy import success

from semdep.parsers.util import consume_line
from semdep.parsers.util import consume_word
from semdep.parsers.util import mark_line
from semdep.parsers.util import transitivity
from semdep.parsers.util import upto
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Maven
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


dep = mark_line(
    upto([":"], consume_other=True)
    >> upto([":"], consume_other=True).bind(
        lambda package: upto(["="]).bind(
            lambda version: consume_line >> success((package, version))
        )
    )
)

PREFIX = """\
# This is a Gradle generated file for dependency locking.
# Manual edits can break the build and are not advised.
# This file is expected to be part of source control.
"""

manifest_dep = upto([":"], consume_other=True) >> upto([":", "'"]).bind(
    lambda package: any_char.bind(
        lambda next: success(package)
        if next == "'"
        else success(package) << upto(["'"])
    )
)

manifest_line = (
    string("\t")
    >> consume_word
    >> string(" '")
    >> upto([":"], consume_other=True)
    >> upto([":", "'"]).bind(lambda package: success(package) << consume_line)
)

manifest = (
    any_char.until(string("dependencies {\n"), consume_other=True)
    >> manifest_line.sep_by(string("\n")).map(lambda xs: set(xs))
    << any_char.many()
)

gradle = string(PREFIX) >> (dep | (string("empty=") >> consume_line)).sep_by(
    string("\n")
).map(lambda xs: [x for x in xs if x])


def parse_gradle(
    lockfile_text: str, manifest_text: Optional[str]
) -> List[FoundDependency]:
    deps = gradle.parse(lockfile_text)
    manifest_deps = manifest.parse(manifest_text) if manifest_text else None
    output = []
    for line_number, (package, version) in deps:
        try:
            Version(version)
        except InvalidVersion:
            logger.info("No valid version found for {name}")
            continue
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
