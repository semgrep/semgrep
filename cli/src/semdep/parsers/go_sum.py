from typing import List
from typing import Optional

from parsy import string
from parsy import success

from semdep.parsers.util import mark_line
from semdep.parsers.util import upto
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Gomod
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity
from semgrep.semgrep_interfaces.semgrep_output_v1 import Unknown

# We currently ignore the +incompatible flag, pseudo versions, and the difference between a go.mod and a direct download
dep = mark_line(
    upto([" "], consume_other=True).bind(
        lambda package: string("v")
        >> upto(["/", "-", "+"]).bind(
            lambda version: upto([" "], consume_other=True)
            >> string("h1:")
            >> upto(["\n"]).bind(lambda hash: success((package, version, hash)))
        )
    )
)

go_sum = dep.sep_by(string("\n")) << (string("\n") | string("\r")).optional()


def parse_go_sum(
    lockfile_text: str, manifest_text: Optional[str]
) -> List[FoundDependency]:
    deps = go_sum.parse(lockfile_text)
    output = []
    for line_number, (package, version, hash) in deps:
        output.append(
            FoundDependency(
                package=package,
                version=version,
                ecosystem=Ecosystem(Gomod()),
                resolved_url=package,  # Go packages are already URLs
                allowed_hashes={"gomod": [hash]},
                transitivity=Transitivity(Unknown()),
                line_number=line_number,
            )
        )
    return output
