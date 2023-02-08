"""
Parser for go.sum files
Based on https://go.dev/ref/mod#go-sum-files
"""
from pathlib import Path
from typing import List
from typing import Optional

from semdep.external.parsy import any_char
from semdep.external.parsy import string
from semdep.external.parsy import success
from semdep.parsers.util import mark_line
from semdep.parsers.util import safe_path_parse
from semdep.parsers.util import upto
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Gomod
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity
from semgrep.semgrep_interfaces.semgrep_output_v1 import Unknown

# We currently ignore the +incompatible flag, pseudo versions, and the difference between a go.mod and a direct download
# Examples:
# github.com/BurntSushi/toml v0.3.1/go.mod h1:xHWCNGjB5oqiDr8zfno3MHue2Ht5sIBksp03qcyfWMU=
# dmitri.shuralyov.com/html/belt v0.0.0-20180602232347-f7d459c86be0/go.mod h1:JLBrvjyP0v+ecvNYvCpyZgu5/xkfAUhi6wJj28eUfSU=
dep = mark_line(
    upto(" ", consume_other=True).bind(
        lambda package: string("v")
        >> upto("/", "-", "+", " ").bind(
            lambda version: any_char.bind(
                lambda next: (
                    success("") if next == " " else upto(" ", consume_other=True)
                )
                >> string("h1:")
                >> upto("\n").bind(lambda hash: success((package, version, hash)))
            )
        )
    )
)

go_sum = dep.sep_by(string("\n")) << (string("\n") | string("\r")).optional()


def parse_go_sum(
    lockfile_path: Path, manifest_path: Optional[Path]
) -> List[FoundDependency]:
    deps = safe_path_parse(lockfile_path, go_sum)
    if not deps:
        return []
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
