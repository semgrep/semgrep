from pathlib import Path
from typing import List
from typing import Optional

from parsy import string
from parsy import success

from semdep.parsers.util import consume_line
from semdep.parsers.util import mark_line
from semdep.parsers.util import safe_path_parse
from semdep.parsers.util import upto
from semgrep.semgrep_interfaces.semgrep_output_v1 import Direct
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Maven
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitive
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity


dep = upto([":"], consume_other=True) >> upto([":"], consume_other=True).bind(
    lambda package: upto([":"], consume_other=True)
    >> upto([":"], consume_other=True).bind(
        lambda version: success((package, version)) << consume_line
    )
)

tree_line = mark_line(
    (
        string("|  ").times(1, max=float("inf"))
        | string("   ").times(1, max=float("inf"))
        | success("")
    ).bind(
        lambda depth: (string("+- ") | string(r"\- "))
        >> dep.map(
            lambda d: (
                Transitivity(Transitive() if len(depth) > 0 else Direct()),
                d[0],
                d[1],
            )
        )
    )
)


pom_tree = (
    consume_line
    >> string("\n")
    >> tree_line.sep_by(string("\n"))
    << string("\n").optional()
)


def parse_pom_tree(tree_path: Path, _: Optional[Path]) -> List[FoundDependency]:
    deps = safe_path_parse(tree_path, pom_tree)
    if not deps:
        return []
    output = []
    for line_number, (transitivity, package, version) in deps:
        output.append(
            FoundDependency(
                package=package,
                version=version,
                ecosystem=Ecosystem(Maven()),
                allowed_hashes={},
                transitivity=transitivity,
                line_number=line_number,
            )
        )
    return output
