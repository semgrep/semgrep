from typing import List
from typing import Optional

from parsy import string
from parsy import success

from semdep.parsers.util import mark_line
from semdep.parsers.util import not_any
from semdep.parsers.util import transitivity
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi

value = not_any(["\n", "["]).map(lambda x: x.strip('"')) | (
    string("[") >> not_any(["]"]) << string("]")
).map(lambda _: "")

key_value = not_any([" ", "\n"]).bind(
    lambda key: string(" = ") >> value.bind(lambda value: success((key, value)))
)

poetry_dep = mark_line(
    string("[[package]]\n") >> key_value.sep_by(string("\n")).map(lambda x: dict(x))
)

poetry_sub_dep = (string("[") >> not_any(["]"]) << string("]\n")) >> key_value.sep_by(
    string("\n")
).map(lambda _: None)


poetry = (poetry_dep | poetry_sub_dep).sep_by(string("\n\n")).map(
    lambda xs: [x for x in xs if x]
) << string("\n").optional()

manifest_deps = string("[tool.poetry.dependencies]\n") >> key_value.map(
    lambda x: x[0]
).sep_by(string("\n"))

manifest = (manifest_deps | poetry_sub_dep).sep_by(string("\n\n")).map(
    lambda xs: {y for x in xs if x for y in x}
) << string("\n").optional()


def parse_poetry(
    lockfile_text: str, manifest_text: Optional[str]
) -> List[FoundDependency]:
    manifest_deps = manifest.parse(manifest_text) if manifest_text else None
    deps = poetry.parse(lockfile_text)
    output = []
    for line_number, dep in deps:
        if "name" not in dep or "version" not in dep:
            continue
        output.append(
            FoundDependency(
                package=dep["name"],
                version=dep["version"],
                ecosystem=Ecosystem(Pypi()),
                allowed_hashes={},
                transitivity=transitivity(manifest_deps, [dep["name"]]),
                line_number=line_number,
            )
        )
    return output
