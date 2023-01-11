from pathlib import Path
from typing import Dict
from typing import List
from typing import Optional
from typing import Set
from typing import Tuple

from parsy import Parser
from parsy import string
from parsy import success

from semdep.parsers.util import any_str
from semdep.parsers.util import consume_line
from semdep.parsers.util import line_number
from semdep.parsers.util import not_any
from semdep.parsers.util import safe_path_parse
from semdep.parsers.util import transitivity
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi


comment = string(" ").many() >> string("#") >> not_any(["\n"])
newline = comment.optional() >> string("\n")

extra_info = not_any(["\\\\", "\n"])

package = not_any(["=", "<", ">", " ", "\n"])
version = not_any([";", " ", "\n"])


def dep(sep: "Parser[str]") -> "Parser[Tuple[str,str]]":
    return package.bind(
        lambda package: sep
        >> version.bind(
            lambda version: extra_info.optional() >> success((package, version))
        )
    )


manifest_dep = dep(any_str(["==", "<=", ">=", ">", "<"])) | package.map(
    lambda x: (x, "")
)

manifest = (
    (manifest_dep << newline.many()).map(lambda t: t[0]).many().map(lambda x: set(x))
)

hash = string("    --hash=sha256:") >> not_any([" ", "\n"])

hashes = hash.sep_by(string(" \\") >> newline.many())

empty_hashes: Dict[str, List[str]] = {}


def dep_hashes(
    manifest_deps: Optional[Set[str]],
) -> "Parser[Optional[FoundDependency]]":
    return (
        dep(string("==")).bind(
            lambda dep: line_number.bind(
                lambda line_number: string("\\").optional()
                >> newline
                >> hashes.map(lambda hashes: {"sha256": hashes})
                .optional(default=empty_hashes)
                .bind(
                    lambda hashes: success(
                        FoundDependency(
                            package=dep[0],
                            version=dep[1],
                            ecosystem=Ecosystem(Pypi()),
                            allowed_hashes=hashes,
                            transitivity=transitivity(manifest_deps, [dep[0]]),
                            line_number=line_number,
                        )
                    )
                )
            )
        )
        | consume_line
    )


def requirements(
    manifest_deps: Optional[Set[str]],
) -> "Parser[List[FoundDependency]]":
    return (
        (dep_hashes(manifest_deps) << newline.many())
        .many()
        .map(lambda xs: [x for x in xs if x])
    )


def parse_requirements(
    lockfile_path: Path, manifest_path: Optional[Path]
) -> List[FoundDependency]:
    manifest_deps = safe_path_parse(manifest_path, manifest)

    output = safe_path_parse(lockfile_path, requirements(manifest_deps))
    return output if output else []
