"""
Parsers for requirements.txt and requirements.in files
Based on https://pip.pypa.io/en/stable/reference/requirements-file-format/
"""
import re
from pathlib import Path
from typing import List
from typing import Optional
from typing import Set
from typing import Tuple

from semdep.external.parsy import string
from semdep.external.parsy import string_from
from semdep.external.parsy import success
from semdep.external.parsy import whitespace
from semdep.parsers.util import consume_line
from semdep.parsers.util import mark_line
from semdep.parsers.util import safe_path_parse
from semdep.parsers.util import transitivity
from semdep.parsers.util import upto
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi


whitespace = whitespace | string("\\\n")

# We define a package by its possible delimiters instead of trying to define a grammar for package names
package = upto("=", "<", ">", " ", "[", "\n")

# Packages can have extra packages that we can tell pip to install along with them. We ignore these for now
# Examples:
# package[extra1]
# package [extra1, extra2, extra3]
extras_specifier = string("[") >> upto("]", consume_other=True)

# An operator and a version. If it's not an == with a specific version, we ignore it
# Examples:
# == 1.0.0
# >= 2.3.*
# see version specifier regex here https://github.com/pypa/pip/blob/e69e265cb7b60fb2dacbbb2ab8fa3baaf24bfe4d/src/pip/_vendor/packaging/specifiers.py#LL302
version_specifier = string_from("===", "==", ">=", "<=", ">", "<", "~=", "!=").bind(
    lambda operator: whitespace.optional()
    >> upto(" ", ";", ",", "\n").bind(
        lambda version: success(
            (operator, version) if operator == "==" and "*" not in version else None
        )
    )
)

# Examples:
# space-eqeq == 0.6.1
# eqeq-star [security] == 2.8.*, <= 2.8.1 ; python_version < "2.7"
# extras-and-two-conditions[security] == 2.8.3 ; python_version < "2.7" or sys_platform == 'darwin'
# at-url-with-hash @ https://github.com/urllib3/urllib3/archive/refs/tags/1.26.8.zip#look-this-is-not-a-comment
dep = package.bind(
    lambda package: whitespace.optional()
    >> (
        (string("@") >> consume_line)
        | extras_specifier.optional()
        >> whitespace.optional()
        >> version_specifier.sep_by(string(",") >> whitespace.optional()).bind(
            lambda version_specifiers: upto("\n").optional()
            >> success((package, [x for x in version_specifiers if x]))
        )
    )
)

# Lines can be pip flags, we ignore them
# Examples:
# --index-url https://foo.bar
# -e file:local-file
flag_line = (string("--") | string("-")) >> consume_line

# We're going to preprocess the comments away
comment_line = success(None)

# A whole requirements file, can be requirements.txt or requirements.in
requirements = (
    mark_line(flag_line | dep | consume_line | comment_line)
    .sep_by(string("\n").at_least(1))
    .map(lambda xs: [(l, x) for (l, x) in xs if x])
)


# We preprocess the file to remove comments
# It's much easier to just strip them out than to weave them into parsing


# first remove comments
# https://github.com/pypa/pip/blob/e69e265cb7b60fb2dacbbb2ab8fa3baaf24bfe4d/src/pip/_internal/req/req_file.py#LL45
COMMENT_RE = re.compile(r"(^|\s+)#.*$")


def strip_comment_from_line(line: str) -> str:
    match = COMMENT_RE.search(line)
    if not match:
        return line
    else:
        return line[: match.span()[0]]


def remove_comments(s: str) -> str:
    return "\n".join(strip_comment_from_line(line) for line in s.splitlines())


def get_manifest_deps(
    parsed: Optional[List[Tuple[int, Tuple[str, List[Tuple[str, str]]]]]]
) -> Optional[Set[str]]:
    return {package for _, (package, _) in parsed} if parsed else None


def parse_requirements(
    lockfile_path: Path, manifest_path: Optional[Path]
) -> List[FoundDependency]:
    deps = safe_path_parse(lockfile_path, requirements, preprocess=remove_comments)
    if deps is None:
        return []
    manifest_deps = get_manifest_deps(safe_path_parse(manifest_path, requirements))
    output = []
    for line_number, (package, constraints) in deps:
        # A package with no pinned version, skip it
        if len(constraints) != 1:
            continue
        operator, version = constraints[0]
        # This should already be enforced by the parser but we'll be careful
        if operator != "==":
            continue
        output.append(
            FoundDependency(
                package=package,
                version=version,
                ecosystem=Ecosystem(Pypi()),
                allowed_hashes={},
                transitivity=transitivity(manifest_deps, [package]),
                line_number=line_number,
            )
        )
    return output
