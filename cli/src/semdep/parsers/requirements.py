#
# Copyright (c) 2023-2025 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
"""
Parsers for requirements.txt and requirements.in files
Based on https://pip.pypa.io/en/stable/reference/requirements-file-format/
"""
import re
from pathlib import Path
from typing import List
from typing import NamedTuple
from typing import Optional
from typing import Set
from typing import Tuple

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.external.parsy import regex
from semdep.external.parsy import seq
from semdep.external.parsy import string
from semdep.external.parsy import string_from
from semdep.external.parsy import success
from semdep.parsers import preprocessors
from semdep.parsers.util import consume_line
from semdep.parsers.util import DependencyFileToParse
from semdep.parsers.util import DependencyParserError
from semdep.parsers.util import mark_line
from semdep.parsers.util import safe_parse_lockfile_and_manifest
from semdep.parsers.util import transitivity
from semdep.parsers.util import upto
from semgrep import telemetry
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Fpath
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaParserName

# The only PEP 440 operator that names a single installable version
EXACT_PIN_OPERATOR = "=="

# A PEP 508 project name. A leading token that is neither a name nor a local
# path, URL or archive (requests$foo) is reported as a parse error.
PEP_508_NAME_RE = re.compile(r"^[A-Za-z0-9]([A-Za-z0-9._-]*[A-Za-z0-9])?$")

# Characters that mark the leading token as a filesystem path or a URL rather
# than a project name: directory separators and the colon of a URL scheme
# (https://, git+ssh://, file:) or a Windows drive. These lines install
# something other than a registry package and are left out of the dependency
# list without an error.
PATH_OR_URL_CHARS = ("/", "\\", ":")

# A bare archive filename (pkg-1.0.tar.gz) is a local distribution, not a
# registry package, even though it matches the project name grammar.
LOCAL_ARCHIVE_SUFFIXES = (
    ".whl",
    ".zip",
    ".tar.gz",
    ".tar.bz2",
    ".tar.xz",
    ".tar.z",
    ".tgz",
    ".tbz",
    ".tar",
    ".egg",
)

# What may legally start the text after the version specifiers on a
# requirement line: an environment marker (; python_version < "3.9"),
# per-requirement options (--hash=...), or a line continuation. Anything
# else means the specifier itself did not parse.
REQUIREMENT_TAIL_PREFIXES = (";", "--", "\\")

# A requirement installed from a URL rather than a registry: pkg @ https://...
URL_REQUIREMENT_PREFIX = "@"


class VersionClause(NamedTuple):
    """One operator and version from a requirement line, e.g. (">=", "2.0")"""

    operator: str
    version: str


class Requirement(NamedTuple):
    """One parsed requirement line"""

    package: str
    clauses: List[VersionClause]
    # Whatever followed the version clauses on the line
    tail: str


# Each parser below ends in .desc("..."), a plain-English label for the piece
# of a requirement line it recognizes. The label documents the grammar and is
# what a parse error reports instead of the list of characters it tried.

# Horizontal whitespace only (not \n) to avoid crossing line boundaries,
# plus line continuations (backslash + newline) per pip spec.
whitespace = (
    (regex(r"[ \t]+") | string("\\\n"))
    .at_least(1)
    .desc("spaces, tabs, or a backslash line continuation")
)

# We define a package by its possible delimiters instead of trying to define a grammar for package names.
# The delimiters are every character that can legally follow a name: the first character of each
# version operator (including "~=" and "!="), extras, a parenthesized specifier list, markers,
# URL specs and whitespace.
package = upto("=", "<", ">", "~", "!", ";", "@", " ", "\t", "[", "(", "\n").desc(
    "a package name"
)

# Packages can have extra packages that we can tell pip to install along with them. We ignore these for now
# Examples:
# package[extra1]
# package [extra1, extra2, extra3]
extras_specifier = (string("[") >> upto("]", consume_other=True)).desc(
    "an [extras] list"
)

# An operator and a version
# Examples:
# == 1.0.0
# >= 2.3.*
# see version specifier regex here https://github.com/pypa/pip/blob/e69e265cb7b60fb2dacbbb2ab8fa3baaf24bfe4d/src/pip/_vendor/packaging/specifiers.py#LL302
version_operator = string_from("===", "==", ">=", "<=", ">", "<", "~=", "!=").desc(
    "a version operator such as == or >="
)
# ")" ends a version so a parenthesized specifier list can close.
version = upto(" ", "\t", ";", ",", "\n", ")").desc("a version such as 2.0 or 1.26.*")
# Parses to a VersionClause
version_specifier = (
    seq(version_operator, whitespace.optional() >> version)
    .combine(VersionClause)
    .desc("a version operator followed by a version, such as >=2.0")
)

# Zero or more specifiers separated by commas. Parses to a list of VersionClause
version_specifiers = version_specifier.sep_by(
    whitespace.optional() >> string(",") >> whitespace.optional()
).desc("comma-separated version specifiers")

# PEP 508 also allows the specifier list in parentheses: `requests (>=2.0)`.
# The parentheses carry no meaning, so the clauses are reported the same way.
# Tried before the bare form because the bare form matches zero specifiers.
parenthesized_version_specifiers = (
    string("(")
    >> whitespace.optional()
    >> version_specifiers
    << whitespace.optional()
    << string(")")
).desc("comma-separated version specifiers in parentheses")

# Whatever follows the specifiers on the line. The caller checks it starts
# with a marker, an option, or a continuation.
rest_of_line = upto("\n").optional(default="").desc("the rest of the line")

# Parses to a Requirement
# Examples:
# space-eqeq == 0.6.1
# eqeq-star [security] == 2.8.*, <= 2.8.1 ; python_version < "2.7"
# extras-and-two-conditions[security] == 2.8.3 ; python_version < "2.7" or sys_platform == 'darwin'
# parenthesized (>=2.0, <3)
# at-url-with-hash @ https://github.com/urllib3/urllib3/archive/refs/tags/1.26.8.zip#look-this-is-not-a-comment
dep = (
    seq(
        package,
        whitespace.optional()
        >> extras_specifier.optional()
        >> whitespace.optional()
        >> (parenthesized_version_specifiers | version_specifiers),
        rest_of_line,
    )
    .combine(Requirement)
    .desc(
        "a requirement line: name, optional [extras], version specifiers, rest of line"
    )
)

# Lines can be pip flags, we ignore them
# Examples:
# --index-url https://foo.bar
# -e file:local-file
flag_line = ((string("--") | string("-")) >> consume_line).desc(
    "a pip option line such as --index-url or -e"
)

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
COMMENT_REGEX = r"(^|\s+)#.*$"


def get_manifest_deps(
    parsed: Optional[List[Tuple[int, Requirement]]],
) -> Optional[Set[str]]:
    return {requirement.package for _, requirement in parsed} if parsed else None


def is_local_or_url_requirement(package: str) -> bool:
    """
    Whether the leading token of a requirement line is a local path
    (./dist/pkg.whl), a bare URL (https://host/pkg.whl), or a local archive
    file (pkg-1.0.tar.gz) rather than the name of a registry package.
    """
    if package.startswith("."):
        return True
    if any(char in package for char in PATH_OR_URL_CHARS):
        return True
    return package.lower().endswith(LOCAL_ARCHIVE_SUFFIXES)


def get_declared_version(constraints: List[VersionClause]) -> str:
    """
    The version string reported for one requirement line, given its
    (operator, version) clauses in source order.

    An exact `==` pin (no wildcard) is reported bare, e.g. "1.4.2", and wins
    over any other clause on the same line. Every other line is reported as
    its canonical PEP 440 specifier string with clauses joined by "," and no
    spaces, e.g. ">=2.0", "~=1.26", "==2.2.*", ">=1,<2", "===1.0", and a bare
    name is reported as "". The backend recognizes a leading operator or an
    empty string as an unpinned requirement.
    """
    for operator, version in constraints:
        if operator == EXACT_PIN_OPERATOR and "*" not in version:
            return version
    return ",".join(operator + version for operator, version in constraints)


def is_unpinned_declaration(declared_version: str) -> bool:
    """
    Whether a reported version string is an unpinned declaration: a bare name
    ("") or a specifier that starts with an operator, as `get_declared_version`
    reports every line that is not an exact pin.
    """
    return declared_version == "" or not declared_version[0].isdigit()


def get_requirement_transitivity(
    manifest_deps: Optional[Set[str]], package: str, declared_version: str
) -> out.DependencyKind:
    """
    Transitivity of one requirement line. A line declared as a range or bare
    name was written by hand, since a resolver emits exact pins, so it is a
    direct dependency whether or not a manifest (requirements.in) names it.
    An exact pin is direct when the manifest names it, transitive when the
    manifest exists and does not, and unknown without a manifest.
    """
    if is_unpinned_declaration(declared_version):
        return out.DependencyKind(out.Direct())
    return transitivity(manifest_deps, [package])


@telemetry.trace(telemetry.TraceOwner.SSC)
def parse_requirements(
    lockfile_path: Path, manifest_path: Optional[Path]
) -> Tuple[List[FoundDependency], List[DependencyParserError]]:
    parsed_lockfile, parsed_manifest, errors = safe_parse_lockfile_and_manifest(
        DependencyFileToParse(
            lockfile_path,
            requirements,
            ScaParserName(out.PRequirements()),
            preprocessors.CommentRemover(),
        ),
        DependencyFileToParse(
            manifest_path,
            requirements,
            ScaParserName(out.PRequirements()),
            preprocessors.CommentRemover(),
        )
        if manifest_path
        else None,
    )
    if parsed_lockfile is None:
        return [], errors

    manifest_deps = get_manifest_deps(parsed_manifest)
    output = []
    for line_number, requirement in parsed_lockfile:
        package = requirement.package
        tail = requirement.tail.strip()
        if is_local_or_url_requirement(package):
            continue
        if not PEP_508_NAME_RE.match(package):
            errors.append(
                DependencyParserError(
                    path=Fpath(str(lockfile_path)),
                    parser=ScaParserName(out.PRequirements()),
                    reason=f"'{package}' is not a valid package name",
                    line=line_number,
                )
            )
            continue
        if tail.startswith(URL_REQUIREMENT_PREFIX):
            continue
        if tail and not tail.startswith(REQUIREMENT_TAIL_PREFIXES):
            errors.append(
                DependencyParserError(
                    path=Fpath(str(lockfile_path)),
                    parser=ScaParserName(out.PRequirements()),
                    reason=f"Could not parse the version specifier for '{package}'",
                    line=line_number,
                )
            )
            continue
        declared_version = get_declared_version(requirement.clauses)
        output.append(
            FoundDependency(
                package=package.lower(),
                version=declared_version,
                ecosystem=Ecosystem(Pypi()),
                allowed_hashes={},
                transitivity=get_requirement_transitivity(
                    manifest_deps, package, declared_version
                ),
                line_number=line_number,
                lockfile_path=Fpath(str(lockfile_path)),
                manifest_path=Fpath(str(manifest_path)) if manifest_path else None,
            )
        )
    return output, errors
