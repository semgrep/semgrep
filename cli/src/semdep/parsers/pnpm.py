"""
Parsers for pnpm-lock.yaml files
Based on https://github.com/pnpm/spec/blob/master/lockfile/5.2.md
"""
from pathlib import Path
from typing import List
from typing import Optional

from semdep.external.parsy import eof
from semdep.external.parsy import regex
from semdep.external.parsy import string
from semdep.external.parsy import success
from semdep.external.parsy import whitespace
from semdep.parsers.util import consume_line
from semdep.parsers.util import mark_line
from semdep.parsers.util import pair
from semdep.parsers.util import safe_path_parse
from semdep.parsers.util import transitivity
from semdep.parsers.util import upto
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Npm

"""
Section for the direct dependencies, should look something like
specifiers:
    '@types/jsdom': ^1.2.3
    jsdom: 1.2.3
    typescript: ~1.2.5
"""

whitespace_not_newline = regex("[^\\S\r\n]+")

package = whitespace_not_newline.optional() >> upto(":", consume_other=True)
version_specifier = upto("\n")
dep = package.bind(
    lambda package: whitespace_not_newline.optional()
    >> string("^").optional()
    >> string("~").optional()
    >> version_specifier.bind(lambda vs: success((package, vs)))
)

direct_dependencies_identifier = string("specifiers:")

direct_dependencies = (
    whitespace.optional()
    >> direct_dependencies_identifier
    >> whitespace.optional()
    >> mark_line(dep).sep_by(string("\n").at_least(1))
)

"""
Section for the transitive dependencies, should look something like
packages:

    /@foo/bar/1.2.3:
        resolution: {integrity: sha512-...}
        engines: {node: '>=1.2.3'}
        dependencies:
            '@fizz/buzz': 2.3.4

    /min/4.5.6:
        resolution: {integrity: sha512-...}
        engines: {node: '>=1.2.3'}
        dependencies:
            'max': 2.3.4
        transitivePeerDependencies:
            - mathPackage
"""

packages_identifier = string("packages:")

# "/foo/1.2.3:"
# "/@foo/bar/1.2.3:"
raw_dependency = whitespace.optional() >> (
    regex("/(@.+/.+)/([^:]+)", flags=0, group=(1,2))
    | regex("/(.+)/([^:]+)", flags=0, group=(1,2))
)

# resolution: {integrity: sha512-...}
# transitivePeerDependencies:
#   - mathPackage
not_used_info = whitespace.optional() >> regex(" *-[^\n]*| *[^:\n]*:[^\n]*").sep_by(
    string("\n")
)

full_raw_dependency = mark_line(raw_dependency) << not_used_info

all_dependencies = (
    whitespace.optional()
    >> packages_identifier
    >> whitespace.optional()
    >> full_raw_dependency.sep_by(string("\n\n"))
)

direct_dependencies_data = (consume_line.optional() >> string("\n").optional()).until(
    direct_dependencies_identifier
) >> direct_dependencies
packages_data = (consume_line.optional() >> string("\n").optional()).until(
    packages_identifier
) >> all_dependencies
all_dependency_data = pair(direct_dependencies_data, packages_data) << (
    consume_line.optional() >> string("\n").optional()
).until(eof)


def parse_pnpm(lockfile_path: Path, _: Optional[Path]) -> List[FoundDependency]:
    direct_deps, all_deps = safe_path_parse(lockfile_path, all_dependency_data)
    if not (direct_deps and all_deps):
        return []

    # packages that start with @ will look like "'@foo/bar'"
    direct_deps_set = {ps.replace("'", "") for _, (ps, _) in direct_deps}
    output = []
    for line_number, (package_str, version_str) in all_deps:
        if not package_str or not version_str:
            continue
        output.append(
            FoundDependency(
                package=package_str,
                version=version_str,
                ecosystem=Ecosystem(Npm()),
                transitivity=transitivity(direct_deps_set, [package_str]),
                line_number=line_number,
                allowed_hashes={},
            )
        )
    return output
