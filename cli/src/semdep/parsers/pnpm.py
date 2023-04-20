"""
Parsers for pnpm-lock.yaml files
Based on https://github.com/pnpm/spec/blob/master/lockfile/5.2.md
"""
from pathlib import Path
from typing import List
from typing import Optional

from semdep.external.parsy import regex
from semdep.external.parsy import string
from semdep.external.parsy import string_from
from semdep.parsers.util import mark_line
from semdep.parsers.util import pair
from semdep.parsers.util import safe_path_parse
from semdep.parsers.util import transitivity
from semdep.parsers.util import upto
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Npm


consume_line = regex(r"[^\n]*\n")

# Section for the direct dependencies, should look something like
# specifiers:
#     '@types/jsdom': ^1.2.3
#     jsdom: 1.2.3
#     typescript: ~1.2.5

whitespace_not_newline = regex("[^\\S\r\n]+")

package = whitespace_not_newline >> upto(":", consume_other=True)
version_specifier = upto("\n")
dep = pair(
    package, string(" ") >> string_from("~", "^").optional() >> version_specifier
)

direct_dependencies_identifier = whitespace_not_newline.optional() >> string(
    "specifiers:"
)

direct_dependencies = (
    direct_dependencies_identifier
    >> string("\n")
    >> mark_line(dep).sep_by(string("\n").at_least(1))
)


# Section for the transitive dependencies, should look something like
# packages:
#
#     /@foo/bar/1.2.3:
#         resolution: {integrity: sha512-...}
#         engines: {node: '>=1.2.3'}
#         dependencies:
#             '@fizz/buzz': 2.3.4
#
#     /min/4.5.6:
#        resolution: {integrity: sha512-...}
#        engines: {node: '>=1.2.3'}
#        dependencies:
#            'max': 2.3.4
#        transitivePeerDependencies:
#             - mathPackage

packages_identifier = whitespace_not_newline.optional() >> string("packages:")

# "/foo/1.2.3:"
# "/@foo/bar/1.2.3:"
raw_dependency = regex(" */(@.+/.+)/([^:]+)", flags=0, group=(1, 2)) | regex(
    " */(.+)/([^:]+)", flags=0, group=(1, 2)
)

# resolution: {integrity: sha512-...}
# transitivePeerDependencies:
#   - mathPackage
not_used_info = regex("( *-[^\n]*)|( *[^:\n]*:[^\n]*)").sep_by(string("\n"))

full_raw_dependency = mark_line(raw_dependency) << not_used_info

all_dependencies = (
    packages_identifier >> string("\n\n") >> full_raw_dependency.sep_by(string("\n\n"))
)

# if using pnpm workspaces, lockfile will have multiple direct dependencies
# sections. in that case, we'll capture all of them, then flatten the array
# below
direct_dependencies_data = (
    consume_line.until(direct_dependencies_identifier) >> direct_dependencies
).at_least(1)
packages_data = consume_line.until(packages_identifier) >> all_dependencies
all_dependency_data = (
    pair(direct_dependencies_data, packages_data) << consume_line.many()
)


def parse_pnpm(lockfile_path: Path, _: Optional[Path]) -> List[FoundDependency]:
    ret = safe_path_parse(lockfile_path, all_dependency_data)
    if not ret:
        return []
    direct_deps, all_deps = ret
    if not direct_deps or not all_deps:
        return []

    # direct deps is an array of arrays b/c of workspaces
    direct_deps_flattened = [wd for workspace in direct_deps for wd in workspace]

    # packages that start with @ will look like "'@foo/bar'"
    direct_deps_set = {ps.replace("'", "") for _, (ps, _) in direct_deps_flattened}
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
