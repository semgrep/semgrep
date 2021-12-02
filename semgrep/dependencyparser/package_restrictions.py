import functools
import operator
from dataclasses import dataclass
from pathlib import Path
from typing import Callable
from typing import Dict
from typing import Generator
from typing import List
from typing import Tuple

import packaging.version
from dependencyparser.find_lockfiles import find_lockfiles
from dependencyparser.models import LockfileDependency
from dependencyparser.models import PackageManagers
from dependencyparser.parse_lockfile import parse_lockfile_str
from packaging.version import parse as parse_version

from semgrep.error import SemgrepError


# For example:
"""
rules:
- id: vulnerable-awscli-apr-2017
  pattern-either:
    - boto3.resource('s3', ...)
    - boto3.client('s3', ...)
  project-depends:
    - pip: awscli "<= 1.11.82"
  message: this version of awscli is subject to a directory traversal vulnerability in the s3 module
  languages: [python]
  severity: WARNING
"""

# step 1: find lockfile
# step 2: parse lockfile


@functools.lru_cache(maxsize=None)
def find_and_parse_lockfiles(current_dir: Path) -> Dict[Path, List[LockfileDependency]]:
    dependencies = {}
    for lockfile in find_lockfiles(current_dir):
        # print(f"parsing lockfile {lockfile}")
        dependencies[lockfile] = list(
            parse_lockfile_str(lockfile.read_text(), lockfile)
        )
    return dependencies


@dataclass(frozen=True, eq=True, order=True)
class ProjectDependsOnEntry:
    namespace: PackageManagers
    package_name: str
    semver_range: str


def semver_matches(expression: str, actual_version: str) -> bool:
    # print(f'compare {expression} {actual_version}')

    expression_operator = expression.split(" ")[0]
    expression = expression.replace(expression_operator + " ", "", 1)
    lhs = parse_version(expression)
    rhs = parse_version(actual_version)
    operator_map: Dict[
        str, Callable[[packaging.version.Version, packaging.version.Version], bool]
    ] = {
        "==": operator.eq,
        "<": operator.lt,
        "<=": operator.le,
        ">": operator.gt,
        ">=": operator.ge,
    }
    if expression not in operator_map:
        raise SemgrepError(
            f"unknown package version comparison operator: {expression_operator}"
        )
    return operator_map[expression](lhs, rhs)


# step 3: compare vulnerable range to version in lockfile
def dependencies_range_match_any(
    search_for_ranges: List[ProjectDependsOnEntry],
    lockfile_deps: Dict[Path, List[LockfileDependency]],
) -> Generator[Tuple[ProjectDependsOnEntry, LockfileDependency, Path], None, None]:
    for lockfile_path, have_deps in lockfile_deps.items():
        for have_dep in have_deps:
            for target_range in search_for_ranges:
                # print(
                #    f"comparing {target_range} <-> {have_dep.namespace} {have_dep.name} {have_dep.version}"
                # )
                if (
                    target_range.namespace == have_dep.namespace
                    and target_range.package_name == have_dep.name
                    and semver_matches(target_range.semver_range, have_dep.version)
                ):
                    yield (target_range, have_dep, lockfile_path)


# step 4: filter output

"""
def main(target_dir: Path) -> :
    lockfiles = find_and_parse_lockfiles(target_dir)
    output = dependencies_range_match_any(
        [
            ProjectDependsOnEntry(PackageManagers.PYPI, "awscli", "<= 1.1182"),
            ProjectDependsOnEntry(PackageManagers.PYPI, "colorama", "0.4.4"),
        ],
        lockfiles,
    )
    print(list(output))
"""

# main(Path("../../"))
