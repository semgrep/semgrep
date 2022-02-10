import functools
from pathlib import Path
from typing import Dict
from typing import Generator
from typing import List
from typing import Tuple

import packaging.version
from attrs import frozen
from dependencyparser.find_lockfiles import find_lockfiles
from dependencyparser.models import LockfileDependency
from dependencyparser.models import PackageManagers
from dependencyparser.parse_lockfile import parse_lockfile_str
from packaging.specifiers import SpecifierSet

from semgrep.error import SemgrepError


@functools.lru_cache(maxsize=None)
def find_and_parse_lockfiles(current_dir: Path) -> Dict[Path, List[LockfileDependency]]:
    # print('looking for lockfiles...')
    dependencies = {}
    for lockfile in find_lockfiles(current_dir):
        dependencies[lockfile] = list(
            parse_lockfile_str(lockfile.read_text(), lockfile)
        )
        # print(f'lockfile: {lockfile} with # deps: {len(dependencies[lockfile])}')
    return dependencies


@frozen(eq=True, order=True)
class ProjectDependsOnEntry:
    namespace: PackageManagers
    package_name: str
    semver_range: str


def semver_matches(expression: str, actual_version: str) -> bool:

    try:
        ss = SpecifierSet(expression)
        matched = len(list(ss.filter([actual_version]))) > 0
        # print(f'does {expression} match {actual_version}?: {matched}')
        return matched
    except packaging.specifiers.InvalidSpecifier:
        raise SemgrepError(
            f"unknown package version comparison expression: {expression}"
        )


# compare vulnerable range to version in lockfile
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
                    target_range.namespace.value.lower()
                    == have_dep.namespace.value.lower()
                    and target_range.package_name == have_dep.name
                    and semver_matches(target_range.semver_range, have_dep.version)
                ):
                    yield (target_range, have_dep, lockfile_path)
