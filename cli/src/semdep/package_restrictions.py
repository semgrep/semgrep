from dataclasses import dataclass
from pathlib import Path
from typing import Generator
from typing import List
from typing import Tuple

import packaging.version
from packaging.specifiers import SpecifierSet

from semdep.models import LockfileDependency
from semdep.models import PackageManagers
from semgrep.error import SemgrepError


@dataclass(eq=True, order=True, frozen=True)
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
    lockfile_path: Path,
    have_deps: List[LockfileDependency],
) -> Generator[Tuple[ProjectDependsOnEntry, LockfileDependency, Path], None, None]:
    for have_dep in have_deps:
        for target_range in search_for_ranges:
            if (
                target_range.namespace.value.lower() == have_dep.namespace.value.lower()
                and target_range.package_name == have_dep.name
                and semver_matches(target_range.semver_range, have_dep.version)
            ):
                yield (target_range, have_dep, lockfile_path)
