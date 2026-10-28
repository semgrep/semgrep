#
# Copyright (c) 2022-2025 Semgrep Inc.
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
import re
from typing import Iterator
from typing import List
from typing import Tuple

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep import semver_specifier
from semdep.external.packaging.specifiers import InvalidSpecifier  # type: ignore
from semdep.external.packaging.specifiers import SpecifierSet  # type: ignore
from semdep.golang_version import compare_golang_specifier
from semdep.maven_version import compare_maven_specifier
from semgrep.error import SemgrepError
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

# First characters of the version string the requirements.txt parser reports
# for a requirement that is not pinned to a single release: the PEP 440
# comparators and wildcard, e.g. ">=2.0", "~=1.26", "==2.2.*". A bare name is
# reported as "". Mirrors `is_version_range` in semgrep-app, which applies the
# same shapes to pypi rows for the Dependencies page and SBOM export.
UNPINNED_VERSION_LEADING_CHARS = "<>!~=*"


def dependency_match_key(ecosystem: Ecosystem, package: str) -> str:
    """
    The key used to match a rule's package name against a dependency's.

    PyPI names are case insensitive and treat runs of "-", "_" and "." as
    equal (PEP 503), and parsers and rules don't all spell them the same way,
    e.g. a rule for "zope.interface" and a lockfile entry "zope-interface".
    """
    if ecosystem == Ecosystem(out.Pypi()):
        return re.sub(r"[-_.]+", "-", package).lower()
    return package


def is_unpinned_version(version: str) -> bool:
    """
    Whether a reported dependency version names no definitive installed
    version: an empty string or a version specifier such as ">=2.0".

    Such a dependency stays in the inventory and the SBOM. What it is not is
    matched for the purpose of generating a finding: it has no definitive
    installed version to compare against an advisory's range, so
    `is_in_range` returns False for it. Reporting it instead as "possibly
    affected" when its declared range overlaps the advisory's range is a
    product decision still open under SC-4200; this predicate is where that
    would branch.
    """
    stripped = version.strip()
    return stripped == "" or stripped[0] in UNPINNED_VERSION_LEADING_CHARS


def is_in_range(ecosystem: Ecosystem, range: str, version: str) -> bool:
    # An unpinned version is a specifier, not a release, so it never matches
    # for the purpose of generating a finding; see is_unpinned_version.
    if is_unpinned_version(version):
        return False
    if ecosystem == Ecosystem(out.Npm()):
        # This is suitable for SemVer-compliant ecosystems that implement
        # the same weird math as NPM for version comparisons. Make sure
        # to review the test cases in test_semver_specifier.py.
        res = semver_specifier.version_matches_str(range, version)
        if res is not None:
            return res
        else:
            # TODO: add a fallback?
            return False
    elif ecosystem == Ecosystem(out.Maven()):
        specifiers = [s.strip(" ") for s in range.split(",")]
        return all(compare_maven_specifier(s, version) for s in specifiers)
    elif ecosystem == Ecosystem(out.Gomod()):
        # TODO: What's the purpose of this check?
        if len(version.split("-")) < 3:
            try:
                ss = SpecifierSet(range)
                matched = len(list(ss.filter([version]))) > 0
                return matched
            except InvalidSpecifier:
                raise SemgrepError(
                    f"unknown package version comparison expression: {range}"
                )
        else:
            specifiers = [s.strip(" ") for s in range.split(",")]
            try:
                result = all(compare_golang_specifier(s, version) for s in specifiers)
            except Exception as e:
                raise SemgrepError(
                    f"bad golang module version comparison between version {version} and spec range {range}: {e}"
                )
            return result
    else:
        try:
            ss = SpecifierSet(range)
            matched = len(list(ss.filter([version]))) > 0
            return matched
        except InvalidSpecifier:
            raise SemgrepError(
                f"unknown package version comparison expression: {range}"
            )


# compare vulnerable range to version in lockfile
def dependencies_range_match_any(
    search_for_ranges: List[out.ScaPattern],
    have_deps: List[out.FoundDependency],
) -> Iterator[Tuple[out.ScaPattern, out.FoundDependency]]:
    for have_dep in have_deps:
        for target_range in search_for_ranges:
            if (
                target_range.ecosystem == have_dep.ecosystem
                and dependency_match_key(target_range.ecosystem, target_range.package)
                == dependency_match_key(have_dep.ecosystem, have_dep.package)
                and is_in_range(
                    target_range.ecosystem, target_range.semver_range, have_dep.version
                )
            ):
                yield (target_range, have_dep)
