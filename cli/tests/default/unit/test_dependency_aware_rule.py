#
# Copyright (c) 2025 Semgrep Inc.
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
from collections import defaultdict

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.dependency_aware_rule import SubprojectDependencyIndex
from semgrep.semgrep_interfaces.semgrep_output_v1 import DependencyKind
from semgrep.semgrep_interfaces.semgrep_output_v1 import DependencySource
from semgrep.semgrep_interfaces.semgrep_output_v1 import Direct
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import Fpath
from semgrep.semgrep_interfaces.semgrep_output_v1 import Lockfile
from semgrep.semgrep_interfaces.semgrep_output_v1 import LockfileKind
from semgrep.semgrep_interfaces.semgrep_output_v1 import LockfileParsing
from semgrep.semgrep_interfaces.semgrep_output_v1 import Manifest
from semgrep.semgrep_interfaces.semgrep_output_v1 import ManifestKind
from semgrep.semgrep_interfaces.semgrep_output_v1 import ManifestLockfile
from semgrep.semgrep_interfaces.semgrep_output_v1 import Npm
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pipfile
from semgrep.semgrep_interfaces.semgrep_output_v1 import PipfileLock
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi
from semgrep.semgrep_interfaces.semgrep_output_v1 import ResolutionMethod
from semgrep.semgrep_interfaces.semgrep_output_v1 import ResolvedDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import ResolvedSubproject
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaPattern
from semgrep.semgrep_interfaces.semgrep_output_v1 import Subproject
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitive
from semgrep.subproject import from_resolved_dependencies


@pytest.fixture
def sample_subproject():
    """Create a subproject with multiple packages and versions for testing."""
    dependencies = [
        ResolvedDependency(
            (
                out.FoundDependency(
                    package="requests",
                    version="2.28.1",
                    ecosystem=Ecosystem(value=Pypi()),
                    allowed_hashes=defaultdict(list),
                    transitivity=DependencyKind(Direct()),
                    resolved_url=None,
                    children=None,
                    git_ref=None,
                    lockfile_path=Fpath("Pipfile.lock"),
                    line_number=10,
                ),
                None,
            )
        ),
        # Same package, different version (e.g., transitive dependency)
        ResolvedDependency(
            (
                out.FoundDependency(
                    package="requests",
                    version="2.25.0",
                    ecosystem=Ecosystem(value=Pypi()),
                    allowed_hashes=defaultdict(list),
                    transitivity=DependencyKind(Transitive()),
                    resolved_url=None,
                    children=None,
                    git_ref=None,
                    lockfile_path=Fpath("requirements.txt"),
                    line_number=5,
                ),
                None,
            )
        ),
        ResolvedDependency(
            (
                out.FoundDependency(
                    package="flask",
                    version="2.0.1",
                    ecosystem=Ecosystem(value=Pypi()),
                    allowed_hashes=defaultdict(list),
                    transitivity=DependencyKind(Direct()),
                    resolved_url=None,
                    children=None,
                    git_ref=None,
                    lockfile_path=Fpath("Pipfile.lock"),
                    line_number=15,
                ),
                None,
            )
        ),
        ResolvedDependency(
            (
                out.FoundDependency(
                    package="werkzeug",
                    version="2.0.0",
                    ecosystem=Ecosystem(value=Pypi()),
                    allowed_hashes=defaultdict(list),
                    transitivity=DependencyKind(Transitive()),
                    resolved_url=None,
                    children=None,
                    git_ref=None,
                    lockfile_path=Fpath("Pipfile.lock"),
                    line_number=20,
                ),
                None,
            )
        ),
    ]

    dependency_source = DependencySource(
        ManifestLockfile(
            (
                Manifest(ManifestKind(Pipfile()), Fpath("Pipfile")),
                Lockfile(LockfileKind(PipfileLock()), Fpath("Pipfile.lock")),
            )
        )
    )

    return ResolvedSubproject(
        info=Subproject(
            root_dir=Fpath("."),
            dependency_source=dependency_source,
            ecosystem=Ecosystem(value=Pypi()),
        ),
        errors=[],
        resolution_method=ResolutionMethod(LockfileParsing()),
        resolved_dependencies=from_resolved_dependencies(dependencies),
        ecosystem=Ecosystem(value=Pypi()),
    )


@pytest.mark.quick
def test_index_matches_specific_version_range(sample_subproject):
    """Should match newer version of requests but not older."""
    index = SubprojectDependencyIndex.from_subproject(sample_subproject)

    patterns = [
        ScaPattern(
            ecosystem=Ecosystem(value=Pypi()),
            package="requests",
            semver_range=">=2.28.0",
        )
    ]
    matches = list(index.get_dependency_matches(patterns))
    assert len(matches) == 1
    assert matches[0][1].version == "2.28.1"


@pytest.mark.quick
def test_index_matches_multiple_versions(sample_subproject):
    """Should match both versions with broader range."""
    index = SubprojectDependencyIndex.from_subproject(sample_subproject)

    patterns = [
        ScaPattern(
            ecosystem=Ecosystem(value=Pypi()),
            package="requests",
            semver_range=">=2.0.0",
        )
    ]
    matches = list(index.get_dependency_matches(patterns))
    assert len(matches) == 2
    versions = {dep.version for _, dep in matches}
    assert versions == {"2.28.1", "2.25.0"}


@pytest.mark.quick
def test_index_matches_multiple_packages(sample_subproject):
    """Should match multiple different packages."""
    index = SubprojectDependencyIndex.from_subproject(sample_subproject)

    patterns = [
        ScaPattern(
            ecosystem=Ecosystem(value=Pypi()),
            package="flask",
            semver_range=">=2.0.0",
        ),
        ScaPattern(
            ecosystem=Ecosystem(value=Pypi()),
            package="werkzeug",
            semver_range=">=2.0.0",
        ),
    ]
    matches = list(index.get_dependency_matches(patterns))
    assert len(matches) == 2
    packages = {dep.package for _, dep in matches}
    assert packages == {"flask", "werkzeug"}


@pytest.mark.quick
def test_index_no_match_nonexistent_package(sample_subproject):
    """Should not match when package doesn't exist."""
    index = SubprojectDependencyIndex.from_subproject(sample_subproject)

    patterns = [
        ScaPattern(
            ecosystem=Ecosystem(value=Pypi()),
            package="nonexistent",
            semver_range=">=1.0.0",
        )
    ]
    matches = list(index.get_dependency_matches(patterns))
    assert len(matches) == 0


@pytest.mark.quick
def test_index_no_match_version_mismatch(sample_subproject):
    """Should not match when version range excludes all versions."""
    index = SubprojectDependencyIndex.from_subproject(sample_subproject)

    patterns = [
        ScaPattern(
            ecosystem=Ecosystem(value=Pypi()),
            package="flask",
            semver_range=">=3.0.0",
        )
    ]
    matches = list(index.get_dependency_matches(patterns))
    assert len(matches) == 0


@pytest.mark.quick
def test_index_no_match_ecosystem_mismatch(sample_subproject):
    """Should not match when ecosystem is different."""
    index = SubprojectDependencyIndex.from_subproject(sample_subproject)

    patterns = [
        ScaPattern(
            ecosystem=Ecosystem(value=Npm()),
            package="requests",
            semver_range=">=2.0.0",
        )
    ]
    matches = list(index.get_dependency_matches(patterns))
    assert len(matches) == 0
