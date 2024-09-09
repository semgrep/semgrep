from pathlib import Path

import pytest

from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import Maven
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi
from semgrep.subproject import find_closest_subproject
from semgrep.subproject import LockfileDependencySource
from semgrep.subproject import Subproject


@pytest.mark.quick
def test_finds_subproject_in_same_directory():
    expected = Subproject(
        root_dir=Path("/a/b/c"),
        dependency_source=LockfileDependencySource(
            lockfile_path=Path("/a/b/c/requirements.txt"),
            manifest_path=Path("/a/b/c/requirements.in"),
        ),
        ecosystem=Ecosystem(Pypi()),
        found_dependencies=[],
    )
    extra = [
        Subproject(
            root_dir=Path("/a/b"),
            dependency_source=LockfileDependencySource(
                lockfile_path=Path("/a/b/requirements.txt"),
                manifest_path=Path("/a/b/requirements.in"),
            ),
            ecosystem=Ecosystem(Pypi()),
            found_dependencies=[],
        )
    ]

    assert (
        find_closest_subproject(
            Path("/a/b/c/test.py"), Ecosystem(Pypi()), [expected, *extra]
        )
        == expected
    ), "Should return subproject with lockfile in same directory"


@pytest.mark.quick
def test_finds_subproject_for_requested_ecosystem():
    expected = Subproject(
        root_dir=Path("/a/b"),
        dependency_source=LockfileDependencySource(
            lockfile_path=Path("/a/b/gradle.lockfile"),
            manifest_path=Path("/a/b/build.gradle"),
        ),
        ecosystem=Ecosystem(Maven()),
        found_dependencies=[],
    )
    extra = [
        Subproject(
            root_dir=Path("/a/b/c"),
            dependency_source=LockfileDependencySource(
                lockfile_path=Path("/a/b/c/requirement.txt"),
                manifest_path=Path("/a/b/c/requirements.in"),
            ),
            ecosystem=Ecosystem(Pypi()),
            found_dependencies=[],
        )
    ]

    result = find_closest_subproject(
        Path("/a/b/c/app/test.java"), Ecosystem(Maven()), [expected, *extra]
    )
    assert result == expected, "Should return subproject with requested ecosystem"
