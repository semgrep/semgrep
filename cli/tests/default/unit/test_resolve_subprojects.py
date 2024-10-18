from pathlib import Path
from typing import List

import pytest

from semdep.subproject_matchers import ExactLockfileManifestMatcher
from semdep.subproject_matchers import SubprojectMatcher
from semgrep.resolve_subprojects import find_subprojects
from semgrep.subproject import LockfileDependencySource
from semgrep.subproject import PackageManagerType
from semgrep.subproject import Subproject


@pytest.mark.quick
@pytest.mark.parametrize(
    ["file_paths", "matchers", "expected_subprojects"],
    [
        (
            # verify that when two matchers both look for the same files, only the first subproject includes the overlapping file.
            [
                Path("requirements.in"),
                Path("requirements.txt"),
                Path("requirements3.txt"),
            ],
            [
                ExactLockfileManifestMatcher(
                    lockfile_name="requirements.txt",
                    manifest_name="requirements.in",
                    package_manager_type=PackageManagerType.PIP,
                ),
                ExactLockfileManifestMatcher(
                    lockfile_name="requirements3.txt",
                    manifest_name="requirements.in",
                    package_manager_type=PackageManagerType.PIP,
                ),
            ],
            [
                Subproject(
                    root_dir=Path(),
                    dependency_source=LockfileDependencySource(
                        package_manager_type=PackageManagerType.PIP,
                        manifest_path=Path("requirements.in"),
                        lockfile_path=Path("requirements.txt"),
                    ),
                ),
                Subproject(
                    root_dir=Path(),
                    dependency_source=LockfileDependencySource(
                        package_manager_type=PackageManagerType.PIP,
                        manifest_path=None,
                        lockfile_path=Path("requirements3.txt"),
                    ),
                ),
            ],
        ),
        (
            # verify that we correctly use the second matcher when the first is a match for the manifest
            # but not for the lockfile.
            [
                Path("requirements.in"),
                Path("requirements3.txt"),
            ],
            [
                ExactLockfileManifestMatcher(
                    lockfile_name="requirements.txt",
                    manifest_name="requirements.in",
                    package_manager_type=PackageManagerType.PIP,
                ),
                ExactLockfileManifestMatcher(
                    lockfile_name="requirements3.txt",
                    manifest_name="requirements.in",
                    package_manager_type=PackageManagerType.PIP,
                ),
            ],
            [
                Subproject(
                    root_dir=Path(),
                    dependency_source=LockfileDependencySource(
                        package_manager_type=PackageManagerType.PIP,
                        manifest_path=Path("requirements.in"),
                        lockfile_path=Path("requirements3.txt"),
                    ),
                ),
            ],
        ),
    ],
)
def test_find_subprojects(
    file_paths: List[Path],
    matchers: List[SubprojectMatcher],
    expected_subprojects: List[Subproject],
) -> None:
    result = find_subprojects(frozenset(file_paths), matchers)
    assert result == expected_subprojects
