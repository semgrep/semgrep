from pathlib import Path
from typing import List
from unittest.mock import patch

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.subproject_matchers import ExactLockfileManifestMatcher
from semdep.subproject_matchers import SubprojectMatcher
from semgrep.resolve_subprojects import _resolve_dependency_source
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
                    manifest_kind=out.ManifestKind(value=out.RequirementsIn()),
                ),
                ExactLockfileManifestMatcher(
                    lockfile_name="requirements3.txt",
                    manifest_name="requirements.in",
                    package_manager_type=PackageManagerType.PIP,
                    manifest_kind=out.ManifestKind(value=out.RequirementsIn()),
                ),
            ],
            [
                Subproject(
                    root_dir=Path(),
                    dependency_source=LockfileDependencySource(
                        package_manager_type=PackageManagerType.PIP,
                        manifest=(
                            out.ManifestKind(value=out.RequirementsIn()),
                            Path("requirements.in"),
                        ),
                        lockfile_path=Path("requirements.txt"),
                    ),
                ),
                Subproject(
                    root_dir=Path(),
                    dependency_source=LockfileDependencySource(
                        package_manager_type=PackageManagerType.PIP,
                        manifest=(out.ManifestKind(value=out.RequirementsIn()), None),
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
                    manifest_kind=out.ManifestKind(value=out.RequirementsIn()),
                ),
                ExactLockfileManifestMatcher(
                    lockfile_name="requirements3.txt",
                    manifest_name="requirements.in",
                    package_manager_type=PackageManagerType.PIP,
                    manifest_kind=out.ManifestKind(value=out.RequirementsIn()),
                ),
            ],
            [
                Subproject(
                    root_dir=Path(),
                    dependency_source=LockfileDependencySource(
                        package_manager_type=PackageManagerType.PIP,
                        manifest=(
                            out.ManifestKind(value=out.RequirementsIn()),
                            Path("requirements.in"),
                        ),
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


@pytest.mark.quick
@patch("semgrep.resolve_subprojects._resolve_dependencies_dynamically")
def test_ptt_unconditionally_generates_dependency_graphs(
    mock_dynamic_resolve, tmp_path
) -> None:
    manifest_file = open(tmp_path / "requirements.in", "w")
    manifest_file.write("requests==2.25.1")
    manifest_file.close()
    lockfile_file = open(tmp_path / "requirements.txt", "w")
    lockfile_file.write("requests==2.25.1")
    lockfile_file.close()

    mock_dynamic_resolve.return_value = ["mock_ecosystem", [], [], []]
    dep_source = LockfileDependencySource(
        package_manager_type=PackageManagerType.PIP,
        manifest=(
            out.ManifestKind(value=out.RequirementsIn()),
            Path(tmp_path / "requirements.in"),
        ),
        lockfile_path=Path(tmp_path / "requirements.txt"),
    )
    _resolve_dependency_source(dep_source, True, True)

    mock_dynamic_resolve.mock_assert_called_once_with(
        Path("requirements.txt"), out.ManifestKind(value=out.RequirementsIn())
    )


@pytest.mark.quick
@patch("semgrep.resolve_subprojects._resolve_dependencies_dynamically")
@patch("semdep.parsers.requirements.parse_requirements")
def test_ptt_unconditional_graph_generation_falls_back_on_lockfile_parsing(
    mock_dynamic_resolve, mock_parse_requirements, tmp_path
) -> None:
    manifest_file = open(tmp_path / "requirements.in", "w")
    manifest_file.write("requests==2.25.1")
    manifest_file.close()
    lockfile_file = open(tmp_path / "requirements.txt", "w")
    lockfile_file.write("requests==2.25.1")
    lockfile_file.close()

    mock_dynamic_resolve.return_value = [None, [], [], []]
    mock_parse_requirements.return_value = (
        [
            out.FoundDependency(
                package="requests",
                version="2.25.1",
                ecosystem=out.Ecosystem(value=out.Pypi()),
                allowed_hashes={},
                transitivity=out.Transitivity(value=out.Direct()),
            )
        ],
        [],
    )
    dep_source = LockfileDependencySource(
        package_manager_type=PackageManagerType.PIP,
        manifest=(
            out.ManifestKind(value=out.RequirementsIn()),
            Path(tmp_path / "requirements.in"),
        ),
        lockfile_path=Path(tmp_path / "requirements.txt"),
    )
    _resolve_dependency_source(dep_source, True, True)

    mock_parse_requirements.mock_assert_called_once_with(
        Path(tmp_path / "requirements.txt"), Path(tmp_path / "requirements.in")
    )
