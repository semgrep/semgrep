from pathlib import Path
from typing import List
from unittest.mock import patch

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.subproject_matchers import ExactLockfileManifestMatcher
from semdep.subproject_matchers import ExactManifestOnlyMatcher
from semdep.subproject_matchers import SubprojectMatcher
from semgrep.resolve_dependency_source import resolve_dependency_source
from semgrep.resolve_subprojects import find_subprojects
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
                    lockfile_kind=out.LockfileKind(value=out.PipRequirementsTxt()),
                    manifest_kind=out.ManifestKind(value=out.RequirementsIn()),
                    ecosystem=out.Ecosystem(value=out.Pypi()),
                    make_manifest_only_subprojects=False,
                ),
                ExactLockfileManifestMatcher(
                    lockfile_name="requirements3.txt",
                    manifest_name="requirements.in",
                    lockfile_kind=out.LockfileKind(value=out.PipRequirementsTxt()),
                    manifest_kind=out.ManifestKind(value=out.RequirementsIn()),
                    ecosystem=out.Ecosystem(value=out.Pypi()),
                    make_manifest_only_subprojects=False,
                ),
            ],
            [
                Subproject(
                    root_dir=Path(),
                    dependency_source=out.DependencySource(
                        out.ManifestLockfileDependencySource(
                            (
                                out.Manifest(
                                    out.ManifestKind(value=out.RequirementsIn()),
                                    out.Fpath("requirements.in"),
                                ),
                                out.Lockfile(
                                    out.LockfileKind(out.PipRequirementsTxt()),
                                    out.Fpath("requirements.txt"),
                                ),
                            )
                        ),
                    ),
                    ecosystem=out.Ecosystem(value=out.Pypi()),
                ),
                Subproject(
                    root_dir=Path(),
                    dependency_source=out.DependencySource(
                        out.LockfileOnlyDependencySource(
                            out.Lockfile(
                                out.LockfileKind(out.PipRequirementsTxt()),
                                out.Fpath("requirements3.txt"),
                            )
                        )
                    ),
                    ecosystem=out.Ecosystem(value=out.Pypi()),
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
                    lockfile_kind=out.LockfileKind(value=out.PipRequirementsTxt()),
                    manifest_kind=out.ManifestKind(value=out.RequirementsIn()),
                    ecosystem=out.Ecosystem(value=out.Pypi()),
                    make_manifest_only_subprojects=False,
                ),
                ExactLockfileManifestMatcher(
                    lockfile_name="requirements3.txt",
                    manifest_name="requirements.in",
                    lockfile_kind=out.LockfileKind(value=out.PipRequirementsTxt()),
                    manifest_kind=out.ManifestKind(value=out.RequirementsIn()),
                    ecosystem=out.Ecosystem(value=out.Pypi()),
                    make_manifest_only_subprojects=False,
                ),
            ],
            [
                Subproject(
                    root_dir=Path(),
                    dependency_source=out.DependencySource(
                        out.ManifestLockfileDependencySource(
                            (
                                out.Manifest(
                                    out.ManifestKind(value=out.RequirementsIn()),
                                    out.Fpath("requirements.in"),
                                ),
                                out.Lockfile(
                                    out.LockfileKind(out.PipRequirementsTxt()),
                                    out.Fpath("requirements3.txt"),
                                ),
                            )
                        ),
                    ),
                    ecosystem=out.Ecosystem(value=out.Pypi()),
                ),
            ],
        ),
        (
            # verify that when one subproject contains another, both the parent and the child are found separately
            [
                Path("pom.xml"),
                Path("child-a/pom.xml"),
                Path("child-b/pom.xml"),
            ],
            [
                ExactManifestOnlyMatcher(
                    manifest_kind=out.ManifestKind(out.PomXml()),
                    manifest_name="pom.xml",
                    ecosystem=out.Ecosystem(value=out.Pypi()),
                )
            ],
            [
                Subproject(
                    root_dir=Path(),
                    dependency_source=out.DependencySource(
                        out.ManifestOnlyDependencySource(
                            out.Manifest(
                                out.ManifestKind(out.PomXml()),
                                out.Fpath("pom.xml"),
                            )
                        )
                    ),
                    ecosystem=out.Ecosystem(value=out.Pypi()),
                ),
                Subproject(
                    root_dir=Path("child-a"),
                    dependency_source=out.DependencySource(
                        out.ManifestOnlyDependencySource(
                            out.Manifest(
                                out.ManifestKind(out.PomXml()),
                                out.Fpath("child-a/pom.xml"),
                            )
                        )
                    ),
                    ecosystem=out.Ecosystem(value=out.Pypi()),
                ),
                Subproject(
                    root_dir=Path("child-b"),
                    dependency_source=out.DependencySource(
                        out.ManifestOnlyDependencySource(
                            out.Manifest(
                                out.ManifestKind(out.PomXml()),
                                out.Fpath("child-b/pom.xml"),
                            )
                        )
                    ),
                    ecosystem=out.Ecosystem(value=out.Pypi()),
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
    assert sorted(result, key=lambda s: s.root_dir) == sorted(
        expected_subprojects, key=lambda s: s.root_dir
    )


@pytest.mark.quick
@patch("semgrep.resolve_dependency_source._resolve_dependencies_rpc")
def test_ptt_unconditionally_generates_dependency_graphs(
    mock_dynamic_resolve, tmp_path: Path
) -> None:
    manifest_file = open(tmp_path / "requirements.in", "w")
    manifest_file.write("requests==2.25.1")
    manifest_file.close()
    lockfile_file = open(tmp_path / "requirements.txt", "w")
    lockfile_file.write("requests==2.25.1")
    lockfile_file.close()

    mock_dynamic_resolve.return_value = [[], [], []]
    dep_source = out.DependencySource(
        out.ManifestLockfileDependencySource(
            (
                out.Manifest(
                    out.ManifestKind(value=out.RequirementsIn()),
                    out.Fpath(str((tmp_path / "requirements.in"))),
                ),
                out.Lockfile(
                    out.LockfileKind(value=out.PipRequirementsTxt()),
                    out.Fpath(str(tmp_path / "requirements.txt")),
                ),
            )
        ),
    )

    deps, _, _ = resolve_dependency_source(dep_source, True, True)
    assert deps is not None
    assert deps[0] == out.ResolutionMethod(out.DynamicResolution())

    mock_dynamic_resolve.mock_assert_called_once_with(
        Path("requirements.txt"), out.ManifestKind(value=out.RequirementsIn())
    )


@pytest.mark.quick
@patch("semdep.parsers.requirements.parse_requirements")
@patch("semgrep.resolve_dependency_source._resolve_dependencies_rpc")
def test_ptt_unconditional_graph_generation_falls_back_on_lockfile_parsing(
    mock_dynamic_resolve, mock_parse_requirements, tmp_path: Path
) -> None:
    manifest_file = open(tmp_path / "requirements.in", "w")
    manifest_file.write("requests==2.25.1")
    manifest_file.close()
    lockfile_file = open(tmp_path / "requirements.txt", "w")
    lockfile_file.write("requests==2.25.1")
    lockfile_file.close()

    mock_dynamic_resolve.return_value = [None, [], []]
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

    dep_source = out.DependencySource(
        out.ManifestLockfileDependencySource(
            (
                out.Manifest(
                    out.ManifestKind(value=out.RequirementsIn()),
                    out.Fpath(str((tmp_path / "requirements.in"))),
                ),
                out.Lockfile(
                    out.LockfileKind(value=out.PipRequirementsTxt()),
                    out.Fpath(str(tmp_path / "requirements.txt")),
                ),
            )
        ),
    )
    deps, _, _ = resolve_dependency_source(dep_source, True, True)
    assert deps is not None
    assert deps[0] == out.ResolutionMethod(out.LockfileParsing())
    assert len(deps[1]) == 1
    assert deps[1][0][0].package == "requests"

    mock_parse_requirements.mock_assert_called_once_with(
        Path(tmp_path / "requirements.txt"), Path(tmp_path / "requirements.in")
    )
