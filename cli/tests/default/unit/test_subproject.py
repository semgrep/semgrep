from pathlib import Path

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Fpath
from semgrep.semgrep_interfaces.semgrep_output_v1 import Maven
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity
from semgrep.semgrep_interfaces.semgrep_output_v1 import Unknown
from semgrep.subproject import find_closest_subproject
from semgrep.subproject import LockfileDependencySource
from semgrep.subproject import MultiLockfileDependencySource
from semgrep.subproject import PackageManagerType
from semgrep.subproject import ResolutionMethod
from semgrep.subproject import ResolvedDependencies
from semgrep.subproject import ResolvedSubproject


def create_tmp_file(path: Path):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.touch()


class TestFindClosestSubproject:
    @pytest.mark.quick
    def test_finds_subproject_in_same_directory(self, tmp_path, monkeypatch):
        lockfile_path = Path("a/b/c/requirements.txt")
        create_tmp_file(tmp_path / lockfile_path)
        extra_lockfile_path = Path("a/b/requirements.txt")
        create_tmp_file(tmp_path / Path(extra_lockfile_path))

        monkeypatch.chdir(tmp_path)

        expected = ResolvedSubproject(
            root_dir=Path("a/b/c"),
            resolution_errors=[],
            dependency_source=LockfileDependencySource(
                lockfile_path=lockfile_path,
                manifest=(
                    out.ManifestKind(value=out.RequirementsIn()),
                    Path("a/b/c/requirements.in"),
                ),
                package_manager_type=PackageManagerType.PIP,
            ),
            found_dependencies=ResolvedDependencies.from_found_dependencies([]),
            ecosystem=Ecosystem(Pypi()),
            resolution_method=ResolutionMethod.LOCKFILE_PARSING,
        )
        extra = [
            ResolvedSubproject(
                root_dir=Path("a/b"),
                resolution_errors=[],
                dependency_source=LockfileDependencySource(
                    lockfile_path=extra_lockfile_path,
                    manifest=(
                        out.ManifestKind(value=out.RequirementsIn()),
                        Path("a/b/requirements.in"),
                    ),
                    package_manager_type=PackageManagerType.PIP,
                ),
                found_dependencies=ResolvedDependencies.from_found_dependencies([]),
                ecosystem=Ecosystem(Pypi()),
                resolution_method=ResolutionMethod.LOCKFILE_PARSING,
            )
        ]

        assert (
            find_closest_subproject(
                Path("a/b/c/test.py"), Ecosystem(Pypi()), [*extra, expected]
            )
            == expected
        ), "Should return subproject with lockfile in same directory"

    @pytest.mark.quick
    def test_finds_subproject_for_requested_ecosystem(self, tmp_path, monkeypatch):
        lockfile_path = Path("a/b/gradle.lockfile")
        create_tmp_file(tmp_path / lockfile_path)
        extra_lockfile_path = Path("a/b/c/requirement.txt")
        create_tmp_file(tmp_path / Path(extra_lockfile_path))

        monkeypatch.chdir(tmp_path)

        expected = ResolvedSubproject(
            root_dir=Path("a/b"),
            resolution_errors=[],
            dependency_source=LockfileDependencySource(
                lockfile_path=lockfile_path,
                manifest=(
                    out.ManifestKind(value=out.BuildGradle()),
                    Path("a/b/build.gradle"),
                ),
                package_manager_type=PackageManagerType.GRADLE,
            ),
            found_dependencies=ResolvedDependencies.from_found_dependencies([]),
            ecosystem=Ecosystem(Maven()),
            resolution_method=ResolutionMethod.LOCKFILE_PARSING,
        )
        extra = [
            ResolvedSubproject(
                root_dir=Path("a/b/c"),
                resolution_errors=[],
                dependency_source=LockfileDependencySource(
                    lockfile_path=extra_lockfile_path,
                    manifest=(
                        out.ManifestKind(value=out.RequirementsIn()),
                        Path("a/b/c/requirements.in"),
                    ),
                    package_manager_type=PackageManagerType.PIP,
                ),
                found_dependencies=ResolvedDependencies.from_found_dependencies([]),
                ecosystem=Ecosystem(Pypi()),
                resolution_method=ResolutionMethod.LOCKFILE_PARSING,
            )
        ]

        result = find_closest_subproject(
            Path("a/b/c/app/test.java"), Ecosystem(Maven()), [expected, *extra]
        )
        assert result == expected, "Should return subproject with requested ecosystem"


class TestSubproject:
    @pytest.mark.quick
    @pytest.mark.parametrize(
        "lockfile_path", [Path("a/b/c/requirements.txt"), Path("requirements.txt")]
    )
    def test_base_case(self, lockfile_path: Path):
        found_dependencies = [
            FoundDependency(
                package="requests",
                version="2.26.0",
                ecosystem=Ecosystem(Pypi()),
                allowed_hashes={},
                transitivity=Transitivity(Unknown()),
                lockfile_path=Fpath(str(lockfile_path)),
            )
        ]

        subproject = ResolvedSubproject(
            root_dir=Path("a/b/c"),
            resolution_errors=[],
            dependency_source=LockfileDependencySource(
                lockfile_path=lockfile_path,
                manifest=(
                    out.ManifestKind(value=out.RequirementsIn()),
                    Path("a/b/c/requirements.in"),
                ),
                package_manager_type=PackageManagerType.PIP,
            ),
            resolution_method=ResolutionMethod.LOCKFILE_PARSING,
            ecosystem=Ecosystem(Pypi()),
            found_dependencies=ResolvedDependencies.from_found_dependencies(
                found_dependencies
            ),
        )
        (
            lockfile_dep_map,
            unknown_lockfile_deps,
        ) = subproject.found_dependencies.make_dependencies_by_source_path()
        assert len(unknown_lockfile_deps) == 0
        assert lockfile_dep_map == {
            str(lockfile_path): found_dependencies
        }, "Should return mapping of lockfile path to dependencies"

        assert subproject.dependency_source.get_display_paths() == [
            lockfile_path
        ], "Should return lockfile path"

    @pytest.mark.quick
    def test_multiple_lockfiles(self):
        lockfile_path = Path("a/b/c/requirements/base.txt")
        extra_lockfile_path = Path("a/b/requirements/dev.txt")
        found_dependencies = [
            FoundDependency(
                package="requests",
                version="2.26.0",
                ecosystem=Ecosystem(Pypi()),
                allowed_hashes={},
                transitivity=Transitivity(Unknown()),
                lockfile_path=Fpath(str(lockfile_path)),
            ),
            FoundDependency(
                package="flask",
                version="2.0.0",
                ecosystem=Ecosystem(Pypi()),
                allowed_hashes={},
                transitivity=Transitivity(Unknown()),
                lockfile_path=Fpath(str(extra_lockfile_path)),
            ),
        ]

        multi_lockfile_source = MultiLockfileDependencySource(
            sources=(
                LockfileDependencySource(
                    lockfile_path=lockfile_path,
                    manifest=(out.ManifestKind(value=out.RequirementsIn()), None),
                    package_manager_type=PackageManagerType.PIP,
                ),
                LockfileDependencySource(
                    lockfile_path=extra_lockfile_path,
                    manifest=(out.ManifestKind(value=out.RequirementsIn()), None),
                    package_manager_type=PackageManagerType.PIP,
                ),
            )
        )

        subproject = ResolvedSubproject(
            root_dir=Path("a/b/c"),
            resolution_errors=[],
            dependency_source=multi_lockfile_source,
            ecosystem=Ecosystem(Pypi()),
            resolution_method=ResolutionMethod.LOCKFILE_PARSING,
            found_dependencies=ResolvedDependencies.from_found_dependencies(
                found_dependencies
            ),
        )

        (
            lockfile_deps_map,
            unknown_lockfile_deps,
        ) = subproject.found_dependencies.make_dependencies_by_source_path()
        assert len(unknown_lockfile_deps) == 0
        assert lockfile_deps_map[str(lockfile_path)][0] == found_dependencies[0]
        assert lockfile_deps_map[str(extra_lockfile_path)][0] == found_dependencies[1]

        assert subproject.dependency_source.get_display_paths() == [
            lockfile_path,
            extra_lockfile_path,
        ], "Should return lockfile paths"

    @pytest.mark.quick
    def test_dep_missing_lockfile_path(self):
        lockfile_path = Path("requirements.txt")
        found_dependencies = [
            FoundDependency(
                package="requests",
                version="2.26.0",
                ecosystem=Ecosystem(Pypi()),
                allowed_hashes={},
                transitivity=Transitivity(Unknown()),
            )
        ]

        subproject = ResolvedSubproject(
            root_dir=Path("a/b/c"),
            resolution_errors=[],
            dependency_source=LockfileDependencySource(
                lockfile_path=lockfile_path,
                manifest=(
                    out.ManifestKind(value=out.RequirementsIn()),
                    Path("a/b/c/requirements.in"),
                ),
                package_manager_type=PackageManagerType.PIP,
            ),
            resolution_method=ResolutionMethod.LOCKFILE_PARSING,
            ecosystem=Ecosystem(Pypi()),
            found_dependencies=ResolvedDependencies.from_found_dependencies(
                found_dependencies
            ),
        )

        (
            lockfile_deps_map,
            unknown_lockfile_deps,
        ) = subproject.found_dependencies.make_dependencies_by_source_path()
        assert len(unknown_lockfile_deps) == 1
        assert len(lockfile_deps_map) == 0

        assert subproject.dependency_source.get_display_paths() == [
            lockfile_path
        ], "Should return lockfile path"


class TestLockfileDependencySource:
    @pytest.mark.quick
    def test_base_case(self):
        lockfile_path = Path("a/b/c/requirements.txt")

        source = LockfileDependencySource(
            lockfile_path=lockfile_path,
            manifest=(out.ManifestKind(value=out.RequirementsIn()), None),
            package_manager_type=PackageManagerType.PIP,
        )

        assert source.get_display_paths() == [
            lockfile_path
        ], "Should return lockfile path"


class TestMultiLockfileDependencySource:
    @pytest.mark.quick
    def test_base_case(self):
        lockfile_path = Path("a/b/c/requirements.txt")
        extra_lockfile_path = Path("a/b/requirements/dev.txt")

        source = MultiLockfileDependencySource(
            sources=(
                LockfileDependencySource(
                    lockfile_path=lockfile_path,
                    manifest=(out.ManifestKind(value=out.RequirementsIn()), None),
                    package_manager_type=PackageManagerType.PIP,
                ),
                LockfileDependencySource(
                    lockfile_path=extra_lockfile_path,
                    manifest=(out.ManifestKind(value=out.RequirementsIn()), None),
                    package_manager_type=PackageManagerType.PIP,
                ),
            )
        )

        assert source.get_display_paths() == [
            lockfile_path,
            extra_lockfile_path,
        ], "Should return lockfile paths"
