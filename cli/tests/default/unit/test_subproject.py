from pathlib import Path

import pytest

from semdep.lockfile import EcosystemLockfiles
from semdep.lockfile import Lockfile
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Fpath
from semgrep.semgrep_interfaces.semgrep_output_v1 import Maven
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity
from semgrep.semgrep_interfaces.semgrep_output_v1 import Unknown
from semgrep.subproject import _create_dependency_source
from semgrep.subproject import find_closest_subproject
from semgrep.subproject import LockfileDependencySource
from semgrep.subproject import MultiLockfileDependencySource
from semgrep.subproject import Subproject


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

        expected = Subproject(
            root_dir=Path("a/b/c"),
            dependency_source=LockfileDependencySource(
                lockfile_path=lockfile_path,
                manifest_path=Path("a/b/c/requirements.in"),
            ),
            ecosystem=Ecosystem(Pypi()),
            found_dependencies=[],
        )
        extra = [
            Subproject(
                root_dir=Path("a/b"),
                dependency_source=LockfileDependencySource(
                    lockfile_path=extra_lockfile_path,
                    manifest_path=Path("a/b/requirements.in"),
                ),
                ecosystem=Ecosystem(Pypi()),
                found_dependencies=[],
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

        expected = Subproject(
            root_dir=Path("a/b"),
            dependency_source=LockfileDependencySource(
                lockfile_path=lockfile_path,
                manifest_path=Path("a/b/build.gradle"),
            ),
            ecosystem=Ecosystem(Maven()),
            found_dependencies=[],
        )
        extra = [
            Subproject(
                root_dir=Path("a/b/c"),
                dependency_source=LockfileDependencySource(
                    lockfile_path=extra_lockfile_path,
                    manifest_path=Path("a/b/c/requirements.in"),
                ),
                ecosystem=Ecosystem(Pypi()),
                found_dependencies=[],
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
    def test_base_case(self, lockfile_path):
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

        subproject = Subproject(
            root_dir=Path("a/b/c"),
            dependency_source=LockfileDependencySource(
                lockfile_path=lockfile_path,
                manifest_path=Path("a/b/c/requirements.in"),
            ),
            ecosystem=Ecosystem(Pypi()),
            found_dependencies=found_dependencies,
        )

        assert subproject.map_lockfile_to_dependencies() == {
            str(lockfile_path): found_dependencies
        }, "Should return mapping of lockfile path to dependencies"

        assert subproject.get_lockfile_paths() == [
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
            sources=[
                LockfileDependencySource(
                    lockfile_path=lockfile_path, manifest_path=None
                ),
                LockfileDependencySource(
                    lockfile_path=extra_lockfile_path, manifest_path=None
                ),
            ]
        )

        subproject = Subproject(
            root_dir=Path("a/b/c"),
            dependency_source=multi_lockfile_source,
            ecosystem=Ecosystem(Pypi()),
            found_dependencies=found_dependencies,
        )

        lockfile_deps_map = subproject.map_lockfile_to_dependencies()
        assert lockfile_deps_map[str(lockfile_path)][0] == found_dependencies[0]
        assert lockfile_deps_map[str(extra_lockfile_path)][0] == found_dependencies[1]

        assert subproject.get_lockfile_paths() == [
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

        subproject = Subproject(
            root_dir=Path("a/b/c"),
            dependency_source=LockfileDependencySource(
                lockfile_path=lockfile_path,
                manifest_path=Path("a/b/c/requirements.in"),
            ),
            ecosystem=Ecosystem(Pypi()),
            found_dependencies=found_dependencies,
        )

        assert subproject.map_lockfile_to_dependencies() == {
            str(
                subproject.root_dir.joinpath(Path("unknown_lockfile"))
            ): found_dependencies
        }, "Should return mapping of lockfile path to dependencies"

        assert subproject.get_lockfile_paths() == [
            lockfile_path
        ], "Should return lockfile path"


class TestLockfileDependencySource:
    @pytest.mark.quick
    def test_base_case(self):
        lockfile_path = Path("a/b/c/requirements.txt")

        source = LockfileDependencySource(
            lockfile_path=lockfile_path, manifest_path=None
        )

        assert source.get_lockfile_paths() == [
            lockfile_path
        ], "Should return lockfile path"


class TestMultiLockfileDependencySource:
    @pytest.mark.quick
    def test_base_case(self):
        lockfile_path = Path("a/b/c/requirements.txt")
        extra_lockfile_path = Path("a/b/requirements/dev.txt")

        source = MultiLockfileDependencySource(
            sources=[
                LockfileDependencySource(
                    lockfile_path=lockfile_path, manifest_path=None
                ),
                LockfileDependencySource(
                    lockfile_path=extra_lockfile_path, manifest_path=None
                ),
            ]
        )

        assert source.get_lockfile_paths() == [
            lockfile_path,
            extra_lockfile_path,
        ], "Should return lockfile paths"


class TestCreateDependencySource:
    @pytest.mark.quick
    def test_single_lockfile(self):
        lockfile_path = Path("requirements.txt")
        lockfiles = [Lockfile.from_path(lockfile_path)]

        source = _create_dependency_source(lockfiles)

        assert source == LockfileDependencySource(
            lockfile_path=lockfile_path, manifest_path=None
        ), "Should return LockfileDependencySource"

    @pytest.mark.quick
    def test_multiple_lockfiles(self):
        EcosystemLockfiles.init(use_new_requirements_matchers=True)

        lockfile_path = Path("requirements.txt")
        extra_lockfile_path = Path("requirements/dev.txt")
        lockfiles = [
            Lockfile.from_path(lockfile_path),
            Lockfile.from_path(extra_lockfile_path),
        ]

        source = _create_dependency_source(lockfiles)

        assert source == MultiLockfileDependencySource(
            sources=[
                LockfileDependencySource(
                    lockfile_path=lockfile_path, manifest_path=None
                ),
                LockfileDependencySource(
                    lockfile_path=extra_lockfile_path, manifest_path=None
                ),
            ]
        ), "Should return MultiLockfileDependencySource"
