#
# Copyright (c) 2024-2025 Semgrep Inc.
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
import hashlib
from pathlib import Path
from pathlib import PurePosixPath
from typing import List

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import Maven
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi
from semgrep.semgrep_interfaces.semgrep_output_v1 import Unknown
from semgrep.subproject import ClosestSubprojectFinder
from semgrep.subproject import find_closest_resolved_subproject
from semgrep.subproject import from_resolved_dependencies
from semgrep.subproject import get_display_paths
from semgrep.subproject import make_dependencies_by_source_path
from semgrep.subproject import subproject_to_stats
from semgrep.subproject import to_stats_output
from semgrep.types import Target


def create_tmp_file(path: Path):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.touch()


class TestFindClosestSubproject:
    @pytest.mark.quick
    def test_finds_subproject_in_same_directory(
        self, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
    ):
        lockfile_path = Path("a/b/c/requirements.txt")
        create_tmp_file(tmp_path / lockfile_path)
        extra_lockfile_path = Path("a/b/requirements.txt")
        create_tmp_file(tmp_path / Path(extra_lockfile_path))

        monkeypatch.chdir(tmp_path)

        expected = out.ResolvedSubproject(
            info=out.Subproject(
                root_dir=out.Fpath("a/b/c"),
                dependency_source=out.DependencySource(
                    out.ManifestLockfile(
                        (
                            out.Manifest(
                                out.ManifestKind(out.RequirementsIn()),
                                out.Fpath("a/b/c/requirements.in"),
                            ),
                            out.Lockfile(
                                out.LockfileKind(out.PipRequirementsTxt()),
                                out.Fpath(str(lockfile_path)),
                            ),
                        )
                    ),
                ),
                ecosystem=Ecosystem(Pypi()),
            ),
            resolution_method=out.ResolutionMethod(out.LockfileParsing()),
            ecosystem=Ecosystem(Pypi()),
            resolved_dependencies=from_resolved_dependencies([]),
            errors=[],
        )
        extra = [
            out.ResolvedSubproject(
                info=out.Subproject(
                    root_dir=out.Fpath("a/b"),
                    dependency_source=out.DependencySource(
                        out.ManifestLockfile(
                            (
                                out.Manifest(
                                    out.ManifestKind(out.RequirementsIn()),
                                    out.Fpath("a/b/requirements.in"),
                                ),
                                out.Lockfile(
                                    out.LockfileKind(out.PipRequirementsTxt()),
                                    out.Fpath(str(extra_lockfile_path)),
                                ),
                            )
                        ),
                    ),
                    ecosystem=Ecosystem(Pypi()),
                ),
                resolution_method=out.ResolutionMethod(out.LockfileParsing()),
                resolved_dependencies=from_resolved_dependencies([]),
                ecosystem=Ecosystem(Pypi()),
                errors=[],
            )
        ]

        assert (
            find_closest_resolved_subproject(
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

        expected = out.ResolvedSubproject(
            info=out.Subproject(
                root_dir=out.Fpath("a/b"),
                dependency_source=out.DependencySource(
                    out.ManifestLockfile(
                        (
                            out.Manifest(
                                out.ManifestKind(out.RequirementsIn()),
                                out.Fpath("a/b/build.gradle"),
                            ),
                            out.Lockfile(
                                out.LockfileKind(out.GradleLockfile()),
                                out.Fpath(str(lockfile_path)),
                            ),
                        )
                    ),
                ),
                ecosystem=Ecosystem(Maven()),
            ),
            resolution_method=out.ResolutionMethod(out.LockfileParsing()),
            resolved_dependencies=from_resolved_dependencies([]),
            ecosystem=Ecosystem(Maven()),
            errors=[],
        )
        extra = [
            out.ResolvedSubproject(
                info=out.Subproject(
                    root_dir=out.Fpath("a/b/c"),
                    dependency_source=out.DependencySource(
                        out.ManifestLockfile(
                            (
                                out.Manifest(
                                    out.ManifestKind(out.RequirementsIn()),
                                    out.Fpath("a/b/c/requirements.in"),
                                ),
                                out.Lockfile(
                                    out.LockfileKind(out.PipRequirementsTxt()),
                                    out.Fpath(str(extra_lockfile_path)),
                                ),
                            )
                        ),
                    ),
                    ecosystem=Ecosystem(Pypi()),
                ),
                resolved_dependencies=from_resolved_dependencies([]),
                ecosystem=Ecosystem(Pypi()),
                resolution_method=out.ResolutionMethod(out.LockfileParsing()),
                errors=[],
            )
        ]

        result = find_closest_resolved_subproject(
            Path("a/b/c/app/test.java"), Ecosystem(Maven()), [expected, *extra]
        )
        assert result == expected, "Should return subproject with requested ecosystem"


class TestClosestSubprojectFinder:
    """Tests for the ClosestSubprojectFinder class."""

    def create_subproject(
        self, root_dir: str, ecosystem: Ecosystem, lockfile_path: str
    ) -> out.Subproject:
        """Helper to create a Subproject with minimal setup."""
        return out.Subproject(
            root_dir=out.Fpath(root_dir),
            dependency_source=out.DependencySource(
                out.LockfileOnly(
                    out.Lockfile(
                        out.LockfileKind(out.PipRequirementsTxt()),
                        out.Fpath(lockfile_path),
                    )
                )
            ),
            ecosystem=ecosystem,
        )

    def create_target(self, path: str) -> Target:
        """Helper to create a Target from a path string."""
        fpath = Path(path)
        ppath = PurePosixPath(path)
        return Target(
            fpath=fpath,
            ppath=ppath,
            original=out.Fppath(
                fpath=out.Fpath(str(fpath)), ppath=out.Ppath(str(ppath))
            ),
        )

    @pytest.mark.quick
    def test_finds_subproject_in_same_directory(self):
        """Should find subproject when target is in the same directory."""
        subproject = self.create_subproject(
            "a/b/c", Ecosystem(Pypi()), "a/b/c/requirements.txt"
        )
        finder = ClosestSubprojectFinder([subproject])

        target = self.create_target("a/b/c/test.py")
        result = finder.find_closest_subproject(target, Ecosystem(Pypi()))

        assert result == subproject

    @pytest.mark.quick
    def test_finds_subproject_in_parent_directory(self):
        """Should find subproject when target is in a subdirectory."""
        subproject = self.create_subproject(
            "a/b", Ecosystem(Pypi()), "a/b/requirements.txt"
        )
        finder = ClosestSubprojectFinder([subproject])

        target = self.create_target("a/b/c/d/test.py")
        result = finder.find_closest_subproject(target, Ecosystem(Pypi()))

        assert result == subproject

    @pytest.mark.quick
    def test_returns_none_when_no_matching_subproject(self):
        """Should return None when no subproject matches the path."""
        subproject = self.create_subproject(
            "a/b/c", Ecosystem(Pypi()), "a/b/c/requirements.txt"
        )
        finder = ClosestSubprojectFinder([subproject])

        # Target in a completely different directory tree
        target = self.create_target("x/y/z/test.py")
        result = finder.find_closest_subproject(target, Ecosystem(Pypi()))

        assert result is None

    @pytest.mark.quick
    def test_returns_none_when_ecosystem_does_not_match(self):
        """Should return None when ecosystem doesn't match."""
        subproject = self.create_subproject(
            "a/b/c", Ecosystem(Pypi()), "a/b/c/requirements.txt"
        )
        finder = ClosestSubprojectFinder([subproject])

        target = self.create_target("a/b/c/test.java")
        # Looking for Maven ecosystem when only Pypi is available
        result = finder.find_closest_subproject(target, Ecosystem(Maven()))

        assert result is None

    @pytest.mark.quick
    def test_returns_most_specific_subproject(self):
        """Should return the deepest (most specific) matching subproject."""
        # Create nested subprojects
        parent_subproject = self.create_subproject(
            "a/b", Ecosystem(Pypi()), "a/b/requirements.txt"
        )
        child_subproject = self.create_subproject(
            "a/b/c", Ecosystem(Pypi()), "a/b/c/requirements.txt"
        )
        finder = ClosestSubprojectFinder([parent_subproject, child_subproject])

        # Target in the child directory should match the child subproject
        target = self.create_target("a/b/c/test.py")
        result = finder.find_closest_subproject(target, Ecosystem(Pypi()))

        assert result == child_subproject

    @pytest.mark.quick
    def test_returns_parent_when_child_not_present(self):
        """Should return parent subproject when child doesn't match."""
        parent_subproject = self.create_subproject(
            "a/b", Ecosystem(Pypi()), "a/b/requirements.txt"
        )
        child_subproject = self.create_subproject(
            "a/b/c", Ecosystem(Pypi()), "a/b/c/requirements.txt"
        )
        finder = ClosestSubprojectFinder([parent_subproject, child_subproject])

        # Target in a sibling directory should match the parent
        target = self.create_target("a/b/d/test.py")
        result = finder.find_closest_subproject(target, Ecosystem(Pypi()))

        assert result == parent_subproject

    @pytest.mark.quick
    def test_handles_multiple_ecosystems(self):
        """Should correctly filter by ecosystem when multiple are present."""
        pypi_subproject = self.create_subproject(
            "a/b", Ecosystem(Pypi()), "a/b/requirements.txt"
        )
        maven_subproject = out.Subproject(
            root_dir=out.Fpath("a/b"),
            dependency_source=out.DependencySource(
                out.LockfileOnly(
                    out.Lockfile(
                        out.LockfileKind(out.GradleLockfile()),
                        out.Fpath("a/b/gradle.lockfile"),
                    )
                )
            ),
            ecosystem=Ecosystem(Maven()),
        )

        finder = ClosestSubprojectFinder([pypi_subproject, maven_subproject])

        target = self.create_target("a/b/test.java")

        # Should find Maven subproject
        result_maven = finder.find_closest_subproject(target, Ecosystem(Maven()))
        assert result_maven == maven_subproject

        # Should find Pypi subproject
        result_pypi = finder.find_closest_subproject(target, Ecosystem(Pypi()))
        assert result_pypi == pypi_subproject

    @pytest.mark.quick
    def test_handles_root_level_subproject(self):
        """Should handle subproject at the root directory."""
        subproject = self.create_subproject(
            ".", Ecosystem(Pypi()), "./requirements.txt"
        )
        finder = ClosestSubprojectFinder([subproject])

        target = self.create_target("a/b/c/test.py")
        result = finder.find_closest_subproject(target, Ecosystem(Pypi()))

        assert result == subproject

    @pytest.mark.quick
    def test_empty_subprojects_list(self):
        """Should handle empty subprojects list gracefully."""
        finder = ClosestSubprojectFinder([])

        target = self.create_target("a/b/c/test.py")
        result = finder.find_closest_subproject(target, Ecosystem(Pypi()))

        assert result is None

    @pytest.mark.quick
    def test_target_matches_exact_root_dir(self):
        """Should match when target file is exactly at the root_dir."""
        subproject = self.create_subproject(
            "a/b/c", Ecosystem(Pypi()), "a/b/c/requirements.txt"
        )
        finder = ClosestSubprojectFinder([subproject])

        # File exactly in the root_dir (not a subdirectory)
        target = self.create_target("a/b/c/test.py")
        result = finder.find_closest_subproject(target, Ecosystem(Pypi()))

        assert result == subproject


class TestSubproject:
    @pytest.mark.quick
    @pytest.mark.parametrize(
        "lockfile_path", [Path("a/b/c/requirements.txt"), Path("requirements.txt")]
    )
    def test_base_case(self, lockfile_path: Path):
        resolved_dependencies: List[out.ResolvedDependency] = [
            out.ResolvedDependency(
                (
                    out.FoundDependency(
                        package="requests",
                        version="2.26.0",
                        ecosystem=Ecosystem(Pypi()),
                        allowed_hashes={},
                        transitivity=out.DependencyKind(Unknown()),
                        lockfile_path=out.Fpath(str(lockfile_path)),
                    ),
                    None,
                )
            )
        ]

        subproject = out.ResolvedSubproject(
            info=out.Subproject(
                root_dir=out.Fpath("a/b/c"),
                dependency_source=out.DependencySource(
                    out.ManifestLockfile(
                        (
                            out.Manifest(
                                out.ManifestKind(out.RequirementsIn()),
                                out.Fpath("a/b/c/requirements.in"),
                            ),
                            out.Lockfile(
                                out.LockfileKind(out.PipRequirementsTxt()),
                                out.Fpath(str(lockfile_path)),
                            ),
                        )
                    ),
                ),
                ecosystem=Ecosystem(Pypi()),
            ),
            resolution_method=out.ResolutionMethod(out.LockfileParsing()),
            ecosystem=Ecosystem(Pypi()),
            resolved_dependencies=from_resolved_dependencies(resolved_dependencies),
            errors=[],
        )
        (
            lockfile_dep_map,
            unknown_lockfile_deps,
        ) = make_dependencies_by_source_path(subproject.resolved_dependencies)
        assert len(unknown_lockfile_deps) == 0
        assert lockfile_dep_map == {
            str(lockfile_path): [d.value[0] for d in resolved_dependencies]
        }, "Should return mapping of lockfile path to dependencies"

        assert get_display_paths(subproject.info.dependency_source) == [
            lockfile_path
        ], "Should return lockfile path"

    @pytest.mark.quick
    def test_multiple_lockfiles(self):
        lockfile_path = Path("a/b/c/requirements/base.txt")
        extra_lockfile_path = Path("a/b/requirements/dev.txt")
        resolved_dependencies: List[out.ResolvedDependency] = [
            out.ResolvedDependency(
                (
                    out.FoundDependency(
                        package="requests",
                        version="2.26.0",
                        ecosystem=Ecosystem(Pypi()),
                        allowed_hashes={},
                        transitivity=out.DependencyKind(Unknown()),
                        lockfile_path=out.Fpath(str(lockfile_path)),
                    ),
                    None,
                ),
            ),
            out.ResolvedDependency(
                (
                    out.FoundDependency(
                        package="flask",
                        version="2.0.0",
                        ecosystem=Ecosystem(Pypi()),
                        allowed_hashes={},
                        transitivity=out.DependencyKind(Unknown()),
                        lockfile_path=out.Fpath(str(extra_lockfile_path)),
                    ),
                    None,
                ),
            ),
        ]

        multi_lockfile_source = out.DependencySource(
            out.MultiLockfile(
                [
                    out.DependencySource(
                        out.LockfileOnly(
                            out.Lockfile(
                                out.LockfileKind(out.PipRequirementsTxt()),
                                out.Fpath(str(lockfile_path)),
                            )
                        )
                    ),
                    out.DependencySource(
                        out.LockfileOnly(
                            out.Lockfile(
                                out.LockfileKind(out.PipRequirementsTxt()),
                                out.Fpath(str(extra_lockfile_path)),
                            )
                        )
                    ),
                ],
            )
        )

        subproject = out.ResolvedSubproject(
            info=out.Subproject(
                root_dir=out.Fpath("a/b/c"),
                dependency_source=multi_lockfile_source,
                ecosystem=Ecosystem(Pypi()),
            ),
            ecosystem=Ecosystem(Pypi()),
            resolution_method=out.ResolutionMethod(out.LockfileParsing()),
            resolved_dependencies=from_resolved_dependencies(resolved_dependencies),
            errors=[],
        )

        (
            lockfile_deps_map,
            unknown_lockfile_deps,
        ) = make_dependencies_by_source_path(subproject.resolved_dependencies)
        assert len(unknown_lockfile_deps) == 0
        assert (
            lockfile_deps_map[str(lockfile_path)][0]
            == resolved_dependencies[0].value[0]
        )
        assert (
            lockfile_deps_map[str(extra_lockfile_path)][0]
            == resolved_dependencies[1].value[0]
        )

        assert get_display_paths(subproject.info.dependency_source) == [
            lockfile_path,
            extra_lockfile_path,
        ], "Should return lockfile paths"

    @pytest.mark.quick
    def test_dep_missing_lockfile_path(self):
        lockfile_path = Path("requirements.txt")
        resolved_dependencies: List[out.ResolvedDependency] = [
            out.ResolvedDependency(
                (
                    out.FoundDependency(
                        package="requests",
                        version="2.26.0",
                        ecosystem=Ecosystem(Pypi()),
                        allowed_hashes={},
                        transitivity=out.DependencyKind(Unknown()),
                    ),
                    None,
                ),
            )
        ]

        subproject = out.ResolvedSubproject(
            info=out.Subproject(
                root_dir=out.Fpath("a/b/c"),
                dependency_source=out.DependencySource(
                    out.ManifestLockfile(
                        (
                            out.Manifest(
                                out.ManifestKind(value=out.RequirementsIn()),
                                out.Fpath("a/b/c/requirements.in"),
                            ),
                            out.Lockfile(
                                out.LockfileKind(out.PipRequirementsTxt()),
                                out.Fpath(str(lockfile_path)),
                            ),
                        )
                    ),
                ),
                ecosystem=Ecosystem(Pypi()),
            ),
            resolution_method=out.ResolutionMethod(out.LockfileParsing()),
            ecosystem=Ecosystem(Pypi()),
            resolved_dependencies=from_resolved_dependencies(resolved_dependencies),
            errors=[],
        )

        (
            lockfile_deps_map,
            unknown_lockfile_deps,
        ) = make_dependencies_by_source_path(subproject.resolved_dependencies)
        assert len(unknown_lockfile_deps) == 1
        assert len(lockfile_deps_map) == 0

        assert get_display_paths(subproject.info.dependency_source) == [
            lockfile_path
        ], "Should return lockfile path"


class TestResolvedSubproject:
    @pytest.mark.quick
    def test_to_stats_output(self):
        lockfile_path = Path("a/b/c/requirements.txt")
        dependency_source = out.DependencySource(
            out.LockfileOnly(
                out.Lockfile(
                    out.LockfileKind(out.PipRequirementsTxt()),
                    out.Fpath(str(lockfile_path)),
                ),
            )
        )
        ecosystem = Ecosystem(Pypi())

        subproject = out.ResolvedSubproject(
            info=out.Subproject(
                root_dir=out.Fpath("a/b/c"),
                dependency_source=dependency_source,
                ecosystem=ecosystem,
            ),
            ecosystem=ecosystem,
            resolution_method=out.ResolutionMethod(out.LockfileParsing()),
            resolved_dependencies=from_resolved_dependencies([]),
            errors=[],
        )

        subproject_id = hashlib.sha256(str(lockfile_path).encode("utf-8")).hexdigest()

        assert subproject_to_stats(subproject) == out.SubprojectStats(
            subproject_id=subproject_id,
            dependency_sources=to_stats_output(dependency_source),
            resolved_stats=out.DependencyResolutionStats(
                resolution_method=out.ResolutionMethod(out.LockfileParsing()),
                dependency_count=0,
                ecosystem=ecosystem,
            ),
        )


class TestLockfileOnlyDependencySource:
    @pytest.fixture
    def lockfile_source(self):
        lockfile_path = Path("a/b/c/requirements.txt")
        return (
            lockfile_path,
            out.LockfileOnly(
                out.Lockfile(
                    out.LockfileKind(out.PipRequirementsTxt()),
                    out.Fpath(str(lockfile_path)),
                )
            ),
        )

    @pytest.mark.quick
    def test_base_case(self, lockfile_source):
        lockfile_path, source = lockfile_source
        assert get_display_paths(out.DependencySource(source)) == [
            lockfile_path
        ], "Should return lockfile path"

    @pytest.mark.quick
    def test_to_stats_output(self, lockfile_source):
        lockfile_path, source = lockfile_source

        assert to_stats_output(out.DependencySource(source)) == [
            out.DependencySourceFile(
                kind=out.DependencySourceFileKind(
                    value=out.Lockfile_(
                        value=out.LockfileKind(out.PipRequirementsTxt())
                    )
                ),
                path=out.Fpath(str(lockfile_path)),
            )
        ]


class TestMultiLockfileDependencySource:
    @pytest.fixture
    def multi_lockfile_source(self):
        lockfile_path = Path("a/b/c/requirements.txt")
        extra_lockfile_path = Path("a/b/requirements/dev.txt")

        source = out.MultiLockfile(
            [
                out.DependencySource(
                    out.LockfileOnly(
                        out.Lockfile(
                            out.LockfileKind(out.PipRequirementsTxt()),
                            out.Fpath(str(lockfile_path)),
                        )
                    )
                ),
                out.DependencySource(
                    out.LockfileOnly(
                        out.Lockfile(
                            out.LockfileKind(out.PoetryLock()),
                            out.Fpath(str(extra_lockfile_path)),
                        )
                    )
                ),
            ]
        )

        return (
            lockfile_path,
            extra_lockfile_path,
            source,
        )

    @pytest.mark.quick
    def test_base_case(self, multi_lockfile_source):
        lockfile_path, extra_lockfile_path, source = multi_lockfile_source

        assert get_display_paths(out.DependencySource(source)) == [
            lockfile_path,
            extra_lockfile_path,
        ], "Should return lockfile paths"

    @pytest.mark.quick
    def test_to_stats_output(self, multi_lockfile_source):
        lockfile_path, extra_lockfile_path, source = multi_lockfile_source

        assert to_stats_output(out.DependencySource(source)) == [
            out.DependencySourceFile(
                kind=out.DependencySourceFileKind(
                    value=out.Lockfile_(
                        value=out.LockfileKind(out.PipRequirementsTxt())
                    )
                ),
                path=out.Fpath(str(lockfile_path)),
            ),
            out.DependencySourceFile(
                kind=out.DependencySourceFileKind(
                    value=out.Lockfile_(value=out.LockfileKind(out.PoetryLock()))
                ),
                path=out.Fpath(str(extra_lockfile_path)),
            ),
        ]


class TestManifestOnlyDependencySource:
    @pytest.fixture
    def manifest_source(self):
        manifest_path = Path("a/b/c/pyproject.toml")
        return (
            manifest_path,
            out.ManifestOnly(
                out.Manifest(
                    out.ManifestKind(out.PyprojectToml()),
                    out.Fpath(str(manifest_path)),
                )
            ),
        )

    @pytest.mark.quick
    def test_base_case(self, manifest_source):
        manifest_path, source = manifest_source
        assert get_display_paths(out.DependencySource(source)) == [
            manifest_path
        ], "Should return manifest path"

    @pytest.mark.quick
    def test_to_stats_output(self, manifest_source):
        manifest_path, source = manifest_source
        assert to_stats_output(out.DependencySource(source)) == [
            out.DependencySourceFile(
                kind=out.DependencySourceFileKind(
                    value=out.Manifest_(value=out.ManifestKind(out.PyprojectToml()))
                ),
                path=out.Fpath(str(manifest_path)),
            )
        ]


class TestManifestLockfileDependencySource:
    @pytest.fixture
    def manifest_lockfile_source(self):
        manifest_path = Path("a/b/c/pyproject.toml")
        lockfile_path = Path("a/b/c/poetry.lock")
        return (
            manifest_path,
            lockfile_path,
            out.ManifestLockfile(
                (
                    out.Manifest(
                        out.ManifestKind(out.PyprojectToml()),
                        out.Fpath(str(manifest_path)),
                    ),
                    out.Lockfile(
                        out.LockfileKind(out.PoetryLock()),
                        out.Fpath(str(lockfile_path)),
                    ),
                ),
            ),
        )

    @pytest.mark.quick
    def test_base_case(self, manifest_lockfile_source):
        _, lockfile_path, source = manifest_lockfile_source
        assert get_display_paths(out.DependencySource(source)) == [
            lockfile_path
        ], "Should return lockfile path"

    @pytest.mark.quick
    def test_to_stats_output(self, manifest_lockfile_source):
        manifest_path, lockfile_path, source = manifest_lockfile_source
        assert to_stats_output(out.DependencySource(source)) == [
            out.DependencySourceFile(
                kind=out.DependencySourceFileKind(
                    value=out.Lockfile_(value=out.LockfileKind(out.PoetryLock()))
                ),
                path=out.Fpath(str(lockfile_path)),
            ),
            out.DependencySourceFile(
                kind=out.DependencySourceFileKind(
                    value=out.Manifest_(value=out.ManifestKind(out.PyprojectToml()))
                ),
                path=out.Fpath(str(manifest_path)),
            ),
        ]
