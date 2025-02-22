from pathlib import Path
from typing import List

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.matchers.base import ExactLockfileManifestMatcher
from semdep.matchers.base import PatternManifestStaticLockfileMatcher
from semdep.matchers.gradle import GradleMatcher
from semdep.matchers.pip_requirements import PipRequirementsMatcher
from semdep.subproject_matchers import filter_dependency_source_files


class TestExactLockfileMatcher:
    @pytest.mark.quick
    def test_lockfile_match(self):
        matcher = ExactLockfileManifestMatcher(
            lockfile_name="Pipfile.lock",
            manifest_name="Pipfile",
            manifest_kind=out.ManifestKind(value=out.Pipfile()),
            lockfile_kind=out.LockfileKind(value=out.PipfileLock()),
            ecosystem=out.Ecosystem(value=out.Pypi()),
            make_manifest_only_subprojects=False,
        )

        assert matcher.is_match(Path("Pipfile.lock")) is True
        assert matcher.is_match(Path("a/b/c/Pipfile.lock")) is True
        assert matcher.is_match(Path("requirements.txt")) is False

    @pytest.mark.quick
    def test_manifest_match(self):
        matcher = ExactLockfileManifestMatcher(
            lockfile_name="Pipfile.lock",
            manifest_name="Pipfile",
            manifest_kind=out.ManifestKind(value=out.Pipfile()),
            lockfile_kind=out.LockfileKind(value=out.PipfileLock()),
            ecosystem=out.Ecosystem(value=out.Pypi()),
            make_manifest_only_subprojects=False,
        )

        assert matcher.is_match(Path("Pipfile")) is True
        assert matcher.is_match(Path("a/b/c/Pipfile")) is True
        assert matcher.is_match(Path("requirements.txt")) is False

    @pytest.mark.quick
    @pytest.mark.parametrize("create_manifest", [True, False])
    def test_make_subprojects(
        self, tmp_path: Path, monkeypatch: pytest.MonkeyPatch, create_manifest: bool
    ):
        manifest_path = tmp_path / "Pipfile"
        lockfile_path = tmp_path / "Pipfile.lock"

        monkeypatch.chdir(tmp_path)

        matcher = ExactLockfileManifestMatcher(
            lockfile_name="Pipfile.lock",
            manifest_name="Pipfile",
            manifest_kind=out.ManifestKind(value=out.Pipfile()),
            lockfile_kind=out.LockfileKind(value=out.PipfileLock()),
            ecosystem=out.Ecosystem(value=out.Pypi()),
            make_manifest_only_subprojects=False,
        )

        assert matcher.is_match(manifest_path)
        assert matcher.is_match(lockfile_path)
        files = frozenset(
            [lockfile_path] + ([manifest_path] if create_manifest else [])
        )
        subprojects, used_files = matcher.make_subprojects(files)
        assert used_files == files
        assert len(subprojects) == 1
        subproject = subprojects[0]
        assert Path(subproject.root_dir.value) == tmp_path
        dep_src = subproject.dependency_source.value

        if create_manifest:
            assert isinstance(dep_src, out.ManifestLockfileDependencySource)
            subproject_manifest_path = dep_src.value[0].path
            assert (
                subproject_manifest_path is not None
                and Path(subproject_manifest_path.value) == manifest_path
            )
            assert dep_src.value[1].path.value == str(lockfile_path)
        else:
            assert isinstance(dep_src, out.LockfileOnlyDependencySource)
            assert dep_src.value.path.value == str(lockfile_path)


class TestPatternManifestStaticLockfileMatcher:
    @pytest.mark.quick
    def test_is_match(self):
        matcher = PatternManifestStaticLockfileMatcher(
            lockfile_name="packages.lock.json",
            manifest_pattern="*.csproj",
            lockfile_kind=out.LockfileKind(out.NugetPackagesLockJson()),
            manifest_kind=out.ManifestKind(value=out.NugetManifestJson()),
            ecosystem=out.Ecosystem(value=out.Nuget()),
            make_manifest_only_subprojects=False,
        )

        assert matcher.is_match(Path("packages.lock.json")) is True
        assert matcher.is_match(Path("test_proj.csproj")) is True
        assert matcher.is_match(Path("a/b/c/packages.lock.json")) is True
        assert matcher.is_match(Path("a/b/c/test_proj.csproj")) is True
        assert matcher.is_match(Path("unknown.lock")) is False
        assert matcher.is_match(Path("a/b/c/requirements/hello/unknown.lock")) is False

    @pytest.mark.quick
    @pytest.mark.parametrize("with_manifest", [True, False])
    def test_make_subprojects(
        self,
        with_manifest: bool,
    ):
        # with two lockfiles and two corresponding manifests
        # map lockfile to root directory and manifest path
        test_data = {
            Path("packages.lock.json"): (Path(""), Path("test_proj.csproj")),
            Path("a/b/c/packages.lock.json"): (
                Path("a/b/c"),
                Path("a/b/c/test_proj.csproj"),
            ),
        }

        # and a pattern matcher
        matcher = PatternManifestStaticLockfileMatcher(
            lockfile_name="packages.lock.json",
            manifest_pattern="*.csproj",
            manifest_kind=out.ManifestKind(value=out.NugetManifestJson()),
            lockfile_kind=out.LockfileKind(value=out.NugetPackagesLockJson()),
            ecosystem=out.Ecosystem(value=out.Nuget()),
            make_manifest_only_subprojects=False,
        )

        # expect the matcher to create four subprojects, with or without manifests
        manifests = (
            [manifest for _lockfile, (_root_dir, manifest) in test_data.items()]
            if with_manifest
            else []
        )
        lockfiles = [lockfile for lockfile in test_data]
        files = frozenset(manifests + lockfiles)
        subprojects, used_files = matcher.make_subprojects(files)
        assert used_files == files
        assert len(subprojects) == 2
        for subproject in subprojects:
            dep_src = subproject.dependency_source.value
            assert isinstance(
                dep_src,
                (
                    out.ManifestLockfileDependencySource,
                    out.LockfileOnlyDependencySource,
                ),
            )
            lockfile = (
                dep_src.value
                if isinstance(dep_src, out.LockfileOnlyDependencySource)
                else dep_src.value[1]
            )
            expected_root, expected_manifest = test_data[Path(lockfile.path.value)]
            assert Path(subproject.root_dir.value) == expected_root
            if with_manifest:
                assert isinstance(dep_src, out.ManifestLockfileDependencySource)
                manifest_path = dep_src.value[0].path
                assert (
                    manifest_path is not None
                    and Path(manifest_path.value) == expected_manifest
                )
            else:
                assert isinstance(dep_src, out.LockfileOnlyDependencySource)

    @pytest.mark.quick
    def test_make_manifest_only_subprojects(self):
        matcher = PatternManifestStaticLockfileMatcher(
            lockfile_name="packages.lock.json",
            manifest_pattern="*.csproj",
            manifest_kind=out.ManifestKind(value=out.NugetManifestJson()),
            lockfile_kind=out.LockfileKind(value=out.NugetPackagesLockJson()),
            ecosystem=out.Ecosystem(value=out.Nuget()),
            make_manifest_only_subprojects=True,
        )

        files = frozenset([Path("test_proj.csproj")])
        subprojects, used_files = matcher.make_subprojects(files)
        assert used_files == files
        assert len(subprojects) == 1
        subproject = subprojects[0]
        assert isinstance(
            subproject.dependency_source.value, out.ManifestOnlyDependencySource
        )


class TestRequirementsLockfileMatcher:
    @pytest.mark.quick
    def test_is_match(self) -> None:
        matcher = PipRequirementsMatcher(
            base_file_pattern="*requirement*",
            requirements_file_extensions=["txt", "pip"],
            manifest_file_extension="in",
            default_manifest_file_base="requirements",
        )

        # Basic cases
        assert matcher.is_match(Path("requirements.txt")) is True
        assert matcher.is_match(Path("requirements3.txt")) is True
        assert matcher.is_match(Path("a/b/c/requirements.txt")) is True
        assert matcher.is_match(Path("a/b/c/requirements3.txt")) is True
        assert matcher.is_match(Path("requirements.pip")) is True
        assert matcher.is_match(Path("requirement.pip")) is True
        assert matcher.is_match(Path("a/b/c/requirements.pip")) is True

        # Non-standard requirements cases
        assert matcher.is_match(Path("requirements.dev.txt")) is True
        assert matcher.is_match(Path("requirements.dev3.txt")) is True
        assert matcher.is_match(Path("requirements-prod.txt")) is True
        assert matcher.is_match(Path("requirements-dev.txt")) is True
        assert matcher.is_match(Path("dev-requirements.txt")) is True
        assert matcher.is_match(Path("dev-requirements3.txt")) is True
        assert matcher.is_match(Path("prod-requirements.txt")) is True
        assert matcher.is_match(Path("requirements_lock.txt")) is True

        # Requirement folder cases
        assert matcher.is_match(Path("requirements/prod.txt")) is True
        assert matcher.is_match(Path("a/b/c/requirements/base.txt")) is True
        assert matcher.is_match(Path("a/b/c/requirements/hello/dev.txt")) is True

        # Manifest cases
        assert matcher.is_match(Path("a/b/c/requirements.in")) is True
        assert matcher.is_match(Path("a/b/c/requirements-prod.in")) is True
        assert matcher.is_match(Path("a/b/requirements/prod.in")) is True

        # Non-requirements cases
        assert matcher.is_match(Path("unknown.lock")) is False
        assert matcher.is_match(Path("a/b/c/requirements/hello/unknown.lock")) is False

    @pytest.mark.quick
    @pytest.mark.parametrize(
        ["source_files", "expected_subprojects"],
        [
            (
                [
                    Path("requirements.txt"),
                    Path("requirements.in"),
                    Path("a/b/c/requirements.txt"),
                    Path("a/b/c/requirements.in"),
                ],
                [
                    out.Subproject(
                        root_dir=out.Fpath("."),
                        dependency_source=out.DependencySource(
                            out.ManifestLockfileDependencySource(
                                (
                                    out.Manifest(
                                        out.ManifestKind(out.RequirementsIn()),
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
                    out.Subproject(
                        root_dir=out.Fpath("a/b/c"),
                        dependency_source=out.DependencySource(
                            out.ManifestLockfileDependencySource(
                                (
                                    out.Manifest(
                                        out.ManifestKind(out.RequirementsIn()),
                                        out.Fpath("a/b/c/requirements.in"),
                                    ),
                                    out.Lockfile(
                                        out.LockfileKind(out.PipRequirementsTxt()),
                                        out.Fpath("a/b/c/requirements.txt"),
                                    ),
                                )
                            ),
                        ),
                        ecosystem=out.Ecosystem(value=out.Pypi()),
                    ),
                ],
            ),
            (
                [
                    Path("requirements3.txt"),
                    Path("requirements3.in"),
                    Path("a/b/c/requirements3.txt"),
                    Path("a/b/c/requirements.in"),
                ],
                [
                    out.Subproject(
                        root_dir=out.Fpath("."),
                        dependency_source=out.DependencySource(
                            out.ManifestLockfileDependencySource(
                                (
                                    out.Manifest(
                                        out.ManifestKind(out.RequirementsIn()),
                                        out.Fpath("requirements3.in"),
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
                    out.Subproject(
                        root_dir=out.Fpath("a/b/c"),
                        dependency_source=out.DependencySource(
                            out.ManifestLockfileDependencySource(
                                (
                                    out.Manifest(
                                        out.ManifestKind(out.RequirementsIn()),
                                        out.Fpath("a/b/c/requirements.in"),
                                    ),
                                    out.Lockfile(
                                        out.LockfileKind(out.PipRequirementsTxt()),
                                        out.Fpath("a/b/c/requirements3.txt"),
                                    ),
                                )
                            ),
                        ),
                        ecosystem=out.Ecosystem(value=out.Pypi()),
                    ),
                ],
            ),
            (
                [
                    Path("requirements-dev.txt"),
                    Path("requirements.in"),
                    Path("requirements-prod.txt"),
                ],
                [
                    out.Subproject(
                        root_dir=out.Fpath("."),
                        dependency_source=out.DependencySource(
                            out.MultiLockfileDependencySource(
                                [
                                    out.DependencySource(
                                        out.ManifestLockfileDependencySource(
                                            (
                                                out.Manifest(
                                                    out.ManifestKind(
                                                        out.RequirementsIn()
                                                    ),
                                                    out.Fpath("requirements.in"),
                                                ),
                                                out.Lockfile(
                                                    out.LockfileKind(
                                                        out.PipRequirementsTxt()
                                                    ),
                                                    out.Fpath("requirements-dev.txt"),
                                                ),
                                            )
                                        ),
                                    ),
                                    out.DependencySource(
                                        out.ManifestLockfileDependencySource(
                                            (
                                                out.Manifest(
                                                    out.ManifestKind(
                                                        out.RequirementsIn()
                                                    ),
                                                    out.Fpath("requirements.in"),
                                                ),
                                                out.Lockfile(
                                                    out.LockfileKind(
                                                        out.PipRequirementsTxt()
                                                    ),
                                                    out.Fpath("requirements-prod.txt"),
                                                ),
                                            )
                                        ),
                                    ),
                                ]
                            )
                        ),
                        ecosystem=out.Ecosystem(value=out.Pypi()),
                    )
                ],
            ),
            (
                [
                    Path("dev-requirements.txt"),
                    Path("requirements.in"),
                    Path("prod-requirements.txt"),
                ],
                [
                    out.Subproject(
                        root_dir=out.Fpath("."),
                        dependency_source=out.DependencySource(
                            out.MultiLockfileDependencySource(
                                [
                                    out.DependencySource(
                                        out.ManifestLockfileDependencySource(
                                            (
                                                out.Manifest(
                                                    out.ManifestKind(
                                                        out.RequirementsIn()
                                                    ),
                                                    out.Fpath("requirements.in"),
                                                ),
                                                out.Lockfile(
                                                    out.LockfileKind(
                                                        out.PipRequirementsTxt()
                                                    ),
                                                    out.Fpath("dev-requirements.txt"),
                                                ),
                                            )
                                        ),
                                    ),
                                    out.DependencySource(
                                        out.ManifestLockfileDependencySource(
                                            (
                                                out.Manifest(
                                                    out.ManifestKind(
                                                        out.RequirementsIn()
                                                    ),
                                                    out.Fpath("requirements.in"),
                                                ),
                                                out.Lockfile(
                                                    out.LockfileKind(
                                                        out.PipRequirementsTxt()
                                                    ),
                                                    out.Fpath("prod-requirements.txt"),
                                                ),
                                            )
                                        ),
                                    ),
                                ]
                            )
                        ),
                        ecosystem=out.Ecosystem(value=out.Pypi()),
                    )
                ],
            ),
            (
                [Path("requirements_lock.txt"), Path("requirements.in")],
                [
                    out.Subproject(
                        root_dir=out.Fpath("."),
                        dependency_source=out.DependencySource(
                            out.ManifestLockfileDependencySource(
                                (
                                    out.Manifest(
                                        out.ManifestKind(out.RequirementsIn()),
                                        out.Fpath("requirements.in"),
                                    ),
                                    out.Lockfile(
                                        out.LockfileKind(out.PipRequirementsTxt()),
                                        out.Fpath("requirements_lock.txt"),
                                    ),
                                )
                            ),
                        ),
                        ecosystem=out.Ecosystem(value=out.Pypi()),
                    )
                ],
            ),
            (
                [
                    Path("requirements/dev.txt"),
                    Path("requirements.in"),
                    Path("requirements/prod.txt"),
                ],
                [
                    out.Subproject(
                        root_dir=out.Fpath("."),
                        dependency_source=out.DependencySource(
                            out.MultiLockfileDependencySource(
                                [
                                    out.DependencySource(
                                        out.ManifestLockfileDependencySource(
                                            (
                                                out.Manifest(
                                                    out.ManifestKind(
                                                        out.RequirementsIn()
                                                    ),
                                                    out.Fpath("requirements.in"),
                                                ),
                                                out.Lockfile(
                                                    out.LockfileKind(
                                                        out.PipRequirementsTxt()
                                                    ),
                                                    out.Fpath("requirements/dev.txt"),
                                                ),
                                            )
                                        ),
                                    ),
                                    out.DependencySource(
                                        out.ManifestLockfileDependencySource(
                                            (
                                                out.Manifest(
                                                    out.ManifestKind(
                                                        out.RequirementsIn()
                                                    ),
                                                    out.Fpath("requirements.in"),
                                                ),
                                                out.Lockfile(
                                                    out.LockfileKind(
                                                        out.PipRequirementsTxt()
                                                    ),
                                                    out.Fpath("requirements/prod.txt"),
                                                ),
                                            )
                                        ),
                                    ),
                                ]
                            )
                        ),
                        ecosystem=out.Ecosystem(value=out.Pypi()),
                    )
                ],
            ),
            (
                [
                    Path("requirements/dev.txt"),
                    Path("requirements.in"),
                    Path("requirements/prod.txt"),
                    Path("requirements/prod.in"),
                ],
                [
                    out.Subproject(
                        root_dir=out.Fpath("."),
                        dependency_source=out.DependencySource(
                            out.MultiLockfileDependencySource(
                                [
                                    out.DependencySource(
                                        out.ManifestLockfileDependencySource(
                                            (
                                                out.Manifest(
                                                    out.ManifestKind(
                                                        out.RequirementsIn()
                                                    ),
                                                    out.Fpath("requirements.in"),
                                                ),
                                                out.Lockfile(
                                                    out.LockfileKind(
                                                        out.PipRequirementsTxt()
                                                    ),
                                                    out.Fpath("requirements/dev.txt"),
                                                ),
                                            )
                                        ),
                                    ),
                                    out.DependencySource(
                                        out.ManifestLockfileDependencySource(
                                            (
                                                out.Manifest(
                                                    out.ManifestKind(
                                                        out.RequirementsIn()
                                                    ),
                                                    out.Fpath("requirements/prod.in"),
                                                ),
                                                out.Lockfile(
                                                    out.LockfileKind(
                                                        out.PipRequirementsTxt()
                                                    ),
                                                    out.Fpath("requirements/prod.txt"),
                                                ),
                                            )
                                        ),
                                    ),
                                ]
                            )
                        ),
                        ecosystem=out.Ecosystem(value=out.Pypi()),
                    )
                ],
            ),
            (
                [
                    Path("requirements/dev.txt"),
                    Path("requirements/prod.txt"),
                ],
                [
                    out.Subproject(
                        root_dir=out.Fpath("."),
                        dependency_source=out.DependencySource(
                            out.MultiLockfileDependencySource(
                                [
                                    out.DependencySource(
                                        out.LockfileOnlyDependencySource(
                                            out.Lockfile(
                                                out.LockfileKind(
                                                    out.PipRequirementsTxt()
                                                ),
                                                out.Fpath("requirements/dev.txt"),
                                            )
                                        )
                                    ),
                                    out.DependencySource(
                                        out.LockfileOnlyDependencySource(
                                            out.Lockfile(
                                                out.LockfileKind(
                                                    out.PipRequirementsTxt()
                                                ),
                                                out.Fpath("requirements/prod.txt"),
                                            )
                                        )
                                    ),
                                ]
                            )
                        ),
                        ecosystem=out.Ecosystem(value=out.Pypi()),
                    )
                ],
            ),
        ],
    )
    @pytest.mark.quick
    def test_make_subprojects(
        self, source_files: List[Path], expected_subprojects: List[out.Subproject]
    ):
        # with a basic requirements matcher
        matcher = PipRequirementsMatcher(
            base_file_pattern="*requirements*",
            requirements_file_extensions=["txt", "pip"],
            manifest_file_extension="in",
            default_manifest_file_base="requirements",
        )

        source_files_set = frozenset(source_files)

        # when we make subprojects from the provided source files
        subprojects, used_files = matcher.make_subprojects(source_files_set)

        # expect all files to be used
        assert used_files == source_files_set

        assert len(subprojects) == len(expected_subprojects)
        assert subprojects.sort() == expected_subprojects.sort()


class TestGradleMatcher:
    @pytest.mark.quick
    @pytest.mark.parametrize(
        ["path", "expected_match"],
        [
            (Path("gradle.lockfile"), True),
            (Path("build.gradle"), True),
            (Path("settings.gradle"), True),
            (Path("java/settings.gradle"), True),
            (Path("java/gradle.lockfile"), True),
            (Path("java/build.gradle"), True),
            (Path("java/subproject_a/build.gradle"), True),
            (Path("requirements.txt"), False),
            (Path("a/b/c/requirements.txt"), False),
            (Path("unknown.lock"), False),
            (Path("a/b/c/requirements/hello/unknown.lock"), False),
        ],
    )
    def test_is_match(self, path: Path, expected_match: bool) -> None:
        assert GradleMatcher().is_match(path) == expected_match

    @pytest.mark.quick
    @pytest.mark.parametrize(
        ["source_files", "expected_subprojects", "unused_files"],
        [
            (
                # this test case mimics the structure of micronaut-core:
                # one large multi-project
                # one separate single-build project at the same level as the multi-project's subprojects
                [
                    Path("build.gradle"),
                    Path("settings.gradle"),
                    Path("buildSrc/build.gradle"),
                    Path("buildSrc/settings.gradle"),
                    Path("subdir_a/build.gradle"),
                    Path("subdir_b/build.gradle"),
                ],
                [
                    out.Subproject(
                        root_dir=out.Fpath("."),
                        dependency_source=out.DependencySource(
                            out.ManifestOnlyDependencySource(
                                out.Manifest(
                                    out.ManifestKind(out.BuildGradle()),
                                    out.Fpath("build.gradle"),
                                )
                            )
                        ),
                        ecosystem=out.Ecosystem(value=out.Maven()),
                    ),
                    out.Subproject(
                        root_dir=out.Fpath("buildSrc"),
                        dependency_source=out.DependencySource(
                            out.ManifestOnlyDependencySource(
                                out.Manifest(
                                    out.ManifestKind(out.BuildGradle()),
                                    out.Fpath("buildSrc/build.gradle"),
                                )
                            )
                        ),
                        ecosystem=out.Ecosystem(value=out.Maven()),
                    ),
                ],
                [
                    Path("subdir_a/build.gradle"),
                    Path("subdir_b/build.gradle"),
                ],
            ),
            (
                # a variant with a lockfile at the top level
                [
                    Path("gradle.lockfile"),
                    Path("build.gradle"),
                    Path("settings.gradle"),
                    Path("buildSrc/build.gradle"),
                    Path("buildSrc/settings.gradle"),
                    Path("subdir_a/build.gradle"),
                    Path("subdir_b/build.gradle"),
                ],
                [
                    out.Subproject(
                        root_dir=out.Fpath("."),
                        dependency_source=out.DependencySource(
                            out.ManifestLockfileDependencySource(
                                (
                                    out.Manifest(
                                        kind=out.ManifestKind(value=out.BuildGradle()),
                                        path=out.Fpath("build.gradle"),
                                    ),
                                    out.Lockfile(
                                        out.LockfileKind(out.GradleLockfile()),
                                        out.Fpath("gradle.lockfile"),
                                    ),
                                )
                            ),
                        ),
                        ecosystem=out.Ecosystem(value=out.Maven()),
                    ),
                    out.Subproject(
                        root_dir=out.Fpath("buildSrc"),
                        dependency_source=out.DependencySource(
                            out.ManifestOnlyDependencySource(
                                out.Manifest(
                                    kind=out.ManifestKind(value=out.BuildGradle()),
                                    path=out.Fpath("buildSrc/build.gradle"),
                                )
                            ),
                        ),
                        ecosystem=out.Ecosystem(value=out.Maven()),
                    ),
                ],
                [
                    Path("subdir_a/build.gradle"),
                    Path("subdir_b/build.gradle"),
                ],
            ),
            (
                # a variant with no lockfile at the top level, but lockfile for one of the children
                [
                    Path("build.gradle"),
                    Path("settings.gradle"),
                    Path("buildSrc/build.gradle"),
                    Path("buildSrc/gradle.lockfile"),
                    Path("subdir_a/build.gradle"),
                    Path("subdir_b/build.gradle"),
                ],
                [
                    out.Subproject(
                        root_dir=out.Fpath("."),
                        dependency_source=out.DependencySource(
                            out.ManifestOnlyDependencySource(
                                out.Manifest(
                                    kind=out.ManifestKind(value=out.BuildGradle()),
                                    path=out.Fpath("build.gradle"),
                                )
                            )
                        ),
                        ecosystem=out.Ecosystem(value=out.Maven()),
                    ),
                    out.Subproject(
                        root_dir=out.Fpath("buildSrc"),
                        dependency_source=out.DependencySource(
                            out.ManifestLockfileDependencySource(
                                (
                                    out.Manifest(
                                        kind=out.ManifestKind(value=out.BuildGradle()),
                                        path=out.Fpath("buildSrc/build.gradle"),
                                    ),
                                    out.Lockfile(
                                        kind=out.LockfileKind(out.GradleLockfile()),
                                        path=out.Fpath("buildSrc/gradle.lockfile"),
                                    ),
                                )
                            ),
                        ),
                        ecosystem=out.Ecosystem(value=out.Maven()),
                    ),
                ],
                [
                    Path("subdir_a/build.gradle"),
                    Path("subdir_b/build.gradle"),
                ],
            ),
            (
                # a variant with no build.gradle in the standalone project
                [
                    Path("gradle.lockfile"),
                    Path("build.gradle"),
                    Path("settings.gradle"),
                    Path("buildSrc/settings.gradle"),
                    Path("subdir_a/build.gradle"),
                    Path("subdir_b/build.gradle"),
                ],
                [
                    out.Subproject(
                        root_dir=out.Fpath("."),
                        dependency_source=out.DependencySource(
                            out.ManifestLockfileDependencySource(
                                (
                                    out.Manifest(
                                        kind=out.ManifestKind(value=out.BuildGradle()),
                                        path=out.Fpath("build.gradle"),
                                    ),
                                    out.Lockfile(
                                        out.LockfileKind(out.GradleLockfile()),
                                        out.Fpath("gradle.lockfile"),
                                    ),
                                )
                            ),
                        ),
                        ecosystem=out.Ecosystem(value=out.Maven()),
                    ),
                    out.Subproject(
                        root_dir=out.Fpath("buildSrc"),
                        dependency_source=out.DependencySource(
                            out.ManifestOnlyDependencySource(
                                out.Manifest(
                                    kind=out.ManifestKind(value=out.SettingsGradle()),
                                    path=out.Fpath("buildSrc/settings.gradle"),
                                )
                            ),
                        ),
                        ecosystem=out.Ecosystem(value=out.Maven()),
                    ),
                ],
                [
                    Path("subdir_a/build.gradle"),
                    Path("subdir_b/build.gradle"),
                ],
            ),
            (
                # mixing .gradle and .gradle.kts
                [
                    Path("gradle.lockfile"),
                    Path("build.gradle"),
                    Path("settings.gradle"),
                    Path("buildSrc/settings.gradle.kts"),
                    Path("subdir_a/build.gradle.kts"),
                    Path("subdir_b/build.gradle"),
                ],
                [
                    out.Subproject(
                        root_dir=out.Fpath("."),
                        dependency_source=out.DependencySource(
                            out.ManifestLockfileDependencySource(
                                (
                                    out.Manifest(
                                        kind=out.ManifestKind(value=out.BuildGradle()),
                                        path=out.Fpath("build.gradle"),
                                    ),
                                    out.Lockfile(
                                        out.LockfileKind(out.GradleLockfile()),
                                        out.Fpath("gradle.lockfile"),
                                    ),
                                )
                            ),
                        ),
                        ecosystem=out.Ecosystem(value=out.Maven()),
                    ),
                    out.Subproject(
                        root_dir=out.Fpath("buildSrc"),
                        dependency_source=out.DependencySource(
                            out.ManifestOnlyDependencySource(
                                out.Manifest(
                                    kind=out.ManifestKind(value=out.SettingsGradle()),
                                    path=out.Fpath("buildSrc/settings.gradle.kts"),
                                )
                            ),
                        ),
                        ecosystem=out.Ecosystem(value=out.Maven()),
                    ),
                ],
                [
                    Path("subdir_a/build.gradle.kts"),
                    Path("subdir_b/build.gradle"),
                ],
            ),
            (
                # a very simple single-project build with only a build.gradle
                [
                    Path("build.gradle"),
                ],
                [
                    out.Subproject(
                        root_dir=out.Fpath("."),
                        dependency_source=out.DependencySource(
                            out.ManifestOnlyDependencySource(
                                out.Manifest(
                                    kind=out.ManifestKind(value=out.BuildGradle()),
                                    path=out.Fpath("build.gradle"),
                                )
                            ),
                        ),
                        ecosystem=out.Ecosystem(value=out.Maven()),
                    ),
                ],
                [],
            ),
        ],
    )
    @pytest.mark.quick
    def test_make_subprojects(
        self,
        source_files: List[Path],
        expected_subprojects: List[out.Subproject],
        unused_files: List[Path],
    ):
        matcher = GradleMatcher()

        source_files_set = frozenset(source_files)

        # when we make subprojects from the provided source files
        subprojects, used_files = matcher.make_subprojects(source_files_set)

        # expect all files to be used except those in unused_files
        assert used_files == (source_files_set - set(unused_files))

        # and expect the returned subprojects to match, ignoring order
        expected = set(expected_subprojects)
        assert len(subprojects) == len(expected_subprojects)
        for subproject in subprojects:
            assert subproject in expected


@pytest.mark.quick
def test_filter_dependency_source_files():
    valid_paths = {Path("Pipfile.lock"), Path("requirements.txt")}
    invalid_paths = {Path("unknown.lock"), Path("setup.py")}
    candidates = valid_paths | invalid_paths

    filtered_paths = filter_dependency_source_files(frozenset(candidates))

    assert filtered_paths == valid_paths
