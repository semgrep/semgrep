from pathlib import Path
from typing import List

import pytest

from semdep.subproject_matchers import ExactLockfileManifestMatcher
from semdep.subproject_matchers import filter_dependency_source_files
from semdep.subproject_matchers import MATCHERS
from semdep.subproject_matchers import NEW_REQUIREMENTS_MATCHERS
from semdep.subproject_matchers import OLD_REQUIREMENTS_MATCHERS
from semdep.subproject_matchers import PatternLockfileMatcher
from semdep.subproject_matchers import PipRequirementsMatcher
from semgrep.resolve_subprojects import ConfiguredMatchers
from semgrep.subproject import LockfileDependencySource
from semgrep.subproject import MultiLockfileDependencySource
from semgrep.subproject import PackageManagerType
from semgrep.subproject import Subproject


class TestExactLockfileMatcher:
    @pytest.mark.quick
    def test_lockfile_match(self):
        matcher = ExactLockfileManifestMatcher(
            package_manager_type=PackageManagerType.PIPENV,
            lockfile_name="Pipfile.lock",
            manifest_name="Pipfile",
        )

        assert matcher.is_match(Path("Pipfile.lock")) is True
        assert matcher.is_match(Path("a/b/c/Pipfile.lock")) is True
        assert matcher.is_match(Path("requirements.txt")) is False

    @pytest.mark.quick
    def test_manifest_match(self):
        matcher = ExactLockfileManifestMatcher(
            package_manager_type=PackageManagerType.PIPENV,
            lockfile_name="Pipfile.lock",
            manifest_name="Pipfile",
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
            package_manager_type=PackageManagerType.PIPENV,
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
        assert subproject.root_dir == tmp_path
        assert isinstance(subproject.dependency_source, LockfileDependencySource)
        assert subproject.dependency_source.lockfile_path == lockfile_path
        if create_manifest:
            assert subproject.dependency_source.manifest_path == manifest_path
        else:
            assert subproject.dependency_source.manifest_path is None


class TestPatternLockfileMatcher:
    @pytest.mark.quick
    def test_is_match(self):
        matcher = PatternLockfileMatcher(
            lockfile_pattern="*requirements*.txt",
            manifest_name="requirements.in",
            package_manager_type=PackageManagerType.PIP,
        )

        # Basic cases
        assert matcher.is_match(Path("requirements.txt")) is True
        assert matcher.is_match(Path("requirements3.txt")) is True
        assert matcher.is_match(Path("a/b/c/requirements.txt")) is True
        assert matcher.is_match(Path("a/b/c/requirements3.txt")) is True

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

        # Non-requirements cases
        assert matcher.is_match(Path("a/b/c/requirements.in")) is True
        assert matcher.is_match(Path("unknown.lock")) is False
        assert matcher.is_match(Path("a/b/c/requirements/hello/unknown.lock")) is False

    @pytest.mark.quick
    @pytest.mark.parametrize("with_manifest", [True, False])
    def test_make_subprojects(
        self,
        with_manifest: bool,
    ):
        # with four lockfiles and four corresponding manifests
        # map lockfile to root directory and manifest path
        test_data = {
            Path("requirements.txt"): (Path(""), Path("requirements.in")),
            Path("requirements3.txt"): (Path(""), Path("requirements.in")),
            Path("a/b/c/requirements.txt"): (
                Path("a/b/c"),
                Path("a/b/c/requirements.in"),
            ),
            Path("a/b/c/requirements3.txt"): (
                Path("a/b/c"),
                Path("a/b/c/requirements.in"),
            ),
        }

        # and a pattern matcher
        matcher = PatternLockfileMatcher(
            lockfile_pattern="*requirements*.txt",
            manifest_name="requirements.in",
            package_manager_type=PackageManagerType.PIP,
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
        assert len(subprojects) == 4
        for subproject in subprojects:
            assert isinstance(subproject.dependency_source, LockfileDependencySource)
            expected_root, expected_manifest = test_data[
                subproject.dependency_source.lockfile_path
            ]
            assert subproject.root_dir == expected_root
            if with_manifest:
                assert subproject.dependency_source.manifest_path == expected_manifest
            else:
                assert subproject.dependency_source.manifest_path is None


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
                    Subproject(
                        root_dir=Path(),
                        dependency_source=LockfileDependencySource(
                            package_manager_type=PackageManagerType.PIP,
                            manifest_path=Path("requirements.in"),
                            lockfile_path=Path("requirements.txt"),
                        ),
                    ),
                    Subproject(
                        root_dir=Path("a/b/c"),
                        dependency_source=LockfileDependencySource(
                            package_manager_type=PackageManagerType.PIP,
                            manifest_path=Path("a/b/c/requirements.in"),
                            lockfile_path=Path("a/b/c/requirements.txt"),
                        ),
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
                    Subproject(
                        root_dir=Path(),
                        dependency_source=LockfileDependencySource(
                            package_manager_type=PackageManagerType.PIP,
                            manifest_path=Path("requirements3.in"),
                            lockfile_path=Path("requirements3.txt"),
                        ),
                    ),
                    Subproject(
                        root_dir=Path("a/b/c"),
                        dependency_source=LockfileDependencySource(
                            package_manager_type=PackageManagerType.PIP,
                            manifest_path=Path("a/b/c/requirements.in"),
                            lockfile_path=Path("a/b/c/requirements3.txt"),
                        ),
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
                    Subproject(
                        root_dir=Path(),
                        dependency_source=MultiLockfileDependencySource(
                            sources=(
                                LockfileDependencySource(
                                    package_manager_type=PackageManagerType.PIP,
                                    manifest_path=Path("requirements.in"),
                                    lockfile_path=Path("requirements-dev.txt"),
                                ),
                                LockfileDependencySource(
                                    package_manager_type=PackageManagerType.PIP,
                                    manifest_path=Path("requirements.in"),
                                    lockfile_path=Path("requirements-prod.txt"),
                                ),
                            )
                        ),
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
                    Subproject(
                        root_dir=Path(),
                        dependency_source=MultiLockfileDependencySource(
                            sources=(
                                LockfileDependencySource(
                                    package_manager_type=PackageManagerType.PIP,
                                    manifest_path=Path("requirements.in"),
                                    lockfile_path=Path("dev-requirements.txt"),
                                ),
                                LockfileDependencySource(
                                    package_manager_type=PackageManagerType.PIP,
                                    manifest_path=Path("requirements.in"),
                                    lockfile_path=Path("prod-requirements.txt"),
                                ),
                            )
                        ),
                    )
                ],
            ),
            (
                [Path("requirements_lock.txt"), Path("requirements.in")],
                [
                    Subproject(
                        root_dir=Path(),
                        dependency_source=LockfileDependencySource(
                            package_manager_type=PackageManagerType.PIP,
                            manifest_path=Path("requirements.in"),
                            lockfile_path=Path("requirements_lock.txt"),
                        ),
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
                    Subproject(
                        root_dir=Path(),
                        dependency_source=MultiLockfileDependencySource(
                            sources=(
                                LockfileDependencySource(
                                    package_manager_type=PackageManagerType.PIP,
                                    manifest_path=Path("requirements.in"),
                                    lockfile_path=Path("requirements/dev.txt"),
                                ),
                                LockfileDependencySource(
                                    package_manager_type=PackageManagerType.PIP,
                                    manifest_path=Path("requirements.in"),
                                    lockfile_path=Path("requirements/prod.txt"),
                                ),
                            )
                        ),
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
                    Subproject(
                        root_dir=Path(),
                        dependency_source=MultiLockfileDependencySource(
                            sources=(
                                LockfileDependencySource(
                                    package_manager_type=PackageManagerType.PIP,
                                    manifest_path=Path("requirements.in"),
                                    lockfile_path=Path("requirements/dev.txt"),
                                ),
                                LockfileDependencySource(
                                    package_manager_type=PackageManagerType.PIP,
                                    manifest_path=Path("requirements/prod.in"),
                                    lockfile_path=Path("requirements/prod.txt"),
                                ),
                            )
                        ),
                    )
                ],
            ),
            (
                [
                    Path("requirements/dev.txt"),
                    Path("requirements/prod.txt"),
                ],
                [
                    Subproject(
                        root_dir=Path(),
                        dependency_source=MultiLockfileDependencySource(
                            sources=(
                                LockfileDependencySource(
                                    package_manager_type=PackageManagerType.PIP,
                                    manifest_path=None,
                                    lockfile_path=Path("requirements/dev.txt"),
                                ),
                                LockfileDependencySource(
                                    package_manager_type=PackageManagerType.PIP,
                                    manifest_path=None,
                                    lockfile_path=Path("requirements/prod.txt"),
                                ),
                            )
                        ),
                    )
                ],
            ),
        ],
    )
    @pytest.mark.quick
    def test_make_subprojects(
        self, source_files: List[Path], expected_subprojects: List[Subproject]
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


class TestConfiguredMatchers:
    @pytest.mark.quick
    def test_base_case(self):
        ConfiguredMatchers.init(use_new_requirements_matchers=False)

        # Use the old requirements lockfile matchers for Python
        assert ConfiguredMatchers.matchers == MATCHERS + OLD_REQUIREMENTS_MATCHERS

        # Use the new requirements lockfile matchers for Python
        ConfiguredMatchers.init(use_new_requirements_matchers=True)
        assert ConfiguredMatchers.matchers == MATCHERS + NEW_REQUIREMENTS_MATCHERS

        # Use the old requirements lockfile matchers for Python
        ConfiguredMatchers.init(use_new_requirements_matchers=False)
        assert ConfiguredMatchers.matchers == MATCHERS + OLD_REQUIREMENTS_MATCHERS
