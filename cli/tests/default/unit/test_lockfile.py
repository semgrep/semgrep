from pathlib import Path

import pytest

from semdep.lockfile import _create_matcher
from semdep.lockfile import _is_valid_lockfile
from semdep.lockfile import EcosystemLockfiles
from semdep.lockfile import ExactLockfileMatcher
from semdep.lockfile import filter_lockfile_paths
from semdep.lockfile import Lockfile
from semdep.lockfile import NEW_REQUIREMENTS_MATCHERS
from semdep.lockfile import OLD_REQUIREMENTS_MATCHERS
from semdep.lockfile import PatternLockfileMatcher
from semdep.lockfile import RequirementsLockfileMatcher
from semdep.parsers.pipfile import parse_pipfile
from semgrep.semgrep_interfaces.semgrep_output_v1 import Cargo
from semgrep.semgrep_interfaces.semgrep_output_v1 import Composer
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import Gem
from semgrep.semgrep_interfaces.semgrep_output_v1 import Gomod
from semgrep.semgrep_interfaces.semgrep_output_v1 import Hex
from semgrep.semgrep_interfaces.semgrep_output_v1 import Maven
from semgrep.semgrep_interfaces.semgrep_output_v1 import Npm
from semgrep.semgrep_interfaces.semgrep_output_v1 import Nuget
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pub
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi
from semgrep.semgrep_interfaces.semgrep_output_v1 import SwiftPM


class TestExactLockfileMatcher:
    @pytest.mark.quick
    def test_is_match(self):
        matcher = ExactLockfileMatcher(
            lockfile="Pipfile.lock",
            manifest="Pipfile",
            parser=parse_pipfile,
            ecosystem=Ecosystem(Pypi()),
        )

        assert matcher.is_match(Path("Pipfile.lock")) is True
        assert matcher.is_match(Path("a/b/c/Pipfile.lock")) is True
        assert matcher.is_match(Path("a/b/c/Pipfile")) is False
        assert matcher.is_match(Path("requirements.txt")) is False

    @pytest.mark.quick
    @pytest.mark.parametrize("create_manifest", [True, False])
    def test_get_manifest_path(self, tmp_path, monkeypatch, create_manifest):
        if create_manifest:
            (tmp_path / "Pipfile").touch()

        monkeypatch.chdir(tmp_path)

        matcher = ExactLockfileMatcher(
            lockfile="Pipfile.lock",
            manifest="Pipfile",
            parser=parse_pipfile,
            ecosystem=Ecosystem(Pypi()),
        )
        manifest_path = matcher.get_manifest_path(Path("Pipfile.lock"))
        assert manifest_path == (Path("Pipfile") if create_manifest else None)

    @pytest.mark.quick
    def test_get_parent_path(self):
        matcher = ExactLockfileMatcher(
            lockfile="Pipfile.lock",
            manifest="Pipfile",
            parser=parse_pipfile,
            ecosystem=Ecosystem(Pypi()),
        )

        assert matcher.get_subproject_root(Path("Pipfile.lock")) == Path(".")
        assert matcher.get_subproject_root(Path("a/b/c/Pipfile.lock")) == Path("a/b/c")


class TestPatternLockfileMatcher:
    @pytest.mark.quick
    def test_is_match(self):
        matcher = PatternLockfileMatcher(
            pattern="*requirements*.txt",
            manifest="requirements.in",
            parser=parse_pipfile,
            ecosystem=Ecosystem(Pypi()),
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
        assert matcher.is_match(Path("a/b/c/requirements.in")) is False
        assert matcher.is_match(Path("unknown.lock")) is False

    @pytest.mark.quick
    @pytest.mark.parametrize("create_manifest", [True, False])
    @pytest.mark.parametrize(
        ("lockfile_path", "manifest_path"),
        [
            ("requirements.txt", "requirements.in"),
            ("requirements3.txt", "requirements.in"),
            ("a/b/c/requirements.txt", "a/b/c/requirements.in"),
            ("a/b/c/requirements3.txt", "a/b/c/requirements.in"),
        ],
    )
    def test_get_manifest_path(
        self, tmp_path, monkeypatch, create_manifest, lockfile_path, manifest_path
    ):
        lockfile_full_path = tmp_path / Path(lockfile_path)
        lockfile_full_path.parent.mkdir(parents=True, exist_ok=True)
        lockfile_full_path.touch()
        if create_manifest:
            manifest_full_path = tmp_path / Path(manifest_path)
            manifest_full_path.touch()

        monkeypatch.chdir(tmp_path)

        matcher = PatternLockfileMatcher(
            pattern="*requirements*.txt",
            manifest="requirements.in",
            parser=parse_pipfile,
            ecosystem=Ecosystem(Pypi()),
        )
        matcher_manifest_path = matcher.get_manifest_path(Path(lockfile_path))
        assert matcher_manifest_path == (
            Path(manifest_path) if create_manifest else None
        )

    @pytest.mark.quick
    def test_get_parent_path(self):
        matcher = PatternLockfileMatcher(
            pattern="*requirements*.txt",
            manifest="requirements.in",
            parser=parse_pipfile,
            ecosystem=Ecosystem(Pypi()),
        )

        assert matcher.get_subproject_root(Path("requirements.txt")) == Path(".")
        assert matcher.get_subproject_root(Path("a/b/c/requirements.txt")) == Path(
            "a/b/c"
        )


class TestRequirementsLockfileMatcher:
    @pytest.mark.quick
    def test_is_match(self) -> None:
        matcher = RequirementsLockfileMatcher(
            pattern="*requirements*.txt",
            manifest="requirements.in",
            parser=parse_pipfile,
            ecosystem=Ecosystem(Pypi()),
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
        assert matcher.is_match(Path("a/b/c/requirements.in")) is False
        assert matcher.is_match(Path("unknown.lock")) is False

    @pytest.mark.quick
    @pytest.mark.parametrize("create_manifest", [True, False])
    @pytest.mark.parametrize(
        ("lockfile_path", "manifest_path"),
        [
            ("requirements.txt", "requirements.in"),
            ("requirements3.txt", "requirements.in"),
            ("a/b/c/requirements.txt", "a/b/c/requirements.in"),
            ("a/b/c/requirements3.txt", "a/b/c/requirements.in"),
            ("requirements-dev.txt", "requirements.in"),
            ("requirements-prod.txt", "requirements.in"),
            ("dev-requirements.txt", "requirements.in"),
            ("prod-requirements.txt", "requirements.in"),
            ("requirements_lock.txt", "requirements.in"),
            ("requirements/prod.txt", "requirements.in"),
            # TODO(sal): Is this even a case that appears?
            ("requirements/prod.txt", "requirements/prod.in"),
        ],
    )
    def test_get_manifest_path(
        self, tmp_path, monkeypatch, create_manifest, lockfile_path, manifest_path
    ):
        lockfile_full_path = tmp_path / Path(lockfile_path)
        lockfile_full_path.parent.mkdir(parents=True, exist_ok=True)
        lockfile_full_path.touch()
        if create_manifest:
            manifest_full_path = tmp_path / Path(manifest_path)
            manifest_full_path.touch()

        monkeypatch.chdir(tmp_path)

        matcher = RequirementsLockfileMatcher(
            pattern="*requirements*.txt",
            manifest="requirements.in",
            parser=parse_pipfile,
            ecosystem=Ecosystem(Pypi()),
        )
        matcher_manifest_path = matcher.get_manifest_path(Path(lockfile_path))
        assert matcher_manifest_path == (
            Path(manifest_path) if create_manifest else None
        )

    @pytest.mark.quick
    def test_get_subproject_root(self):
        matcher = RequirementsLockfileMatcher(
            pattern="*requirements*.txt",
            manifest="requirements.in",
            parser=parse_pipfile,
            ecosystem=Ecosystem(Pypi()),
        )

        assert matcher.get_subproject_root(Path("requirements.txt")) == Path(".")
        assert matcher.get_subproject_root(Path("a/b/c/requirements.txt")) == Path(
            "a/b/c"
        )
        assert matcher.get_subproject_root(Path("requirements-dev.txt")) == Path(".")
        assert matcher.get_subproject_root(Path("requirements/prod.txt")) == Path(".")
        assert matcher.get_subproject_root(Path("a/b/c/requirements/base.txt")) == Path(
            "a/b/c"
        )

        # TODO(sal): Is this even a case we want to support? Unsure if we want to support
        # non standard requirements files in subdirectories under `requirements/`.
        assert matcher.get_subproject_root(
            Path("a/b/c/requirements/base/dev.txt")
        ) == Path("a/b/c")


class TestCreateMatcher:
    @pytest.mark.quick
    @pytest.mark.parametrize(
        "lockfile",
        [
            "Pipfile.lock",
            "poetry.lock",
            "package-lock.json",
            "yarn.lock",
            "pnpm-lock.yaml",
            "Gemfile.lock",
            "go.mod",
            "Cargo.lock",
            "maven_dep_tree.txt",
            "gradle.lockfile",
            "composer.lock",
            "packages.lock.json",
            "pubspec.lock",
            "Package.resolved",
            "mix.lock",
        ],
    )
    def test_valid_match(self, lockfile):
        path = Path(lockfile)
        matcher = _create_matcher(path)
        assert isinstance(matcher, ExactLockfileMatcher)
        assert matcher.lockfile == lockfile

    @pytest.mark.quick
    @pytest.mark.parametrize(
        "lockfile",
        [
            "requirements.txt",
            "requirements.dev.txt",
            "requirements/prod.txt",
            "a/b/c/requirements/base.txt",
        ],
    )
    def test_valid_pattern_match(self, lockfile):
        EcosystemLockfiles.init(use_new_requirements_matchers=True)

        matcher = _create_matcher(Path(lockfile))

        assert isinstance(matcher, PatternLockfileMatcher)
        assert matcher.pattern == "*requirement*.txt"

    @pytest.mark.quick
    def test_unknown_match(self):
        path = Path("unknown.lock")
        with pytest.raises(ValueError, match="Unknown lockfile"):
            _create_matcher(path)


class TestLockfile:
    @pytest.mark.kinda_slow
    @pytest.mark.parametrize(
        "lockfile_path, ecosystem, manifest",
        [
            ("Pipfile.lock", Ecosystem(Pypi()), "Pipfile"),
            ("poetry.lock", Ecosystem(Pypi()), "pyproject.toml"),
            ("requirements.txt", Ecosystem(Pypi()), "requirements.in"),
            ("package-lock.json", Ecosystem(Npm()), "package.json"),
            ("yarn.lock", Ecosystem(Npm()), "package.json"),
            ("pnpm-lock.yaml", Ecosystem(Npm()), None),
            ("Gemfile.lock", Ecosystem(Gem()), None),
            ("go.mod", Ecosystem(Gomod()), None),
            ("Cargo.lock", Ecosystem(Cargo()), None),
            ("maven_dep_tree.txt", Ecosystem(Maven()), None),
            ("gradle.lockfile", Ecosystem(Maven()), "build.gradle"),
            ("composer.lock", Ecosystem(Composer()), "composer.json"),
            ("packages.lock.json", Ecosystem(Nuget()), None),
            ("pubspec.lock", Ecosystem(Pub()), "pubspec.yaml"),
            ("Package.resolved", Ecosystem(SwiftPM()), "Package.swift"),
            ("mix.lock", Ecosystem(Hex()), "mix.exs"),
            ("a/Pipfile.lock", Ecosystem(Pypi()), "Pipfile"),
            ("a/b/poetry.lock", Ecosystem(Pypi()), "pyproject.toml"),
            ("a/b/c/requirements.txt", Ecosystem(Pypi()), "requirements.in"),
            ("./package-lock.json", Ecosystem(Npm()), "package.json"),
        ],
    )
    def test_lockfile_from_path(
        self, lockfile_path, ecosystem, manifest, tmp_path, monkeypatch
    ):
        # Create the lockfile and manifest in the temporary directory
        lockfile_full_path = tmp_path / Path(lockfile_path)
        lockfile_full_path.parent.mkdir(parents=True, exist_ok=True)
        lockfile_full_path.touch()
        if manifest:
            manifest_full_path = tmp_path / Path(lockfile_path).parent / Path(manifest)
            manifest_full_path.touch()
        monkeypatch.chdir(tmp_path)

        # Create the Lockfile instance
        lockfile = Lockfile.from_path(Path(lockfile_path))

        # Assert the Lockfile instance is correct
        assert lockfile.path == Path(lockfile_path)
        assert lockfile.ecosystem == ecosystem
        assert lockfile.manifest_path == (
            Path(lockfile_path).parent / Path(manifest) if manifest else None
        )

    @pytest.mark.kinda_slow
    @pytest.mark.parametrize(
        "lockfile_path, ecosystem, manifest_path",
        [
            ("requirements.txt", Ecosystem(Pypi()), "requirements.in"),
            ("a/b/c/requirements.txt", Ecosystem(Pypi()), "a/b/c/requirements.in"),
            (
                "a/b/c/requirements/base.txt",
                Ecosystem(Pypi()),
                "a/b/c/requirements/base.in",
            ),
        ],
    )
    def test_pattern_matchers(
        self, lockfile_path, ecosystem, manifest_path, tmp_path, monkeypatch
    ):
        EcosystemLockfiles.init(use_new_requirements_matchers=True)

        # Create the lockfile and manifest in the temporary directory
        lockfile_full_path = tmp_path / Path(lockfile_path)
        lockfile_full_path.parent.mkdir(parents=True, exist_ok=True)
        lockfile_full_path.touch()
        manifest_full_path = tmp_path / Path(manifest_path)
        manifest_full_path.touch()
        monkeypatch.chdir(tmp_path)

        # Create the Lockfile instance
        lockfile = Lockfile.from_path(Path(lockfile_path))

        # Assert the Lockfile instance is correct
        assert lockfile.path == Path(lockfile_path)
        assert lockfile.ecosystem == ecosystem
        assert lockfile.manifest_path == Path(manifest_path)


@pytest.mark.quick
def test_filter_lockfile_paths():
    ecosystem = Ecosystem(Pypi())
    valid_paths = {Path("Pipfile.lock"), Path("requirements.txt")}
    invalid_paths = {Path("unknown.lock"), Path("setup.py")}
    candidates = valid_paths | invalid_paths

    filtered_paths = filter_lockfile_paths(ecosystem, frozenset(candidates))

    assert filtered_paths == valid_paths


@pytest.mark.quick
@pytest.mark.parametrize(
    ("lockfile", "expected"),
    [
        (Path("Pipfile.lock"), True),
        (Path("requirements.txt"), True),
        (Path("unknown.lock"), False),
        (Path("setup.py"), False),
    ],
)
def test_is_valid_lockfile(lockfile, expected):
    ecosystem = Ecosystem(Pypi())
    assert _is_valid_lockfile(ecosystem, lockfile) is expected


class TestEcosystemLockfiles:
    @pytest.mark.quick
    def test_base_case(self):
        EcosystemLockfiles.init(use_new_requirements_matchers=False)

        # Use the old requirements lockfile matchers for Python
        assert (
            EcosystemLockfiles.ecosystem_to_lockfiles[Ecosystem(Pypi())]
            == OLD_REQUIREMENTS_MATCHERS
        )

        # Use the new requirements lockfile matchers for Python
        EcosystemLockfiles.init(use_new_requirements_matchers=True)
        assert (
            EcosystemLockfiles.ecosystem_to_lockfiles[Ecosystem(Pypi())]
            == NEW_REQUIREMENTS_MATCHERS
        )

        # Use the old requirements lockfile matchers for Python
        EcosystemLockfiles.init(use_new_requirements_matchers=False)
        assert (
            EcosystemLockfiles.ecosystem_to_lockfiles[Ecosystem(Pypi())]
            == OLD_REQUIREMENTS_MATCHERS
        )
