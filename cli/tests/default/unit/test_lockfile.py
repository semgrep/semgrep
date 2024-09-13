from pathlib import Path

import pytest

from semdep.lockfile import create_matcher
from semdep.lockfile import ExactLockfileMatcher
from semdep.lockfile import filter_lockfile_paths
from semdep.lockfile import is_valid_lockfile
from semdep.lockfile import Lockfile
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


class TestCreateMatcher:
    @pytest.mark.quick
    @pytest.mark.parametrize(
        "lockfile",
        [
            "Pipfile.lock",
            "poetry.lock",
            "requirements.txt",
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
        matcher = create_matcher(path)
        assert isinstance(matcher, ExactLockfileMatcher)
        assert matcher.lockfile == lockfile

    @pytest.mark.quick
    def test_unknown_match(self):
        path = Path("unknown.lock")
        with pytest.raises(ValueError, match="Unknown lockfile"):
            create_matcher(path)


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
    assert is_valid_lockfile(ecosystem, lockfile) is expected
