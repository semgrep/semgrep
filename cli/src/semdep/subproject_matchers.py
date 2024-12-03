from pathlib import Path
from typing import FrozenSet
from typing import List

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.matchers.base import ExactLockfileManifestMatcher
from semdep.matchers.base import ExactManifestOnlyMatcher
from semdep.matchers.base import SubprojectMatcher
from semdep.matchers.gradle import GradleMatcher
from semdep.matchers.pip_requirements import PipRequirementsMatcher

# NOTE: the order that these matchers are defined in matters. In find_subprojects, we
# use each dependency source file for at most one matcher, running the matchers in the
# order that they are defined here. This means that if a catch-all matcher were placed
# first, the rest of the matchers would have no chance of matching any subprojects.
MATCHERS: List[SubprojectMatcher] = [
    PipRequirementsMatcher(
        base_file_pattern="*requirement*",
        requirements_file_extensions=["txt", "pip"],
        manifest_file_extension="in",
        default_manifest_file_base="requirements",
    ),
    # Npm
    ExactLockfileManifestMatcher(
        lockfile_name="package-lock.json",
        manifest_name="package.json",
        lockfile_kind=out.LockfileKind(out.NpmPackageLockJson()),
        manifest_kind=out.ManifestKind(out.PackageJson()),
    ),
    ExactLockfileManifestMatcher(
        lockfile_name="yarn.lock",
        manifest_name="package.json",
        lockfile_kind=out.LockfileKind(out.YarnLock()),
        manifest_kind=out.ManifestKind(out.PackageJson()),
    ),
    ExactLockfileManifestMatcher(
        lockfile_name="pnpm-lock.yaml",
        manifest_name="package.json",
        lockfile_kind=out.LockfileKind(out.PnpmLock()),
        manifest_kind=out.ManifestKind(out.PackageJson()),
    ),
    # Gem
    ExactLockfileManifestMatcher(
        lockfile_name="Gemfile.lock",
        manifest_name="Gemfile",
        lockfile_kind=out.LockfileKind(out.GemfileLock()),
        manifest_kind=out.ManifestKind(out.Gemfile()),
    ),
    # Go modules
    ExactLockfileManifestMatcher(
        lockfile_name="go.mod",
        manifest_name="go.mod",
        lockfile_kind=out.LockfileKind(out.GoMod()),
        manifest_kind=out.ManifestKind(out.GoMod_()),
    ),
    # Cargo
    ExactLockfileManifestMatcher(
        lockfile_name="Cargo.lock",
        manifest_name="Cargo.toml",
        lockfile_kind=out.LockfileKind(out.CargoLock()),
        manifest_kind=out.ManifestKind(out.CargoToml()),
    ),
    # Maven
    ExactLockfileManifestMatcher(
        lockfile_name="maven_dep_tree.txt",
        manifest_name="pom.xml",
        lockfile_kind=out.LockfileKind(out.MavenDepTree()),
        manifest_kind=out.ManifestKind(out.PomXml()),
    ),
    ExactManifestOnlyMatcher(
        manifest_kind=out.ManifestKind(out.PomXml()),
        manifest_name="pom.xml",
    ),
    GradleMatcher(),
    # Composer
    ExactLockfileManifestMatcher(
        lockfile_name="composer.lock",
        manifest_name="composer.json",
        lockfile_kind=out.LockfileKind(out.ComposerLock()),
        manifest_kind=out.ManifestKind(out.ComposerJson()),
    ),
    # Nuget
    ExactLockfileManifestMatcher(
        lockfile_name="packages.lock.json",
        manifest_name="nuget.manifest.json",
        lockfile_kind=out.LockfileKind(out.NugetPackagesLockJson()),
        manifest_kind=out.ManifestKind(out.NugetManifestJson()),
    ),
    # Pub
    ExactLockfileManifestMatcher(
        lockfile_name="pubspec.lock",
        manifest_name="pubspec.yaml",
        lockfile_kind=out.LockfileKind(out.PubspecLock()),
        manifest_kind=out.ManifestKind(out.PubspecYaml()),
    ),
    # Swift PM
    ExactLockfileManifestMatcher(
        lockfile_name="Package.resolved",
        manifest_name="Package.swift",
        lockfile_kind=out.LockfileKind(out.SwiftPackageResolved()),
        manifest_kind=out.ManifestKind(out.PackageSwift()),
    ),
    # Hex
    ExactLockfileManifestMatcher(
        lockfile_name="mix.lock",
        manifest_name="mix.exs",
        lockfile_kind=out.LockfileKind(out.MixLock()),
        manifest_kind=out.ManifestKind(out.MixExs()),
    ),
    # Pipenv
    ExactLockfileManifestMatcher(
        lockfile_name="Pipfile.lock",
        manifest_name="Pipfile",
        lockfile_kind=out.LockfileKind(out.PipfileLock()),
        manifest_kind=out.ManifestKind(out.Pipfile()),
    ),
    # Poetry
    ExactLockfileManifestMatcher(
        lockfile_name="poetry.lock",
        manifest_name="pyproject.toml",
        lockfile_kind=out.LockfileKind(out.PoetryLock()),
        manifest_kind=out.ManifestKind(out.PyprojectToml()),
    ),
    # UV
    ExactLockfileManifestMatcher(
        lockfile_name="uv.lock",
        manifest_name="pyproject.toml",
        lockfile_kind=out.LockfileKind(out.UvLock()),
        manifest_kind=out.ManifestKind(out.PyprojectToml()),
    ),
]


def filter_dependency_source_files(candidates: FrozenSet[Path]) -> FrozenSet[Path]:
    """
    Returns the paths in `candidates` that are dependency source files.
    """
    return frozenset(path for path in candidates if _is_dependency_source_file(path))


def _is_dependency_source_file(path: Path) -> bool:
    """
    Check if a path is a valid dependency source file (lockfile, manifest, SBOM, etc)
    """
    return any(matcher.is_match(path) for matcher in MATCHERS)
