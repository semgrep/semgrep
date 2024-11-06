from pathlib import Path
from typing import FrozenSet
from typing import List

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.matchers.base import ExactLockfileManifestMatcher
from semdep.matchers.base import ExactManifestOnlyMatcher
from semdep.matchers.base import SubprojectMatcher
from semdep.matchers.gradle import GradleMatcher
from semdep.matchers.pip_requirements import PipRequirementsMatcher
from semgrep.subproject import PackageManagerType

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
        package_manager_type=PackageManagerType.NPM,
        manifest_kind=out.ManifestKind(out.PackageJson()),
    ),
    ExactLockfileManifestMatcher(
        lockfile_name="yarn.lock",
        manifest_name="package.json",
        package_manager_type=PackageManagerType.YARN,
        manifest_kind=out.ManifestKind(out.PackageJson()),
    ),
    ExactLockfileManifestMatcher(
        lockfile_name="pnpm-lock.yaml",
        manifest_name="package.json",
        package_manager_type=PackageManagerType.PNPM,
        manifest_kind=out.ManifestKind(out.PackageJson()),
    ),
    # Gem
    ExactLockfileManifestMatcher(
        lockfile_name="Gemfile.lock",
        manifest_name="Gemfile",
        package_manager_type=PackageManagerType.RUBY_GEM,
        manifest_kind=out.ManifestKind(out.Gemfile()),
    ),
    # Go modules
    ExactLockfileManifestMatcher(
        lockfile_name="go.mod",
        manifest_name="go.mod",
        package_manager_type=PackageManagerType.GO_MOD,
        manifest_kind=out.ManifestKind(out.GoMod_()),
    ),
    # Cargo
    ExactLockfileManifestMatcher(
        lockfile_name="Cargo.lock",
        manifest_name="Cargo.toml",
        package_manager_type=PackageManagerType.CARGO,
        manifest_kind=out.ManifestKind(out.CargoToml()),
    ),
    # Maven
    ExactLockfileManifestMatcher(
        lockfile_name="maven_dep_tree.txt",
        manifest_name="pom.xml",
        package_manager_type=PackageManagerType.MAVEN,
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
        package_manager_type=PackageManagerType.COMPOSER,
        manifest_kind=out.ManifestKind(out.ComposerJson()),
    ),
    # Nuget
    ExactLockfileManifestMatcher(
        lockfile_name="packages.lock.json",
        manifest_name="nuget.manifest.json",
        package_manager_type=PackageManagerType.NUGET,
        manifest_kind=out.ManifestKind(out.NugetManifestJson()),
    ),
    # Pub
    ExactLockfileManifestMatcher(
        lockfile_name="pubspec.lock",
        manifest_name="pubspec.yaml",
        package_manager_type=PackageManagerType.DART_PUB,
        manifest_kind=out.ManifestKind(out.PubspecYaml()),
    ),
    # Swift PM
    ExactLockfileManifestMatcher(
        lockfile_name="Package.resolved",
        manifest_name="Package.swift",
        package_manager_type=PackageManagerType.SWIFT_PM,
        manifest_kind=out.ManifestKind(out.PackageSwift_()),
    ),
    # Hex
    ExactLockfileManifestMatcher(
        lockfile_name="mix.lock",
        manifest_name="mix.exs",
        package_manager_type=PackageManagerType.ELIXIR_HEX,
        manifest_kind=out.ManifestKind(out.MixExs()),
    ),
    # Pipenv
    ExactLockfileManifestMatcher(
        lockfile_name="Pipfile.lock",
        manifest_name="Pipfile",
        package_manager_type=PackageManagerType.PIPENV,
        manifest_kind=out.ManifestKind(out.Pipfile_()),
    ),
    # Poetry
    ExactLockfileManifestMatcher(
        lockfile_name="poetry.lock",
        manifest_name="pyproject.toml",
        package_manager_type=PackageManagerType.POETRY,
        manifest_kind=out.ManifestKind(out.PyprojectToml_()),
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
