#
# Copyright (c) 2025 Semgrep Inc.
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
from collections import defaultdict
from pathlib import Path

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.dependency_aware_rule import generate_reachable_sca_findings
from semgrep.dependency_aware_rule import SubprojectDependencyIndex
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.semgrep_interfaces.semgrep_output_v1 import DependencyKind
from semgrep.semgrep_interfaces.semgrep_output_v1 import DependencySource
from semgrep.semgrep_interfaces.semgrep_output_v1 import Direct
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import Fpath
from semgrep.semgrep_interfaces.semgrep_output_v1 import Lockfile
from semgrep.semgrep_interfaces.semgrep_output_v1 import LockfileKind
from semgrep.semgrep_interfaces.semgrep_output_v1 import LockfileParsing
from semgrep.semgrep_interfaces.semgrep_output_v1 import Manifest
from semgrep.semgrep_interfaces.semgrep_output_v1 import ManifestKind
from semgrep.semgrep_interfaces.semgrep_output_v1 import ManifestLockfile
from semgrep.semgrep_interfaces.semgrep_output_v1 import Maven
from semgrep.semgrep_interfaces.semgrep_output_v1 import Npm
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pipfile
from semgrep.semgrep_interfaces.semgrep_output_v1 import PipfileLock
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi
from semgrep.semgrep_interfaces.semgrep_output_v1 import ResolutionMethod
from semgrep.semgrep_interfaces.semgrep_output_v1 import ResolvedDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import ResolvedSubproject
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaPattern
from semgrep.semgrep_interfaces.semgrep_output_v1 import Subproject
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitive
from semgrep.subproject import from_resolved_dependencies


@pytest.fixture
def sample_subproject():
    """Create a subproject with multiple packages and versions for testing."""
    dependencies = [
        ResolvedDependency(
            (
                out.FoundDependency(
                    package="requests",
                    version="2.28.1",
                    ecosystem=Ecosystem(value=Pypi()),
                    allowed_hashes=defaultdict(list),
                    transitivity=DependencyKind(Direct()),
                    resolved_url=None,
                    children=None,
                    git_ref=None,
                    lockfile_path=Fpath("Pipfile.lock"),
                    line_number=10,
                ),
                None,
            )
        ),
        # Same package, different version (e.g., transitive dependency)
        ResolvedDependency(
            (
                out.FoundDependency(
                    package="requests",
                    version="2.25.0",
                    ecosystem=Ecosystem(value=Pypi()),
                    allowed_hashes=defaultdict(list),
                    transitivity=DependencyKind(Transitive()),
                    resolved_url=None,
                    children=None,
                    git_ref=None,
                    lockfile_path=Fpath("requirements.txt"),
                    line_number=5,
                ),
                None,
            )
        ),
        ResolvedDependency(
            (
                out.FoundDependency(
                    package="flask",
                    version="2.0.1",
                    ecosystem=Ecosystem(value=Pypi()),
                    allowed_hashes=defaultdict(list),
                    transitivity=DependencyKind(Direct()),
                    resolved_url=None,
                    children=None,
                    git_ref=None,
                    lockfile_path=Fpath("Pipfile.lock"),
                    line_number=15,
                ),
                None,
            )
        ),
        ResolvedDependency(
            (
                out.FoundDependency(
                    package="werkzeug",
                    version="2.0.0",
                    ecosystem=Ecosystem(value=Pypi()),
                    allowed_hashes=defaultdict(list),
                    transitivity=DependencyKind(Transitive()),
                    resolved_url=None,
                    children=None,
                    git_ref=None,
                    lockfile_path=Fpath("Pipfile.lock"),
                    line_number=20,
                ),
                None,
            )
        ),
    ]

    dependency_source = DependencySource(
        ManifestLockfile(
            (
                Manifest(ManifestKind(Pipfile()), Fpath("Pipfile")),
                Lockfile(LockfileKind(PipfileLock()), Fpath("Pipfile.lock")),
            )
        )
    )

    return ResolvedSubproject(
        info=Subproject(
            root_dir=Fpath("."),
            dependency_source=dependency_source,
            ecosystem=Ecosystem(value=Pypi()),
        ),
        errors=[],
        resolution_method=ResolutionMethod(LockfileParsing()),
        resolved_dependencies=from_resolved_dependencies(dependencies),
        ecosystem=Ecosystem(value=Pypi()),
    )


@pytest.mark.quick
def test_index_matches_specific_version_range(sample_subproject):
    """Should match newer version of requests but not older."""
    index = SubprojectDependencyIndex.from_subproject(sample_subproject)

    patterns = [
        ScaPattern(
            ecosystem=Ecosystem(value=Pypi()),
            package="requests",
            semver_range=">=2.28.0",
        )
    ]
    matches = list(index.get_dependency_matches(patterns))
    assert len(matches) == 1
    assert matches[0][1].version == "2.28.1"


@pytest.mark.quick
def test_index_matches_multiple_versions(sample_subproject):
    """Should match both versions with broader range."""
    index = SubprojectDependencyIndex.from_subproject(sample_subproject)

    patterns = [
        ScaPattern(
            ecosystem=Ecosystem(value=Pypi()),
            package="requests",
            semver_range=">=2.0.0",
        )
    ]
    matches = list(index.get_dependency_matches(patterns))
    assert len(matches) == 2
    versions = {dep.version for _, dep in matches}
    assert versions == {"2.28.1", "2.25.0"}


@pytest.mark.quick
def test_index_matches_multiple_packages(sample_subproject):
    """Should match multiple different packages."""
    index = SubprojectDependencyIndex.from_subproject(sample_subproject)

    patterns = [
        ScaPattern(
            ecosystem=Ecosystem(value=Pypi()),
            package="flask",
            semver_range=">=2.0.0",
        ),
        ScaPattern(
            ecosystem=Ecosystem(value=Pypi()),
            package="werkzeug",
            semver_range=">=2.0.0",
        ),
    ]
    matches = list(index.get_dependency_matches(patterns))
    assert len(matches) == 2
    packages = {dep.package for _, dep in matches}
    assert packages == {"flask", "werkzeug"}


@pytest.mark.quick
def test_index_no_match_nonexistent_package(sample_subproject):
    """Should not match when package doesn't exist."""
    index = SubprojectDependencyIndex.from_subproject(sample_subproject)

    patterns = [
        ScaPattern(
            ecosystem=Ecosystem(value=Pypi()),
            package="nonexistent",
            semver_range=">=1.0.0",
        )
    ]
    matches = list(index.get_dependency_matches(patterns))
    assert len(matches) == 0


@pytest.mark.quick
def test_index_no_match_version_mismatch(sample_subproject):
    """Should not match when version range excludes all versions."""
    index = SubprojectDependencyIndex.from_subproject(sample_subproject)

    patterns = [
        ScaPattern(
            ecosystem=Ecosystem(value=Pypi()),
            package="flask",
            semver_range=">=3.0.0",
        )
    ]
    matches = list(index.get_dependency_matches(patterns))
    assert len(matches) == 0


@pytest.mark.quick
def test_index_no_match_ecosystem_mismatch(sample_subproject):
    """Should not match when ecosystem is different."""
    index = SubprojectDependencyIndex.from_subproject(sample_subproject)

    patterns = [
        ScaPattern(
            ecosystem=Ecosystem(value=Npm()),
            package="requests",
            semver_range=">=2.0.0",
        )
    ]
    matches = list(index.get_dependency_matches(patterns))
    assert len(matches) == 0


###############################################################################
# Gradle module scoping of reachable findings (SC-4027)
###############################################################################
#
# With Gradle module attribution on, the RPC reports one dependency entry per
# module build file. A code match must only pair with the dependencies of the
# module containing the code (plus anything reached through an explicit
# Gradle project dependency), and the direct-over-transitive preference must
# not look at other modules.

GUAVA = "com.google.guava:guava"
GUAVA_VERSION = "32.1.1-jre"


def _gradle_dep(package, version, *, build_file, direct, children=()):
    """
    `children` are (package, version) or (package, version, build file)
    tuples; the resolver names each child's build file with the flag on.
    """
    return ResolvedDependency(
        (
            out.FoundDependency(
                package=package,
                version=version,
                ecosystem=Ecosystem(Maven()),
                allowed_hashes=defaultdict(list),
                transitivity=DependencyKind(Direct() if direct else Transitive()),
                resolved_url=None,
                git_ref=None,
                manifest_path=Fpath("settings.gradle.kts"),
                lockfile_path=Fpath(build_file),
                line_number=1,
                children=[
                    out.DependencyChild(
                        package=child[0],
                        version=child[1],
                        lockfile_path=Fpath(child[2]) if len(child) > 2 else None,
                    )
                    for child in children
                ],
            ),
            None,
        )
    )


def _gradle_subproject(dependencies, *, manifest="settings.gradle.kts"):
    kind = (
        out.ManifestKind(out.SettingsGradle())
        if manifest.startswith("settings")
        else out.ManifestKind(out.BuildGradleKts())
    )
    return ResolvedSubproject(
        info=Subproject(
            root_dir=Fpath("."),
            dependency_source=DependencySource(
                out.ManifestOnly(Manifest(kind, Fpath(manifest)))
            ),
            ecosystem=Ecosystem(Maven()),
        ),
        errors=[],
        resolution_method=ResolutionMethod(out.DynamicResolution()),
        resolved_dependencies=from_resolved_dependencies(dependencies),
        ecosystem=Ecosystem(Maven()),
    )


def _guava_rule():
    return Rule.from_json(
        {
            "id": "java-gradle-sca",
            "pattern": "bad()",
            "r2c-internal-project-depends-on": {
                "namespace": "maven",
                "package": GUAVA,
                "version": "<= 40.0",
            },
            "message": "oh no",
            "languages": ["java"],
            "severity": "WARNING",
        }
    )


def _code_match(path):
    return RuleMatch(
        message="oh no",
        severity=out.MatchSeverity(out.Warning()),
        match=out.CoreMatch(
            check_id=out.RuleId("java-gradle-sca"),
            path=out.Fpath(path),
            start=out.Position(3, 9, 40),
            end=out.Position(3, 14, 45),
            extra=out.CoreMatchExtra(
                metavars=out.Metavars({}),
                engine_kind=out.EngineOfFinding(out.OSS()),
                is_ignored=False,
            ),
        ),
        match_formula_string="bad()",
    )


def _paired_dependency(match: RuleMatch) -> out.FoundDependency:
    sca_match = match.match.extra.sca_match
    assert sca_match is not None
    return sca_match.dependency_match.found_dependency


def _lockfile_path(dependency: out.FoundDependency) -> str:
    assert dependency.lockfile_path is not None
    return dependency.lockfile_path.value


def _reachable(subproject, paths, *, gradle_module_attribution=True):
    index = SubprojectDependencyIndex.from_subproject(
        subproject, gradle_module_attribution=gradle_module_attribution
    )
    matches, errors, _ = generate_reachable_sca_findings(
        [_code_match(path) for path in paths],
        _guava_rule(),
        {Ecosystem(Maven()): [(subproject, index)]},
        gradle_module_attribution=gradle_module_attribution,
    )
    assert errors == []
    return sorted(
        (
            str(m.path),
            _lockfile_path(_paired_dependency(m)),
            _paired_dependency(m).transitivity.to_json(),
            m.match_based_id,
        )
        for m in matches
    )


def _layout(tmp_path, monkeypatch, files):
    """Create the Gradle build files and source files, then chdir into it."""
    for relative in files:
        path = tmp_path / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(
            "class X { void run() { bad(); } }\n" if relative.endswith(".java") else ""
        )
    monkeypatch.chdir(tmp_path)


MODULE_FILES = [
    "settings.gradle.kts",
    "app/build.gradle.kts",
    "app/src/main/java/App.java",
    "lib/build.gradle.kts",
]


@pytest.mark.quick
def test_reachable_gradle_both_modules_direct_pairs_only_own_module(
    tmp_path, monkeypatch
):
    _layout(tmp_path, monkeypatch, MODULE_FILES)
    subproject = _gradle_subproject(
        [
            _gradle_dep(
                GUAVA, GUAVA_VERSION, build_file="app/build.gradle.kts", direct=True
            ),
            _gradle_dep(
                GUAVA, GUAVA_VERSION, build_file="lib/build.gradle.kts", direct=True
            ),
        ]
    )
    findings = _reachable(subproject, ["app/src/main/java/App.java"])
    assert [f[:3] for f in findings] == [
        ("app/src/main/java/App.java", "app/build.gradle.kts", "direct")
    ]


@pytest.mark.quick
def test_reachable_gradle_other_module_direct_does_not_suppress_own_transitive(
    tmp_path, monkeypatch
):
    _layout(tmp_path, monkeypatch, MODULE_FILES)
    subproject = _gradle_subproject(
        [
            _gradle_dep(
                "com.example:app-parent",
                "1.0",
                build_file="app/build.gradle.kts",
                direct=True,
                children=[(GUAVA, GUAVA_VERSION)],
            ),
            _gradle_dep(
                GUAVA, GUAVA_VERSION, build_file="app/build.gradle.kts", direct=False
            ),
            _gradle_dep(
                GUAVA, GUAVA_VERSION, build_file="lib/build.gradle.kts", direct=True
            ),
        ]
    )
    findings = _reachable(subproject, ["app/src/main/java/App.java"])
    assert [f[:3] for f in findings] == [
        ("app/src/main/java/App.java", "app/build.gradle.kts", "transitive")
    ]


@pytest.mark.quick
def test_reachable_gradle_direct_preference_stays_within_module(tmp_path, monkeypatch):
    # The direct-over-transitive preference exists to pick between two packages
    # the rule lists (e.g. foo vs foo-special). It still applies within app.
    _layout(tmp_path, monkeypatch, MODULE_FILES)
    rule = Rule.from_json(
        {
            "id": "java-gradle-sca",
            "pattern": "bad()",
            "r2c-internal-project-depends-on": {
                "depends-on-either": [
                    {"namespace": "maven", "package": GUAVA, "version": "<= 40.0"},
                    {
                        "namespace": "maven",
                        "package": "com.example:guava-special",
                        "version": "<= 40.0",
                    },
                ]
            },
            "message": "oh no",
            "languages": ["java"],
            "severity": "WARNING",
        }
    )
    subproject = _gradle_subproject(
        [
            _gradle_dep(
                GUAVA, GUAVA_VERSION, build_file="app/build.gradle.kts", direct=True
            ),
            _gradle_dep(
                "com.example:guava-special",
                "1.0",
                build_file="app/build.gradle.kts",
                direct=False,
            ),
            _gradle_dep(
                "com.example:guava-special",
                "1.0",
                build_file="lib/build.gradle.kts",
                direct=True,
            ),
        ]
    )
    index = SubprojectDependencyIndex.from_subproject(
        subproject, gradle_module_attribution=True
    )
    matches, errors, _ = generate_reachable_sca_findings(
        [_code_match("app/src/main/java/App.java")],
        rule,
        {Ecosystem(Maven()): [(subproject, index)]},
        gradle_module_attribution=True,
    )
    assert errors == []
    assert [
        (_paired_dependency(m).package, _lockfile_path(_paired_dependency(m)))
        for m in matches
    ] == [(GUAVA, "app/build.gradle.kts")]


@pytest.mark.quick
def test_reachable_gradle_root_nested_and_empty_modules(tmp_path, monkeypatch):
    _layout(
        tmp_path,
        monkeypatch,
        [
            "settings.gradle.kts",
            "build.gradle.kts",
            "src/main/java/Root.java",
            "empty/build.gradle.kts",
            "empty/src/main/java/Empty.java",
            "services/api/build.gradle.kts",
            "services/api/src/main/java/Api.java",
            # a source directory under services/ that has no build file of
            # its own belongs to the nearest enclosing module, here the root
            "services/src/main/java/Shared.java",
        ],
    )
    subproject = _gradle_subproject(
        [
            _gradle_dep(
                GUAVA, GUAVA_VERSION, build_file="build.gradle.kts", direct=True
            ),
            _gradle_dep(
                GUAVA,
                GUAVA_VERSION,
                build_file="services/api/build.gradle.kts",
                direct=True,
            ),
        ],
        manifest="build.gradle.kts",
    )
    findings = _reachable(
        subproject,
        [
            "src/main/java/Root.java",
            "empty/src/main/java/Empty.java",
            "services/api/src/main/java/Api.java",
            "services/src/main/java/Shared.java",
        ],
    )
    assert [f[:3] for f in findings] == [
        (
            "services/api/src/main/java/Api.java",
            "services/api/build.gradle.kts",
            "direct",
        ),
        ("services/src/main/java/Shared.java", "build.gradle.kts", "direct"),
        ("src/main/java/Root.java", "build.gradle.kts", "direct"),
    ]
    assert len({f[3] for f in findings}) == len(findings)


PROJECT_DEPENDENCY_FILES = MODULE_FILES + [
    "build.gradle.kts",
    "services/api/build.gradle.kts",
]


def _project_dependency_deps(child_build_file):
    # app -> project(":lib") -> guava. The project node lives in app and its
    # children live in lib, mirroring how the resolver reports the graph. The
    # root and services/api modules have their own, unrelated copies of guava.
    child = (
        (GUAVA, GUAVA_VERSION, child_build_file)
        if child_build_file
        else (GUAVA, GUAVA_VERSION)
    )
    return [
        _gradle_dep(
            "reachable:lib",
            "unspecified",
            build_file="app/build.gradle.kts",
            direct=True,
            children=[child],
        ),
        _gradle_dep(
            GUAVA,
            GUAVA_VERSION,
            build_file="lib/build.gradle.kts",
            direct=True,
            children=[("com.example:lib-child", "1.0", "lib/build.gradle.kts")],
        ),
        _gradle_dep(
            "com.example:lib-child",
            "1.0",
            build_file="lib/build.gradle.kts",
            direct=False,
        ),
        _gradle_dep(GUAVA, GUAVA_VERSION, build_file="build.gradle.kts", direct=True),
        _gradle_dep(
            GUAVA,
            GUAVA_VERSION,
            build_file="services/api/build.gradle.kts",
            direct=True,
        ),
    ]


@pytest.mark.quick
def test_reachable_gradle_project_dependency_reaches_other_module(
    tmp_path, monkeypatch
):
    _layout(tmp_path, monkeypatch, PROJECT_DEPENDENCY_FILES)
    subproject = _gradle_subproject(_project_dependency_deps("lib/build.gradle.kts"))
    findings = _reachable(subproject, ["app/src/main/java/App.java"])
    assert [f[:3] for f in findings] == [
        ("app/src/main/java/App.java", "lib/build.gradle.kts", "direct")
    ]


@pytest.mark.quick
def test_reachable_gradle_two_project_dependencies_same_package(tmp_path, monkeypatch):
    # app -> project(":lib") and app -> project(":lib2"); both use the same
    # Guava. The app code gets one reachable finding, paired with the first
    # copy reached, and the other copy stays a dependency-only finding.
    _layout(tmp_path, monkeypatch, MODULE_FILES + ["lib2/build.gradle.kts"])
    guava_lib2 = _gradle_dep(
        GUAVA, GUAVA_VERSION, build_file="lib2/build.gradle.kts", direct=True
    )
    subproject = _gradle_subproject(
        [
            _gradle_dep(
                "reachable:lib",
                "unspecified",
                build_file="app/build.gradle.kts",
                direct=True,
                children=[(GUAVA, GUAVA_VERSION, "lib/build.gradle.kts")],
            ),
            _gradle_dep(
                "reachable:lib2",
                "unspecified",
                build_file="app/build.gradle.kts",
                direct=True,
                children=[(GUAVA, GUAVA_VERSION, "lib2/build.gradle.kts")],
            ),
            _gradle_dep(
                GUAVA, GUAVA_VERSION, build_file="lib/build.gradle.kts", direct=True
            ),
            guava_lib2,
        ]
    )
    index = SubprojectDependencyIndex.from_subproject(
        subproject, gradle_module_attribution=True
    )
    matches, errors, already_reachable = generate_reachable_sca_findings(
        [_code_match("app/src/main/java/App.java")],
        _guava_rule(),
        {Ecosystem(Maven()): [(subproject, index)]},
        gradle_module_attribution=True,
    )
    assert errors == []
    assert [_lockfile_path(_paired_dependency(m)) for m in matches] == [
        "lib/build.gradle.kts"
    ]
    assert len({m.match_based_id for m in matches}) == 1
    assert not already_reachable(Path("lib2/build.gradle.kts"), guava_lib2.value[0])


@pytest.mark.quick
def test_reachable_gradle_repeated_package_still_visits_its_children(
    tmp_path, monkeypatch
):
    # app -> lib -> shared:1 and app -> lib2 -> shared:1 -> guava. Both copies
    # of shared must be traversed even though only one is reported, or the
    # guava that only lib2's copy pulls in is never found.
    _layout(tmp_path, monkeypatch, MODULE_FILES + ["lib2/build.gradle.kts"])
    subproject = _gradle_subproject(
        [
            _gradle_dep(
                "reachable:lib",
                "unspecified",
                build_file="app/build.gradle.kts",
                direct=True,
                children=[("com.example:shared", "1", "lib/build.gradle.kts")],
            ),
            _gradle_dep(
                "reachable:lib2",
                "unspecified",
                build_file="app/build.gradle.kts",
                direct=True,
                children=[("com.example:shared", "1", "lib2/build.gradle.kts")],
            ),
            _gradle_dep(
                "com.example:shared",
                "1",
                build_file="lib/build.gradle.kts",
                direct=True,
            ),
            _gradle_dep(
                "com.example:shared",
                "1",
                build_file="lib2/build.gradle.kts",
                direct=True,
                children=[(GUAVA, GUAVA_VERSION, "lib2/build.gradle.kts")],
            ),
            _gradle_dep(
                GUAVA, GUAVA_VERSION, build_file="lib2/build.gradle.kts", direct=False
            ),
        ]
    )
    findings = _reachable(subproject, ["app/src/main/java/App.java"])
    assert [f[:3] for f in findings] == [
        ("app/src/main/java/App.java", "lib2/build.gradle.kts", "transitive")
    ]


@pytest.mark.quick
def test_reachable_gradle_sbom_backed_subproject_keeps_package_level(
    tmp_path, monkeypatch
):
    # A Gradle build resolved from an SBOM is not reported per module, so the
    # index must not be built for it (this used to fail an assertion) and the
    # pairing stays package-level with the flag on or off.
    _layout(tmp_path, monkeypatch, MODULE_FILES)
    manifest = Manifest(ManifestKind(out.BuildGradleKts()), Fpath("build.gradle.kts"))
    sbom = out.Sbom(
        kind=out.SbomKind(out.CycloneDXJson()),
        path=Fpath("bom.json"),
        is_ephemeral=False,
    )
    subproject = ResolvedSubproject(
        info=Subproject(
            root_dir=Fpath("."),
            dependency_source=DependencySource(
                out.AuxillarySBOM((sbom, DependencySource(out.ManifestOnly(manifest))))
            ),
            ecosystem=Ecosystem(Maven()),
        ),
        errors=[],
        resolution_method=ResolutionMethod(out.SbomParsing()),
        resolved_dependencies=from_resolved_dependencies(
            [_gradle_dep(GUAVA, GUAVA_VERSION, build_file="bom.json", direct=True)]
        ),
        ecosystem=Ecosystem(Maven()),
    )
    for flag in (True, False):
        findings = _reachable(
            subproject, ["app/src/main/java/App.java"], gradle_module_attribution=flag
        )
        assert [f[:3] for f in findings] == [
            ("app/src/main/java/App.java", "bom.json", "direct")
        ]


@pytest.mark.quick
def test_reachable_gradle_project_dependency_child_without_build_file(
    tmp_path, monkeypatch
):
    # Older resolver output does not say which module a child comes from. With
    # no copy in app itself, the copies in other modules are interchangeable,
    # so the first one reported is used and there is still one finding.
    _layout(tmp_path, monkeypatch, PROJECT_DEPENDENCY_FILES)
    subproject = _gradle_subproject(_project_dependency_deps(None))
    findings = _reachable(subproject, ["app/src/main/java/App.java"])
    assert [f[1] for f in findings] == ["lib/build.gradle.kts"]


@pytest.mark.quick
def test_reachable_gradle_flag_off_keeps_package_level_pairing(tmp_path, monkeypatch):
    # Without the flag the resolver reports one package-level entry at the root
    # manifest. Nested build files on disk must not change the pairing.
    _layout(tmp_path, monkeypatch, MODULE_FILES)
    subproject = _gradle_subproject(
        [
            _gradle_dep(
                GUAVA, GUAVA_VERSION, build_file="settings.gradle.kts", direct=True
            )
        ]
    )
    findings = _reachable(
        subproject, ["app/src/main/java/App.java"], gradle_module_attribution=False
    )
    assert [f[:3] for f in findings] == [
        ("app/src/main/java/App.java", "settings.gradle.kts", "direct")
    ]


@pytest.mark.quick
def test_reachable_non_gradle_subproject_ignores_module_scoping(
    sample_subproject, tmp_path, monkeypatch
):
    _layout(tmp_path, monkeypatch, ["pkg/app.py"])
    rule = Rule.from_json(
        {
            "id": "py-sca",
            "pattern": "bad()",
            "r2c-internal-project-depends-on": {
                "namespace": "pypi",
                "package": "requests",
                "version": ">=2.28.0",
            },
            "message": "oh no",
            "languages": ["python"],
            "severity": "WARNING",
        }
    )
    index = SubprojectDependencyIndex.from_subproject(sample_subproject)
    results = []
    for flag in (False, True):
        matches, errors, _ = generate_reachable_sca_findings(
            [_code_match("pkg/app.py")],
            rule,
            {Ecosystem(Pypi()): [(sample_subproject, index)]},
            gradle_module_attribution=flag,
        )
        assert errors == []
        results.append([_paired_dependency(m).version for m in matches])
    assert results == [["2.28.1"], ["2.28.1"]]
