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
import subprocess
from pathlib import Path
from typing import List
from typing import Optional
from unittest.mock import patch

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.subproject_matchers import ExactLockfileManifestMatcher
from semdep.subproject_matchers import ExactManifestOnlyMatcher
from semdep.subproject_matchers import SubprojectMatcher
from semgrep.resolve_dependency_source import resolve_dependency_source
from semgrep.resolve_dependency_source import ResolveDependenciesRpcResult
from semgrep.resolve_subprojects import filter_subprojects_by_rule_ecosystems
from semgrep.resolve_subprojects import match_subprojects
from semgrep.rule import Rule
from semgrep.run_scan import resolve_dependencies
from semgrep.subproject import collect_skipped_subprojects
from semgrep.subproject import DependencyResolutionConfig
from semgrep.subproject import subproject_to_plan_output
from semgrep.target_manager import TargetManager
from semgrep.types import fake_targets_of_paths


@pytest.mark.quick
@pytest.mark.parametrize(
    ["file_paths", "matchers", "expected_subprojects"],
    [
        (
            # verify that when two matchers both look for the same files, only the first subproject includes the overlapping file.
            [
                Path("requirements.in"),
                Path("requirements.txt"),
                Path("requirements3.txt"),
            ],
            [
                ExactLockfileManifestMatcher(
                    lockfile_name="requirements.txt",
                    manifest_name="requirements.in",
                    lockfile_kind=out.LockfileKind(value=out.PipRequirementsTxt()),
                    manifest_kind=out.ManifestKind(value=out.RequirementsIn()),
                    ecosystem=out.Ecosystem(value=out.Pypi()),
                    make_manifest_only_subprojects=False,
                ),
                ExactLockfileManifestMatcher(
                    lockfile_name="requirements3.txt",
                    manifest_name="requirements.in",
                    lockfile_kind=out.LockfileKind(value=out.PipRequirementsTxt()),
                    manifest_kind=out.ManifestKind(value=out.RequirementsIn()),
                    ecosystem=out.Ecosystem(value=out.Pypi()),
                    make_manifest_only_subprojects=False,
                ),
            ],
            [
                out.Subproject(
                    root_dir=out.Fpath("."),
                    dependency_source=out.DependencySource(
                        out.ManifestLockfile(
                            (
                                out.Manifest(
                                    out.ManifestKind(value=out.RequirementsIn()),
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
                    root_dir=out.Fpath("."),
                    dependency_source=out.DependencySource(
                        out.LockfileOnly(
                            out.Lockfile(
                                out.LockfileKind(out.PipRequirementsTxt()),
                                out.Fpath("requirements3.txt"),
                            )
                        )
                    ),
                    ecosystem=out.Ecosystem(value=out.Pypi()),
                ),
            ],
        ),
        (
            # verify that we correctly use the second matcher when the first is a match for the manifest
            # but not for the lockfile.
            [
                Path("requirements.in"),
                Path("requirements3.txt"),
            ],
            [
                ExactLockfileManifestMatcher(
                    lockfile_name="requirements.txt",
                    manifest_name="requirements.in",
                    lockfile_kind=out.LockfileKind(value=out.PipRequirementsTxt()),
                    manifest_kind=out.ManifestKind(value=out.RequirementsIn()),
                    ecosystem=out.Ecosystem(value=out.Pypi()),
                    make_manifest_only_subprojects=False,
                ),
                ExactLockfileManifestMatcher(
                    lockfile_name="requirements3.txt",
                    manifest_name="requirements.in",
                    lockfile_kind=out.LockfileKind(value=out.PipRequirementsTxt()),
                    manifest_kind=out.ManifestKind(value=out.RequirementsIn()),
                    ecosystem=out.Ecosystem(value=out.Pypi()),
                    make_manifest_only_subprojects=False,
                ),
            ],
            [
                out.Subproject(
                    root_dir=out.Fpath("."),
                    dependency_source=out.DependencySource(
                        out.ManifestLockfile(
                            (
                                out.Manifest(
                                    out.ManifestKind(value=out.RequirementsIn()),
                                    out.Fpath("requirements.in"),
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
            ],
        ),
        (
            # verify that when one subproject contains another, both the parent and the child are found separately
            [
                Path("pom.xml"),
                Path("child-a/pom.xml"),
                Path("child-b/pom.xml"),
            ],
            [
                ExactManifestOnlyMatcher(
                    manifest_kind=out.ManifestKind(out.PomXml()),
                    manifest_name="pom.xml",
                    ecosystem=out.Ecosystem(value=out.Pypi()),
                )
            ],
            [
                out.Subproject(
                    root_dir=out.Fpath("."),
                    dependency_source=out.DependencySource(
                        out.ManifestOnly(
                            out.Manifest(
                                out.ManifestKind(out.PomXml()),
                                out.Fpath("pom.xml"),
                            )
                        )
                    ),
                    ecosystem=out.Ecosystem(value=out.Pypi()),
                ),
                out.Subproject(
                    root_dir=out.Fpath("child-a"),
                    dependency_source=out.DependencySource(
                        out.ManifestOnly(
                            out.Manifest(
                                out.ManifestKind(out.PomXml()),
                                out.Fpath("child-a/pom.xml"),
                            )
                        )
                    ),
                    ecosystem=out.Ecosystem(value=out.Pypi()),
                ),
                out.Subproject(
                    root_dir=out.Fpath("child-b"),
                    dependency_source=out.DependencySource(
                        out.ManifestOnly(
                            out.Manifest(
                                out.ManifestKind(out.PomXml()),
                                out.Fpath("child-b/pom.xml"),
                            )
                        )
                    ),
                    ecosystem=out.Ecosystem(value=out.Pypi()),
                ),
            ],
        ),
    ],
)
def test_find_subprojects(
    file_paths: List[Path],
    matchers: List[SubprojectMatcher],
    expected_subprojects: List[out.Subproject],
) -> None:
    result = match_subprojects(fake_targets_of_paths(file_paths), matchers)
    assert sorted(result, key=lambda s: str(s.root_dir)) == sorted(
        expected_subprojects, key=lambda s: str(s.root_dir)
    )


@pytest.mark.quick
def test_subproject_plan_output_fields() -> None:
    """subproject_to_plan_output produces the expected fields."""
    sub = out.Subproject(
        root_dir=out.Fpath("my-app"),
        dependency_source=out.DependencySource(
            out.ManifestOnly(
                out.Manifest(
                    out.ManifestKind(out.PomXml()),
                    out.Fpath("my-app/pom.xml"),
                )
            )
        ),
        ecosystem=out.Ecosystem(value=out.Maven()),
    )
    plan = subproject_to_plan_output(sub, True)
    assert plan.root_dir == out.Fpath("my-app")
    assert plan.resolution_planned is True
    assert len(plan.subproject_id) == 64  # SHA-256 hex digest


@pytest.mark.quick
def test_subproject_id_is_deterministic() -> None:
    """The same subproject always produces the same ID."""
    sub = out.Subproject(
        root_dir=out.Fpath("src"),
        dependency_source=out.DependencySource(
            out.ManifestLockfile(
                (
                    out.Manifest(
                        out.ManifestKind(out.PackageJson()),
                        out.Fpath("src/package.json"),
                    ),
                    out.Lockfile(
                        out.LockfileKind(out.NpmPackageLockJson()),
                        out.Fpath("src/package-lock.json"),
                    ),
                )
            )
        ),
        ecosystem=out.Ecosystem(value=out.Npm()),
    )
    id1 = subproject_to_plan_output(sub, True).subproject_id
    id2 = subproject_to_plan_output(sub, False).subproject_id
    assert id1 == id2  # resolution_planned doesn't affect ID


@pytest.mark.quick
def test_subproject_id_differs_for_different_paths() -> None:
    """Different dependency source paths produce different IDs."""
    sub_a = out.Subproject(
        root_dir=out.Fpath("a"),
        dependency_source=out.DependencySource(
            out.ManifestOnly(
                out.Manifest(
                    out.ManifestKind(out.BuildGradle()),
                    out.Fpath("a/build.gradle"),
                )
            )
        ),
        ecosystem=out.Ecosystem(value=out.Maven()),
    )
    sub_b = out.Subproject(
        root_dir=out.Fpath("b"),
        dependency_source=out.DependencySource(
            out.ManifestOnly(
                out.Manifest(
                    out.ManifestKind(out.BuildGradle()),
                    out.Fpath("b/build.gradle"),
                )
            )
        ),
        ecosystem=out.Ecosystem(value=out.Maven()),
    )
    id_a = subproject_to_plan_output(sub_a, True).subproject_id
    id_b = subproject_to_plan_output(sub_b, True).subproject_id
    assert id_a != id_b


def make_depends_on_rule(rule_id: str, namespace: str) -> Rule:
    return Rule(
        {
            "id": rule_id,
            "r2c-internal-project-depends-on": {
                "namespace": namespace,
                "package": "some-package",
                "version": ">=1.0.0",
            },
            "languages": ["python"],
            "patterns": ["pattern"],
        }
    )


def make_ecosystem_subproject(
    root_dir: str, ecosystem: Optional[out.Ecosystem]
) -> out.Subproject:
    return out.Subproject(
        root_dir=out.Fpath(root_dir),
        dependency_source=out.DependencySource(
            out.LockfileOnly(
                out.Lockfile(
                    # the lockfile kind is irrelevant to ecosystem filtering
                    out.LockfileKind(out.PipRequirementsTxt()),
                    out.Fpath(f"{root_dir}/requirements.txt"),
                )
            )
        ),
        ecosystem=ecosystem,
    )


@pytest.mark.quick
def test_filter_subprojects_by_rule_ecosystems() -> None:
    """Only subprojects in an ecosystem some rule evaluates are kept."""
    pypi_subproject = make_ecosystem_subproject("py", out.Ecosystem(out.Pypi()))
    npm_subproject = make_ecosystem_subproject("js", out.Ecosystem(out.Npm()))
    maven_subproject = make_ecosystem_subproject("java", out.Ecosystem(out.Maven()))

    relevant, irrelevant = filter_subprojects_by_rule_ecosystems(
        [
            make_depends_on_rule("rules.pypi-rule", "pypi"),
            make_depends_on_rule("rules.npm-rule", "npm"),
        ],
        [pypi_subproject, npm_subproject, maven_subproject],
    )

    assert relevant == [pypi_subproject, npm_subproject]
    assert [sub.info for sub in irrelevant] == [maven_subproject]
    assert irrelevant[0].reason == out.UnresolvedReason(out.UnresolvedSkipped())


@pytest.mark.quick
def test_filter_subprojects_by_rule_ecosystems__keeps_unknown_ecosystem() -> None:
    """
    Subprojects with no ecosystem are kept so that they are reported with the
    more precise "unsupported" reason during resolution.
    """
    unknown_subproject = make_ecosystem_subproject("mystery", None)

    relevant, irrelevant = filter_subprojects_by_rule_ecosystems(
        [make_depends_on_rule("rules.pypi-rule", "pypi")], [unknown_subproject]
    )

    assert relevant == [unknown_subproject]
    assert irrelevant == []


@pytest.mark.quick
def test_filter_subprojects_by_rule_ecosystems__keeps_precomputed_sbom() -> None:
    """
    A subproject with a precomputed SBOM is kept even when no rule evaluates its
    ecosystem: resolving it only means reading a file that is already on disk.
    """
    maven_subproject = make_ecosystem_subproject("java", out.Ecosystem(out.Maven()))
    with_sbom = out.Subproject(
        root_dir=maven_subproject.root_dir,
        dependency_source=out.DependencySource(
            out.AuxillarySBOM(
                (
                    out.Sbom(
                        kind=out.SbomKind(out.CycloneDXJson()),
                        is_ephemeral=True,
                        path=out.Fpath("java/sbom.cdx.json"),
                    ),
                    maven_subproject.dependency_source,
                )
            )
        ),
        ecosystem=maven_subproject.ecosystem,
    )

    relevant, irrelevant = filter_subprojects_by_rule_ecosystems(
        [make_depends_on_rule("rules.pypi-rule", "pypi")],
        [with_sbom, maven_subproject],
    )

    assert relevant == [with_sbom]
    assert [sub.info for sub in irrelevant] == [maven_subproject]


@pytest.mark.quick
def test_collect_skipped_subprojects_only_includes_skipped() -> None:
    """Only deliberately skipped subprojects are reported to the app."""
    skipped = out.UnresolvedSubproject(
        info=make_ecosystem_subproject("skipped", out.Ecosystem(out.Pypi())),
        reason=out.UnresolvedReason(out.UnresolvedSkipped()),
        errors=[],
    )
    failed = out.UnresolvedSubproject(
        info=make_ecosystem_subproject("failed", out.Ecosystem(out.Pypi())),
        reason=out.UnresolvedReason(out.UnresolvedFailed()),
        errors=[],
    )
    unsupported = out.UnresolvedSubproject(
        info=make_ecosystem_subproject("unsupported", None),
        reason=out.UnresolvedReason(out.UnresolvedUnsupported()),
        errors=[],
    )
    resolved = out.ResolvedSubproject(
        info=make_ecosystem_subproject("resolved", out.Ecosystem(out.Pypi())),
        resolution_method=out.ResolutionMethod(out.LockfileParsing()),
        ecosystem=out.Ecosystem(out.Pypi()),
        resolved_dependencies={},
        errors=[],
    )

    assert collect_skipped_subprojects([skipped, failed, unsupported, resolved]) == [
        out.SkippedSubproject(
            root_dir=out.Fpath("skipped"),
            dependency_sources=[
                out.DependencySourceFile(
                    kind=out.DependencySourceFileKind(
                        out.Lockfile_(out.LockfileKind(out.PipRequirementsTxt()))
                    ),
                    path=out.Fpath("skipped/requirements.txt"),
                )
            ],
        )
    ]


def make_restricted_resolution_config(
    restrict_resolution_to_rule_ecosystems: bool,
) -> DependencyResolutionConfig:
    return DependencyResolutionConfig(
        allow_local_builds=False,
        ptt_enabled=False,
        resolve_untargeted_subprojects=False,
        download_dependency_source_code=False,
        restrict_resolution_to_rule_ecosystems=restrict_resolution_to_rule_ecosystems,
    )


def make_repo_with_lockfile(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
    monkeypatch.chdir(tmp_path)
    subprocess.check_call(["git", "init"])
    subprocess.check_call(["git", "config", "user.email", "test@semgrep.com"])
    subprocess.check_call(["git", "config", "user.name", "Test"])
    Path("requirements.txt").touch()
    subprocess.check_call(["git", "add", "."])
    subprocess.check_call(["git", "commit", "-m", "first"])


@pytest.mark.kinda_slow
def test_restricted_resolution_reports_subprojects_without_dependency_aware_rules(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    """
    A partial scan whose rules include no dependency-aware rule must still
    discover subprojects and report them as skipped, so that the app does not
    read their absence from the reported dependencies as a removal.
    """
    make_repo_with_lockfile(monkeypatch, tmp_path)

    (_rules, _errors, _targets, all_subprojects, resolved) = resolve_dependencies(
        dependency_aware_rules=[],
        target_manager=TargetManager(scanning_root_strings=frozenset([Path(".")])),
        dependency_resolution_config=make_restricted_resolution_config(True),
    )

    assert resolved == {}
    assert [
        (sub.info.root_dir, sub.reason)
        for sub in all_subprojects
        if isinstance(sub, out.UnresolvedSubproject)
    ] == [(out.Fpath("."), out.UnresolvedReason(out.UnresolvedSkipped()))]
    assert collect_skipped_subprojects(all_subprojects) != []


@pytest.mark.kinda_slow
def test_unrestricted_resolution_skips_discovery_without_dependency_aware_rules(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    """Normal scans keep the fast path: no rules means no discovery at all."""
    make_repo_with_lockfile(monkeypatch, tmp_path)

    (_rules, _errors, _targets, all_subprojects, resolved) = resolve_dependencies(
        dependency_aware_rules=[],
        target_manager=TargetManager(scanning_root_strings=frozenset([Path(".")])),
        dependency_resolution_config=make_restricted_resolution_config(False),
    )

    assert resolved == {}
    assert all_subprojects == []


# Please don't use @patch because it can't be typechecked and makes refactoring
# particularly tricky.
@pytest.mark.quick
@patch("semgrep.resolve_dependency_source._resolve_dependencies_rpc")
def test_ptt_unconditionally_generates_dependency_graphs(
    mock_dynamic_resolve, tmp_path: Path
) -> None:
    manifest_file = open(tmp_path / "requirements.in", "w")
    manifest_file.write("requests==2.25.1")
    manifest_file.close()
    lockfile_file = open(tmp_path / "requirements.txt", "w")
    lockfile_file.write("requests==2.25.1")
    lockfile_file.close()

    mock_dynamic_resolve.return_value = ResolveDependenciesRpcResult(
        new_deps=[], new_errors=[], new_targets=[]
    )
    dep_source = out.DependencySource(
        out.ManifestLockfile(
            (
                out.Manifest(
                    out.ManifestKind(value=out.RequirementsIn()),
                    out.Fpath(str((tmp_path / "requirements.in"))),
                ),
                out.Lockfile(
                    out.LockfileKind(value=out.PipRequirementsTxt()),
                    out.Fpath(str(tmp_path / "requirements.txt")),
                ),
            )
        ),
    )

    res = resolve_dependency_source(
        dep_source, DependencyResolutionConfig(True, True, True, False)
    )
    assert not isinstance(res.deps, out.UnresolvedReason)
    assert res.deps[0] == out.ResolutionMethod(out.DynamicResolution())

    mock_dynamic_resolve.mock_assert_called_once_with(
        Path("requirements.txt"), out.ManifestKind(value=out.RequirementsIn())
    )


# Please don't use @patch because it can't be typechecked and makes refactoring
# particularly tricky.
@pytest.mark.quick
@patch("semdep.parsers.requirements.parse_requirements")
@patch("semgrep.resolve_dependency_source._resolve_dependencies_rpc")
def test_ptt_unconditional_graph_generation_falls_back_on_lockfile_parsing(
    mock_dynamic_resolve, mock_parse_requirements, tmp_path: Path
) -> None:
    manifest_file = open(tmp_path / "requirements.in", "w")
    manifest_file.write("requests==2.25.1")
    manifest_file.close()
    lockfile_file = open(tmp_path / "requirements.txt", "w")
    lockfile_file.write("requests==2.25.1")
    lockfile_file.close()

    mock_dynamic_resolve.return_value = ResolveDependenciesRpcResult(
        new_deps=None, new_errors=[], new_targets=[]
    )
    mock_parse_requirements.return_value = (
        [
            out.FoundDependency(
                package="requests",
                version="2.25.1",
                ecosystem=out.Ecosystem(value=out.Pypi()),
                allowed_hashes={},
                transitivity=out.DependencyKind(value=out.Direct()),
            )
        ],
        [],
    )

    dep_source = out.DependencySource(
        out.ManifestLockfile(
            (
                out.Manifest(
                    out.ManifestKind(value=out.RequirementsIn()),
                    out.Fpath(str((tmp_path / "requirements.in"))),
                ),
                out.Lockfile(
                    out.LockfileKind(value=out.PipRequirementsTxt()),
                    out.Fpath(str(tmp_path / "requirements.txt")),
                ),
            )
        ),
    )
    res = resolve_dependency_source(
        dep_source, DependencyResolutionConfig(True, True, True, False)
    )
    deps = res.deps
    assert not isinstance(deps, out.UnresolvedReason)
    assert deps[0] == out.ResolutionMethod(out.LockfileParsing())
    assert len(deps[1]) == 1
    assert deps[1][0].value[0].package == "requests"

    mock_parse_requirements.mock_assert_called_once_with(
        Path(tmp_path / "requirements.txt"), Path(tmp_path / "requirements.in")
    )
