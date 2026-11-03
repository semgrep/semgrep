# Copyright (c) 2026 Semgrep Inc.
# This library is free software under the GNU Lesser General Public License 2.1.
from dataclasses import replace
from pathlib import Path
from types import SimpleNamespace
from typing import Any
from typing import cast
from unittest.mock import patch

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.matchers.gradle import GradleMatcher
from semgrep.gradle_build import discover_gradle_ownership
from semgrep.gradle_build import GradleProject
from semgrep.gradle_build import GradleSubproject
from semgrep.resolve_subprojects import filter_changed_subprojects
from semgrep.resolve_subprojects import find_subprojects
from semgrep.subproject import ClosestSubprojectFinder
from semgrep.subproject import find_closest_resolved_subproject
from semgrep.subproject import from_resolved_dependencies
from semgrep.target_manager import TargetManager
from semgrep.types import fake_target_of_path

MAVEN = out.Ecosystem(out.Maven())


def sources(tmp_path, *names):
    paths = []
    for name in names:
        path = tmp_path / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.touch()
        paths.append(path)
    return frozenset(fake_target_of_path(path) for path in paths)


def discover(tmp_path, files, model):
    original, _ = GradleMatcher().make_subprojects(files)
    with patch("semgrep.gradle_build.evaluate_gradle_projects", side_effect=model):
        result = discover_gradle_ownership(
            original, frozenset(t.fpath for t in files), [tmp_path], {}
        )
    return original, result


def root_of(sub):
    return Path(sub.root_dir.value)


def resolved(sub):
    return out.ResolvedSubproject(
        info=sub,
        ecosystem=MAVEN,
        resolution_method=out.ResolutionMethod(out.DynamicResolution()),
        resolved_dependencies=from_resolved_dependencies([]),
        errors=[],
    )


@pytest.mark.quick
def test_mapped_sources_inputs_and_independent_boundaries(tmp_path):
    files = sources(
        tmp_path,
        "root/settings.gradle.kts",
        "root/build.gradle.kts",
        "leaf/build.gradle.kts",
        "leaf/independent/settings.gradle.kts",
        "leaf/independent/build.gradle.kts",
        "leaf/locked/build.gradle.kts",
        "leaf/locked/gradle.lockfile",
        "other/settings.gradle.kts",
        "other/build.gradle.kts",
    )
    root, leaf = tmp_path / "root", tmp_path / "leaf"

    def model(path, env, *allowed):
        projects = [GradleProject(":", path, path / "build.gradle.kts")]
        if path == root:
            projects.append(GradleProject(":logical", leaf, leaf / "build.gradle.kts"))
        return tuple(projects)

    original, grouped = discover(tmp_path, files, model)
    assert len(grouped) == len(original) - 1
    finder = ClosestSubprojectFinder(grouped)
    owner = finder.find_closest_subproject(
        fake_target_of_path(leaf / "src/A.java"), MAVEN
    )
    assert owner is not None and root_of(owner) == root
    assert isinstance(owner, GradleSubproject)
    assert owner.absorbed_manifests == (leaf / "build.gradle.kts",)
    assert (
        root_of(
            finder.find_closest_subproject(
                fake_target_of_path(leaf / "independent/src/B.java"), MAVEN
            )
        )
        == leaf / "independent"
    )
    assert (
        root_of(
            finder.find_closest_subproject(
                fake_target_of_path(leaf / "locked/src/C.java"), MAVEN
            )
        )
        == leaf / "locked"
    )
    assert (
        finder.find_closest_subproject(
            fake_target_of_path(tmp_path / "unrelated/src/D.java"), MAVEN
        )
        is None
    )
    resolved_owner = find_closest_resolved_subproject(
        leaf / "src/A.java", MAVEN, [resolved(owner)]
    )
    assert resolved_owner is not None and resolved_owner.info is owner
    assert (
        find_closest_resolved_subproject(
            leaf / "independent/src/B.java", MAVEN, [resolved(owner)]
        )
        is None
    )
    assert isinstance(
        replace(owner, dependency_source=owner.dependency_source), GradleSubproject
    )
    assert set(owner.to_json()) == {"root_dir", "ecosystem", "dependency_source"}
    manager = SimpleNamespace(
        get_all_dependency_source_files=lambda **kw: frozenset(
            [fake_target_of_path(leaf / "build.gradle.kts")]
        ),
        baseline_handler=None,
        get_all_files=lambda **kw: frozenset(),
    )
    relevant, skipped = filter_changed_subprojects(
        cast(TargetManager, manager), [], grouped
    )
    assert [root_of(sub) for sub in relevant] == [root]
    assert len(skipped) == len(grouped) - 1


@pytest.mark.quick
def test_ambiguity_preserves_leaf_and_blocks_aggregate_attribution(tmp_path):
    files = sources(
        tmp_path,
        "a/settings.gradle.kts",
        "a/build.gradle.kts",
        "b/settings.gradle.kts",
        "b/build.gradle.kts",
        "leaf/build.gradle.kts",
    )
    leaf = tmp_path / "leaf"

    def model(path, env, *allowed):
        return (
            GradleProject(":", path, path / "build.gradle.kts"),
            GradleProject(":mapped", leaf, leaf / "build.gradle.kts"),
        )

    original, grouped = discover(tmp_path, files, model)
    assert len(grouped) == len(original)
    assert (
        ClosestSubprojectFinder(grouped).find_closest_subproject(
            fake_target_of_path(leaf / "src/A.java"), MAVEN
        )
        is None
    )


@pytest.mark.quick
def test_failed_model_and_outside_paths_preserve_discovery(tmp_path):
    files = sources(
        tmp_path,
        "root/settings.gradle.kts",
        "root/build.gradle.kts",
        "leaf/build.gradle.kts",
    )
    original, grouped = discover(tmp_path, files, ValueError("model failed"))
    assert original == grouped
    original, grouped = discover(
        tmp_path,
        files,
        lambda root, env, *allowed: (
            GradleProject(":", tmp_path.parent, tmp_path.parent / "build.gradle"),
        ),
    )
    assert original == grouped


@pytest.mark.quick
@pytest.mark.parametrize("allow,precise", [(False, False), (False, True), (True, True)])
def test_no_model_execution_without_default_local_build_permission(
    tmp_path, allow, precise
):
    files = sources(
        tmp_path,
        "root/settings.gradle.kts",
        "root/build.gradle.kts",
        "leaf/build.gradle.kts",
    )
    manager = SimpleNamespace(
        get_all_dependency_source_files=lambda **kw: files,
        scanning_root_strings=frozenset([tmp_path]),
    )
    with patch(
        "semgrep.resolve_subprojects.discover_gradle_ownership"
    ) as discover_model:
        projects, _ = find_subprojects(
            cast(TargetManager, manager),
            True,
            [],
            allow_local_builds=allow,
            gradle_module_attribution=precise,
        )
    discover_model.assert_not_called()
    assert len(projects) == 2


@pytest.mark.quick
def test_custom_build_filename_and_overlapping_project_directories(tmp_path):
    files = sources(tmp_path, "root/settings.gradle.kts", "root/build.gradle.kts")
    leaf = tmp_path / "mapped"
    leaf.mkdir()
    (leaf / "custom.gradle.kts").touch()
    nested = leaf / "nested"
    nested.mkdir()
    (nested / "build.gradle.kts").touch()
    other = tmp_path / "other"
    other.mkdir()
    (other / "build.gradle.kts").touch()
    (other / "settings.gradle.kts").touch()
    files |= frozenset(
        fake_target_of_path(path)
        for path in [other / "build.gradle.kts", other / "settings.gradle.kts"]
    )

    def model(root, env, *allowed):
        projects = [GradleProject(":", root, root / "build.gradle.kts")]
        projects.append(
            GradleProject(
                ":custom",
                leaf if root.name == "root" else nested,
                (
                    leaf / "custom.gradle.kts"
                    if root.name == "root"
                    else nested / "build.gradle.kts"
                ),
            )
        )
        return tuple(projects)

    _, grouped = discover(tmp_path, files, model)
    finder = ClosestSubprojectFinder(grouped)
    assert (
        root_of(
            finder.find_closest_subproject(
                fake_target_of_path(leaf / "src/A.java"), MAVEN
            )
        )
        == tmp_path / "root"
    )
    assert (
        root_of(
            finder.find_closest_subproject(
                fake_target_of_path(nested / "src/A.java"), MAVEN
            )
        )
        == other
    )


@pytest.mark.quick
def test_custom_filename_does_not_absorb_unrelated_manifest(tmp_path):
    files = sources(
        tmp_path,
        "root/settings.gradle.kts",
        "root/build.gradle.kts",
        "leaf/build.gradle.kts",
    )
    leaf = tmp_path / "leaf"
    (leaf / "custom.gradle.kts").touch()
    original, grouped = discover(
        tmp_path,
        files,
        lambda root, env, *allowed: (
            GradleProject(":", root, root / "build.gradle.kts"),
            GradleProject(":custom", leaf, leaf / "custom.gradle.kts"),
        ),
    )
    assert len(original) == len(grouped)
    assert (
        root_of(
            ClosestSubprojectFinder(grouped).find_closest_subproject(
                fake_target_of_path(leaf / "src/A.java"), MAVEN
            )
        )
        == leaf
    )


@pytest.mark.quick
def test_project_without_build_script_still_owns_source_and_custom_change_is_relevant(
    tmp_path,
):
    files = sources(tmp_path, "root/settings.gradle.kts", "root/build.gradle.kts")
    leaf = tmp_path / "empty"
    leaf.mkdir()
    custom = tmp_path / "custom"
    custom.mkdir()
    build = custom / "custom.gradle.kts"
    build.touch()
    _, grouped = discover(
        tmp_path,
        files,
        lambda root, env, *allowed: (
            GradleProject(":", root, root / "build.gradle.kts"),
            GradleProject(":empty", leaf, leaf / "build.gradle"),
            GradleProject(":custom", custom, build),
        ),
    )
    owner = ClosestSubprojectFinder(grouped).find_closest_subproject(
        fake_target_of_path(leaf / "src/A.java"), MAVEN
    )
    assert root_of(owner) == tmp_path / "root"
    manager = SimpleNamespace(
        get_all_dependency_source_files=lambda **kw: frozenset(),
        baseline_handler=None,
        get_all_files=lambda **kw: frozenset([fake_target_of_path(build)]),
    )
    relevant, skipped = filter_changed_subprojects(
        cast(TargetManager, manager), [], grouped
    )
    assert relevant == grouped and skipped == []


@pytest.mark.quick
def test_nested_maven_build_keeps_ownership_even_when_unresolved(tmp_path):
    files = sources(
        tmp_path,
        "root/settings.gradle.kts",
        "root/build.gradle.kts",
        "leaf/build.gradle.kts",
    )
    leaf = tmp_path / "leaf"
    nested = leaf / "maven"
    pom = out.Subproject(
        root_dir=out.Fpath(str(nested)),
        ecosystem=MAVEN,
        dependency_source=out.DependencySource(
            out.ManifestOnly(
                out.Manifest(
                    out.ManifestKind(out.PomXml()), out.Fpath(str(nested / "pom.xml"))
                )
            )
        ),
    )
    original, _ = GradleMatcher().make_subprojects(files)
    with patch(
        "semgrep.gradle_build.evaluate_gradle_projects",
        return_value=(
            GradleProject(":", tmp_path / "root", tmp_path / "root/build.gradle.kts"),
            GradleProject(":leaf", leaf, leaf / "build.gradle.kts"),
        ),
    ):
        grouped = discover_gradle_ownership(
            [*original, pom], frozenset(t.fpath for t in files), [tmp_path], {}
        )
    assert (
        root_of(
            ClosestSubprojectFinder(grouped).find_closest_subproject(
                fake_target_of_path(nested / "src/A.java"), MAVEN
            )
        )
        == nested
    )
    owner = next(sub for sub in grouped if root_of(sub) == tmp_path / "root")
    assert (
        find_closest_resolved_subproject(
            nested / "src/A.java", MAVEN, [resolved(owner)]
        )
        is None
    )


@pytest.mark.quick
def test_co_located_other_ecosystem_is_never_absorbed(tmp_path):
    files = sources(
        tmp_path,
        "root/settings.gradle.kts",
        "root/build.gradle.kts",
        "leaf/build.gradle.kts",
    )
    leaf = tmp_path / "leaf"
    npm = out.Subproject(
        root_dir=out.Fpath(str(leaf)),
        ecosystem=out.Ecosystem(out.Npm()),
        dependency_source=out.DependencySource(
            out.ManifestOnly(
                out.Manifest(
                    out.ManifestKind(out.PackageJson()),
                    out.Fpath(str(leaf / "package.json")),
                )
            )
        ),
    )
    original, _ = GradleMatcher().make_subprojects(files)
    with patch(
        "semgrep.gradle_build.evaluate_gradle_projects",
        return_value=(
            GradleProject(":", tmp_path / "root", tmp_path / "root/build.gradle.kts"),
            GradleProject(":leaf", leaf, leaf / "build.gradle.kts"),
        ),
    ):
        grouped = discover_gradle_ownership(
            [*original, npm], frozenset(t.fpath for t in files), [tmp_path], {}
        )
    assert npm in grouped


@pytest.mark.quick
def test_checkout_ownership_survives_head_baseline_cwd_switch(tmp_path, monkeypatch):
    captured = []
    for checkout in ("head", "baseline"):
        directory = tmp_path / checkout
        module = "module"
        files = sources(
            directory,
            "root/settings.gradle.kts",
            "root/build.gradle.kts",
            f"{module}/build.gradle.kts",
        )
        monkeypatch.chdir(directory)
        relative = frozenset(
            fake_target_of_path(target.fpath.relative_to(directory)) for target in files
        )
        _, grouped = discover(
            directory,
            relative,
            lambda root, env, *allowed: (
                GradleProject(":", root, root / "build.gradle.kts"),
                GradleProject(
                    ":" + checkout,
                    directory / module,
                    directory / module / "build.gradle.kts",
                ),
            ),
        )
        owner = grouped[0]
        graph = replace(
            resolved(owner),
            resolved_dependencies=from_resolved_dependencies(
                [
                    out.ResolvedDependency(
                        (
                            out.FoundDependency(
                                package="com.example:widget",
                                version="1.0" if checkout == "head" else "2.0",
                                ecosystem=MAVEN,
                                allowed_hashes={},
                                transitivity=out.DependencyKind(out.Direct()),
                            ),
                            None,
                        )
                    )
                ]
            ),
        )
        captured.append((directory, owner, graph, ClosestSubprojectFinder(grouped)))
    monkeypatch.chdir(tmp_path / "head")
    for directory, owner, graph, finder in captured:
        assert (
            finder.find_closest_subproject(
                fake_target_of_path(Path("module/src/A.java")), MAVEN
            )
            is owner
        )
        assert (
            finder.find_closest_subproject(
                fake_target_of_path(directory / "module/src/A.java"), MAVEN
            )
            is owner
        )
        assert (
            find_closest_resolved_subproject(Path("module/src/A.java"), MAVEN, [graph])
            is graph
        )


@pytest.mark.quick
def test_deleted_custom_build_input_keeps_root_selected(tmp_path):
    files = sources(tmp_path, "root/settings.gradle.kts", "root/build.gradle.kts")
    leaf = tmp_path / "empty"
    leaf.mkdir()
    build = leaf / "custom.gradle.kts"
    _, grouped = discover(
        tmp_path,
        files,
        lambda root, env, *allowed: (
            GradleProject(":", root, root / "build.gradle.kts"),
            GradleProject(":empty", leaf, build),
        ),
    )
    manager = SimpleNamespace(
        get_all_dependency_source_files=lambda **kw: frozenset(),
        get_all_files=lambda **kw: frozenset(),
        baseline_handler=SimpleNamespace(
            status=SimpleNamespace(added=[], modified=[], removed=[build])
        ),
    )
    relevant, skipped = filter_changed_subprojects(
        cast(TargetManager, manager), [], grouped
    )
    assert relevant == grouped and skipped == []


@pytest.mark.quick
def test_symbol_file_mapping_uses_shared_owner_when_language_is_supported(tmp_path):
    from semgrep.symbol_analysis import build_subproject_file_mapping

    files = sources(
        tmp_path,
        "root/settings.gradle.kts",
        "root/build.gradle.kts",
        "leaf/build.gradle.kts",
    )
    leaf = tmp_path / "leaf"
    _, grouped = discover(
        tmp_path,
        files,
        lambda root, env, *allowed: (
            GradleProject(":", root, root / "build.gradle.kts"),
            GradleProject(":leaf", leaf, leaf / "build.gradle.kts"),
        ),
    )
    code = leaf / "src/A.java"
    manager = SimpleNamespace(
        get_files_for_language=lambda **kw: SimpleNamespace(
            kept=frozenset([fake_target_of_path(code)])
        )
    )
    # Maven symbol extraction is currently unsupported; exercise only the shared mapping contract.
    with patch(
        "semgrep.symbol_analysis._ecosystem_to_language",
        return_value=SimpleNamespace(all_languages=lambda: ["java"]),
    ):
        mapping = build_subproject_file_mapping(
            {MAVEN: [resolved(grouped[0])]}, cast(TargetManager, manager)
        )
    assert mapping == {(MAVEN, tmp_path / "root"): [code]}


@pytest.mark.quick
def test_precomputed_sbom_keeps_current_checkout_ownership(tmp_path):
    from semgrep.resolve_subprojects import attach_auxillary_sboms
    from semgrep.subproject import (
        DependencyResolutionConfig,
        generate_dependency_source_id,
    )

    files = sources(
        tmp_path,
        "root/settings.gradle.kts",
        "root/build.gradle.kts",
        "leaf/build.gradle.kts",
    )
    leaf = tmp_path / "leaf"
    _, grouped = discover(
        tmp_path,
        files,
        lambda root, env, *allowed: (
            GradleProject(":", root, root / "build.gradle.kts"),
            GradleProject(":leaf", leaf, leaf / "build.gradle.kts"),
        ),
    )
    sboms = tmp_path / "sboms"
    (sboms / "head").mkdir(parents=True)
    (
        sboms
        / "head"
        / (generate_dependency_source_id(grouped[0].dependency_source) + ".cdx.json")
    ).touch()
    wrapped = attach_auxillary_sboms(
        grouped,
        DependencyResolutionConfig(
            allow_local_builds=False,
            ptt_enabled=False,
            resolve_untargeted_subprojects=True,
            download_dependency_source_code=False,
            precomputed_dependencies_dir=sboms,
        ),
        [],
    )
    assert isinstance(wrapped[0], GradleSubproject)
    assert wrapped[0].ownership == grouped[0].ownership
    assert wrapped[0].checkout_dir == grouped[0].checkout_dir
    assert (
        root_of(
            ClosestSubprojectFinder(wrapped).find_closest_subproject(
                fake_target_of_path(leaf / "src/A.java"), MAVEN
            )
        )
        == tmp_path / "root"
    )


@pytest.mark.quick
def test_multiple_gradle_manifests_in_one_directory_are_not_silently_absorbed(tmp_path):
    files = sources(
        tmp_path,
        "root/settings.gradle.kts",
        "root/build.gradle.kts",
        "leaf/build.gradle",
        "leaf/build.gradle.kts",
    )
    leaf = tmp_path / "leaf"
    original, grouped = discover(
        tmp_path,
        files,
        lambda root, env, *allowed: (
            GradleProject(":", root, root / "build.gradle.kts"),
            GradleProject(":leaf", leaf, leaf / "build.gradle.kts"),
        ),
    )
    assert len(original) == len(grouped) == 2
    assert sum(root_of(sub) == leaf for sub in grouped) == 1


@pytest.mark.quick
@pytest.mark.parametrize(
    "invalid",
    [
        "missing-root",
        "duplicate-root",
        "duplicate-child",
        "wrong-settings",
        "relocated-root",
        "relative-path",
        "invalid-logical",
        "missing-parent",
    ],
)
def test_model_validation_rejects_incomplete_or_wrong_build_context(tmp_path, invalid):
    import json
    from semgrep.gradle_build import evaluate_gradle_projects

    root = tmp_path / "root"
    root.mkdir()
    (root / "gradlew").touch()
    project = dict(
        logical_path=":",
        project_dir=str(root),
        build_file=str(root / "custom.gradle.kts"),
    )
    child = dict(
        logical_path=":child",
        project_dir=str(tmp_path / "child"),
        build_file=str(tmp_path / "child/build.gradle.kts"),
    )
    payload: dict[str, Any] = dict(settings_dir=str(root), projects=[project, child])
    if invalid == "missing-root":
        payload["projects"] = [child]
    elif invalid == "duplicate-root":
        payload["projects"].append(project)
    elif invalid == "duplicate-child":
        payload["projects"].append(child)
    elif invalid == "wrong-settings":
        payload["settings_dir"] = str(tmp_path / "other")
    elif invalid == "relocated-root":
        project["project_dir"] = str(tmp_path / "relocated")
    elif invalid == "relative-path":
        child["project_dir"] = "child"
    elif invalid == "missing-parent":
        child["logical_path"] = ":missing:child"
    else:
        child["logical_path"] = "::child"

    def run(command, **kwargs):
        assert command[:2] == ["sh", str(root / "gradlew")]
        output = next(
            arg.split("=", 1)[1]
            for arg in command
            if arg.startswith("-Dsemgrep.discovery.output=")
        )
        Path(output).write_text(json.dumps(payload))
        return SimpleNamespace(returncode=0)

    with patch("semgrep.gradle_build.subprocess.run", side_effect=run), pytest.raises(
        ValueError
    ):
        evaluate_gradle_projects(root, {})


@pytest.mark.quick
def test_file_only_discovery_uses_verified_worktree_context(tmp_path):
    files = sources(
        tmp_path,
        "root/settings.gradle.kts",
        "root/build.gradle.kts",
        "leaf/build.gradle.kts",
    )
    root, leaf = tmp_path / "root", tmp_path / "leaf"
    original, _ = GradleMatcher().make_subprojects(files)
    with patch("semgrep.gradle_build.get_git_root_path", return_value=tmp_path), patch(
        "semgrep.gradle_build.evaluate_gradle_projects",
        return_value=(
            GradleProject(":", root, root / "build.gradle.kts"),
            GradleProject(":leaf", leaf, leaf / "build.gradle.kts"),
        ),
    ):
        grouped = discover_gradle_ownership(
            original, frozenset(t.fpath for t in files), [t.fpath for t in files], {}
        )
    assert len(grouped) == 1
    assert isinstance(grouped[0], GradleSubproject)
    assert grouped[0].absorbed_manifests == (leaf / "build.gradle.kts",)
