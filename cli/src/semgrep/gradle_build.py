# Copyright (c) 2026 Semgrep Inc.
# This library is free software under the GNU Lesser General Public License 2.1.
"""Evaluated Gradle build ownership for Python discovery and source attribution."""
import json
import os
import shutil
import subprocess
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Mapping
from typing import Sequence

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.matchers.gradle import GradleMatcher
from semgrep.error import SemgrepError
from semgrep.git import get_git_root_path
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


@dataclass(frozen=True)
class GradleProject:
    logical_path: str
    project_dir: Path
    build_file: Path


@dataclass(frozen=True)
class GradleSubproject(out.Subproject):
    # Boundaries include independent and ambiguous builds, even if unresolved.
    ownership: tuple[tuple[Path, Path | None], ...] = ()
    checkout_dir: Path | None = None
    build_inputs: tuple[Path, ...] = ()
    absorbed_manifests: tuple[Path, ...] = ()
    projects: tuple[GradleProject, ...] = ()


_INIT_SCRIPT = """
import groovy.json.JsonOutput
def owningSettingsDir = null
gradle.settingsEvaluated { settings ->
    if (settings.settingsDir.canonicalPath == System.getProperty('semgrep.discovery.root')) {
        owningSettingsDir = settings.settingsDir.canonicalPath
    }
}
gradle.projectsLoaded { current ->
    if (owningSettingsDir != null) {
        def projects = current.rootProject.allprojects.collect { project ->
            [logical_path: project.path, project_dir: project.projectDir.canonicalPath,
             build_file: project.buildFile.canonicalPath]
        }
        new File(System.getProperty('semgrep.discovery.output')).text =
            JsonOutput.toJson([settings_dir: owningSettingsDir, projects: projects])
    }
}
"""


def evaluate_gradle_projects(
    root: Path, local_build_env: Mapping[str, str], allowed_roots: Sequence[Path] = ()
) -> tuple[GradleProject, ...]:
    """Evaluate a real settings model with the same working directory as resolution."""
    root = root.resolve()
    allowed_roots = tuple(path.resolve() for path in allowed_roots) or (root,)
    wrapper = next(
        (
            p / "gradlew"
            for p in (root, *root.parents)
            if (p / "gradlew").is_file()
            and any(
                (p / "gradlew").resolve().is_relative_to(base) for base in allowed_roots
            )
        ),
        None,
    )
    executable = shutil.which("gradle", path=local_build_env.get("PATH"))
    command = ["sh", str(wrapper)] if wrapper else ([executable] if executable else [])
    if not command:
        raise ValueError("No Gradle wrapper or executable for settings model")
    with tempfile.TemporaryDirectory(prefix="semgrep-gradle-") as tmp:
        init = Path(tmp) / "ownership.gradle"
        output = Path(tmp) / "projects.json"
        init.write_text(_INIT_SCRIPT)
        result = subprocess.run(
            command
            + [
                "--no-daemon",
                "--console=plain",
                "--init-script",
                str(init),
                "-Dsemgrep.discovery.root=" + str(root),
                "-Dsemgrep.discovery.output=" + str(output),
                "help",
            ],
            cwd=root,
            env={**os.environ, **local_build_env},
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            timeout=180,
        )
        if result.returncode != 0:
            raise ValueError("Gradle settings evaluation failed")
        data = json.loads(output.read_text())
    if not isinstance(data, dict) or data.get("settings_dir") != str(root):
        raise ValueError("Gradle settings context does not match owning build")
    descriptors = data.get("projects")
    if not isinstance(descriptors, list) or not descriptors:
        raise ValueError("Invalid Gradle project descriptors")
    projects = []
    for project in descriptors:
        if not isinstance(project, dict) or any(
            not isinstance(project.get(key), str)
            for key in ("logical_path", "project_dir", "build_file")
        ):
            raise ValueError("Invalid Gradle project descriptor")
        directory = Path(project["project_dir"])
        build_file = Path(project["build_file"])
        if not directory.is_absolute() or not build_file.is_absolute():
            raise ValueError("Gradle descriptor paths must be absolute")
        projects.append(
            GradleProject(
                project["logical_path"], directory.resolve(), build_file.resolve()
            )
        )
    paths = [project.logical_path for project in projects]
    if (
        paths.count(":") != 1
        or len(set(paths)) != len(paths)
        or any(
            not path.startswith(":")
            or (path != ":" and any(not part for part in path[1:].split(":")))
            for path in paths
        )
    ):
        raise ValueError("Incomplete or ambiguous Gradle logical project paths")
    if any(
        (path.rpartition(":")[0] or ":") not in paths for path in paths if path != ":"
    ):
        raise ValueError("Incomplete Gradle project hierarchy")
    if (
        next(project for project in projects if project.logical_path == ":").project_dir
        != root
    ):
        raise ValueError(
            "Relocated Gradle root project is not supported by owning-build discovery"
        )
    return tuple(projects)


def discover_gradle_ownership(
    subprojects: Sequence[out.Subproject],
    dependency_files: frozenset[Path],
    scanning_roots: Sequence[Path],
    local_build_env: Mapping[str, str],
) -> list[out.Subproject]:
    """Group uniquely mapped leaves without merging independent settings/lockfile builds."""
    allowed = tuple(path.resolve() for path in scanning_roots if path.is_dir())
    if not allowed:
        try:
            checkout_root = get_git_root_path().resolve()
        except (SemgrepError, OSError, subprocess.SubprocessError):
            return list(subprojects)
        if not all(
            path.resolve().is_relative_to(checkout_root) for path in scanning_roots
        ):
            return list(subprojects)
        allowed = (checkout_root,)
    settings = {
        path.parent.resolve(): path
        for path in dependency_files
        if path.name in GradleMatcher.SETTINGS_FILENAMES
    }
    matcher = GradleMatcher()
    gradle_subprojects = [
        sub
        for sub in subprojects
        if sub.ecosystem == matcher.ECOSYSTEM
        and any(matcher.is_match(path) for path in _source_files(sub.dependency_source))
    ]
    gradle = {Path(sub.root_dir.value).resolve(): sub for sub in gradle_subprojects}
    gradle_ids = {id(sub) for sub in gradle_subprojects}
    protected = {
        root
        for root, sub in gradle.items()
        if root in settings
        or not isinstance(sub.dependency_source.value, out.ManifestOnly)
    }
    models: dict[Path, tuple[GradleProject, ...]] = {}
    for root in sorted(settings.keys() & gradle.keys()):
        if not isinstance(gradle[root].dependency_source.value, out.ManifestOnly):
            continue
        try:
            projects = evaluate_gradle_projects(root, local_build_env, allowed)
            if any(
                not any(
                    project.project_dir.is_relative_to(base)
                    and project.build_file.is_relative_to(base)
                    for base in allowed
                )
                for project in projects
            ):
                raise ValueError("Gradle descriptor escapes scan directories")
            models[root] = projects
        except (OSError, ValueError, subprocess.SubprocessError):
            logger.warning(
                "Could not evaluate Gradle project ownership; retaining existing dependency sources at %s",
                root,
            )
    if not models:
        return list(subprojects)

    independent = {
        Path(sub.root_dir.value).resolve()
        for sub in subprojects
        if sub.ecosystem == GradleMatcher.ECOSYSTEM and id(sub) not in gradle_ids
    }
    protected.update(independent)
    claims: dict[Path, set[Path]] = {}
    for owner, projects in models.items():
        for project in projects:
            directory = project.project_dir
            if directory != owner and directory in protected:
                continue
            claims.setdefault(directory, set()).add(owner)
    absorbed = {
        directory: next(iter(owners))
        for directory, owners in claims.items()
        if len(owners) == 1
        and directory in gradle
        and directory not in protected
        and sum(
            path.parent.resolve() == directory and path.name in matcher.BUILD_FILENAMES
            for path in dependency_files
        )
        == 1
        and any(
            project.project_dir == directory
            and project.build_file
            in {
                path.resolve()
                for path in _source_files(gradle[directory].dependency_source)
            }
            for project in models[next(iter(owners))]
        )
    }
    boundaries: dict[Path, Path | None] = {
        root: absorbed.get(root, root) for root in gradle.keys() | independent
    }
    for directory, owners in claims.items():
        if directory not in protected:
            if len(owners) > 1:
                logger.warning(
                    "Ambiguous Gradle project ownership; retaining existing dependency sources at %s",
                    directory,
                )
                boundaries[directory] = None
            elif directory not in gradle or directory in absorbed:
                boundaries[directory] = next(iter(owners))
    ownership = tuple(
        sorted(boundaries.items(), key=lambda item: len(item[0].parts), reverse=True)
    )
    result = []
    for sub in subprojects:
        root = Path(sub.root_dir.value).resolve()
        if id(sub) not in gradle_ids:
            result.append(sub)
            continue
        if root in absorbed and absorbed[root] != root:
            continue
        projects = tuple(
            project
            for project in models.get(root, ())
            if boundaries.get(project.project_dir) == root
        )
        absorbed_manifests = tuple(
            sorted(
                path.resolve()
                for directory, owner in absorbed.items()
                if owner == root and directory != root
                for path in _source_files(gradle[directory].dependency_source)
            )
        )
        for project in projects:
            logger.debug(
                "Gradle build %s owns project %s (directory %s, build file %s)",
                root,
                project.logical_path,
                project.project_dir,
                project.build_file,
            )
        for manifest in absorbed_manifests:
            logger.debug("Gradle build %s absorbed manifest %s", root, manifest)
        inputs = tuple(
            Path(os.path.relpath(path, Path.cwd()))
            for path in sorted(
                {settings[root].resolve()} if root in settings else set()
            )
            + sorted({project.build_file for project in projects})
        )
        result.append(
            GradleSubproject(
                root_dir=sub.root_dir,
                ecosystem=sub.ecosystem,
                dependency_source=sub.dependency_source,
                ownership=ownership,
                checkout_dir=Path.cwd().resolve(),
                build_inputs=inputs,
                absorbed_manifests=absorbed_manifests,
                projects=projects,
            )
        )
    return result


def _source_files(source: out.DependencySource) -> list[Path]:
    ds = source.value
    if isinstance(ds, (out.ManifestOnly, out.LockfileOnly)):
        return [Path(ds.value.path.value)]
    if isinstance(ds, out.ManifestLockfile):
        manifest, lockfile = ds.value
        return [Path(manifest.path.value), Path(lockfile.path.value)]
    return []
