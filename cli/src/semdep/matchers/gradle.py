from dataclasses import dataclass
from pathlib import Path
from typing import FrozenSet
from typing import List
from typing import Optional
from typing import Set
from typing import Tuple

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.matchers.base import SubprojectMatcher


@dataclass(frozen=True)
class GradleMatcher(SubprojectMatcher):
    """
    Matcher for gradle project root directories. Matches projects based on gradle.lockfile,
    settings.gradle, or build.gradle (for single-project builds only). build.gradle files
    that are encountered in a subdirectory of another gradle project are assumed to be part
    of the containing project unless they contain a gradle.lockfile or a settings.gradle.

    Does not support gradle directories that use `include` and `includeFlat` to customize
    the shape of the project directory: gradle.lockfile, settings.gradle, or build.gradle
    must be located at the root of the gradle project.

    For more information on the various shapes of gradle projects, see
    https://docs.gradle.org/current/userguide/intro_multi_project_builds.htm
    """

    BUILD_FILENAMES = ["build.gradle", "build.gradle.kts"]
    SETTINGS_FILENAMES = ["settings.gradle", "settings.gradle.kts"]
    LOCKFILE_FILENAME = "gradle.lockfile"
    ECOSYSTEM = out.Ecosystem(out.Maven())

    def is_match(self, path: Path) -> bool:
        return path.name in [
            *self.BUILD_FILENAMES,
            *self.SETTINGS_FILENAMES,
            self.LOCKFILE_FILENAME,
        ]

    def _lockfile_to_settings_and_build(
        self, lockfile_path: Path, candidates: FrozenSet[Path]
    ) -> Tuple[Optional[Path], Optional[Path]]:
        """
        Finds the corresponding settings.gradle and build.gradle files for the given
        lockfile, if they exist.

        Returns (settings_path, build_path) if each path exists in candidates.
        """
        possible_build_paths = [lockfile_path.parent / x for x in self.BUILD_FILENAMES]
        possible_settings_paths = [
            lockfile_path.parent / x for x in self.SETTINGS_FILENAMES
        ]

        build_path: Optional[Path] = None
        settings_path: Optional[Path] = None
        for possible_build_path in possible_build_paths:
            if possible_build_path in candidates:
                build_path = possible_build_path
                break
        for possible_settings_path in possible_settings_paths:
            if possible_settings_path in candidates:
                settings_path = possible_settings_path
                break
        return settings_path, build_path

    def _sort_source_files(
        self, dep_source_files: FrozenSet[Path]
    ) -> Tuple[Set[Path], Set[Path], Set[Path]]:
        """
        Classifies the provided source files as settings.gradle, build.gradle, and lockfiles

        Returns a tuple of (settings.gradle, build.gradle, lockfiles)
        """
        settings_files: Set[Path] = set()
        build_files: Set[Path] = set()
        lockfiles: Set[Path] = set()

        for path in dep_source_files:
            if path.name in self.BUILD_FILENAMES:
                build_files.add(path)
            elif path.name in self.SETTINGS_FILENAMES:
                settings_files.add(path)
            elif path.name == self.LOCKFILE_FILENAME:
                lockfiles.add(path)
        return settings_files, build_files, lockfiles

    def make_subprojects(
        self, dep_source_files: FrozenSet[Path]
    ) -> Tuple[List[out.Subproject], FrozenSet[Path]]:
        settings_files, build_files, lockfiles = self._sort_source_files(
            dep_source_files
        )

        subprojects: List[out.Subproject] = []

        # as we create each subproject, we will add its root directory here. We will use this later
        # to remove any build.gradle files that are already inside one of the subproject we have created
        root_dirs: Set[Path] = set()
        used_settings_paths: Set[Path] = set()
        used_build_paths: Set[Path] = set()

        # first, make subprojects from any lockfiles---any accompanying build.gradle and settings.gradle
        for lockfile_path in lockfiles:
            project_root = lockfile_path.parent
            root_dirs.add(project_root)
            settings_path, build_path = self._lockfile_to_settings_and_build(
                lockfile_path, dep_source_files
            )

            lockfile = out.Lockfile(
                kind=out.LockfileKind(out.GradleLockfile()),
                path=out.Fpath(str(lockfile_path)),
            )

            # track that these build and settings files are already accounted for
            if build_path is not None:
                used_build_paths.add(build_path)
            if settings_path is not None:
                used_settings_paths.add(settings_path)

            manifest: Optional[out.Manifest] = None
            if build_path is not None:
                # if both settings.gradle and build.gradle exist, prefer build.gradle as it more closely resembles a manifest
                manifest = out.Manifest(
                    kind=out.ManifestKind(out.BuildGradle()),
                    path=out.Fpath(str(build_path)),
                )
            elif settings_path is not None:
                # Gradle doesn't really have a manifest, but we treat both build.gradle and settings.gradle the same,
                # so just classify them both as BuildGradle
                manifest = out.Manifest(
                    kind=out.ManifestKind(out.BuildGradle()),
                    path=out.Fpath(str(settings_path)),
                )

            if manifest is not None:
                subprojects.append(
                    out.Subproject(
                        root_dir=out.Fpath(str(project_root)),
                        dependency_source=out.DependencySource(
                            out.ManifestLockfileDependencySource((manifest, lockfile))
                        ),
                        ecosystem=self.ECOSYSTEM,
                    )
                )
            else:
                subprojects.append(
                    out.Subproject(
                        root_dir=out.Fpath(str(project_root)),
                        dependency_source=out.DependencySource(
                            out.LockfileOnlyDependencySource(lockfile)
                        ),
                        ecosystem=self.ECOSYSTEM,
                    )
                )

        # next, handle settings.gradle files. Settings.gradle defines a multi-project gradle build,
        # so any time we see one, we know that it is at the root of a gradle project.
        # excludes settings files that have accompanying lockfiles; those were accounted for above.
        for settings_path in settings_files:
            if settings_path in used_settings_paths:
                # skip any settings path that we used above
                continue

            project_root = settings_path.parent
            build_paths = dep_source_files.intersection(
                settings_path.parent / x for x in self.BUILD_FILENAMES
            )
            if len(build_paths) > 0:
                # it doesn't really make sense for there to be multiple build files, so pick one arbitrarily
                first_build_path = next(x for x in build_paths)
                # if a build file is present, favor using that as the manifest file, but build
                # files might be missing for multi-project builds
                used_build_paths.add(first_build_path)
                manifest = out.Manifest(
                    kind=out.ManifestKind(out.BuildGradle()),
                    path=out.Fpath(str(first_build_path)),
                )
            else:
                manifest = out.Manifest(
                    kind=out.ManifestKind(out.SettingsGradle()),
                    path=out.Fpath(str(settings_path)),
                )

            root_dirs.add(project_root)
            used_settings_paths.add(settings_path)

            subprojects.append(
                out.Subproject(
                    root_dir=out.Fpath(str(project_root)),
                    dependency_source=out.DependencySource(
                        out.ManifestOnlyDependencySource(manifest)
                    ),
                    ecosystem=self.ECOSYSTEM,
                )
            )

        # finally, we need to handle any build.gradle files that:
        # a) do not have a corresponding settings.gradle file (those were handled above) AND
        # b) are not part of one of the subprojects we already created
        for build_path in build_files:
            if build_path in used_build_paths:
                # this build path was directly considered above, skip it
                continue

            if len(root_dirs.intersection(build_path.parents)) > 0:
                # any parent is a root dir of a subproject that we already created, consider
                # this to be part of that subproject and do not create a separate one.
                continue

            # if we make it to here, we have decided that this build.gradle file defines a single-project
            # gradle build, so we should create a subproject from it.
            subprojects.append(
                out.Subproject(
                    root_dir=out.Fpath(str(build_path.parent)),
                    dependency_source=out.DependencySource(
                        out.ManifestOnlyDependencySource(
                            out.Manifest(
                                kind=out.ManifestKind(out.BuildGradle()),
                                path=out.Fpath(str(build_path)),
                            )
                        )
                    ),
                    ecosystem=self.ECOSYSTEM,
                )
            )
            root_dirs.add(build_path.parent)
            used_build_paths.add(build_path)

        return subprojects, frozenset(
            lockfiles | used_settings_paths | used_build_paths
        )
