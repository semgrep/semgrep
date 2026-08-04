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
"""
Matcher for SBT multi-module projects.

In SBT, running `dependencyTree` at the project root resolves dependencies
for all submodules. Submodules may have their own `build.sbt` files, but
these should not trigger separate resolutions — only the root-most
`build.sbt` files should be used.

This matcher collects all `build.sbt` files, then filters out any that are
nested under another `build.sbt`'s directory.
"""
import functools
import glob
from dataclasses import dataclass
from pathlib import Path
from typing import FrozenSet
from typing import List
from typing import Set
from typing import Tuple

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.matchers.base import SubprojectMatcher
from semgrep.types import Target


@dataclass(frozen=True)
class SbtMatcher(SubprojectMatcher):
    MANIFEST_NAME = "build.sbt"
    ECOSYSTEM = out.Ecosystem(out.Maven())
    MANIFEST_KIND = out.ManifestKind(out.BuildSbt())

    @functools.cached_property
    def subproject_identifying_glob_filters(self) -> FrozenSet[str]:
        return frozenset([glob.escape(self.MANIFEST_NAME)])

    def is_match(self, path: Path) -> bool:
        return path.name == self.MANIFEST_NAME

    def make_subprojects(
        self, dep_source_files: FrozenSet[Target]
    ) -> Tuple[List[out.Subproject], FrozenSet[Path]]:
        all_build_sbts = sorted(
            (t.fpath for t in dep_source_files if self.is_match(t.fpath)),
            key=lambda p: len(p.parts),
        )

        root_dirs: Set[Path] = set()
        subprojects: List[out.Subproject] = []
        used_paths: Set[Path] = set()

        for build_sbt in all_build_sbts:
            project_root = build_sbt.parent

            # Skip build.sbt files nested under an already-matched root.
            if len(root_dirs.intersection(build_sbt.parents)) > 0:
                used_paths.add(build_sbt)
                continue

            root_dirs.add(project_root)
            used_paths.add(build_sbt)

            subprojects.append(
                out.Subproject(
                    root_dir=out.Fpath(str(project_root)),
                    dependency_source=out.DependencySource(
                        out.ManifestOnly(
                            out.Manifest(
                                kind=self.MANIFEST_KIND,
                                path=out.Fpath(str(build_sbt)),
                            )
                        )
                    ),
                    ecosystem=self.ECOSYSTEM,
                )
            )

        return subprojects, frozenset(used_paths)
