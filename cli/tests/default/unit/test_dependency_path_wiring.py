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
# Wiring test: a ResolvedSubproject (with a real children graph) flows through
# SubprojectDependencyIndex into the dependency-path computation. The pure
# algorithm is covered in test_dependency_path.py; this covers the integration
# point added when connecting dependency paths into the SCA finding flow.
from collections import defaultdict

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.dependency_aware_rule import SubprojectDependencyIndex
from semgrep.dependency_path import DependencyParentIndex
from semgrep.subproject import from_resolved_dependencies


def _build_test_dep(package, version, *, direct, children=()):
    return out.ResolvedDependency(
        (
            out.FoundDependency(
                package=package,
                version=version,
                ecosystem=out.Ecosystem(out.Pypi()),
                allowed_hashes=defaultdict(list),
                transitivity=out.DependencyKind(
                    out.Direct() if direct else out.Transitive()
                ),
                resolved_url=None,
                git_ref=None,
                lockfile_path=out.Fpath("Pipfile.lock"),
                line_number=1,
                children=[
                    out.DependencyChild(package=p, version=v) for (p, v) in children
                ],
            ),
            None,
        )
    )


def _build_test_subproject(dependencies):
    dependency_source = out.DependencySource(
        out.ManifestLockfile(
            (
                out.Manifest(out.ManifestKind(out.Pipfile()), out.Fpath("Pipfile")),
                out.Lockfile(
                    out.LockfileKind(out.PipfileLock()), out.Fpath("Pipfile.lock")
                ),
            )
        )
    )
    return out.ResolvedSubproject(
        info=out.Subproject(
            root_dir=out.Fpath("."),
            dependency_source=dependency_source,
            ecosystem=out.Ecosystem(out.Pypi()),
        ),
        errors=[],
        resolution_method=out.ResolutionMethod(out.LockfileParsing()),
        resolved_dependencies=from_resolved_dependencies(dependencies),
        ecosystem=out.Ecosystem(out.Pypi()),
    )


@pytest.mark.quick
def test_subproject_deps_feed_the_parent_index():
    # a (direct) -> b -> c (vulnerable transitive)
    deps = [
        _build_test_dep("a", "1.0", direct=True, children=[("b", "1.0")]),
        _build_test_dep("b", "1.0", direct=False, children=[("c", "1.0")]),
        _build_test_dep("c", "1.0", direct=False),
    ]
    subproject = _build_test_subproject(deps)

    # the index exposes the flat deps; the parent index is built from them only
    # when --x-dependency-paths is on (in run_scan), never inside the index.
    index = SubprojectDependencyIndex.from_subproject(subproject)
    parent_index = DependencyParentIndex.from_dependencies(index.deps)

    vulnerable = deps[2].value[0]  # the FoundDependency for c
    paths = parent_index.paths_for(vulnerable)
    rendered = [[f"{n.package}@{n.version}" for n in p.nodes] for p in paths]

    # ordered from the direct introducer down to the matched transitive dep
    assert rendered == [["a@1.0", "b@1.0", "c@1.0"]]
