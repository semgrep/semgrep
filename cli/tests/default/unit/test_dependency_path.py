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

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.dependency_path import DependencyParentIndex
from semgrep.dependency_path import MAX_DEPENDENCY_PATHS

NPM = out.Ecosystem(out.Npm())

# These are pure in-memory graph tests with no I/O (well under 100 ms).
pytestmark = pytest.mark.quick


def dep(package, version, *, direct, children=()):
    """Build a FoundDependency with the given children (graph edges).

    ``children=None`` builds a dependency whose ``children`` field is ``None``
    (as real data carries when path-to-transitivity didn't populate edges),
    rather than an empty list.
    """
    return out.FoundDependency(
        package=package,
        version=version,
        ecosystem=NPM,
        allowed_hashes=defaultdict(list),
        transitivity=out.DependencyKind(out.Direct() if direct else out.Transitive()),
        resolved_url=None,
        git_ref=None,
        lockfile_path=out.Fpath("package-lock.json"),
        line_number=1,
        children=(
            None
            if children is None
            else [out.DependencyChild(package=p, version=v) for (p, v) in children]
        ),
    )


def as_strings(paths):
    """Render paths as lists of 'package@version' for easy assertions."""
    return [[f"{n.package}@{n.version}" for n in path.nodes] for path in paths]


def test_simple_chain():
    # a (direct) -> b -> c (vulnerable transitive)
    deps = [
        dep("a", "1.0.0", direct=True, children=[("b", "1.0.0")]),
        dep("b", "1.0.0", direct=False, children=[("c", "1.0.0")]),
        dep("c", "1.0.0", direct=False),
    ]
    index = DependencyParentIndex.from_dependencies(deps)
    # node 0 = direct introducer; last node = matched (vulnerable) dep
    assert as_strings(index.paths_for(deps[2])) == [["a@1.0.0", "b@1.0.0", "c@1.0.0"]]


def test_direct_dependency_has_no_path():
    deps = [
        dep("a", "1.0.0", direct=True, children=[("b", "1.0.0")]),
        dep("b", "1.0.0", direct=False),
    ]
    index = DependencyParentIndex.from_dependencies(deps)
    # the path concept only applies to transitive deps
    assert index.paths_for(deps[0]) == []


def test_diamond_yields_multiple_paths():
    # a (direct) -> b -> c  AND  a -> x -> c
    deps = [
        dep("a", "1.0.0", direct=True, children=[("b", "1.0.0"), ("x", "1.0.0")]),
        dep("b", "1.0.0", direct=False, children=[("c", "1.0.0")]),
        dep("x", "1.0.0", direct=False, children=[("c", "1.0.0")]),
        dep("c", "1.0.0", direct=False),
    ]
    index = DependencyParentIndex.from_dependencies(deps)
    paths = as_strings(index.paths_for(deps[3]))
    assert sorted(paths) == [
        ["a@1.0.0", "b@1.0.0", "c@1.0.0"],
        ["a@1.0.0", "x@1.0.0", "c@1.0.0"],
    ]


def test_cycle_terminates():
    # a (direct) -> b ; b <-> c (cycle)
    deps = [
        dep("a", "1.0.0", direct=True, children=[("b", "1.0.0")]),
        dep("b", "1.0.0", direct=False, children=[("c", "1.0.0")]),
        dep("c", "1.0.0", direct=False, children=[("b", "1.0.0")]),
    ]
    index = DependencyParentIndex.from_dependencies(deps)
    assert as_strings(index.paths_for(deps[2])) == [["a@1.0.0", "b@1.0.0", "c@1.0.0"]]


def test_no_graph_yields_empty():
    # transitive dep but no children edges anywhere (e.g. PTT didn't run)
    deps = [dep("c", "1.0.0", direct=False)]
    index = DependencyParentIndex.from_dependencies(deps)
    assert index.paths_for(deps[0]) == []


def test_best_effort_when_root_not_marked_direct():
    # graph data incomplete: top of chain is transitive with no parents
    deps = [
        dep("b", "1.0.0", direct=False, children=[("c", "1.0.0")]),
        dep("c", "1.0.0", direct=False),
    ]
    index = DependencyParentIndex.from_dependencies(deps)
    # still emit the chain up to the root rather than nothing
    assert as_strings(index.paths_for(deps[1])) == [["b@1.0.0", "c@1.0.0"]]


def test_paths_are_capped():
    children = [(f"p{i}", "1.0.0") for i in range(MAX_DEPENDENCY_PATHS + 10)]
    deps = (
        [dep("a", "1.0.0", direct=True, children=children)]
        + [
            dep(f"p{i}", "1.0.0", direct=False, children=[("c", "1.0.0")])
            for i in range(MAX_DEPENDENCY_PATHS + 10)
        ]
        + [dep("c", "1.0.0", direct=False)]
    )
    index = DependencyParentIndex.from_dependencies(deps)
    assert len(index.paths_for(deps[-1])) == MAX_DEPENDENCY_PATHS


def test_distinct_versions_are_distinct_nodes():
    # same package name, different versions are different graph nodes
    deps = [
        dep("a", "1.0.0", direct=True, children=[("b", "2.0.0")]),
        dep("b", "1.0.0", direct=False),
        dep("b", "2.0.0", direct=False, children=[("c", "1.0.0")]),
        dep("c", "1.0.0", direct=False),
    ]
    index = DependencyParentIndex.from_dependencies(deps)
    assert as_strings(index.paths_for(deps[3])) == [["a@1.0.0", "b@2.0.0", "c@1.0.0"]]


def test_duplicate_edges_are_deduplicated():
    # 'a' lists the same child 'c' twice; the parent must appear only once,
    # so a single path is emitted rather than a duplicate.
    deps = [
        dep("a", "1.0.0", direct=True, children=[("c", "1.0.0"), ("c", "1.0.0")]),
        dep("c", "1.0.0", direct=False),
    ]
    index = DependencyParentIndex.from_dependencies(deps)
    assert as_strings(index.paths_for(deps[1])) == [["a@1.0.0", "c@1.0.0"]]


def test_multiple_distinct_direct_introducers():
    # two separate direct deps each pull in the same transitive 'c'
    deps = [
        dep("a", "1.0.0", direct=True, children=[("c", "1.0.0")]),
        dep("b", "1.0.0", direct=True, children=[("c", "1.0.0")]),
        dep("c", "1.0.0", direct=False),
    ]
    index = DependencyParentIndex.from_dependencies(deps)
    paths = as_strings(index.paths_for(deps[2]))
    assert sorted(paths) == [["a@1.0.0", "c@1.0.0"], ["b@1.0.0", "c@1.0.0"]]


def test_deep_chain():
    # a (direct) -> b -> c -> d (vulnerable transitive): chain deeper than 3
    deps = [
        dep("a", "1.0.0", direct=True, children=[("b", "1.0.0")]),
        dep("b", "1.0.0", direct=False, children=[("c", "1.0.0")]),
        dep("c", "1.0.0", direct=False, children=[("d", "1.0.0")]),
        dep("d", "1.0.0", direct=False),
    ]
    index = DependencyParentIndex.from_dependencies(deps)
    assert as_strings(index.paths_for(deps[3])) == [
        ["a@1.0.0", "b@1.0.0", "c@1.0.0", "d@1.0.0"]
    ]


def test_explicit_max_paths_override():
    # three direct introducers, but cap the result to 2 via the parameter
    deps = [
        dep("a", "1.0.0", direct=True, children=[("c", "1.0.0")]),
        dep("b", "1.0.0", direct=True, children=[("c", "1.0.0")]),
        dep("d", "1.0.0", direct=True, children=[("c", "1.0.0")]),
        dep("c", "1.0.0", direct=False),
    ]
    index = DependencyParentIndex.from_dependencies(deps)
    assert len(index.paths_for(deps[3], max_paths=2)) == 2


def test_children_none_is_treated_as_no_edges():
    # real-world data may carry children=None (PTT didn't populate edges)
    deps = [dep("c", "1.0.0", direct=False, children=None)]
    index = DependencyParentIndex.from_dependencies(deps)
    assert index.paths_for(deps[0]) == []
