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
Compute dependency paths for SCA findings.

A "dependency path" is the chain by which a (transitive) dependency was pulled
into a project, e.g. ``app -> A -> B -> vulnerable-lib``. We reconstruct it
locally at scan time from the resolved dependency graph that the CLI already
carries: every ``FoundDependency`` lists its direct ``children`` (when
path-to-transitivity resolution ran for the ecosystem). We invert those
downward child edges into a parent map and walk *upward* from the matched
dependency to the direct dependency(ies) that introduced it.

Each emitted path is ordered direct-introducer-first: node 0 is the direct
dependency that introduced the chain and the last node is the matched
(transitive) dependency, e.g. ``[top, mid, victim]``.
"""
import itertools
from collections import defaultdict
from dataclasses import dataclass
from dataclasses import field
from typing import Dict
from typing import FrozenSet
from typing import Iterable
from typing import Iterator
from typing import List
from typing import Tuple

import semgrep.semgrep_interfaces.semgrep_output_v1 as out

# Cap on the number of paths emitted per finding, to bound output size for
# diamond-heavy graphs.
MAX_DEPENDENCY_PATHS = 50

# (package, version)
_DepKey = Tuple[str, str]


def _key(package: str, version: str) -> _DepKey:
    return (package, version)


@dataclass
class DependencyParentIndex:
    """
    Reverse index over a subproject's dependency graph, used to walk from a
    dependency up to the direct dependencies that introduced it.

    Build once per subproject (the graph is shared across all findings in it)
    and reuse for every matched dependency.
    """

    # (package, version) -> ordered, de-duplicated list of parent keys
    # (dependencies that list this one among their children)
    parents: Dict[_DepKey, List[_DepKey]] = field(default_factory=dict)
    # (package, version) of every dependency tagged Direct; an introducing
    # chain terminates as soon as it reaches one of these.
    direct_keys: FrozenSet[_DepKey] = field(default_factory=frozenset)

    @classmethod
    def from_dependencies(
        cls, deps: Iterable[out.FoundDependency]
    ) -> "DependencyParentIndex":
        deps = list(deps)
        direct_keys = frozenset(
            _key(d.package, d.version)
            for d in deps
            if d.transitivity.value == out.Direct()
        )

        # (child_key, parent_key) edges; dict.fromkeys dedups while preserving
        # first-seen order.
        edges = dict.fromkeys(
            (_key(child.package, child.version), _key(dep.package, dep.version))
            for dep in deps
            for child in (dep.children or [])
        )
        parents: Dict[_DepKey, List[_DepKey]] = defaultdict(list)
        for child_key, parent_key in edges:
            parents[child_key].append(parent_key)

        return cls(parents=dict(parents), direct_keys=direct_keys)

    def _is_direct(self, key: _DepKey) -> bool:
        return key in self.direct_keys

    def paths_for(
        self,
        dep: out.FoundDependency,
        max_paths: int = MAX_DEPENDENCY_PATHS,
    ) -> List[out.DependencyPath]:
        """
        Return the dependency paths introducing ``dep``, ordered from the
        direct dependency that introduced it (node 0) down to ``dep`` (last
        node).

        Returns an empty list when ``dep`` is itself a direct dependency, when
        the graph carries no edges (e.g. PTT did not run for this ecosystem),
        or when no introducing chain can be found.
        """
        # The path concept only applies to transitive dependencies.
        if dep.transitivity.value == out.Direct():
            return []

        start = _key(dep.package, dep.version)

        # DFS upward, yielding each introducing chain. ``path`` is the current
        # chain from the matched dep to the node under consideration; ``on_path``
        # guards against cycles. Both are rebuilt immutably per step rather than
        # mutated in place.
        def walk(
            path: Tuple[_DepKey, ...], on_path: FrozenSet[_DepKey]
        ) -> Iterator[Tuple[_DepKey, ...]]:
            current = path[-1]

            # A direct dependency is an introducer: terminate the chain here.
            if len(path) > 1 and self._is_direct(current):
                yield path
                return

            parent_keys = self.parents.get(current, [])
            if not parent_keys:
                # Reached a root that isn't tagged Direct (incomplete graph
                # data). Emit the best-effort chain rather than nothing.
                if len(path) > 1:
                    yield path
                return

            for parent_key in parent_keys:
                if parent_key not in on_path:  # skip cycles
                    yield from walk(path + (parent_key,), on_path | {parent_key})

        paths = itertools.islice(walk((start,), frozenset({start})), max_paths)
        # ``walk`` yields each chain matched-dep-first; reverse it so the emitted
        # path runs direct-introducer (node 0) -> matched dep (last).
        return [
            out.DependencyPath(
                nodes=[
                    out.DependencyChild(package=pkg, version=ver)
                    for (pkg, ver) in reversed(path)
                ]
            )
            for path in paths
        ]
