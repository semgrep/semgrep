# find lockfiles
from dataclasses import dataclass
from pathlib import Path
from typing import Dict
from typing import List
from typing import Optional

from dependencyparser.models import LANGUAGE_TO_NAMESPACE
from dependencyparser.models import LockfileDependency
from dependencyparser.models import NAMESPACE_TO_LOCKFILES
from dependencyparser.models import PackageManagers
from dependencyparser.parse_lockfile import LOCKFILE_PARSERS
from dependencyparser.parse_lockfile import parse_lockfile_str
from semgrep.semgrep_types import Language

TARGET_LOCKFILE_FILENAMES = LOCKFILE_PARSERS.keys()


@dataclass
class Node:
    """
    Nodes of a DependencyTrie
    """

    children: Dict[str, "Node"]
    val: Optional[Dict[Path, List[LockfileDependency]]]


class DependencyTrie:
    """
    A trie of lockfile paths (root)
    Also contains a mapping from lockfile namespaces to mappings from lockfiles to their dependencies (all_deps)

    The path `A/B/lock1`, containing the parsed dependencies `deps1` is represented as

    A -> B -> {A/B/lock1 : deps1}

    If we insert `A/B/lock2` it becomes

    A -> B -> {A/B/lock1 : deps1, A/B/lock2 : deps2}

    If we insert `A/C/lock3` it becomes

    A -> B -> {A/B/lock1 : deps1, A/B/lock2 : deps2}
    |
     ->  C -> {A/C/lock3 : deps3}
    """

    def __init__(self) -> None:
        self.root: Node = Node(children={}, val=None)
        self.all_deps: Dict[PackageManagers, Dict[Path, List[LockfileDependency]]] = {}

    def insert(
        self, path: Path, deps: List[LockfileDependency], namespace: PackageManagers
    ) -> None:
        """
        Inserts `path` into the trie in accordance with the behavior described in the class comment
        `namespace` is namespace that this lockfile will be listed under in all_deps
        """
        if namespace in self.all_deps:
            self.all_deps[namespace][path] = deps
        else:
            self.all_deps[namespace] = {path: deps}

        curr = self.root
        for part in path.parts[:-1]:
            if part not in curr.children:
                curr.children[part] = Node(children={}, val=None)
            curr = curr.children[part]

        if curr.val is None:
            curr.val = {path: deps}
        else:
            curr.val[path] = deps

    def find_dependencies(
        self, path: Path
    ) -> Optional[Dict[Path, List[LockfileDependency]]]:
        """
        Finds all the lockfiles and their parsed dependencies that `path` depends on
        Traversing the trie finds the lockfile whose path shares the longest common
        prefix with `path`
        """
        curr = self.root
        for part in path.parts:
            if part not in curr.children:
                return curr.val
            else:
                curr = curr.children[part]
        return None


def make_dependency_trie(target: Path, langs: List[Language]) -> DependencyTrie:
    dep_trie = DependencyTrie()
    # Triple for loop, but the outer two are (basically) constant time and guaranteed to be almost instant
    for namespace in [LANGUAGE_TO_NAMESPACE[l] for l in langs]:
        for lockfile_type in NAMESPACE_TO_LOCKFILES[namespace]:
            for lockfile in target.glob("**/" + lockfile_type):
                deps = list(parse_lockfile_str(lockfile.read_text(), lockfile))
                dep_trie.insert(lockfile, deps, namespace)

    return dep_trie
