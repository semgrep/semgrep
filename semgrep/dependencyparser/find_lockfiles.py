# find lockfiles
import os
from dataclasses import dataclass
from pathlib import Path
from typing import Dict
from typing import FrozenSet
from typing import Generator
from typing import List
from typing import Optional

from dependencyparser.models import LockfileDependency
from dependencyparser.parse_lockfile import LOCKFILE_PARSERS
from dependencyparser.parse_lockfile import parse_lockfile_str


TARGET_LOCKFILE_FILENAMES = LOCKFILE_PARSERS.keys()


@dataclass
class Node:
    children: Dict[str, "Node"]
    val: Optional[Dict[str, List[LockfileDependency]]]


class DependencyTrie:
    def __init__(self) -> None:
        self.root: Node = Node(children={}, val=None)

    def insert(self, path: Path, deps: List[LockfileDependency]) -> None:
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
    ) -> Optional[Dict[str, List[LockfileDependency]]]:
        curr = self.root
        for part in path.parts:
            if part not in curr.children:
                return curr.val
            else:
                curr = curr.children[part]


def make_dependency_trie(target: Path) -> DependencyTrie:
    dep_trie = DependencyTrie()

    def go(current: Path, seen_paths: FrozenSet[Path]) -> DependencyTrie:
        if current.is_file() and current.name.lower() in TARGET_LOCKFILE_FILENAMES:
            dep_trie.insert(
                current, list(parse_lockfile_str(current.read_text(), current))
            )
        else:
            for entry in os.scandir(current):
                full_path = Path(os.path.join(current, entry.name))
                resolved_path = full_path.resolve()
                # avoid symlink loops by making sure we haven't seen this path before
                if entry.is_dir() and (not (resolved_path in seen_paths)):
                    new_paths = set([resolved_path]).union(seen_paths)
                    go(full_path, frozenset(new_paths))
                if entry.is_file() and entry.name.lower() in TARGET_LOCKFILE_FILENAMES:
                    dep_trie.insert(
                        current, parse_lockfile_str(current.read_text(), current)
                    )

    go(target, FrozenSet(), DependencyTrie())
    return dep_trie
