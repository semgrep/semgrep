# find lockfiles
from dataclasses import dataclass
from pathlib import Path
from typing import Dict
from typing import List
from typing import Optional
from typing import Tuple

from semdep.models import LockfileDependency
from semdep.models import NAMESPACE_TO_LOCKFILES
from semdep.models import PackageManagers
from semdep.parse_lockfile import parse_lockfile_str


@dataclass
class Node:
    """
    Nodes of a DependencyTrie
    """

    children: Dict[str, "Node"]
    val: Optional[Dict[Path, List[LockfileDependency]]]


def find_single_lockfile(
    p: Path, ecosystem: PackageManagers
) -> Optional[Tuple[Path, List[LockfileDependency]]]:
    """
    Find the nearest lockfile in a given ecosystem to P
    Searches only up the directory tree
    """
    for path in p.parents:
        for lockfile_pattern in NAMESPACE_TO_LOCKFILES[ecosystem]:
            lockfile_path = path / lockfile_pattern
            if lockfile_path.exists():
                return lockfile_path, list(
                    parse_lockfile_str(
                        lockfile_path.read_text(encoding="utf8"), lockfile_path
                    )
                )
            else:
                continue
    return None
