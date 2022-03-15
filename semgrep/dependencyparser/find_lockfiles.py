# find lockfiles
import os
from pathlib import Path
from typing import FrozenSet
from typing import Generator
from typing import Optional

from dependencyparser.parse_lockfile import LOCKFILE_PARSERS

TARGET_LOCKFILE_FILENAMES = LOCKFILE_PARSERS.keys()


def find_lockfiles(
    current: Path, seen_paths: Optional[FrozenSet[Path]] = None
) -> Generator[Path, None, None]:
    if current.is_file() and current.name.lower() in TARGET_LOCKFILE_FILENAMES:
        yield current
    else:
        for entry in os.scandir(current):
            full_path = Path(os.path.join(current, entry.name))
            resolved_path = full_path.resolve()
            # avoid symlink loops by making sure we haven't seen this path before
            if entry.is_dir() and (
                seen_paths is None or not (resolved_path in seen_paths)
            ):
                new_paths = set([resolved_path]).union(
                    seen_paths if seen_paths else set([])
                )
                yield from find_lockfiles(full_path, frozenset(new_paths))
            if entry.is_file() and entry.name.lower() in TARGET_LOCKFILE_FILENAMES:
                yield full_path
