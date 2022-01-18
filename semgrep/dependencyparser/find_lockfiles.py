# find lockfiles
import functools
import os
from pathlib import Path
from typing import Generator
from typing import Optional
from typing import Set

TARGET_LOCKFILE_FILENAMES = ["pipfile.lock", "yarn.lock", "package-lock.json"]


@functools.lru_cache(maxsize=None)
def find_lockfiles(
    current_dir: Path, seen_paths: Optional[Set[Path]] = None
) -> Generator[Path, None, None]:
    for entry in os.scandir(current_dir):
        full_path = Path(os.path.join(current_dir, entry.name))
        resolved_path = full_path.resolve()
        # avoid symlink loops by making sure we haven't seen this path before
        if entry.is_dir() and (seen_paths is None or not (resolved_path in seen_paths)):
            new_paths: Set[Path] = set([resolved_path]).union(
                seen_paths if seen_paths else set([])
            )
            yield from find_lockfiles(full_path, frozenset(new_paths))
        if entry.is_file() and entry.name.lower() in TARGET_LOCKFILE_FILENAMES:
            yield full_path
