# find lockfiles
import functools
import os
from pathlib import Path
from typing import Generator

TARGET_LOCKFILE_FILENAMES = ["pipfile.lock", "yarn.lock", "package-lock.json"]


@functools.lru_cache(maxsize=None)
def find_lockfiles(current_dir: Path) -> Generator[Path, None, None]:
    for entry in os.scandir(current_dir):
        full_path = Path(os.path.join(current_dir, entry.name))
        # avoid loops by not following symlinks
        if entry.is_dir(follow_symlinks=False):
            yield from find_lockfiles(full_path)
        if entry.is_file() and entry.name.lower() in TARGET_LOCKFILE_FILENAMES:
            yield full_path
