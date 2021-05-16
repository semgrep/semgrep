import collections
from pathlib import Path
from typing import Any
from typing import cast
from typing import Dict
from typing import Set

from semgrep.target_manager_extensions import ext_to_lang
from semgrep.target_manager_extensions import FileExtension
from semgrep.target_manager_extensions import Language


def make_target_stats(all_targets: Set[Path]) -> Dict[str, Any]:
    """
    Given a set of all the targets semgrep ran on, compute stats about
    the distribution of files involved.

    You could argue that we could compute the languages from the extensions later,
    which is true. But since semgrep's mapping might change by version, I think it's
    helpful to also record what language semgrep used.
    """
    # example:
    # languages = { 'python': 105, javascript: '308', 'generic': 20 }
    # extensions = { 'py': 10, 'pyi': 8 }
    languages: Dict[Language, int] = collections.defaultdict(int)
    extensions: Dict[FileExtension, int] = collections.defaultdict(int)
    for path in all_targets:
        suffix: FileExtension = cast(FileExtension, path.suffix)
        extensions[suffix] += 1
        # an extension could map to multiple languages; just take the first one
        lang = ext_to_lang(suffix)
        languages[lang] += 1

    return {
        # explicit convert from a defaultdict to a dict to make sure nothing else
        # accidentally inserts entries
        "extensions": dict(extensions),
        "languages": dict(languages),
    }


def make_loc_stats(all_targets: Set[Path]) -> Dict[str, Any]:
    return {"by_extension": count_lines_by_path_extension(all_targets)}


def count_lines_in_file(p: Path) -> int:
    """
    Return number of lines in the path. We're assuming this path is an openable file,
    not a symlink or a directory -- it should have already been checked when it was
    generated in the TargetManager
    """
    # https://stackoverflow.com/a/37600991
    return sum(1 for i in open(p, "rb"))


def count_lines_by_path_extension(paths: Set[Path]) -> Dict[FileExtension, int]:
    all_counts = {p: count_lines_in_file(p) for p in paths}
    by_extension: Dict[FileExtension, int] = collections.defaultdict(int)
    for p, count in all_counts.items():
        by_extension[cast(FileExtension, p.suffix)] += count
    return dict(by_extension)
