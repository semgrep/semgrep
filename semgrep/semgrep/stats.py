import collections
from pathlib import Path
from typing import Any
from typing import cast
from typing import Dict
from typing import Set
from typing import Tuple

from semgrep.target_manager_extensions import ext_to_langs
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
        lang = sorted(ext_to_langs(suffix))[0]
        languages[lang] += 1

    return {
        # explicit convert from a defaultdict to a dict to make sure nothing else
        # accidentally inserts entries
        "extensions": dict(extensions),
        "languages": dict(languages),
    }


def make_runtime_per_stats(rule_matches: Any) -> Dict[str, Any]:
    pass
