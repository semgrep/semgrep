from typing import Dict, Any, Set

from pathlib import Path

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
    ...

def make_loc_stats(all_targets: Set[Path]) -> Dict[str, Any]: ...
