"""
Utility for identifying the URL of the current git project
"""
import subprocess
from typing import cast
from typing import Optional
from typing import Sequence

from semgrep.util import manually_search_file
from semgrep.util import sub_check_output
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


def get_project_url(targets: Optional[Sequence[str]] = None) -> Optional[str]:
    """
    Returns the current git project's default remote URL, or None if not a git project / no remote

    If multiple targets are specified, only the first will be used to evaluate the git remote URL.
    If no target is specified, will fall back to using current working directory
    """
    target = None
    if targets and len(targets) > 0:
        if targets and len(targets) > 1:
            logger.debug(
                f"only examining directory {targets[0]} to determine project url"
            )
        target = targets[0]

    try:
        return cast(
            str,
            sub_check_output(
                ["git", "ls-remote", "--get-url"],
                encoding="utf-8",
                stderr=subprocess.DEVNULL,
                cwd=target,
            ).rstrip(),
        )
    except Exception as e:
        logger.debug(f"Failed to get project url from 'git ls-remote': {e}")
        try:
            # add \n to match urls from git ls-remote (backwards compatability)
            return manually_search_file(".git/config", ".com", "\n")
        except Exception as e:
            logger.debug(f"Failed to get project url from .git/config: {e}")
            return None
