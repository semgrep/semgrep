"""
Utility for identifying the URL of the current git project
"""
from typing import Optional

from semgrep.util import git_check_output
from semgrep.util import manually_search_file
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


def get_project_url() -> Optional[str]:
    """
    Returns the current git project's default remote URL, or None if not a git project / no remote
    """
    try:
        return git_check_output(["git", "ls-remote", "--get-url"])
    except Exception as e:
        logger.debug(f"Failed to get project url from 'git ls-remote': {e}")
        try:
            # add \n to match urls from git ls-remote (backwards compatability)
            return manually_search_file(".git/config", ".com", "\n")
        except Exception as e:
            logger.debug(f"Failed to get project url from .git/config: {e}")
            return None
