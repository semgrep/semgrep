import subprocess
from typing import cast
from typing import Optional

from semgrep.util import manually_search_file
from semgrep.util import sub_check_output
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


def get_project_url() -> Optional[str]:
    try:
        return cast(
            str,
            sub_check_output(
                ["git", "ls-remote", "--get-url"],
                encoding="utf-8",
                stderr=subprocess.DEVNULL,
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
