
class BaselineHandler:
    """
    base_commit: Git ref to compare against
    """

    def __init__(self, base_commit: str) -> None:
        """
        Raises Exception if
        - cwd is not in git repo
        - base_commit is not valid git hash
        - there are tracked files with pending changes
        - there are untracked files that will be overwritten by a file in the base commit
        """
        ...
