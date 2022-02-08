from pathlib import Path

VERSION_CACHE_PATH: Path = ...

def version_check(version_cache_path: Path = VERSION_CACHE_PATH) -> None:
    """
    Checks for messages from the backend, displaying any messages that match the current version

    :param version_cache_path: Path where we cache the backend response
    """
    ...
