"""
Utility for identifying the URL of the current git project
"""
import re
from pathlib import Path
from typing import Any
from typing import Dict
from typing import List
from typing import Optional

import ruamel.yaml
from attr import asdict
from attr import define
from attr import field

from semgrep.git import get_git_root_path
from semgrep.util import git_check_output
from semgrep.util import manually_search_file
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

CONFIG_FILE_PATTERN = re.compile(r"^\.semgrepconfig(\.yml|\.yaml)?$")


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


@define
class ProjectConfig:
    """
    Class that handles loading and validating semgrepconfig files.

    Example:

    version: v1
    tags:
        - tag1
        - tag2
    """

    FILE_VERSION = "v1"

    version: str = field(default=FILE_VERSION)
    tags: Optional[List[str]] = field(default=None)

    @tags.validator
    def check_tags(self, _attribute: Any, value: Optional[List[str]]) -> None:
        if value is None:
            return
        if not isinstance(value, list):
            raise ValueError("tags must be a list of strings")
        for val in value:
            if not isinstance(val, str):
                raise ValueError("tags must be a list of strings")

    @staticmethod
    def is_project_config_file(file_path: Path) -> bool:
        return CONFIG_FILE_PATTERN.search(file_path.name) is not None

    @classmethod
    def _find_all_config_files(cls, src_directory: Path, cwd_path: Path) -> List[Path]:
        conf_files = []

        # Populate stack of directories to traverse
        stack = {cwd_path}
        temp_path = src_directory
        dir_route = cwd_path.relative_to(src_directory)
        for parent in dir_route.parents:
            temp_path = temp_path / parent
            stack.add(temp_path)

        # Traverse stack looking for config files
        while stack:
            cur_path = stack.pop()
            if not cur_path.exists():
                continue
            conf_files += [
                f for f in cur_path.iterdir() if cls.is_project_config_file(f)
            ]
        return conf_files

    @classmethod
    def load_from_file(cls, file_path: Path) -> "ProjectConfig":
        yaml = ruamel.yaml.YAML(typ="safe")
        logger.debug(f"Loading semgrepconfig file: {file_path}")
        with file_path.open("r") as fp:
            config: Dict[str, Any] = yaml.load(fp)
            cfg = cls(**config)
            return cfg

    @classmethod
    def load_all(cls) -> "ProjectConfig":
        src_directory = get_git_root_path()
        cwd_path = Path.cwd()
        conf_files = cls._find_all_config_files(src_directory, cwd_path)

        # Sort by depth asc so deeper configs take precedence
        conf_files.sort(key=lambda x: len(x.parts))

        # Merge metadata from all config files
        all_metadata: Dict[Any, Any] = {}
        for conf_file in conf_files:
            project_conf = cls.load_from_file(conf_file)
            project_conf_data = asdict(project_conf)
            all_metadata = {**all_metadata, **project_conf_data}
        return cls(**all_metadata)

    def to_dict(self) -> Dict[str, Any]:
        data = asdict(self)
        # Strip out version
        data.pop("version", None)
        # Strip out tags if None
        if data.get("tags") is None:
            data.pop("tags", None)
        return data
