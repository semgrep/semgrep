"""
Utility for identifying the URL of the current git project
"""
import json
import re
from copy import deepcopy
from pathlib import Path
from typing import Any
from typing import Dict
from typing import List
from typing import Optional

import ruamel.yaml
from attr import define
from attr import Factory

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
    metadata: Dict[str, Any] = Factory(dict)

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
            conf_files += [
                f for f in cur_path.iterdir() if cls.is_project_config_file(f)
            ]
        return conf_files

    @classmethod
    def load_from_file(cls, file_path: Path) -> Dict[str, Any]:
        yaml = ruamel.yaml.YAML(typ="safe")
        logger.debug(f"Loading semgrepconfig file: {file_path}")
        with file_path.open("r") as fp:
            config: Dict[str, Any] = yaml.load(fp)
            return config

    @classmethod
    def load_all(cls) -> "ProjectConfig":
        src_directory = get_git_root_path()
        cwd_path = Path.cwd()
        conf_files = cls._find_all_config_files(src_directory, cwd_path)

        # Sort by depth asc
        conf_files.sort(key=lambda x: len(x.parts))

        # Merge metadata from all config files
        all_metadata: Dict[Any, Any] = {}
        for conf_file in conf_files:
            local_metadata = cls.load_from_file(conf_file)
            all_metadata = {**all_metadata, **local_metadata}
        return cls(all_metadata)

    def to_dict(self) -> Dict[str, Any]:
        # Convert all tag values to strings
        data = deepcopy(self.metadata)
        tags = data.pop("tags", None)
        if tags:
            data["tags"] = []
            for tag in tags:
                if isinstance(tag, dict):
                    data["tags"].append(json.dumps(tag))
                else:
                    data["tags"].append(str(tag))
        # Strip out version
        data.pop("version", None)
        return data
