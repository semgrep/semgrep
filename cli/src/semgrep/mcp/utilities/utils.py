#
# Copyright (c) 2025 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
import os
from pathlib import Path

from ruamel.yaml import YAML

from semgrep.git import git_check_output
from semgrep.state import get_state

SETTINGS_FILENAME = "settings.yml"


def is_hosted() -> bool:
    """
    Check if the user is using the hosted version of the MCP server.
    """
    return os.environ.get("SEMGREP_IS_HOSTED", "false").lower() == "true"


def get_semgrep_api_url() -> str:
    url = get_state().env.semgrep_url
    return f"{url}/api"


def get_user_settings_file() -> Path:
    def get_user_data_folder() -> Path:
        config_home = os.getenv("XDG_CONFIG_HOME")
        if config_home is None or not Path(config_home).is_dir():
            parent_dir = Path.home()
        else:
            parent_dir = Path(config_home)
        return parent_dir / ".semgrep"

    path = os.getenv(
        "SEMGREP_SETTINGS_FILE", str(get_user_data_folder() / SETTINGS_FILENAME)
    )
    return Path(path)


def get_field_from_settings_file(field: str) -> str | None:
    """
    Returns the value of the field from the settings file, if it exists
    """
    user_settings_file = get_user_settings_file()
    if user_settings_file.exists():
        with open(user_settings_file) as f:
            yaml = YAML(typ="safe", pure=True)
            settings = yaml.load(f)
            return str(settings.get(field))
    return None


def get_semgrep_app_token() -> str | None:
    """
    Returns the Semgrep app token, if it exists
    """
    env_token = os.environ.get("SEMGREP_APP_TOKEN")
    if env_token is not None:
        return env_token

    return get_field_from_settings_file("api_token")


def get_anonymous_user_id() -> str:
    """
    Returns the anonymous user ID, if it exists
    """
    id = get_field_from_settings_file("anonymous_user_id")
    return id if id else "unknown"


def run_git_command(workspace_dir: str | None, args: list[str]) -> str:
    if workspace_dir is None:
        return "unknown"
    try:
        return git_check_output(["git", *args], cwd=workspace_dir)
    except Exception:
        return "unknown"


def get_git_info(workspace_dir: str | None) -> dict[str, str]:
    git_username = run_git_command(workspace_dir, ["config", "user.name"])
    git_repo = run_git_command(workspace_dir, ["config", "--get", "remote.origin.url"])
    git_branch = run_git_command(workspace_dir, ["rev-parse", "--abbrev-ref", "HEAD"])
    return {"username": git_username, "repo": git_repo, "branch": git_branch}
