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

from semgrep.app import auth
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


def get_semgrep_app_token() -> str | None:
    """
    Returns the Semgrep app token, if it exists
    """
    return auth.get_token()


def get_anonymous_user_id() -> str:
    """
    Returns the anonymous user ID, if it exists
    """
    id = get_state().settings.get("anonymous_user_id")
    if isinstance(id, str):
        return id
    return "unknown"


def get_deployment_id_from_token(token: str | None) -> str:
    """
    Returns the deployment ID the token is for, if token is valid
    """
    if not token:
        return ""

    deployment = auth.get_deployment_from_token(token)
    return str(deployment.id) if deployment else ""


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
