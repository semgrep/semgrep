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
import re
from typing import Any

import requests
from mcp.shared.exceptions import McpError
from mcp.types import ErrorData
from mcp.types import INTERNAL_ERROR
from mcp.types import INVALID_PARAMS

from semgrep.app import auth
from semgrep.git import git_check_output
from semgrep.state import get_state

SETTINGS_FILENAME = "settings.yml"

# A regex to match { "identity": "... roles=[<content>] ..." }
# We need this because the form of the response from the /agent/identity
# endpoint looks like:
# {"identity":"id=<id> deployment_id=<d_id> user_id=<u_id> created_at=<time> roles=[<roles>] ...}
# The string mapped to by `"identity"` is not an actual JSON, its a whitespace-separated
# list of fields, so we have to manually extract the `roles` field.
# We really just need to match "roles=[<stuff>]"
re_identity_string = re.compile(r"roles=\[(.*)\]")


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


async def get_identity() -> dict[str, Any]:
    """
    Fetches the identity from Semgrep API.

    Returns:
        dict[str, Any]: The identity object

    Raises:
        McpError: If unable to fetch identity or no identity found
    """

    # Get API token
    api_token = get_semgrep_app_token()
    if not api_token:
        raise McpError(
            ErrorData(
                code=INVALID_PARAMS,
                message="""
                  SEMGREP_APP_TOKEN environment variable must be set or user
                  must be logged in to use this tool
                """,
            )
        )

    # Fetch identity
    url = f"{get_semgrep_api_url()}/agent/identity"
    headers = {"Authorization": f"Bearer {api_token}", "Accept": "application/json"}

    try:
        response = requests.get(url, headers=headers, timeout=(2, 30))
        response.raise_for_status()
        data: dict[str, Any] = response.json()

        return data

    except requests.exceptions.HTTPError as e:
        if e.response.status_code == 401:
            raise McpError(
                ErrorData(
                    code=INVALID_PARAMS,
                    message="Invalid API token: check your SEMGREP_APP_TOKEN environment variable.",
                )
            ) from e
        else:
            raise McpError(
                ErrorData(
                    code=INTERNAL_ERROR,
                    message=f"Error fetching deployments: {e.response.text}",
                )
            ) from e
    except Exception as e:
        raise McpError(
            ErrorData(
                code=INTERNAL_ERROR,
                message=f"Error fetching deployments from Semgrep: {e!s}",
            )
        ) from e
