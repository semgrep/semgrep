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
from functools import lru_cache
from typing import Any

import requests
from mcp.server.auth.middleware.auth_context import get_access_token
from mcp.server.fastmcp.server import Context
from mcp.shared.exceptions import McpError
from mcp.types import ErrorData
from mcp.types import INTERNAL_ERROR
from mcp.types import INVALID_PARAMS

from semgrep.app import auth
from semgrep.git import git_check_output
from semgrep.mcp.models import WhoamiResult
from semgrep.semgrep_interfaces.semgrep_output_v1 import DeploymentConfig
from semgrep.state import get_state
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

SETTINGS_FILENAME = "settings.yml"

# A regex to match { "identity": "... roles=(<content>) ..." }
# We need this because the form of the response from the /agent/identity
# endpoint looks like:
# {"identity":"id=<id> deployment_id=<d_id> user_id=<u_id> created_at=<time> roles=(<roles>) ...}
# The string mapped to by `"identity"` is not an actual JSON, its a whitespace-separated
# list of fields, so we have to manually extract the `roles` field.
# We really just need to match "roles=(<stuff>)"
re_identity_string = re.compile(r"roles=\((.*)\)")


def is_hosted() -> bool:
    """
    Check if the user is using the hosted version of the MCP server.
    """
    return os.environ.get("SEMGREP_IS_HOSTED", "false").lower() == "true"


def is_oauth_authenticated() -> bool:
    """
    Check if the user is authenticated using OAuth.
    """
    return get_access_token() is not None


def findings_elicitation_enabled() -> bool:
    """
    Check if findings elicitation is enabled.
    """
    return (
        os.environ.get("SEMGREP_FINDINGS_ELICITATION_ENABLED", "false").lower()
        == "true"
    )


async def get_workspace_dir(ctx: Context) -> str | None:
    """
    Get the workspace directory from the context

    Note: We must invoke this method at request time, and not lifespan time,
    because it relies on the `ctx.request_context`, which does not exist
    when we initialize the server.
    """
    # This step fails when we are running tests, so I am wrapping it in a try/except
    try:
        # This URI is supposed to begin with `file://`
        roots = await ctx.request_context.session.list_roots()
        logger.debug(f"Got roots from client: {roots}")

        # Just to be safe. It's probably impossible.
        if len(roots.roots) == 0:
            logger.warning("Somehow, no roots found")
            return None

        uri: str = str(roots.roots[0].uri)
        path = uri[7:] if uri.startswith("file://") else uri

        logger.debug(f"Determined path of workspace directory: {path}")

        return path
    except Exception:
        return ""


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


def get_deployment_from_token(token: str | None) -> DeploymentConfig | None:
    """
    Returns the deployment the token is for, if token is valid
    """
    if not token:
        return None
    return auth.get_deployment_from_token(token)


def get_deployment_id_from_token(token: str | None) -> int | None:
    """
    Returns the deployment ID the token is for, if token is valid
    """
    deployment = get_deployment_from_token(token)
    return deployment.id if deployment else None


def get_deployment_name_from_token(token: str | None) -> str | None:
    """
    Returns the deployment name the token is for, if token is valid
    """
    deployment = get_deployment_from_token(token)
    return deployment.name if deployment else None


def get_deployment_from_jwt() -> dict[str, Any]:
    """
    Returns the deployment data the JWT is for.

    Raises:
        McpError: If unable to fetch deployment
    """

    token = get_semgrep_access_token()
    if not token:
        raise McpError(
            ErrorData(
                code=INVALID_PARAMS,
                message="No access token found. Please try connecting to the MCP server again",
            )
        )

    url = f"{get_semgrep_api_url()}/v2/deployments"
    headers = {"Authorization": f"Bearer {token}", "Accept": "application/json"}

    try:
        request = requests.get(url, headers=headers, timeout=(2, 30))
        request.raise_for_status()
        data = request.json()

        deployments = data.get("deployments", [])
        if len(deployments) == 0:
            raise McpError(
                ErrorData(
                    code=INTERNAL_ERROR,
                    message="No deployments found for this API token",
                )
            )

        return dict(deployments[0])

    except requests.exceptions.HTTPError as e:
        if e.response.status_code == 401:
            raise McpError(
                ErrorData(
                    code=INVALID_PARAMS,
                    message="Invalid authorization: check if you are properly authenticated to the MCP server.",
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


def get_deployment_id_from_jwt() -> int:
    deployment = get_deployment_from_jwt()
    if deployment.get("id") is None:
        raise McpError(
            ErrorData(
                code=INTERNAL_ERROR,
                message="No deployment ID found. Try reconnecting to the MCP server.",
            )
        )
    return int(deployment["id"])


def get_deployment_name_from_jwt() -> str:
    deployment = get_deployment_from_jwt()
    if deployment.get("name") is None:
        raise McpError(
            ErrorData(
                code=INTERNAL_ERROR,
                message="No deployment name found. Try reconnecting to the MCP server.",
            )
        )
    return str(deployment["name"])


def get_current_user_from_jwt(access_token: str | None = None) -> WhoamiResult:
    """
    Returns the identity of the current user for a JWT access token.

    NOTE: This only works with JWTs (not API tokens).
    """
    token = access_token or get_semgrep_access_token()
    if not token:
        raise McpError(ErrorData(code=INVALID_PARAMS, message="No access token found"))

    url = f"{get_semgrep_api_url()}/auth/users/current"
    headers = {"Authorization": f"Bearer {token}", "Accept": "application/json"}

    try:
        response = requests.get(url, headers=headers, timeout=(2, 30))
        response.raise_for_status()
        payload: dict[str, Any] = response.json()
        data = payload.get("user")
        if not isinstance(data, dict):
            raise McpError(
                ErrorData(
                    code=INTERNAL_ERROR,
                    message="Malformed response from Semgrep: missing 'user' field",
                )
            )
        return WhoamiResult.model_validate(data)
    except requests.exceptions.HTTPError as e:
        if e.response is not None and e.response.status_code == 401:
            raise McpError(
                ErrorData(
                    code=INVALID_PARAMS,
                    message="Invalid authorization: check if you are properly authenticated to the MCP server.",
                )
            ) from e
        detail = e.response.text if e.response is not None else str(e)
        raise McpError(
            ErrorData(
                code=INTERNAL_ERROR, message=f"Error getting current user: {detail}"
            )
        ) from e
    except McpError:
        raise
    except Exception as e:
        raise McpError(
            ErrorData(code=INTERNAL_ERROR, message=f"Error getting current user: {e!s}")
        ) from e


def get_deployment_id() -> int | None:
    """
    Returns the deployment ID, if it exists. Gets id from JWT if hosted, app token if not.
    """
    if is_oauth_authenticated():
        return get_deployment_id_from_jwt()
    else:
        return get_deployment_id_from_token(get_semgrep_app_token())


def get_deployment_name() -> str | None:
    """
    Returns the deployment name, if it exists. Gets name from JWT if hosted, app token if not.
    """
    if is_oauth_authenticated():
        return get_deployment_name_from_jwt()
    else:
        return get_deployment_name_from_token(get_semgrep_app_token())


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
    Only works with API tokens (not JWTs).

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


def get_semgrep_access_token() -> str | None:
    """
    Returns the JWT access token, if it exists.
    """
    token = get_access_token()
    if token:
        return token.token

    return None


@lru_cache(maxsize=1)
def get_oauth_authorization_server_metadata(semgrep_api_url: str) -> dict[str, str]:
    oauth_url = f"{semgrep_api_url}/auth/oauth2/.well-known/oauth-authorization-server"
    response = requests.get(oauth_url, timeout=(2, 30))
    metadata = response.json()
    return dict(metadata)


def get_authorization_server_url(semgrep_api_url: str) -> str:
    metadata = get_oauth_authorization_server_metadata(semgrep_api_url)
    return metadata["issuer"]


def get_authorization_server_jwks_uri(semgrep_api_url: str) -> str:
    metadata = get_oauth_authorization_server_metadata(semgrep_api_url)
    return metadata["jwks_uri"]


def get_authorization_server_introspection_endpoint(semgrep_api_url: str) -> str:
    metadata = get_oauth_authorization_server_metadata(semgrep_api_url)
    return metadata["introspection_endpoint"]
