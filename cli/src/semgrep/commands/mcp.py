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
import sys

import click
from mcp.server.auth.settings import AuthSettings
from mcp.server.auth.settings import ClientRegistrationOptions
from mcp.server.fastmcp import FastMCP
from pydantic import AnyHttpUrl

from semgrep import __VERSION__
from semgrep.mcp.hooks.inject_secure_defaults import run_inject_secure_defaults_hook
from semgrep.mcp.hooks.post_tool import run_post_tool_scan_cli
from semgrep.mcp.hooks.stop import run_after_file_edit_hook
from semgrep.mcp.hooks.stop import run_stop_scan_cli
from semgrep.mcp.server import deregister_tools
from semgrep.mcp.server import get_authorization_server_url
from semgrep.mcp.server import get_semgrep_api_url
from semgrep.mcp.server import register
from semgrep.mcp.server import server_lifespan
from semgrep.mcp.server import setup_oauth_routes
from semgrep.mcp.utilities.token_verifier import make_token_verifier
from semgrep.verbose_logging import getLogger


logger = getLogger(__name__)

POST_TOOL_CLI_SCAN_FLAG = "post-tool-cli-scan"
INJECT_SECURE_DEFAULTS_FLAG = "inject-secure-defaults"
INJECT_SHORT_CONTEXT_FLAG = "inject-secure-defaults-short"
STOP_CLI_SCAN_FLAG = "stop-cli-scan"
RECORD_FILE_EDIT_HOOK_FLAG = "record-file-edit"


def setup_mcp_server(host: str, port: int) -> FastMCP:
    server_url = f"http://{host}:{port}"
    semgrep_api_url = get_semgrep_api_url()
    authorization_server_url = get_authorization_server_url(semgrep_api_url)
    auth_settings = AuthSettings(
        issuer_url=AnyHttpUrl(authorization_server_url),
        resource_server_url=AnyHttpUrl(server_url),
        required_scopes=[],
        client_registration_options=ClientRegistrationOptions(
            enabled=True,
            client_secret_expiry_seconds=3600,
            valid_scopes=["openid", "profile", "email", "offline_access"],
            default_scopes=["openid", "profile", "email", "offline_access"],
        ),
    )
    token_verifier = make_token_verifier(server_url, semgrep_api_url)

    mcp = FastMCP(
        "Semgrep",
        stateless_http=False,
        json_response=True,
        lifespan=server_lifespan,
        port=port,
        auth=auth_settings,
        token_verifier=token_verifier,
    )

    setup_oauth_routes(mcp, server_url)

    return mcp


# ---------------------------------------------------------------------------------
# MCP Server Entry Point
# ---------------------------------------------------------------------------------


@click.command(context_settings={"help_option_names": ["-h", "--help"]})
@click.version_option(
    __VERSION__,
    "-v",
    "--version",
    help="Show version and exit.",
)
@click.option(
    "-t",
    "--transport",
    type=click.Choice(["stdio", "streamable-http"]),
    default="stdio",
    envvar="MCP_TRANSPORT",
    help="Transport protocol to use: stdio or streamable-http",
)
@click.option(
    "-p",
    "--port",
    type=int,
    default=8000,
    envvar="SEMGREP_MCP_PORT",
    help="Port to use for the MCP server",
)
@click.option(
    "-k",
    "--hook",
    type=click.Choice(
        [
            POST_TOOL_CLI_SCAN_FLAG,
            STOP_CLI_SCAN_FLAG,
            RECORD_FILE_EDIT_HOOK_FLAG,
            INJECT_SECURE_DEFAULTS_FLAG,
            INJECT_SHORT_CONTEXT_FLAG,
        ]
    ),
    default=None,
    help=f"""Run specified functionality for agent hooks.
    Currently supports:
    1. Running a Semgrep CLI scan (via PostTool hook, flag: `{POST_TOOL_CLI_SCAN_FLAG}`).
    2. Running a Semgrep CLI scan (via Stop hook, flag: `{STOP_CLI_SCAN_FLAG}`), must be used in conjunction with an AfterFileEdit hook (flag: `{RECORD_FILE_EDIT_HOOK_FLAG}`).
    3. Injecting secure defaults context (via UserPromptSubmit hook or SessionStart hook, flag: `{INJECT_SECURE_DEFAULTS_FLAG}`).
    """,
)
@click.option(
    "-a",
    "--agent",
    type=click.Choice(["claude", "cursor"]),
    default="claude",
    help="Agent to use for the MCP server",
)
def semgrep_mcp(transport: str, port: int, hook: str | None, agent: str) -> None:
    """Entry point for the MCP server

    Supports stdio and streamable-http transports.
    For stdio, it will read from stdin and write to stdout.
    For streamable-http, it will start an HTTP server on port 8000.
    """
    # Set environment variable to track scans by MCP
    os.environ["SEMGREP_MCP"] = "true"

    if hook == POST_TOOL_CLI_SCAN_FLAG:
        run_post_tool_scan_cli(agent)
        return

    if hook == RECORD_FILE_EDIT_HOOK_FLAG:
        run_after_file_edit_hook(agent)
        return

    if hook == STOP_CLI_SCAN_FLAG:
        run_stop_scan_cli(agent)
        return

    if hook == INJECT_SECURE_DEFAULTS_FLAG:
        run_inject_secure_defaults_hook(agent)
        return

    if hook == INJECT_SHORT_CONTEXT_FLAG:
        run_inject_secure_defaults_hook(agent, inject_short_context=True)
        return

    # Log the start of the MCP server
    logger.info(f"Starting Semgrep MCP server version v{__VERSION__}")

    # Create a fast MCP server
    # Note: stateless_http should be False for proper session management
    # When True, it causes ClosedResourceError in streamable-http transport

    mcp = setup_mcp_server("localhost", port)

    # based on env vars, disable certain tools
    register(mcp)
    deregister_tools(mcp, transport)

    try:
        if transport == "stdio":
            mcp.run(transport="stdio")
        elif transport == "streamable-http":
            mcp.run(transport="streamable-http")
        else:
            raise ValueError(f"Invalid transport: {transport}")
    except KeyboardInterrupt:
        logger.info("Stopping MCP server")
        sys.exit(0)
    except Exception as e:
        logger.error(f"Error running MCP server: {e}")
        raise
