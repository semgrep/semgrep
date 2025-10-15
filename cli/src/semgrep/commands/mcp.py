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

import click
from mcp.server.fastmcp import FastMCP

from semgrep import __VERSION__
from semgrep.mcp.server import deregister_tools
from semgrep.mcp.server import register
from semgrep.mcp.server import server_lifespan
from semgrep.verbose_logging import getLogger


logger = getLogger(__name__)

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
    type=click.Choice(["stdio", "streamable-http", "sse"]),
    default="stdio",
    envvar="MCP_TRANSPORT",
    help="Transport protocol to use: stdio, streamable-http, or sse (legacy)",
)
@click.option(
    "-p",
    "--port",
    type=int,
    default=8000,
    envvar="SEMGREP_MCP_PORT",
    help="Port to use for the MCP server",
)
def semgrep_mcp(transport: str, port: int) -> None:
    """Entry point for the MCP server

    Supports stdio, streamable-http, and sse transports.
    For stdio, it will read from stdin and write to stdout.
    For streamable-http and sse, it will start an HTTP server on port 8000.
    """
    logger.info(f"Starting Semgrep MCP server version v{__VERSION__}")

    # Create a fast MCP server
    # Note: stateless_http should be False for proper session management
    # When True, it causes ClosedResourceError in streamable-http transport
    mcp = FastMCP(
        "Semgrep",
        stateless_http=False,
        json_response=True,
        lifespan=server_lifespan,
        port=port,
    )

    # Set environment variable to track scans by MCP
    os.environ["SEMGREP_MCP"] = "true"
    os.environ["SEMGREP_USER_AGENT_APPEND"] = "(MCP)"

    # based on env vars, disable certain tools
    register(mcp)
    deregister_tools(mcp)

    if transport == "stdio":
        mcp.run(transport="stdio")
    elif transport == "streamable-http":
        mcp.run(transport="streamable-http")
    elif transport == "sse":
        mcp.run(transport="sse")
    else:
        raise ValueError(f"Invalid transport: {transport}")
