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
# Agent identifiers used by the MCP server and its hooks.
#
# This module intentionally has no imports: the identifiers are needed
# to define the `semgrep mcp` command-line interface, which must be
# importable without the MCP SDK installed. The SDK is only required
# when the MCP server itself runs, so anything that imports the SDK
# must live elsewhere (see semgrep/commands/mcp.py).
CLAUDE_AGENT_STRING = "claude"
CURSOR_AGENT_STRING = "cursor"
WINDSURF_AGENT_STRING = "windsurf"
