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
"""Unit tests for lazy loading of the MCP command.

See https://github.com/semgrep/semgrep/issues/12069: with `mcp` 2.x
installed, `mcp.server.fastmcp` raises ModuleNotFoundError at import
time, and importing `semgrep.cli` used to pull that in through
`semgrep.commands.mcp`, so every `semgrep` command failed at startup.
The MCP SDK must only be imported when `semgrep mcp` actually runs.
"""
import sys
import types

import pytest
from click.testing import CliRunner

# Modules whose import must not be required to import the CLI. These
# mirror the `mcp.server` submodules `semgrep.commands.mcp` used to
# import at module level.
MCP_SERVER_MODULES = [
    "mcp.server.auth.settings",
    "mcp.server.fastmcp",
    "mcp.server.transport_security",
]


def _raising_module(name: str) -> types.ModuleType:
    """A stand-in for an `mcp.server` submodule that fails to import.

    Mirrors the mcp 2.x stub, which raises ModuleNotFoundError when
    anything is imported from it.
    """
    module = types.ModuleType(name)

    def _getattr(attr: str) -> None:
        raise ModuleNotFoundError(
            f"No module named '{name}'. This is mcp 2.x, where FastMCP "
            "was renamed to MCPServer."
        )

    module.__getattr__ = _getattr  # type: ignore[method-assign]
    return module


@pytest.mark.quick
def test_cli_imports_without_mcp_server(monkeypatch: pytest.MonkeyPatch) -> None:
    """`import semgrep.cli` works when the MCP SDK cannot be imported."""
    # Evict the CLI modules so the import below really re-executes
    # them against the stubbed SDK.
    for name in ("semgrep.cli", "semgrep.commands.mcp"):
        monkeypatch.delitem(sys.modules, name, raising=False)
    for name in MCP_SERVER_MODULES:
        monkeypatch.setitem(sys.modules, name, _raising_module(name))

    import semgrep.cli

    assert "mcp" in semgrep.cli.cli.commands


@pytest.mark.quick
def test_mcp_help_without_mcp_server(monkeypatch: pytest.MonkeyPatch) -> None:
    """`semgrep mcp --help` renders without importing the MCP SDK."""
    for name in ("semgrep.cli", "semgrep.commands.mcp"):
        monkeypatch.delitem(sys.modules, name, raising=False)
    for name in MCP_SERVER_MODULES:
        monkeypatch.setitem(sys.modules, name, _raising_module(name))

    from semgrep.cli import cli

    result = CliRunner().invoke(cli, ["mcp", "--help"])
    assert result.exit_code == 0
    assert "Entry point for the MCP server" in result.output
