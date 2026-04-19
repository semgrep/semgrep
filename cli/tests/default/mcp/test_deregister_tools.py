#
# Copyright (c) 2026 Semgrep Inc.
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
import pytest
from mcp.server.fastmcp import FastMCP

from semgrep.mcp.server import deregister_tools
from semgrep.mcp.server import register
from semgrep.mcp.server import TOOL_DISABLE_ENV_VARS


@pytest.fixture
def clean_gate_env(monkeypatch):
    """
    Remove any ambient `*_DISABLED` / `SEMGREP_IS_HOSTED` env vars so each
    test exercises only the gates it sets itself. Without this, a runner
    that exports e.g. `SEMGREP_SCAN_REMOTE_DISABLED=true` would make the
    env-gate loop silently remove tools and invalidate assertions.
    """
    for env_var in TOOL_DISABLE_ENV_VARS:
        monkeypatch.delenv(env_var, raising=False)
    monkeypatch.delenv("SEMGREP_IS_HOSTED", raising=False)
    return monkeypatch


@pytest.mark.quick
def test_deregister_tools_idempotent_stdio(clean_gate_env):
    """
    Calling deregister_tools twice in a row with transport="stdio" must not
    raise, even though the second call encounters tools that the first call
    already removed. Regression for
    https://github.com/semgrep/semgrep/issues/11646.
    """
    mcp = FastMCP("test")
    register(mcp)
    deregister_tools(mcp, "stdio")
    deregister_tools(mcp, "stdio")


@pytest.mark.quick
def test_deregister_tools_env_gate_collision(clean_gate_env):
    """
    If SEMGREP_SCAN_REMOTE_DISABLED=true has already removed
    "semgrep_scan_remote" in the env-gate loop at the top of the function,
    the non-hosted branch that also tries to remove "semgrep_scan_remote"
    must not raise KeyError. Regression for
    https://github.com/semgrep/semgrep/issues/11646.
    """
    clean_gate_env.setenv("SEMGREP_SCAN_REMOTE_DISABLED", "true")
    mcp = FastMCP("test")
    register(mcp)
    deregister_tools(mcp, "streamable-http")
    assert "semgrep_scan_remote" not in mcp._tool_manager._tools


@pytest.mark.quick
def test_deregister_tools_hosted_mode(clean_gate_env):
    """
    Hosted mode removes semgrep_scan + semgrep_scan_supply_chain and leaves
    semgrep_scan_remote in place. Running it twice must remain idempotent.
    """
    clean_gate_env.setenv("SEMGREP_IS_HOSTED", "true")
    mcp = FastMCP("test")
    register(mcp)
    deregister_tools(mcp, "streamable-http")
    deregister_tools(mcp, "streamable-http")
    assert "semgrep_scan" not in mcp._tool_manager._tools
    assert "semgrep_scan_supply_chain" not in mcp._tool_manager._tools
    assert "semgrep_scan_remote" in mcp._tool_manager._tools
