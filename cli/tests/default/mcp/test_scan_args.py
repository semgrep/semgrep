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
from mcp.shared.exceptions import McpError

from semgrep.mcp.server import get_semgrep_scan_args


@pytest.mark.quick
def test_scan_args_use_semgrep_rules_envvar(monkeypatch):
    monkeypatch.setenv("SEMGREP_SEND_METRICS", "off")
    monkeypatch.setenv("SEMGREP_RULES", "p/default p/python")

    assert get_semgrep_scan_args("/tmp/project") == [
        "scan",
        "--json",
        "--experimental",
        "--x-mcp",
        "--config",
        "p/default",
        "--config",
        "p/python",
        "/tmp/project",
    ]


@pytest.mark.quick
def test_scan_args_keep_explicit_config(monkeypatch):
    monkeypatch.setenv("SEMGREP_SEND_METRICS", "off")
    monkeypatch.setenv("SEMGREP_RULES", "p/default")

    assert get_semgrep_scan_args("/tmp/project", config="custom.yaml") == [
        "scan",
        "--json",
        "--experimental",
        "--x-mcp",
        "--config",
        "custom.yaml",
        "/tmp/project",
    ]


@pytest.mark.quick
def test_scan_args_reject_auto_config_with_metrics_off(monkeypatch):
    monkeypatch.setenv("SEMGREP_SEND_METRICS", "off")
    monkeypatch.delenv("SEMGREP_RULES", raising=False)

    with pytest.raises(McpError, match="Cannot run scan with auto config"):
        get_semgrep_scan_args("/tmp/project")


@pytest.mark.quick
def test_scan_args_reject_auto_from_semgrep_rules_with_metrics_off(monkeypatch):
    monkeypatch.setenv("SEMGREP_SEND_METRICS", "off")
    monkeypatch.setenv("SEMGREP_RULES", "auto")

    with pytest.raises(McpError, match="Cannot run scan with auto config"):
        get_semgrep_scan_args("/tmp/project")
