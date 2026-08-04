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
import pytest

from semgrep.mcp.server import _build_findings_filter


@pytest.mark.quick
def test_filter_defaults_to_primary_branch_with_no_optional_inputs():
    """When refs and verdict are absent, the filter falls back to the primary
    branch and leaves verdict/severities/confidences empty."""
    assert _build_findings_filter(
        status="ISSUE_TAB_OPEN",
        repos=["owner/repo"],
        severities=None,
        confidence=None,
        autotriage_verdict=None,
        refs=[],
    ) == {
        "status": ["ISSUE_TAB_OPEN"],
        "repositoryNames": ["owner/repo"],
        "severities": [],
        "confidences": [],
        "aiVerdicts": [],
        "on_primary_branch": True,
    }


@pytest.mark.quick
def test_filter_with_all_optional_inputs():
    """When refs is supplied it replaces on_primary_branch, and verdict /
    severities / confidences flow through into the filter."""
    assert _build_findings_filter(
        status="ISSUE_TAB_FIXING",
        repos=["owner/a", "owner/b"],
        severities=["SEVERITY_HIGH"],
        confidence=["CONFIDENCE_MEDIUM"],
        autotriage_verdict="VERDICT_TRUE_POSITIVE",
        refs=["main", "feature-x"],
    ) == {
        "status": ["ISSUE_TAB_FIXING"],
        "repositoryNames": ["owner/a", "owner/b"],
        "severities": ["SEVERITY_HIGH"],
        "confidences": ["CONFIDENCE_MEDIUM"],
        "aiVerdicts": ["VERDICT_TRUE_POSITIVE"],
        "refs": ["main", "feature-x"],
    }
