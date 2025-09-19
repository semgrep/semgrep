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

import pytest
from mcp import ClientSession
from mcp import StdioServerParameters
from mcp.client.stdio import stdio_client
from mcp.types import TextContent

from semgrep.mcp.models import Finding


@pytest.mark.slow
@pytest.mark.skipif(
    not os.environ.get("SEMGREP_APP_TOKEN"),
    reason="SEMGREP_APP_TOKEN not set; skipping integration test.",
)
async def test_semgrep_findings_sca():
    server_params = StdioServerParameters(
        command="python",
        args=["src/semgrep_mcp/server.py"],
        env={**os.environ},
    )

    async with stdio_client(server_params) as (read, write):
        async with ClientSession(read, write) as session:
            await session.initialize()
            results = await session.call_tool(
                "semgrep_findings", {"issue_type": ["sca"]}
            )
            assert results is not None

            # Validate findings against the model
            for content in results.content:
                assert isinstance(content, TextContent)
                Finding.model_validate_json(content.text)


@pytest.mark.slow
@pytest.mark.skipif(
    not os.environ.get("SEMGREP_APP_TOKEN"),
    reason="SEMGREP_APP_TOKEN not set; skipping integration test.",
)
async def test_semgrep_findings_sast():
    server_params = StdioServerParameters(
        command="python",
        args=["src/semgrep_mcp/server.py"],
        env={**os.environ},
    )

    async with stdio_client(server_params) as (read, write):
        async with ClientSession(read, write) as session:
            await session.initialize()
            results = await session.call_tool(
                "semgrep_findings", {"issue_type": ["sast", "sca"]}
            )
            assert results is not None

            # Validate findings against the model
            for content in results.content:
                assert isinstance(content, TextContent)
                finding = Finding.model_validate_json(content.text)
                print(finding)
