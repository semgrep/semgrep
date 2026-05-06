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
import os
from pathlib import Path
from tempfile import NamedTemporaryFile

import pytest
from mcp import stdio_client
from mcp import StdioServerParameters
from mcp.client.session import ClientSession
from mcp.types import TextContent


def mk_server_params(port: int):
    # Create server parameters for stdio connection
    return StdioServerParameters(
        command="semgrep",  # Executable
        args=["mcp", "--port", str(port)],  # Optional command line arguments
        env={
            "SEMGREP_SEND_METRICS": "off",
            "SEMGREP_METRICS": "off",
            "USE_SEMGREP_RPC": "false",
            **os.environ,
        },
    )


@pytest.mark.slow
@pytest.mark.skipif(
    not os.environ.get("SEMGREP_APP_TOKEN"),
    reason="SEMGREP_APP_TOKEN not set; skipping integration test.",
)
async def test_local_scan(available_port):
    async with stdio_client(mk_server_params(available_port)) as (read, write):
        async with ClientSession(read, write) as session:
            # Initializing session...
            await session.initialize()
            # Session initialized

            with NamedTemporaryFile(
                "w", prefix="hello_world", suffix=".py", encoding="utf-8"
            ) as tmp:
                tmp.write("def hello(): print('Hello, World!')")
                tmp.flush()

                path = tmp.name

                # Scan code for security issues using local semgrep_scan
                results = await session.call_tool(
                    "semgrep_scan",
                    {
                        "code_files": [
                            {
                                "path": str(Path(path).absolute()),
                            }
                        ],
                    },
                )
                print(results)
                assert results is not None
                content_block = results.content[0]
                assert isinstance(content_block, TextContent)
                print(content_block.text)
                content = content_block.text
                error_message = "Cannot run scan with auto config when metrics are off."
                assert error_message in content
