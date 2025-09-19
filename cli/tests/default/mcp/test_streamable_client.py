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
import json
import os
import subprocess
import time

import pytest
from mcp.client.session import ClientSession
from mcp.client.streamable_http import streamablehttp_client
from mcp.types import TextContent


@pytest.fixture
def streamable_server(available_port):
    """Start the streamable-http server on the available port."""
    # Start the streamable-http server
    proc = subprocess.Popen(
        ["semgrep", "mcp", "-t", "streamable-http", "--port", str(available_port)],
        env={"SEMGREP_IS_HOSTED": "true", **os.environ},
    )
    # Wait briefly to ensure the server starts
    time.sleep(5)
    yield available_port
    # Teardown: terminate the server
    proc.terminate()
    proc.wait()


@pytest.mark.slow
async def test_streamable_client_smoke(streamable_server):
    port = streamable_server
    base_url = f"http://127.0.0.1:{port}"
    async with streamablehttp_client(f"{base_url}/mcp") as (
        read_stream,
        write_stream,
        _,
    ):
        async with ClientSession(read_stream, write_stream) as session:
            # Initializing session...
            await session.initialize()
            # Session initialized

            # Scan code for security issues
            results = await session.call_tool(
                "semgrep_scan_remote",
                {
                    "code_files": [
                        {
                            "path": "hello_world.py",
                            "content": "def hello(): print('Hello, World!')",
                        }
                    ]
                },
            )
            # We have results!
            assert results is not None
            content_block = results.content[0]
            assert isinstance(content_block, TextContent)
            content = json.loads(content_block.text)
            assert isinstance(content, dict)
            assert content["paths"]["scanned"] == ["hello_world.py"]
            print(json.dumps(content, indent=2))
