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
    """Start the streamable-http server on the available port.

    Returns a tuple of (port, process) to allow tests to check server logs.
    """
    # Start the streamable-http server, capturing output
    proc = subprocess.Popen(
        ["semgrep", "mcp", "-t", "streamable-http", "--port", str(available_port)],
        env={"SEMGREP_IS_HOSTED": "true", **os.environ},
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
    )
    # Wait briefly to ensure the server starts
    time.sleep(5)
    yield (available_port, proc)
    # Teardown: terminate the server
    proc.terminate()
    proc.wait()


@pytest.mark.slow
@pytest.mark.skip("flakey MCP test, see: semgrep-proprietary/pull/4985")
async def test_streamable_client_smoke(streamable_server):
    port, proc = streamable_server
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


@pytest.mark.slow
@pytest.mark.skip("flakey MCP test, see: semgrep-proprietary/pull/4985")
async def test_streamable_client_no_closed_resource_error(streamable_server):
    """Test that connecting to semgrep mcp with streamable-http doesn't get a ClosedResourceError.

    This test verifies that the FastMCP server is configured with stateless_http=False,
    which prevents ClosedResourceError when using streamable-http transport.

    When stateless_http=True, the server will log ClosedResourceError even if client
    operations appear to succeed.

    See OSS/cli/src/semgrep/commands/mcp.py:65-66 for context.
    """
    port, proc = streamable_server
    base_url = f"http://127.0.0.1:{port}"

    # Connect to the server and perform multiple operations
    async with streamablehttp_client(f"{base_url}/mcp") as (
        read_stream,
        write_stream,
        _,
    ):
        async with ClientSession(read_stream, write_stream) as session:
            # Initialize the session - this creates session state on the server
            await session.initialize()

            # List available tools - this should work if session state is maintained
            tools = await session.list_tools()
            assert tools is not None
            assert len(tools.tools) > 0

            # Make a second request to ensure session state persists
            tools_again = await session.list_tools()
            assert tools_again is not None
            assert len(tools_again.tools) > 0

    # Give the server a moment to flush logs
    time.sleep(1)

    # Check server logs for ClosedResourceError
    # We need to read without blocking, so check if data is available
    proc.terminate()
    stdout, stderr = proc.communicate(timeout=5)
    server_output = stderr.decode() + stdout.decode()

    # Assert that ClosedResourceError does NOT appear in server logs
    assert (
        "ClosedResourceError" not in server_output
    ), f"ClosedResourceError found in server logs! This means some improper resource management happened. Server output:\n{server_output[-2000:]}"
