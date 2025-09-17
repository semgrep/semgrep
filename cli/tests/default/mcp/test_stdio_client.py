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

import pytest
from mcp import ClientSession
from mcp import StdioServerParameters
from mcp.client.stdio import stdio_client


def mk_server_params(port: int):
    # Create server parameters for stdio connection
    return StdioServerParameters(
        command="semgrep",  # Executable
        args=["mcp", "--port", str(port)],  # Optional command line arguments
        env={
            "USE_SEMGREP_RPC": "false",
            "SEMGREP_IS_HOSTED": "true",
            **os.environ,
        },  # Optional environment variables
    )


@pytest.mark.slow
async def test_stdio_client(available_port):
    async with stdio_client(mk_server_params(available_port)) as (read, write):
        async with ClientSession(read, write) as session:
            # Initialize the connection
            await session.initialize()

            # List available prompts
            prompts = await session.list_prompts()

            print(prompts)
            # List available resources
            resources = await session.list_resources()

            # List available tools
            print(resources)

            tools = await session.list_tools()

            print(tools)

            # Read a resource
            print("Reading resource")
            content, _ = await session.read_resource("semgrep://rule/schema")

            # Call a tool
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
            content = json.loads(results.content[0].text)
            assert isinstance(content, dict)
            assert content["paths"]["scanned"] == ["hello_world.py"]
            print(json.dumps(content, indent=2))
