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
import socket

import pytest


@pytest.fixture(autouse=True)
def mcp_test_env():
    # Disable tracing for MCP tests. We don't want tests to send tracing data to Datadog since
    # they show up as abnormally short spans. This also implicitly disables metrics.
    os.environ["SEMGREP_MCP_DISABLE_TRACING"] = "true"
    yield
    if "SEMGREP_MCP_DISABLE_TRACING" in os.environ:
        del os.environ["SEMGREP_MCP_DISABLE_TRACING"]


@pytest.fixture
def available_port():
    """Find and return an available port number."""
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.bind(("", 0))  # Bind to any available port
        s.listen(1)
        port = s.getsockname()[1]  # Get the port number that was assigned
    return port
