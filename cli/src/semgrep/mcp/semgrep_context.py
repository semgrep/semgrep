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
import asyncio
import json
from typing import Any

from mcp.shared.exceptions import McpError
from mcp.types import ErrorData
from mcp.types import INTERNAL_ERROR
from mcp.types import INVALID_REQUEST
from opentelemetry import trace

from semgrep.mcp.models import WhoamiResult
from semgrep.mcp.utilities.utils import get_current_user_from_jwt
from semgrep.mcp.utilities.utils import is_oauth_authenticated
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

################################################################################
# Communicating with Semgrep over RPC #
################################################################################


class SemgrepContext:
    process: asyncio.subprocess.Process | None
    stdin: asyncio.StreamWriter | None
    stdout: asyncio.StreamReader | None
    top_level_span: trace.Span | None
    session_id: str

    is_hosted: bool
    pro_engine_available: bool
    use_rpc: bool | None
    oauth_info: WhoamiResult | None

    def __init__(
        self,
        top_level_span: trace.Span | None,
        is_hosted: bool,
        pro_engine_available: bool,
        use_rpc: bool | None,
        session_id: str,
        process: asyncio.subprocess.Process | None = None,
    ) -> None:
        self.process = process
        self.top_level_span = top_level_span
        self.is_hosted = is_hosted
        self.pro_engine_available = pro_engine_available
        self.use_rpc = use_rpc
        self.session_id = session_id
        self.oauth_info = None

        if process is None:
            self.stdin = None
            self.stdout = None
        elif process.stdin is not None and process.stdout is not None:
            self.stdin = process.stdin
            self.stdout = process.stdout
        else:
            raise McpError(
                ErrorData(
                    code=INTERNAL_ERROR,
                    message="Semgrep process stdin/stdout not available",
                )
            )

        if is_oauth_authenticated():
            self.oauth_info = get_current_user_from_jwt()

    async def communicate(self, line: str) -> str:
        if self.stdin is None or self.stdout is None:
            raise McpError(
                ErrorData(
                    code=INTERNAL_ERROR,
                    message="Semgrep process stdin/stdout not available",
                )
            )

        self.stdin.write(f"{line}\n".encode())
        await self.stdin.drain()

        stdout = await self.stdout.readline()
        return stdout.decode()

    async def send_request(self, request: str, **kwargs: Any) -> str:
        if self.is_hosted:
            error_string = """
                Cannot run semgrep scan via RPC because the MCP server is hosted.
                RPC is only available when the MCP server is running locally.
                Use the `semgrep_scan` tool instead.
                """
            raise McpError(ErrorData(code=INVALID_REQUEST, message=error_string))

        if not self.pro_engine_available:
            error_string = """
                Cannot run semgrep scan via RPC because the Pro Engine is not installed.
                Try running `semgrep install-semgrep-pro` to install it.
                """
            raise McpError(ErrorData(code=INVALID_REQUEST, message=error_string))

        payload = {"method": request, **kwargs}

        try:
            return await self.communicate(json.dumps(payload))
        except Exception as e:
            # TODO: move this code out of send_request, make a proper result
            # type and interpret at the call site
            # this is not specific to semgrep_scan_rpc, but it is for now!!!
            msg = f"""
              Error sending request to semgrep (RPC server may not be running): {e}.
              Try using `semgrep_scan` instead.
            """
            logger.error(msg)

            raise McpError(ErrorData(code=INTERNAL_ERROR, message=msg)) from e

    def shutdown(self) -> None:
        if self.process is not None:
            self.process.terminate()
