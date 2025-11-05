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
import asyncio
import json
import sys
from typing import Any
from typing import Literal

from opentelemetry import trace
from pydantic import BaseModel

from semgrep.mcp.models import CodeFile
from semgrep.mcp.models import SemgrepScanResult
from semgrep.mcp.semgrep import run_semgrep_output
from semgrep.mcp.server import get_semgrep_scan_args
from semgrep.mcp.utilities.tracing import attach_scan_metrics
from semgrep.mcp.utilities.tracing import start_tracing
from semgrep.mcp.utilities.tracing import with_span
from semgrep.mcp.utilities.utils import get_deployment_id_from_token
from semgrep.mcp.utilities.utils import get_deployment_name_from_token
from semgrep.mcp.utilities.utils import get_semgrep_app_token
from semgrep.state import get_state


class PostToolHookResponse(BaseModel):
    # response = {
    #     "decision": "block"|undefined,
    #     "reason": ...,
    #     "hookSpecificOutput": {
    #         "hookEventName": ...,
    #         "additionalContext": ...,
    #     }
    # }
    decision: Literal["block"] | None = None
    reason: str | None = None


def load_file_path() -> tuple[CodeFile, Any]:
    hook_data = json.load(sys.stdin)
    print(hook_data, file=sys.stderr)
    return (
        CodeFile(
            path=hook_data["tool_input"]["file_path"],
            content=hook_data["tool_input"].get(
                "new_string", hook_data["tool_input"].get("content", "")
            ),
        ),
        hook_data["cwd"],
    )


async def run_cli_scan(top_level_span: trace.Span | None) -> PostToolHookResponse:
    with with_span(top_level_span, "semgrep_scan_cli (hook)") as span:
        state = get_state()
        state.metrics.clear_mcp()
        state.metrics.add_mcp(
            deployment_id=get_deployment_id_from_token(get_semgrep_app_token()),
            deployment_name=get_deployment_name_from_token(get_semgrep_app_token())
            or "",
            session_id="hook",  # TODO: No session id for hooks yet, using a placeholder
            tool_name="semgrep_scan_cli (hook)",
        )
        code_file, workspace_dir = load_file_path()
        args = get_semgrep_scan_args(code_file.path, None)
        output = await run_semgrep_output(top_level_span, args)
        scan_result: SemgrepScanResult = SemgrepScanResult.model_validate_json(output)
        hook_response = PostToolHookResponse(decision=None, reason=None)
        if len(scan_result.results) > 0:
            hook_response = PostToolHookResponse(
                decision="block",
                reason=str(scan_result.results),
            )
        else:
            hook_response = PostToolHookResponse(decision=None, reason=None)
        attach_scan_metrics(span, scan_result, workspace_dir, [code_file])
        state.metrics.send()
        return hook_response


def run_post_tool_scan_cli() -> None:
    with start_tracing("mcp-hook") as span:
        response = asyncio.run(run_cli_scan(top_level_span=span))
        print(response.model_dump_json(exclude_none=True))
        sys.exit(0)
