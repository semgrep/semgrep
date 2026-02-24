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
from typing import Literal

from opentelemetry import trace
from pydantic import BaseModel

from semgrep.mcp.models import CodeFile
from semgrep.mcp.models import SemgrepScanResult
from semgrep.mcp.semgrep import run_semgrep_output
from semgrep.mcp.server import get_semgrep_app_token
from semgrep.mcp.server import get_semgrep_scan_args
from semgrep.mcp.utilities.tracing import attach_git_info
from semgrep.mcp.utilities.tracing import attach_scan_metrics
from semgrep.mcp.utilities.tracing import start_tracing
from semgrep.mcp.utilities.tracing import with_hook_span
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


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


def load_file_path() -> tuple[CodeFile, str]:
    """
    Returns the code file and the current working directory
    """
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


@with_hook_span(
    span_name="semgrep_scan_cli (hook) (post-tool)",
    send_metrics=True,
    is_semgrep_scan=True,
)
async def run_cli_scan(top_level_span: trace.Span | None) -> PostToolHookResponse:
    code_file, workspace_dir = load_file_path()
    attach_git_info(trace.get_current_span(), workspace_dir)
    args = get_semgrep_scan_args(code_file.path, config="hooks")
    logger.info(f"Running scan with args: {args}")
    output = await run_semgrep_output(top_level_span, args)
    scan_result: SemgrepScanResult = SemgrepScanResult.model_validate_json(output)
    hook_response = PostToolHookResponse(decision=None, reason=None)
    if len(scan_result.results) > 0:
        reason = str(
            [
                {
                    "line": r["start"]["line"],
                    "display_name": r["extra"]["metadata"].get("display-name"),
                    "message": r["extra"]["message"],
                    "severity": r["extra"]["severity"],
                    "cwe": r["extra"]["metadata"].get("cwe"),
                }
                for r in scan_result.results
            ]
        )
        hook_response = PostToolHookResponse(decision="block", reason=reason)
    else:
        hook_response = PostToolHookResponse(decision=None, reason=None)
    attach_scan_metrics(
        trace.get_current_span(), scan_result, workspace_dir, [code_file]
    )
    return hook_response


def run_post_tool_scan_cli(agent: str) -> None:
    with start_tracing("mcp-hook") as span:
        if agent == "cursor":
            # This hook is not supported for Cursor yet because
            # Cursor's afterFileEdit hook does not support
            # any outputs. There is no way to communicate the scan result to Cursor.
            # See: https://cursor.com/docs/agent/hooks#afterfileedit
            #
            # The input format of a Cursor hook is also different
            # from that of a Claude Code hook. And we are assuming the
            # Claude Code format here.
            print("This hook is not supported for Cursor.", file=sys.stderr)
            sys.exit(2)

        if get_semgrep_app_token() is None:
            # According to Claude docs, when the hook errors with exit code 2, it shows stderr to Claude.
            # So, we are printing the warning to stderr here.
            # See: https://code.claude.com/docs/en/hooks#exit-code-2-behavior-per-event
            print(
                "No SEMGREP_APP_TOKEN found, please login to Semgrep to use this hook.",
                file=sys.stderr,
            )
            sys.exit(2)

        response = asyncio.run(run_cli_scan(span))
        print(response.model_dump_json(exclude_none=True))
        sys.exit(0)
