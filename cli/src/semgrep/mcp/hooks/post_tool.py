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
from pathlib import Path
from typing import Literal

from opentelemetry import trace
from pydantic import BaseModel

from semgrep.mcp.models import CodeFile
from semgrep.mcp.models import SemgrepScanResult
from semgrep.mcp.semgrep import run_semgrep_output
from semgrep.mcp.server import get_semgrep_app_token
from semgrep.mcp.server import get_semgrep_scan_args
from semgrep.mcp.utilities.tracing import attach_agent_info
from semgrep.mcp.utilities.tracing import attach_git_info
from semgrep.mcp.utilities.tracing import attach_scan_metrics
from semgrep.mcp.utilities.tracing import start_tracing
from semgrep.mcp.utilities.tracing import with_hook_span
from semgrep.mcp.utilities.utils import CLAUDE_AGENT_STRING
from semgrep.mcp.utilities.utils import CURSOR_AGENT_STRING
from semgrep.mcp.utilities.utils import HookResultStatus
from semgrep.mcp.utilities.utils import WINDSURF_AGENT_STRING
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


def load_file_path_claude() -> tuple[CodeFile, str]:
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


def load_file_path_windsurf() -> tuple[CodeFile, str]:
    hook_data = json.load(sys.stdin)
    print(hook_data, file=sys.stderr)
    # Get content from file path in the windsurf tool input.
    # We have to do this because Windsurf's tool input is an array
    # of edits, and it is non-trivial to combine those edits into a single file.
    # It is easier to just read the file.
    file_path = hook_data["tool_info"]["file_path"]
    with open(file_path, "r", encoding="utf-8") as f:
        content = f.read()
        return (
            CodeFile(
                path=file_path,
                content=content,
            ),
            # We are simply using the parent directory as the workspace directory because
            # Windsurf hook inputs do not contain the workspace directory.
            # We are only using the workspace directory to attach git info in this
            # code path.
            str(Path(file_path).parent),
        )


@with_hook_span(
    span_name="semgrep_scan_cli (hook) (post-tool)",
    send_metrics=True,
    is_semgrep_scan=True,
)
async def run_cli_scan(
    top_level_span: trace.Span | None, agent: str
) -> tuple[PostToolHookResponse, HookResultStatus]:
    code_file, workspace_dir = (
        load_file_path_claude()
        if agent == CLAUDE_AGENT_STRING
        else load_file_path_windsurf()
    )
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
        hook_response, status = (
            PostToolHookResponse(decision="block", reason=reason),
            HookResultStatus.BLOCK,
        )
    else:
        hook_response, status = (
            PostToolHookResponse(decision=None, reason=None),
            HookResultStatus.SUCCESS,
        )
    attach_scan_metrics(
        trace.get_current_span(), scan_result, workspace_dir, [code_file]
    )
    return hook_response, status


def run_post_tool_scan_cli(agent: str) -> None:
    with start_tracing("mcp-hook") as span:
        attach_agent_info(span, agent)
        if agent == CURSOR_AGENT_STRING:
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

        response, status = asyncio.run(run_cli_scan(span, agent))
        if agent == WINDSURF_AGENT_STRING:
            print(response.model_dump_json(exclude_none=True), file=sys.stderr)
            if status == HookResultStatus.BLOCK:
                # Since Windsurf does not support a specific output format, I found
                # that asking the agent to ask the user if they want to fix the security issues
                # works best. It makes the agent behave similar to how Claude responds
                # to a block result.
                print(
                    f"Based on the results above, ask the user if they want to fix the security issues.",
                    file=sys.stderr,
                )
                sys.exit(2)
            else:
                sys.exit(0)
        print(response.model_dump_json(exclude_none=True))
        sys.exit(0)
