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
import shutil
import sys
import tempfile
from pathlib import Path

from opentelemetry import trace
from pydantic import BaseModel

from semgrep.mcp.models import CodePath
from semgrep.mcp.models import SemgrepScanResult
from semgrep.mcp.semgrep import run_semgrep_output
from semgrep.mcp.server import create_temp_files_from_code_content
from semgrep.mcp.server import get_semgrep_scan_args
from semgrep.mcp.server import validate_local_files
from semgrep.mcp.utilities.tracing import attach_scan_metrics
from semgrep.mcp.utilities.tracing import start_tracing
from semgrep.mcp.utilities.tracing import with_hook_span

# ---------------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------------

CACHE_FILE = Path(tempfile.gettempdir()) / "semgrep-mcp" / "edited-files.json"
CACHE_FILE.parent.mkdir(parents=True, exist_ok=True)

FILE_EDIT_LIMIT = 500

# ---------------------------------------------------------------------------------
# Models
# ---------------------------------------------------------------------------------


class StopHookResponse(BaseModel):
    followup_message: str | None = None


# ---------------------------------------------------------------------------------
# Utils for recording file edits
# ---------------------------------------------------------------------------------


def load_edited_file_paths() -> list[str]:
    if not CACHE_FILE.exists():
        return []
    with open(CACHE_FILE, "r") as f:
        file_paths: list[str] = json.load(f)
        print(f"Loaded edited file paths: {file_paths}", file=sys.stderr)
        return file_paths


def append_edited_file_path(file_path: str) -> None:
    edited_file_paths = load_edited_file_paths()
    if len(edited_file_paths) >= FILE_EDIT_LIMIT:
        print(
            f"File edit limit reached ({FILE_EDIT_LIMIT}). Ignoring file path {file_path}",
            file=sys.stderr,
        )
        return
    if file_path in edited_file_paths:
        print(f"File path {file_path} already recorded", file=sys.stderr)
        return
    edited_file_paths.append(file_path)
    with open(CACHE_FILE, "w") as f:
        json.dump(edited_file_paths, f)


def clear_edited_file_paths() -> None:
    CACHE_FILE.unlink(missing_ok=True)


# ---------------------------------------------------------------------------------
# Utils for loading hook data
# ---------------------------------------------------------------------------------


def load_file_path() -> str:
    hook_data = json.load(sys.stdin)
    return str(hook_data["file_path"])


def load_workspace_root() -> str:
    hook_data = json.load(sys.stdin)
    return str(hook_data["workspace_roots"][0])  # assume only one workspace root


# ---------------------------------------------------------------------------------
# Hooks
# ---------------------------------------------------------------------------------


@with_hook_span(
    span_name="record_file_edit (hook)",
    send_metrics=True,
    is_semgrep_scan=False,
)
async def record_file_edit(top_level_span: trace.Span | None) -> None:
    file_path = load_file_path()
    append_edited_file_path(file_path)
    return


@with_hook_span(
    span_name="semgrep_scan_cli (hook) (stop)",
    send_metrics=True,
    is_semgrep_scan=True,
)
async def run_cli_scan(top_level_span: trace.Span | None) -> StopHookResponse:
    temp_dir = None
    try:
        edited_file_paths = [
            CodePath(path=file_path) for file_path in load_edited_file_paths()
        ]
        validated_local_files = validate_local_files(edited_file_paths)
        temp_dir = create_temp_files_from_code_content(validated_local_files)
        args = get_semgrep_scan_args(temp_dir, None)
        output = await run_semgrep_output(top_level_span, args)
        scan_result: SemgrepScanResult = SemgrepScanResult.model_validate_json(output)

        if len(scan_result.results) > 0:
            hook_response = StopHookResponse(
                followup_message=f"Found {len(scan_result.results)} security findings in {dir}. Details: {scan_result.results}"
            )
        else:
            hook_response = StopHookResponse(followup_message=None)

        attach_scan_metrics(
            trace.get_current_span(),
            scan_result,
            load_workspace_root(),
            validated_local_files,  # TODO: need to refactor attach_scan_metrics to not require code_files
        )
        return hook_response
    finally:
        if temp_dir:
            # Clean up temporary files
            shutil.rmtree(temp_dir, ignore_errors=True)
        clear_edited_file_paths()


# ---------------------------------------------------------------------------------
# Entry points
# ---------------------------------------------------------------------------------


def run_stop_scan_cli(agent: str) -> None:
    with start_tracing("mcp-hook") as span:
        if agent == "claude":
            # This hook is not supported for Claude because Claude hooks' input format
            # is different from that of Cursor hooks. We are assuming the
            # Cursor format here.
            #
            # Also, this is a workaround for Cursor so we can scan after file edits.
            # There are limitations in Cursor's afterFileEdit hook that make it difficult
            # to do so, which is why we are using a stop hook instead.
            #
            # Ideally, we would want to use a post-tool/ post-edit hook for Cursor,
            # similar to what we already do for Claude.
            print("This hook is not supported for Claude.", file=sys.stderr)
            sys.exit(2)

        response = asyncio.run(run_cli_scan(span))
        print(response.model_dump_json(exclude_none=True))
        sys.exit(0)


def run_after_file_edit_hook(agent: str) -> None:
    with start_tracing("mcp-hook") as span:
        if agent == "claude":
            print("This hook is not supported for Claude.", file=sys.stderr)
            sys.exit(2)

        asyncio.run(record_file_edit(span))
        sys.exit(0)
