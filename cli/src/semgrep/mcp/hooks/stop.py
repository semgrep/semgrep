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
import os
import re
import shutil
import sys
import tempfile
from pathlib import Path
from typing import Any

from opentelemetry import trace
from pydantic import BaseModel

from semgrep.mcp.models import CodePath
from semgrep.mcp.models import SemgrepScanResult
from semgrep.mcp.semgrep import run_semgrep_output
from semgrep.mcp.server import create_temp_files_from_code_content
from semgrep.mcp.server import get_semgrep_app_token
from semgrep.mcp.server import get_semgrep_scan_args
from semgrep.mcp.server import remove_temp_dir_from_results
from semgrep.mcp.server import resolve_within_workspace
from semgrep.mcp.server import validate_local_files
from semgrep.mcp.utilities.tracing import attach_agent_info
from semgrep.mcp.utilities.tracing import attach_git_info
from semgrep.mcp.utilities.tracing import attach_scan_metrics
from semgrep.mcp.utilities.tracing import start_tracing
from semgrep.mcp.utilities.tracing import with_hook_span
from semgrep.mcp.utilities.utils import CLAUDE_AGENT_STRING
from semgrep.mcp.utilities.utils import scan_outside_workspace_allowed
from semgrep.mcp.utilities.utils import WINDSURF_AGENT_STRING
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

# ---------------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------------

CACHE_DIR = Path(tempfile.gettempdir()) / "semgrep-mcp"
CACHE_DIR.mkdir(parents=True, exist_ok=True)

# Used when the hook input has no usable conversation_id.
SHARED_CACHE_FILE = CACHE_DIR / "edited-files.json"

SAFE_CONVERSATION_ID = re.compile(r"[A-Za-z0-9._-]+")

FILE_EDIT_LIMIT = 500

# Not available on Windows, where the is_symlink() checks below still apply.
O_NOFOLLOW = getattr(os, "O_NOFOLLOW", 0)

# ---------------------------------------------------------------------------------
# Models
# ---------------------------------------------------------------------------------


class StopHookResponse(BaseModel):
    followup_message: str | None = None


# ---------------------------------------------------------------------------------
# Utils for recording file edits
# ---------------------------------------------------------------------------------


def cache_file_for(conversation_id: str | None) -> Path:
    """
    Returns the edit cache for a Cursor conversation, so that concurrent
    conversations don't scan or clear each other's edits. An ID that is
    missing or unsafe to put in a filename falls back to a shared cache.
    """
    if (
        conversation_id is None
        or not SAFE_CONVERSATION_ID.fullmatch(conversation_id)
        or set(conversation_id) == {"."}
    ):
        return SHARED_CACHE_FILE
    cache_file = CACHE_DIR / f"edited-files-{conversation_id}.json"
    # The ID check above should already keep the file in CACHE_DIR; checking the
    # resolved path keeps that true on its own (and rejects a planted symlink).
    if cache_file.resolve().parent != CACHE_DIR.resolve():
        return SHARED_CACHE_FILE
    return cache_file


def in_cache_dir(cache_file: Path) -> bool:
    """
    Whether cache_file names a file directly in CACHE_DIR. The functions below
    check this themselves rather than trusting their caller to have used
    cache_file_for.
    """
    return cache_file.name not in ("", ".", "..") and (
        cache_file.parent.resolve() == CACHE_DIR.resolve()
    )


def load_edited_file_paths(cache_file: Path) -> list[str]:
    if not in_cache_dir(cache_file):
        return []
    # CACHE_DIR is in the shared temp dir, so never follow a planted symlink
    # (e.g. to ~/.ssh/id_rsa) out of it.
    # O_NOFOLLOW also covers a symlink swapped in after this check.
    if cache_file.is_symlink() or not cache_file.is_file():
        return []
    try:
        fd = os.open(cache_file, os.O_RDONLY | O_NOFOLLOW)
    except OSError:
        return []
    with os.fdopen(fd, "r") as f:
        file_paths = json.load(f)
    # Only use (and echo) what this hook writes: a list of paths.
    if not isinstance(file_paths, list) or not all(
        isinstance(p, str) for p in file_paths
    ):
        return []
    print(f"Loaded edited file paths: {file_paths}", file=sys.stderr)
    return file_paths


def append_edited_file_path(cache_file: Path, file_path: str) -> None:
    if not in_cache_dir(cache_file):
        print(f"Ignoring edit cache outside {CACHE_DIR}: {cache_file}", file=sys.stderr)
        return
    if cache_file.is_symlink():
        print(f"Ignoring symlinked edit cache {cache_file}", file=sys.stderr)
        return
    edited_file_paths = load_edited_file_paths(cache_file)
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
    fd = os.open(cache_file, os.O_WRONLY | os.O_CREAT | os.O_TRUNC | O_NOFOLLOW, 0o600)
    with os.fdopen(fd, "w") as f:
        json.dump(edited_file_paths, f)


def clear_edited_file_paths(cache_file: Path) -> None:
    if in_cache_dir(cache_file):
        cache_file.unlink(missing_ok=True)


# ---------------------------------------------------------------------------------
# Utils for loading hook data
# ---------------------------------------------------------------------------------


def load_conversation_id(hook_data: dict[str, Any]) -> str | None:
    conversation_id = hook_data.get("conversation_id")
    return str(conversation_id) if conversation_id is not None else None


def load_file_path() -> tuple[str, str, str | None]:
    """
    Returns the file path, the workspace root, and the conversation ID
    """
    hook_data = json.load(sys.stdin)
    return (
        str(hook_data["file_path"]),
        str(hook_data["workspace_roots"][0]),  # assume only one workspace root
        load_conversation_id(hook_data),
    )


def load_workspace_root() -> tuple[str, str | None]:
    """
    Returns the workspace root and the conversation ID
    """
    hook_data = json.load(sys.stdin)
    return (
        str(hook_data["workspace_roots"][0]),  # assume only one workspace root
        load_conversation_id(hook_data),
    )


def in_workspace(file_path: str, workspace_root: str) -> bool:
    try:
        resolve_within_workspace(file_path, workspace_root)
        return True
    except ValueError:
        return False


# ---------------------------------------------------------------------------------
# Hooks
# ---------------------------------------------------------------------------------


@with_hook_span(
    span_name="record_file_edit (hook)",
    send_metrics=True,
    is_semgrep_scan=False,
)
async def record_file_edit(top_level_span: trace.Span | None) -> None:
    file_path, workspace_root, conversation_id = load_file_path()
    attach_git_info(trace.get_current_span(), workspace_root)
    append_edited_file_path(cache_file_for(conversation_id), file_path)
    return


@with_hook_span(
    span_name="semgrep_scan_cli (hook) (stop)",
    send_metrics=True,
    is_semgrep_scan=True,
)
async def run_cli_scan(top_level_span: trace.Span | None) -> StopHookResponse:
    temp_dir = None
    workspace_root, conversation_id = load_workspace_root()
    cache_file = cache_file_for(conversation_id)
    attach_git_info(trace.get_current_span(), workspace_root)
    try:
        # A conversation's cache can still hold edits outside its workspace
        # (and the shared fallback cache holds every workspace's), so those
        # are dropped here rather than failing the whole scan.
        edited_file_paths = [
            CodePath(path=file_path)
            for file_path in load_edited_file_paths(cache_file)
            if scan_outside_workspace_allowed()
            or in_workspace(file_path, workspace_root)
        ]
        if not edited_file_paths:
            return StopHookResponse(followup_message=None)
        validated_local_files = validate_local_files(edited_file_paths, workspace_root)
        temp_dir = create_temp_files_from_code_content(validated_local_files)
        args = get_semgrep_scan_args(temp_dir, config="hooks")
        logger.info(f"Running scan with args: {args}")
        output = await run_semgrep_output(top_level_span, args)
        scan_result: SemgrepScanResult = SemgrepScanResult.model_validate_json(output)

        if len(scan_result.results) > 0:
            remove_temp_dir_from_results(scan_result, temp_dir)
            reason = str(
                [
                    {
                        "path": r["path"],
                        "line": r["start"]["line"],
                        "display_name": r["extra"]["metadata"].get("display-name"),
                        "message": r["extra"]["message"],
                        "severity": r["extra"]["severity"],
                        "cwe": r["extra"]["metadata"].get("cwe"),
                    }
                    for r in scan_result.results
                ]
            )
            hook_response = StopHookResponse(followup_message=reason)
        else:
            hook_response = StopHookResponse(followup_message=None)

        attach_scan_metrics(
            trace.get_current_span(),
            scan_result,
            workspace_root,
            validated_local_files,  # TODO: need to refactor attach_scan_metrics to not require code_files
        )
        return hook_response
    finally:
        if temp_dir:
            # Clean up temporary files
            shutil.rmtree(temp_dir, ignore_errors=True)
        clear_edited_file_paths(cache_file)


# ---------------------------------------------------------------------------------
# Entry points
# ---------------------------------------------------------------------------------


def run_stop_scan_cli(agent: str) -> None:
    with start_tracing("mcp-hook") as span:
        attach_agent_info(span, agent)
        if agent == CLAUDE_AGENT_STRING or agent == WINDSURF_AGENT_STRING:
            # This hook is not supported for Claude and Windsurf because their hooks' input format
            # is different from that of Cursor hooks. We are assuming the
            # Cursor format here.
            #
            # Also, this is a workaround for Cursor so we can scan after file edits.
            # There are limitations in Cursor's afterFileEdit hook that make it difficult
            # to do so, which is why we are using a stop hook instead.
            #
            # Ideally, we would want to use a post-tool/ post-edit hook for Cursor,
            # similar to what we already do for Claude and Windsurf.
            print(f"This hook is not supported for {agent}.", file=sys.stderr)
            sys.exit(2)

        if get_semgrep_app_token() is None:
            hook_response = StopHookResponse(
                followup_message="No SEMGREP_APP_TOKEN found, please login to Semgrep to use the hook you setup."
            )
            # Cursor docs does not specify if stderr is shown to Cursor when the hook errors with exit code 2,
            # and from our testing, it does not show stderr. So, we are returning a response to the agent stating the errora,
            # similar to what we do when the hook runs successfully, but with an exit code of 2.
            print(hook_response.model_dump_json(exclude_none=True))
            sys.exit(2)

        response = asyncio.run(run_cli_scan(span))
        print(response.model_dump_json(exclude_none=True))
        sys.exit(0)


def run_after_file_edit_hook(agent: str) -> None:
    with start_tracing("mcp-hook") as span:
        attach_agent_info(span, agent)
        if agent == CLAUDE_AGENT_STRING or agent == WINDSURF_AGENT_STRING:
            print(f"This hook is not supported for {agent}.", file=sys.stderr)
            sys.exit(2)

        asyncio.run(record_file_edit(span))
        sys.exit(0)
