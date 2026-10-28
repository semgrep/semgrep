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
import io
import json
import os
from pathlib import Path

import pytest

from semgrep.mcp.hooks import stop
from semgrep.mcp.hooks.stop import append_edited_file_path
from semgrep.mcp.hooks.stop import cache_file_for
from semgrep.mcp.hooks.stop import clear_edited_file_paths
from semgrep.mcp.hooks.stop import load_edited_file_paths
from semgrep.mcp.hooks.stop import StopHookResponse
from semgrep.mcp.utilities.utils import ALLOW_SCAN_OUTSIDE_WORKSPACE_ENV_VAR


@pytest.fixture(autouse=True)
def cache_dir(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> Path:
    cache_dir = tmp_path / "semgrep-mcp"
    cache_dir.mkdir()
    monkeypatch.setattr(stop, "CACHE_DIR", cache_dir)
    monkeypatch.setattr(stop, "SHARED_CACHE_FILE", cache_dir / "edited-files.json")
    monkeypatch.delenv(ALLOW_SCAN_OUTSIDE_WORKSPACE_ENV_VAR, raising=False)
    return cache_dir


def run_stop_hook(
    monkeypatch: pytest.MonkeyPatch, workspace: Path, conversation_id: str
) -> StopHookResponse:
    hook_input = {
        "conversation_id": conversation_id,
        "workspace_roots": [str(workspace)],
    }
    monkeypatch.setattr("sys.stdin", io.StringIO(json.dumps(hook_input)))
    monkeypatch.setattr(stop, "attach_git_info", lambda *_: None)
    # Skip the tracing/metrics wrapper; only the cache handling is under test.
    response: StopHookResponse = asyncio.run(stop.run_cli_scan.__wrapped__(None))  # type: ignore[attr-defined]
    return response


@pytest.mark.quick
def test_conversations_do_not_see_each_others_edits() -> None:
    append_edited_file_path(cache_file_for("conv-a"), "/ws/a.py")
    append_edited_file_path(cache_file_for("conv-b"), "/ws/b.py")
    assert load_edited_file_paths(cache_file_for("conv-a")) == ["/ws/a.py"]
    assert load_edited_file_paths(cache_file_for("conv-b")) == ["/ws/b.py"]


@pytest.mark.quick
@pytest.mark.parametrize(
    "conversation_id",
    [
        "0f8e6c2a-1b2c-4d5e-8f90-123456789abc",
        "conv_1.2-3",
    ],
)
def test_safe_conversation_id_gets_own_file(
    cache_dir: Path, conversation_id: str
) -> None:
    assert (
        cache_file_for(conversation_id)
        == cache_dir / f"edited-files-{conversation_id}.json"
    )


@pytest.mark.quick
@pytest.mark.parametrize(
    "conversation_id",
    [None, "", ".", "..", "...", "../../etc/passwd", "a/b", "a\\b", "a b", "ü"],
)
def test_unsafe_or_missing_conversation_id_uses_shared_file(
    cache_dir: Path, conversation_id: str | None
) -> None:
    assert cache_file_for(conversation_id) == cache_dir / "edited-files.json"


@pytest.mark.quick
def test_symlinked_cache_file_uses_shared_file(tmp_path: Path, cache_dir: Path) -> None:
    target = tmp_path / "victim.txt"
    target.write_text("do not overwrite")
    (cache_dir / "edited-files-conv.json").symlink_to(target)
    assert cache_file_for("conv") == cache_dir / "edited-files.json"
    append_edited_file_path(cache_file_for("conv"), "/ws/a.py")
    assert target.read_text() == "do not overwrite"


@pytest.mark.quick
def test_symlinked_shared_cache_is_not_followed(
    tmp_path: Path, cache_dir: Path
) -> None:
    secret = tmp_path / "credentials.json"
    secret.write_text('["SECRET"]')
    (cache_dir / "edited-files.json").symlink_to(secret)
    shared = cache_file_for(None)
    assert load_edited_file_paths(shared) == []
    append_edited_file_path(shared, "/ws/a.py")
    assert secret.read_text() == '["SECRET"]'


@pytest.mark.quick
@pytest.mark.skipif(not hasattr(os, "O_NOFOLLOW"), reason="needs O_NOFOLLOW")
def test_symlink_swapped_in_after_check_is_not_followed(
    tmp_path: Path, cache_dir: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    secret = tmp_path / "credentials.json"
    secret.write_text('["SECRET"]')
    (cache_dir / "edited-files-conv.json").symlink_to(secret)
    # Simulate the symlink appearing between the is_symlink() check and the open.
    monkeypatch.setattr(Path, "is_symlink", lambda self: False)
    cache_file = cache_dir / "edited-files-conv.json"
    assert load_edited_file_paths(cache_file) == []
    with pytest.raises(OSError):
        append_edited_file_path(cache_file, "/ws/a.py")
    assert secret.read_text() == '["SECRET"]'


@pytest.mark.quick
def test_cache_that_is_not_a_list_of_paths_is_ignored(cache_dir: Path) -> None:
    cache_file = cache_file_for("conv")
    cache_file.write_text('{"aws_secret_access_key": "SECRET"}')
    assert load_edited_file_paths(cache_file) == []


@pytest.mark.quick
def test_stop_clears_only_its_own_conversation(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    workspace = tmp_path / "workspace"
    workspace.mkdir()
    # Outside the workspace, so the scan has nothing to do and exits early.
    append_edited_file_path(cache_file_for("conv-a"), str(tmp_path / "elsewhere.py"))
    append_edited_file_path(cache_file_for("conv-b"), str(workspace / "b.py"))

    run_stop_hook(monkeypatch, workspace, "conv-a")

    assert not cache_file_for("conv-a").exists()
    assert load_edited_file_paths(cache_file_for("conv-b")) == [str(workspace / "b.py")]


@pytest.mark.quick
@pytest.mark.parametrize("cached", ["empty", "all-filtered"])
def test_nothing_to_scan_returns_empty_response(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, cached: str
) -> None:
    workspace = tmp_path / "workspace"
    workspace.mkdir()
    if cached == "all-filtered":
        outside = tmp_path / "home" / ".ssh" / "id_rsa"
        append_edited_file_path(cache_file_for("conv"), str(outside))

    response = run_stop_hook(monkeypatch, workspace, "conv")

    assert response == StopHookResponse(followup_message=None)
    assert not cache_file_for("conv").exists()


@pytest.mark.quick
@pytest.mark.parametrize("where", ["sibling", "dot-dot", "nested"])
def test_cache_functions_ignore_paths_outside_cache_dir(
    tmp_path: Path, cache_dir: Path, where: str
) -> None:
    victim = tmp_path / "victim.json"
    victim.write_text('["/ws/original.py"]')
    cache_file = {
        "sibling": victim,
        "dot-dot": cache_dir / ".." / "victim.json",
        "nested": cache_dir / "sub" / ".." / ".." / "victim.json",
    }[where]

    assert load_edited_file_paths(cache_file) == []
    append_edited_file_path(cache_file, "/ws/a.py")
    clear_edited_file_paths(cache_file)

    assert victim.read_text() == '["/ws/original.py"]'
