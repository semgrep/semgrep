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
import os
from pathlib import Path
from types import SimpleNamespace

import pytest
from mcp.types import ListRootsResult
from mcp.types import Root
from pydantic import FileUrl

from semgrep.mcp.server import CodePath
from semgrep.mcp.server import McpError
from semgrep.mcp.server import validate_local_files
from semgrep.mcp.utilities.utils import ALLOW_SCAN_OUTSIDE_WORKSPACE_ENV_VAR
from semgrep.mcp.utilities.utils import get_workspace_dir


@pytest.fixture
def layout(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> dict[str, Path]:
    """A workspace, and a secret next to it standing in for ~/.ssh/id_rsa."""
    monkeypatch.delenv(ALLOW_SCAN_OUTSIDE_WORKSPACE_ENV_VAR, raising=False)
    workspace = tmp_path / "workspace"
    workspace.mkdir()
    (workspace / "app.py").write_text("print('hello')")
    secret = tmp_path / "home" / ".ssh" / "id_rsa"
    secret.parent.mkdir(parents=True)
    secret.write_text("DUMMY-PRIVATE-KEY")
    return {"workspace": workspace, "secret": secret}


@pytest.mark.quick
def test_reads_file_inside_workspace(layout: dict[str, Path]) -> None:
    files = validate_local_files(
        [CodePath(path=str(layout["workspace"] / "app.py"))],
        str(layout["workspace"]),
    )
    assert [(f.path, f.content) for f in files] == [("app.py", "print('hello')")]


@pytest.mark.quick
@pytest.mark.parametrize(
    "spelling",
    [
        "absolute",
        "dot-dot",
    ],
)
def test_rejects_path_outside_workspace(layout: dict[str, Path], spelling: str) -> None:
    workspace, secret = layout["workspace"], layout["secret"]
    path = (
        str(secret)
        if spelling == "absolute"
        else os.path.join(str(workspace), "..", "home", ".ssh", "id_rsa")
    )
    with pytest.raises(McpError, match="outside the workspace") as e:
        validate_local_files([CodePath(path=path)], str(workspace))
    assert "DUMMY-PRIVATE-KEY" not in str(e.value)


@pytest.mark.quick
def test_rejects_symlink_out_of_workspace(layout: dict[str, Path]) -> None:
    workspace = layout["workspace"]
    link = workspace / "config.py"
    link.symlink_to(layout["secret"])
    with pytest.raises(McpError, match="outside the workspace"):
        validate_local_files([CodePath(path=str(link))], str(workspace))


@pytest.mark.quick
def test_without_root_falls_back_to_cwd(
    layout: dict[str, Path], monkeypatch: pytest.MonkeyPatch
) -> None:
    workspace = layout["workspace"]
    monkeypatch.chdir(workspace)
    files = validate_local_files([CodePath(path=str(workspace / "app.py"))], None)
    assert [f.path for f in files] == ["app.py"]
    with pytest.raises(McpError, match="outside the workspace"):
        validate_local_files([CodePath(path=str(layout["secret"]))], None)


@pytest.mark.quick
@pytest.mark.parametrize("cwd", ["home", "filesystem root"])
def test_without_root_refuses_cwd_containing_home(
    layout: dict[str, Path], monkeypatch: pytest.MonkeyPatch, cwd: str
) -> None:
    home = layout["secret"].parent.parent
    monkeypatch.setenv("HOME", str(home))
    monkeypatch.chdir(home if cwd == "home" else Path(home.anchor))
    with pytest.raises(McpError, match="reported no workspace root") as e:
        validate_local_files([CodePath(path=str(layout["secret"]))], None)
    assert "configured at user level" in str(e.value)
    assert ALLOW_SCAN_OUTSIDE_WORKSPACE_ENV_VAR in str(e.value)


@pytest.mark.quick
def test_opt_in_allows_path_outside_workspace(
    layout: dict[str, Path], monkeypatch: pytest.MonkeyPatch
) -> None:
    monkeypatch.setenv(ALLOW_SCAN_OUTSIDE_WORKSPACE_ENV_VAR, "true")
    files = validate_local_files(
        [CodePath(path=str(layout["secret"]))], str(layout["workspace"])
    )
    assert [f.path for f in files] == ["id_rsa"]


@pytest.mark.quick
def test_workspace_dir_is_decoded(layout: dict[str, Path], tmp_path: Path) -> None:
    workspace = tmp_path / "my wörkspace"
    workspace.mkdir()
    (workspace / "app.py").write_text("print('hello')")

    async def list_roots() -> ListRootsResult:
        return ListRootsResult(roots=[Root(uri=FileUrl(workspace.as_uri()))])

    ctx = SimpleNamespace(
        request_context=SimpleNamespace(session=SimpleNamespace(list_roots=list_roots))
    )
    workspace_dir = asyncio.run(get_workspace_dir(ctx))  # type: ignore[arg-type]
    assert workspace_dir == str(workspace)
    files = validate_local_files(
        [CodePath(path=str(workspace / "app.py"))], workspace_dir
    )
    assert [f.path for f in files] == ["app.py"]
