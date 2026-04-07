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
import subprocess
from pathlib import Path

import pytest


# ---------------------------------------------------------------------------------
# Sample project fixtures
# ---------------------------------------------------------------------------------

# Minimal uv.lock pinning pyjwt 2.8.0, which is vulnerable (CVE: crit-header bypass,
# fixed in >= 2.10.0). Semgrep SCA should flag this under supply-chain rules.
_VULNERABLE_UV_LOCK = """\
version = 1
requires-python = ">=3.9"

[[package]]
name = "test-project"
version = "0.1.0"
source = { virtual = "." }
dependencies = [
    { name = "pyjwt" },
]

[package.metadata]
requires-dist = [{ name = "pyjwt", specifier = ">=2.0.0" }]

[[package]]
name = "pyjwt"
version = "2.8.0"
source = { registry = "https://pypi.org/simple" }
sdist = { url = "https://files.pythonhosted.org/packages/30/72/8259b2bccfe4673330cea843ab23f86858a419d8f79493c1db9988a6da5e/PyJWT-2.8.0.tar.gz", hash = "sha256:57e28d156e3d5c10088e0c68abb90bfac3df82b40a71bd0daa20c65ccd5c23de", size = 56701 }
wheels = [
    { url = "https://files.pythonhosted.org/packages/2b/4f/e04a8067c7c96c364cef7ef73906504e2f40d690811c021e1a1901473a19/PyJWT-2.8.0-py3-none-any.whl", hash = "sha256:59127c392cc44c2da5bb3192169a91f429924e17aff6534d70fdc02ab3e04320", size = 22591 },
]
"""

_VULNERABLE_PYPROJECT_TOML = """\
[project]
name = "test-project"
version = "0.1.0"
requires-python = ">=3.9"
dependencies = [
    "pyjwt>=2.0.0",
]
"""

# Minimal Python source that calls jwt.decode(), making the pyjwt vulnerability reachable.
_VULNERABLE_APP_PY = """\
import jwt

def verify_token(token: str, secret: str) -> dict:
    return jwt.decode(token, secret, algorithms=["HS256"])
"""

# Minimal uv.lock with only click (no known vulnerabilities).
_CLEAN_UV_LOCK = """\
version = 1
requires-python = ">=3.9"

[[package]]
name = "test-project"
version = "0.1.0"
source = { virtual = "." }
dependencies = [
    { name = "click" },
]

[package.metadata]
requires-dist = [{ name = "click", specifier = ">=8.0.0" }]

[[package]]
name = "click"
version = "8.1.8"
source = { registry = "https://pypi.org/simple" }
sdist = { url = "https://files.pythonhosted.org/packages/b9/2e/0090cbf739cee7d23781ad4b89a9894a41538e4fcf4c31dcdd705b78eb8b/click-8.1.8.tar.gz", hash = "sha256:ed53c9d8990d83c2a27deae68e4ee337473f6330c040a31d4225c9574d16096a", size = 226593 }
wheels = [
    { url = "https://files.pythonhosted.org/packages/7e/d4/7ebdbd03970677812aac39c869717059dbb71a4cfc033ca6e5221787892c/click-8.1.8-py3-none-any.whl", hash = "sha256:63c132bbbed01578a06712a2d1f497bb62d9c1c0d329b7903a866228027263b2", size = 98188 },
]
"""

_CLEAN_PYPROJECT_TOML = """\
[project]
name = "test-project"
version = "0.1.0"
requires-python = ">=3.9"
dependencies = [
    "click>=8.0.0",
]
"""


@pytest.fixture
def vulnerable_project(tmp_path: Path) -> Path:
    """Temp project with pyjwt 2.8.0 (vulnerable < 2.10.0) in uv.lock.

    Includes app.py that calls jwt.decode() so the finding is reachable.
    """
    (tmp_path / "pyproject.toml").write_text(_VULNERABLE_PYPROJECT_TOML)
    (tmp_path / "uv.lock").write_text(_VULNERABLE_UV_LOCK)
    (tmp_path / "app.py").write_text(_VULNERABLE_APP_PY)
    return tmp_path


@pytest.fixture
def clean_project(tmp_path: Path) -> Path:
    """Temp project with no known-vulnerable packages in uv.lock."""
    (tmp_path / "pyproject.toml").write_text(_CLEAN_PYPROJECT_TOML)
    (tmp_path / "uv.lock").write_text(_CLEAN_UV_LOCK)
    return tmp_path


def _write_plugin_json(project_dir: Path, *, disable: bool) -> None:
    semgrep_dir = project_dir / ".semgrep"
    semgrep_dir.mkdir(exist_ok=True)
    (semgrep_dir / "plugin.json").write_text(
        json.dumps({"disable_supply_chain_scan": disable})
    )


def _run_hook(
    project_dir: Path,
    command: str = "uv sync",
    extra_env: dict[str, str] | None = None,
) -> subprocess.CompletedProcess[bytes]:
    """
    Invokes `semgrep mcp -k supply-chain-scan` the same way Claude does:
      echo '<hook-payload>' | semgrep mcp -k supply-chain-scan
    """
    payload = json.dumps({"tool_input": {"command": command}, "cwd": str(project_dir)})
    env = {
        **os.environ,
        "SEMGREP_MCP_DISABLE_TRACING": "true",
        "CLAUDE_PROJECT_DIR": str(project_dir),
        **(extra_env or {}),
    }
    return subprocess.run(
        ["semgrep", "mcp", "-k", "supply-chain-scan"],
        input=payload.encode(),
        capture_output=True,
        env=env,
    )


# ---------------------------------------------------------------------------------
# Settings / config file tests (no SEMGREP_APP_TOKEN required)
# ---------------------------------------------------------------------------------


@pytest.mark.quick
def test_disabled_by_config_exits_without_scanning(vulnerable_project: Path) -> None:
    """Hook exits 0 with no output when disable_supply_chain_scan=true in plugin.json."""
    _write_plugin_json(vulnerable_project, disable=True)
    result = _run_hook(vulnerable_project, extra_env={"SEMGREP_APP_TOKEN": "test"})
    assert result.returncode == 0
    assert result.stdout == b""


@pytest.mark.quick
def test_not_disabled_by_config_proceeds_to_scan(tmp_path: Path) -> None:
    """Hook proceeds past settings check when disable_supply_chain_scan=false.

    Unlike the disabled case (which exits with empty stdout), the hook runs the
    scan and produces output — either findings JSON or an empty `{}`.
    """
    _write_plugin_json(tmp_path, disable=False)
    result = _run_hook(tmp_path, extra_env={"SEMGREP_APP_TOKEN": "test"})
    assert result.stdout != b""


@pytest.mark.quick
def test_no_config_file_proceeds_to_scan(tmp_path: Path) -> None:
    """Without any plugin.json the hook uses defaults and proceeds to scan."""
    result = _run_hook(tmp_path, extra_env={"SEMGREP_APP_TOKEN": "test"})
    assert result.stdout != b""


@pytest.mark.quick
def test_non_install_command_exits_cleanly(tmp_path: Path) -> None:
    """Commands that are not package-manager installs are silently ignored."""
    result = _run_hook(
        tmp_path, command="git status", extra_env={"SEMGREP_APP_TOKEN": "test"}
    )
    assert result.returncode == 0
    assert result.stdout == b""


@pytest.mark.quick
def test_non_install_command_disabled_config(tmp_path: Path) -> None:
    """Non-install command is ignored even when disable_supply_chain_scan=false."""
    _write_plugin_json(tmp_path, disable=False)
    result = _run_hook(
        tmp_path, command="make build", extra_env={"SEMGREP_APP_TOKEN": "test"}
    )
    assert result.returncode == 0
    assert result.stdout == b""


# ---------------------------------------------------------------------------------
# Full CLI integration (requires SEMGREP_APP_TOKEN, can only be run locally)
# ---------------------------------------------------------------------------------


@pytest.mark.slow
@pytest.mark.skipif(
    not os.environ.get("SEMGREP_APP_TOKEN"),
    reason="SEMGREP_APP_TOKEN not set; skipping integration test.",
)
def test_supply_chain_scan_integration_blocks_on_vulnerable_project(
    vulnerable_project: Path,
) -> None:
    """End-to-end: hook blocks when the project has a vulnerable dependency."""
    result = _run_hook(vulnerable_project)
    assert result.returncode == 0
    output = json.loads(result.stdout.decode())
    assert output.get("decision") == "block"
    assert "reason" in output


@pytest.mark.slow
@pytest.mark.skipif(
    not os.environ.get("SEMGREP_APP_TOKEN"),
    reason="SEMGREP_APP_TOKEN not set; skipping integration test.",
)
def test_supply_chain_scan_integration_no_block_on_clean_project(
    clean_project: Path,
) -> None:
    """End-to-end: hook does not block when the project has no vulnerable dependencies."""
    result = _run_hook(clean_project)
    assert result.returncode == 0
    output = json.loads(result.stdout.decode())
    assert "decision" not in output


@pytest.mark.slow
@pytest.mark.skipif(
    not os.environ.get("SEMGREP_APP_TOKEN"),
    reason="SEMGREP_APP_TOKEN not set; skipping integration test.",
)
def test_supply_chain_scan_integration_disabled_skips_scan(
    vulnerable_project: Path,
) -> None:
    """End-to-end: disabled hook does not scan even when vulnerable deps exist."""
    _write_plugin_json(vulnerable_project, disable=True)
    result = _run_hook(vulnerable_project)
    assert result.returncode == 0
    assert result.stdout == b""
