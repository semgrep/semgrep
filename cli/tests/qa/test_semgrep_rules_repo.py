import os
import shutil
import subprocess
from pathlib import Path

import pytest
from click.testing import CliRunner

from semgrep.cli import cli

# We use this to locate the semgrep-rules submodule
def get_git_project_root() -> str:
    res = subprocess.Popen(
        ["git", "rev-parse", "--show-toplevel"], stdout=subprocess.PIPE
    )
    return res.communicate()[0].rstrip().decode("utf-8")


@pytest.fixture(scope="session", autouse=True)
def in_semgrep_rules_repo(tmpdir_factory):
    project_root = Path(get_git_project_root())
    monkeypatch = pytest.MonkeyPatch()
    # semgrep-rules is available as a git submodule:
    repo_dir = project_root / "semgrep-core/tests/semgrep-rules"
    # The old code was modifying the semgrep-rules folder. Better not do this
    # since it's a shared resource. If writes are necessary, make a copy
    # of the folder to create a safe read-write workspace.
    #
    # Remove subdir that contains yaml files that are not rules, causing
    # 'semgrep scan --validate --config=.' to fail.
    shutil.rmtree(repo_dir / "stats")
    monkeypatch.chdir(repo_dir)
    if not os.listdir("."):
        raise Exception(
            "The semgrep-rules folder is empty. Was it properly checked out as a git submodule?"
        )
    yield
    monkeypatch.undo()


@pytest.mark.slow
def test_semgrep_rules_repo__test(in_semgrep_rules_repo):
    runner = CliRunner()
    results = runner.invoke(
        cli,
        [
            "--disable-version-check",
            "--metrics=off",
            "--strict",
            "--test",
            "--test-ignore-todo",
        ],
    )
    print(results.output)
    assert results.exit_code == 0


@pytest.mark.slow
def test_semgrep_rules_repo__validate(in_semgrep_rules_repo):
    runner = CliRunner()
    results = runner.invoke(
        cli,
        [
            "--disable-version-check",
            "--metrics=off",
            "--strict",
            "--validate",
            "--config=.",
        ],
    )
    print(results.output)
    assert results.exit_code == 0
