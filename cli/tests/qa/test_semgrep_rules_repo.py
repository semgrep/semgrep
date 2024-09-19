import os
import subprocess
from pathlib import Path

import pytest


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

    if project_root.name == "semgrep":
        repo_dir = project_root / "tests/semgrep-rules"
    elif project_root.name == "semgrep-proprietary":
        repo_dir = project_root / "OSS/tests/semgrep-rules"
    else:
        raise Exception("Unknown git repository. Couldn't locate semgrep-rules.")
    monkeypatch.chdir(repo_dir)
    if not os.listdir("."):
        raise Exception(
            "The semgrep-rules folder is empty. Was it properly checked out as a git submodule?"
        )
    yield
    monkeypatch.undo()


@pytest.mark.slow
def test_semgrep_rules_repo__validate(in_semgrep_rules_repo):
    """Validate the rule files found in the semgrep-rules repo.

    This runs the same tests as semgrep-rules' own CI does to avoid
    diverging expectations.
    """
    p = subprocess.Popen(["make", "validate"])
    p.communicate()
    exit_code = p.wait()
    assert exit_code == 0


@pytest.mark.slow
def test_semgrep_rules_repo__test(in_semgrep_rules_repo):
    """Test the pairs rule/target found in the semgrep-rules repo.

    This runs the same tests as semgrep-rules' own CI does to avoid
    diverging expectations.
    """
    p = subprocess.Popen(["make", "test-only"])
    p.communicate()
    exit_code = p.wait()
    assert exit_code == 0
