import subprocess
from textwrap import dedent
from unittest import mock

import pytest
from click.testing import CliRunner

from semgrep.cli import cli
from semgrep.commands.login import Authentication
from semgrep.config_resolver import ConfigPath

REPO_DIR_NAME = "project_name"
AUTHOR_EMAIL = "test_environment@test.r2c.dev"
AUTHOR_NAME = "Environment Test"
BRANCH_NAME = "some/branch-name"
COMMIT_MESSAGE = "some: commit message! foo"
COMMIT_MESSAGE_2 = "Some other commit/ message"
DEPLOYMENT_ID = 33
from semgrep.constants import SEMGREP_SETTING_ENVVAR_NAME
import shutil
from semgrep.semgrep_app import ScanHandler
from requests import Session
from pathlib import Path
import json


@pytest.fixture
def git_tmp_path_with_commit(monkeypatch, tmp_path):
    """
    Initialize a git repo at a temporary directory and add single
    dummy commit
    """
    repo_base = tmp_path / REPO_DIR_NAME
    repo_base.mkdir()

    monkeypatch.chdir(repo_base)

    # Initialize State
    subprocess.run(
        ["git", "init"], check=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE
    )
    subprocess.run(
        ["git", "config", "user.email", AUTHOR_EMAIL],
        check=True,
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
    )
    subprocess.run(
        ["git", "config", "user.name", AUTHOR_NAME],
        check=True,
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
    )
    subprocess.run(
        ["git", "checkout", "-B", BRANCH_NAME],
        check=True,
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
    )

    foo = repo_base / "foo.py"
    foo.write_text(f"x = 1\n")
    subprocess.run(
        ["git", "add", "."], check=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE
    )
    subprocess.run(
        ["git", "commit", "-m", COMMIT_MESSAGE],
        check=True,
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
    )

    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    shutil.copy(
        Path(__file__).parent.parent / "e2e" / "targets" / "ci" / "foo.py",
        repo_base / "foo.py",
    )
    subprocess.run(
        ["git", "add", "."], check=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE
    )
    subprocess.run(
        ["git", "commit", "-m", COMMIT_MESSAGE_2],
        check=True,
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
    )
    head_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    yield (repo_base, base_commit, head_commit)


@pytest.mark.parametrize(
    "env",
    [
        {},  # Local run with no env vars
    ],
    ids=["local"],
)
def test_full_run(tmp_path, git_tmp_path_with_commit, snapshot, env):
    repo_base, base_commit, head_commit = git_tmp_path_with_commit

    file_content = dedent(
        """
        rules:
        - id: eqeq-bad
          pattern: $X == $X
          message: "useless comparison"
          languages: [python]
          severity: ERROR
        - id: eqeq-five
          pattern: $X == 5
          message: "useless comparison to 5"
          languages: [python]
          severity: ERROR
          metadata:
            dev.semgrep.actions: []
        - id: eqeq-four
          pattern: $X == 4
          message: "useless comparison to 4"
          languages: [python]
          severity: ERROR
          metadata:
            dev.semgrep.actions: ["block"]
        - id: __r2c-internal-cai_eqeq
          pattern: $X == 3
          message: "useless comparison to 3"
          languages: [python]
          severity: INFO
        """
    ).lstrip()

    # Return a different mock so post_mock only contains direct calls to session.post
    post_mock = mock.MagicMock(return_value=mock.MagicMock())

    # Mock rules
    with mock.patch.object(
        ConfigPath, "_make_config_request", mock.Mock(return_value=file_content)
    ):
        # Mock session.post
        with mock.patch.object(Session, "post", post_mock):
            # Mock deployment_id
            with mock.patch.object(
                ScanHandler, "_get_deployment_id", mock.Mock(return_value=DEPLOYMENT_ID)
            ):
                with mock.patch.object(
                    Authentication, "is_valid_token", return_value=True
                ):
                    runner = CliRunner(
                        env={
                            **env,
                            SEMGREP_SETTING_ENVVAR_NAME: str(tmp_path),
                        }
                    )
                    result = runner.invoke(cli, ["ci"], env={})

                    snapshot.assert_match(result.output, "output.txt")

                    # Check correct metadata
                    meta_json = post_mock.mock_calls[0].kwargs["json"]
                    assert meta_json["meta"]["commit"] == head_commit
                    meta_json["meta"]["commit"] = "sanitized"
                    snapshot.assert_match(json.dumps(meta_json, indent=4), "meta.json")

                    findings_json = post_mock.mock_calls[1].kwargs["json"]
                    snapshot.assert_match(
                        json.dumps(findings_json, indent=4), "findings.json"
                    )

                    ignores_json = post_mock.mock_calls[2].kwargs["json"]
                    snapshot.assert_match(
                        json.dumps(ignores_json, indent=4), "ignores.json"
                    )

                    complete_json = post_mock.mock_calls[3].kwargs["json"]
                    complete_json["stats"][
                        "total_time"
                    ] = 0.5  # Sanitize time for comparison
                    snapshot.assert_match(
                        json.dumps(complete_json, indent=4), "complete.json"
                    )
