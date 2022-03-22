import json
import shutil
import subprocess
from pathlib import Path
from textwrap import dedent
from unittest import mock

import pytest
from click.testing import CliRunner
from requests import Session

from semgrep.cli import cli
from semgrep.commands.login import Authentication
from semgrep.config_resolver import ConfigPath
from semgrep.constants import SEMGREP_SETTING_ENVVAR_NAME
from semgrep.meta import GitlabMeta
from semgrep.semgrep_app import ScanHandler

REPO_DIR_NAME = "project_name"
AUTHOR_EMAIL = "test_environment@test.r2c.dev"
AUTHOR_NAME = "Environment Test"
BRANCH_NAME = "some/branch-name"
MAIN_BRANCH_NAME = "main"
COMMIT_MESSAGE = "some: commit message! foo"
COMMIT_MESSAGE_2 = "Some other commit/ message"
DEPLOYMENT_ID = 33


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
        ["git", "checkout", "-B", MAIN_BRANCH_NAME],
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

    subprocess.run(
        ["git", "checkout", "-B", BRANCH_NAME],
        check=True,
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
    )

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


import contextlib


@contextlib.contextmanager
def ci_mocks(base_commit):
    """
    Necessary patches to run `semgrep ci` tests
    """

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

    with mock.patch.object(
        GitlabMeta, "_fetch_branch_get_merge_base", return_value=base_commit
    ):
        # Mock rules
        with mock.patch.object(
            ConfigPath, "_make_config_request", mock.Mock(return_value=file_content)
        ):
            # Mock deployment_id
            with mock.patch.object(
                ScanHandler, "_get_deployment_id", mock.Mock(return_value=DEPLOYMENT_ID)
            ):
                with mock.patch.object(
                    Authentication, "is_valid_token", return_value=True
                ):
                    yield


@pytest.mark.parametrize(
    "env",
    [
        {},  # Local run with no env vars
        # {   # Github full scan
        #     "CI": "true",
        #     "GITHUB_ACTIONS": "true",
        #     "GITHUB_ACTOR": "some_test_username",
        #     "GITHUB_REPOSITORY": f"{REPO_DIR_NAME}/{REPO_DIR_NAME}",
        #     "GITHUB_SHA":
        # },
        # {},  # Github push to branch
        # {},  # Github pull request
        {  # Gitlab
            "CI": "true",
            "GITLAB_CI": "true",
            "CI_PROJECT_PATH": f"{REPO_DIR_NAME}/{REPO_DIR_NAME}",
            "CI_PIPELINE_SOURCE": "merge_request_event",  # or push
            "CI_MERGE_REQUEST_TARGET_BRANCH_NAME": MAIN_BRANCH_NAME,
            # Sent in metadata but no actual functionality change
            "CI_MERGE_REQUEST_PROJECT_URL": "https://some.project.url.test.placeholder",
            "CI_JOB_TOKEN": "some-token-test-placeholder",
            "CI_COMMIT_REF_NAME": BRANCH_NAME,
            "CI_COMMIT_SHA": "unused-commit-test-placeholder",
            "CI_PROJECT_URL": "https://example.com/gitlab-org/gitlab-foss",
            "CI_JOB_URL": "https://gitlab.com/gitlab-examples/ci-debug-trace/-/jobs/379424655",
            "CI_MERGE_REQUEST_IID": "unused-iid-test-placeholder",
            "CI_MERGE_REQUEST_DIFF_BASE_SHA": "unused-commit-test-placeholder",
            "CI_MERGE_REQUEST_TITLE": "unused-merge-request-title-test-placeholder",
        },
    ],
    ids=["local", "gitlab"],
)
def test_full_run(tmp_path, git_tmp_path_with_commit, snapshot, env):
    repo_base, base_commit, head_commit = git_tmp_path_with_commit

    # Set envvars that depend on commit hashes:
    if env.get("GITLAB_CI"):
        env["CI_COMMIT_SHA"] = head_commit

    # Return a different mock so post_mock only contains direct calls to session.post
    post_mock = mock.MagicMock(return_value=mock.MagicMock())

    with ci_mocks(base_commit):
        # Mock session.post
        with mock.patch.object(Session, "post", post_mock):
            runner = CliRunner(
                env={
                    **env,
                    SEMGREP_SETTING_ENVVAR_NAME: str(tmp_path),
                }
            )
            result = runner.invoke(cli, ["ci"], env={})

            # Remove commit hashes from output
            sanitized_output = result.output.replace(
                head_commit[:7], "<sanitized head_commit>"
            ).replace(base_commit, "<sanitized base_commit>")
            snapshot.assert_match(sanitized_output, "output.txt")

            # Check correct metadata
            meta_json = post_mock.mock_calls[0].kwargs["json"]
            assert meta_json["meta"]["commit"] == head_commit
            meta_json["meta"]["commit"] = "sanitized"

            if env.get("GITLAB_CI"):
                assert meta_json["meta"]["base_sha"] == base_commit
                meta_json["meta"]["base_sha"] = "sanitized"

            snapshot.assert_match(json.dumps(meta_json, indent=4), "meta.json")

            findings_json = post_mock.mock_calls[1].kwargs["json"]
            snapshot.assert_match(json.dumps(findings_json, indent=4), "findings.json")

            ignores_json = post_mock.mock_calls[2].kwargs["json"]
            snapshot.assert_match(json.dumps(ignores_json, indent=4), "ignores.json")

            complete_json = post_mock.mock_calls[3].kwargs["json"]
            complete_json["stats"]["total_time"] = 0.5  # Sanitize time for comparison
            snapshot.assert_match(json.dumps(complete_json, indent=4), "complete.json")
