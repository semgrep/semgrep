import json
import re
import shutil
import subprocess
import sys
from pathlib import Path
from textwrap import dedent
from unittest import mock

import pytest
from click.testing import CliRunner

from semgrep import __VERSION__
from semgrep.app import app_session
from semgrep.app import auth
from semgrep.app.scans import ScanHandler
from semgrep.cli import cli
from semgrep.config_resolver import ConfigPath
from semgrep.constants import SEMGREP_SETTING_ENVVAR_NAME
from semgrep.meta import GitlabMeta

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
    subprocess.run(["git", "init"], check=True, capture_output=True)
    subprocess.run(
        ["git", "config", "user.email", AUTHOR_EMAIL],
        check=True,
        capture_output=True,
    )
    subprocess.run(
        ["git", "config", "user.name", AUTHOR_NAME],
        check=True,
        capture_output=True,
    )
    subprocess.run(
        ["git", "checkout", "-B", MAIN_BRANCH_NAME],
        check=True,
        capture_output=True,
    )

    foo = repo_base / "foo.py"
    foo.write_text(f"x = 1\n")
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    subprocess.run(
        ["git", "commit", "-m", COMMIT_MESSAGE],
        check=True,
        capture_output=True,
    )

    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    subprocess.run(
        ["git", "checkout", "-B", BRANCH_NAME],
        check=True,
        capture_output=True,
    )

    shutil.copy(
        Path(__file__).parent.parent / "e2e" / "targets" / "ci" / "foo.py",
        repo_base / "foo.py",
    )
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    subprocess.run(
        ["git", "commit", "-m", COMMIT_MESSAGE_2],
        check=True,
        capture_output=True,
    )
    head_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    yield (repo_base, base_commit, head_commit)


import contextlib


@contextlib.contextmanager
def ci_mocks(base_commit, autofix):
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
          fix: $X == 2
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
                ScanHandler,
                "_get_deployment_details",
                mock.Mock(return_value=(DEPLOYMENT_ID, "org_name")),
            ):
                with mock.patch.object(auth, "is_valid_token", return_value=True):
                    with mock.patch.object(
                        ScanHandler, "autofix", mock.PropertyMock(return_value=autofix)
                    ):
                        yield


@pytest.mark.kinda_slow
@pytest.mark.parametrize("autofix", [True, False], ids=["autofix", "noautofix"])
@pytest.mark.parametrize(
    "env",
    [
        {"SEMGREP_APP_TOKEN": "dummy"},  # Local run with no CI env vars
        {  # Github full scan
            "CI": "true",
            "GITHUB_ACTIONS": "true",
            "GITHUB_EVENT_NAME": "push",
            "GITHUB_REPOSITORY": f"{REPO_DIR_NAME}/{REPO_DIR_NAME}",
            # Sent in metadata but no functionality change
            "GITHUB_RUN_ID": "35",
            "GITHUB_ACTOR": "some_test_username",
            "GITHUB_REF": BRANCH_NAME,
            "GITHUB_SERVER_URL": "https://github.com",
        },
        {  # github but different server url - full scan
            "CI": "true",
            "GITHUB_ACTIONS": "true",
            "GITHUB_EVENT_NAME": "push",
            "GITHUB_REPOSITORY": f"{REPO_DIR_NAME}/{REPO_DIR_NAME}",
            # Sent in metadata but no functionality change
            "GITHUB_RUN_ID": "35",
            "GITHUB_ACTOR": "some_test_username",
            "GITHUB_REF": BRANCH_NAME,
            "GITHUB_SERVER_URL": "https://some.enterprise.url.com",
        },
        {  # Github PR
            "CI": "true",
            "GITHUB_ACTIONS": "true",
            "GITHUB_EVENT_NAME": "pull_request",
            "GITHUB_REPOSITORY": f"{REPO_DIR_NAME}/{REPO_DIR_NAME}",
            # Sent in metadata but no functionality change
            "GITHUB_RUN_ID": "35",
            "GITHUB_ACTOR": "some_test_username",
            "GITHUB_REF": BRANCH_NAME,
        },
        {  # Gitlab PR
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
        {  # Gitlab
            "CI": "true",
            "GITLAB_CI": "true",
            "CI_PROJECT_PATH": f"{REPO_DIR_NAME}/{REPO_DIR_NAME}",
            "CI_PIPELINE_SOURCE": "push",
            # Sent in metadata but no actual functionality change
            "CI_JOB_TOKEN": "some-token-test-placeholder",
            "CI_COMMIT_REF_NAME": BRANCH_NAME,
            "CI_COMMIT_SHA": "unused-commit-test-placeholder",
            "CI_PROJECT_URL": "https://example.com/gitlab-org/gitlab-foss",
            "CI_JOB_URL": "https://gitlab.com/gitlab-examples/ci-debug-trace/-/jobs/379424655",
        },
    ],
    ids=[
        "local",
        "github-push",
        "github-enterprise",
        "github-pr",
        "gitlab",
        "gitlab-push",
    ],
)
@pytest.mark.skipif(
    sys.version_info < (3, 8),
    reason="snapshotting mock call kwargs doesn't work on py3.7",
)
def test_full_run(tmp_path, git_tmp_path_with_commit, snapshot, env, autofix):
    repo_base, base_commit, head_commit = git_tmp_path_with_commit

    # Set envvars that depend on commit hashes:
    if env.get("GITLAB_CI"):
        env["CI_COMMIT_SHA"] = head_commit
    elif env.get("GITHUB_ACTIONS"):
        env["GITHUB_SHA"] = head_commit

        if env["GITHUB_EVENT_NAME"] == "pull_request":
            event = {
                "pull_request": {
                    "user": {
                        "login": "user-login",
                        "avatar_url": "some.user.avatar.com",
                    },
                    "head": {
                        "sha": head_commit,
                        "number": "7",
                        "title": "placeholder-pr-title",
                    },
                    "base": {
                        "sha": base_commit,
                    },
                },
                "sender": {
                    "login": "test-username",
                    "avatar_url": "some.test.avatar.url.com",
                },
            }
            event_path = tmp_path / "event_path.json"
            event_path.write_text(json.dumps(event))
            env["GITHUB_EVENT_PATH"] = str(event_path)

    # Return a different mock so post_mock only contains direct calls to session.post
    post_mock = mock.MagicMock(return_value=mock.MagicMock())

    with ci_mocks(base_commit, autofix):
        # Mock session.post
        with mock.patch.object(app_session, "post", post_mock):
            runner = CliRunner(
                env={
                    **env,
                    SEMGREP_SETTING_ENVVAR_NAME: str(tmp_path),
                    auth.SEMGREP_LOGIN_TOKEN_ENVVAR_NAME: "fake_key",
                }
            )
            result = runner.invoke(cli, ["ci"], env={})

            # Remove commit hashes from output
            sanitized_output = (
                result.output.replace(head_commit, "<sanitized head_commit>")
                .replace(head_commit[:7], "<sanitized head_commit>")
                .replace(base_commit, "<sanitized base_commit>")
                .replace(__VERSION__, "<sanitized semgrep_version>")
            )
            sanitized_output = re.sub(
                r"python 3\.\d+\.\d+", "python <sanitized_version>", sanitized_output
            )
            snapshot.assert_match(sanitized_output, "output.txt")

            # Check correct metadata
            meta_json = post_mock.mock_calls[0].kwargs["json"]
            assert meta_json["meta"]["commit"] == head_commit
            meta_json["meta"]["commit"] = "sanitized"
            assert meta_json["meta"]["semgrep_version"] == __VERSION__
            meta_json["meta"]["semgrep_version"] = "<sanitized version>"

            if env.get("GITLAB_CI"):
                # If in a merge pipeline, base_sha is defined, otherwise is None
                gitlab_base_sha = (
                    base_commit
                    if env.get("CI_MERGE_REQUEST_TARGET_BRANCH_NAME")
                    else None
                )

                assert meta_json["meta"]["base_sha"] == gitlab_base_sha
                meta_json["meta"]["base_sha"] = "sanitized"

            snapshot.assert_match(json.dumps(meta_json, indent=4), "meta.json")

            findings_json = post_mock.mock_calls[1].kwargs["json"]
            for f in findings_json["findings"]:
                assert f["commit_date"] is not None
                f["commit_date"] = "sanitized"
            snapshot.assert_match(json.dumps(findings_json, indent=4), "findings.json")

            ignores_json = post_mock.mock_calls[2].kwargs["json"]
            for f in ignores_json["findings"]:
                assert f["commit_date"] is not None
                f["commit_date"] = "sanitized"
            snapshot.assert_match(json.dumps(ignores_json, indent=4), "ignores.json")

            complete_json = post_mock.mock_calls[3].kwargs["json"]
            complete_json["stats"]["total_time"] = 0.5  # Sanitize time for comparison
            snapshot.assert_match(json.dumps(complete_json, indent=4), "complete.json")


@pytest.mark.kinda_slow
@pytest.mark.parametrize("autofix", [True, False], ids=["autofix", "noautofix"])
def test_config_run(tmp_path, git_tmp_path_with_commit, snapshot, autofix):
    repo_base, base_commit, head_commit = git_tmp_path_with_commit

    with ci_mocks(base_commit, autofix):
        runner = CliRunner(
            env={
                SEMGREP_SETTING_ENVVAR_NAME: str(tmp_path),
                auth.SEMGREP_LOGIN_TOKEN_ENVVAR_NAME: "",
            }
        )
        result = runner.invoke(cli, ["ci", "--config", "p/something"], env={})
        sanitized_output = result.output.replace(
            __VERSION__, "<sanitized semgrep_version>"
        )
        sanitized_output = re.sub(
            r"python 3\.\d+\.\d+", "python <sanitized_version>", sanitized_output
        )
        snapshot.assert_match(sanitized_output, "output.txt")


@pytest.mark.kinda_slow
@pytest.mark.parametrize("autofix", [True, False], ids=["autofix", "noautofix"])
def test_dryrun(tmp_path, git_tmp_path_with_commit, snapshot, autofix):
    repo_base, base_commit, head_commit = git_tmp_path_with_commit

    post_mock = mock.MagicMock(return_value=mock.MagicMock())

    with ci_mocks(base_commit, autofix):
        # Mock session.post
        with mock.patch.object(app_session, "post", post_mock):
            runner = CliRunner(
                env={
                    SEMGREP_SETTING_ENVVAR_NAME: str(tmp_path),
                    auth.SEMGREP_LOGIN_TOKEN_ENVVAR_NAME: "fake_key",
                }
            )
            result = runner.invoke(cli, ["ci", "--dry-run"], env={})

            post_mock.assert_not_called()
            sanitized_output = (
                result.output.replace(head_commit, "<sanitized head_commit>")
                .replace(head_commit[:7], "<sanitized head_commit>")
                .replace(base_commit, "<sanitized base_commit>")
                .replace(__VERSION__, "<sanitized semgrep_version>")
            )
            sanitized_output = re.sub(
                r"python 3\.\d+\.\d+", "python <sanitized_version>", sanitized_output
            )
            # Sanitize commit_date
            sanitized_output = re.sub(
                r"\"commit_date\": .*\"",
                '"commit_date": <sanitized date>',
                sanitized_output,
            )
            sanitized_output = re.sub(
                r"\"total_time\": .*",
                '"total_time": <sanitized date>',
                sanitized_output,
            )
            snapshot.assert_match(sanitized_output, "output.txt")
