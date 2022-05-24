import json
import re
import shutil
import subprocess
import sys
from pathlib import Path
from textwrap import dedent

import pytest
from click.testing import CliRunner

from semgrep import __VERSION__
from semgrep.app import auth
from semgrep.app.scans import ScanHandler
from semgrep.app.session import AppSession
from semgrep.cli import cli
from semgrep.config_resolver import ConfigPath
from semgrep.constants import SEMGREP_SETTING_ENVVAR_NAME
from semgrep.meta import GitlabMeta
from semgrep.meta import GitMeta
from tests.conftest import CLEANERS

pytestmark = pytest.mark.kinda_slow

REPO_DIR_NAME = "project_name"
AUTHOR_EMAIL = "test_environment@test.r2c.dev"
AUTHOR_NAME = "Environment Test"
BRANCH_NAME = "some/branch-name"
MAIN_BRANCH_NAME = "main"
COMMIT_MESSAGE = "some: commit message! foo"
COMMIT_MESSAGE_2 = "Some other commit/ message"
DEPLOYMENT_ID = 33


@pytest.fixture
def git_tmp_path_with_commit(monkeypatch, tmp_path, mocker):
    """
    Initialize a git repo at a temp directory with one dummy commit.
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

    mocker.patch.object(
        GitlabMeta, "_fetch_branch_get_merge_base", return_value=base_commit
    )

    yield (repo_base, base_commit, head_commit)


@pytest.fixture(autouse=True)
def automocks(mocker):
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

    mocker.patch.object(ConfigPath, "_make_config_request", return_value=file_content)
    mocker.patch.object(
        ScanHandler, "_get_deployment_details", return_value=(DEPLOYMENT_ID, "org_name")
    )
    mocker.patch("semgrep.app.auth.is_valid_token", return_value=True)
    mocker.patch.object(AppSession, "post")


@pytest.fixture(params=[True, False], ids=["autofix", "noautofix"])
def autofix(request, mocker):
    mocker.patch.object(ScanHandler, "autofix", request.param)


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
def test_full_run(tmp_path, git_tmp_path_with_commit, snapshot, env, autofix, mocker):
    repo_base, base_commit, head_commit = git_tmp_path_with_commit

    # Set envvars that depend on commit hashes:
    if env.get("GITLAB_CI"):
        env["CI_COMMIT_SHA"] = head_commit
    if env.get("GITHUB_ACTIONS"):
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

    post_calls = AppSession.post.call_args_list  # type: ignore

    # Check correct metadata
    scan_create_json = post_calls[0].kwargs["json"]
    meta_json = scan_create_json["meta"]

    assert meta_json["commit"] == head_commit
    meta_json["commit"] = "sanitized"

    assert meta_json["semgrep_version"] == __VERSION__
    meta_json["semgrep_version"] = "<sanitized version>"

    if env.get("GITLAB_CI"):
        # If in a merge pipeline, base_sha is defined, otherwise is None
        gitlab_base_sha = (
            base_commit if env.get("CI_MERGE_REQUEST_TARGET_BRANCH_NAME") else None
        )

        assert meta_json["base_sha"] == gitlab_base_sha
        meta_json["base_sha"] = "sanitized"

    snapshot.assert_match(json.dumps(scan_create_json, indent=4), "meta.json")

    findings_json = post_calls[1].kwargs["json"]
    for f in findings_json["findings"]:
        assert f["commit_date"] is not None
        f["commit_date"] = "sanitized"
    snapshot.assert_match(json.dumps(findings_json, indent=4), "findings.json")

    ignores_json = post_calls[2].kwargs["json"]
    for f in ignores_json["findings"]:
        assert f["commit_date"] is not None
        f["commit_date"] = "sanitized"
    snapshot.assert_match(json.dumps(ignores_json, indent=4), "ignores.json")

    complete_json = post_calls[3].kwargs["json"]
    complete_json["stats"]["total_time"] = 0.5  # Sanitize time for comparison
    snapshot.assert_match(json.dumps(complete_json, indent=4), "complete.json")


def test_config_run(tmp_path, git_tmp_path_with_commit, snapshot, autofix):
    runner = CliRunner(
        env={
            SEMGREP_SETTING_ENVVAR_NAME: str(tmp_path),
            auth.SEMGREP_LOGIN_TOKEN_ENVVAR_NAME: "",
        }
    )
    result = runner.invoke(cli, ["ci", "--config", "p/something"], env={})
    sanitized_output = result.output.replace(__VERSION__, "<sanitized semgrep_version>")
    sanitized_output = re.sub(
        r"python 3\.\d+\.\d+", "python <sanitized_version>", sanitized_output
    )
    snapshot.assert_match(sanitized_output, "output.txt")


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "format",
    ["--json", "--gitlab-sast", "--gitlab-secrets", "--sarif", "--emacs", "--vim"],
)
def test_outputs(tmp_path, git_tmp_path_with_commit, snapshot, autofix, format):
    runner = CliRunner(
        env={
            SEMGREP_SETTING_ENVVAR_NAME: str(tmp_path),
            auth.SEMGREP_LOGIN_TOKEN_ENVVAR_NAME: "",
        }
    )
    result = runner.invoke(cli, ["ci", "--config", "p/something", format], env={})
    sanitized_output = result.output.replace(__VERSION__, "<sanitized semgrep_version>")
    sanitized_output = re.sub(
        r"python 3\.\d+\.\d+", "python <sanitized_version>", sanitized_output
    )
    clean = CLEANERS.get(format, lambda s: s)(sanitized_output)
    snapshot.assert_match(clean, "results.out")


def test_dryrun(tmp_path, git_tmp_path_with_commit, snapshot, autofix):
    repo_base, base_commit, head_commit = git_tmp_path_with_commit

    runner = CliRunner(
        env={
            SEMGREP_SETTING_ENVVAR_NAME: str(tmp_path),
            auth.SEMGREP_LOGIN_TOKEN_ENVVAR_NAME: "fake_key",
        }
    )
    result = runner.invoke(cli, ["ci", "--dry-run", "--disable-metrics"], env={})

    AppSession.post.assert_not_called()  # type: ignore
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


def test_fail_auth(tmp_path, mocker):
    """
    Test that failure to authenticate does not have exit code 0 or 1
    """
    mocker.patch("semgrep.app.auth.is_valid_token", return_value=False)

    runner = CliRunner(
        env={
            SEMGREP_SETTING_ENVVAR_NAME: str(tmp_path),
            auth.SEMGREP_LOGIN_TOKEN_ENVVAR_NAME: "fake_key",
        }
    )
    result = runner.invoke(cli, ["ci"], env={})
    assert result.exit_code == 13

    mocker.patch("semgrep.app.auth.is_valid_token", side_effect=Exception)

    runner = CliRunner(
        env={
            SEMGREP_SETTING_ENVVAR_NAME: str(tmp_path),
            auth.SEMGREP_LOGIN_TOKEN_ENVVAR_NAME: "fake_key",
        }
    )
    result = runner.invoke(cli, ["ci"], env={})
    assert result.exit_code == 2


def test_fail_start_scan(tmp_path, mocker):
    """
    Test that failing to start scan does not have exit code 0 or 1
    """
    mocker.patch.object(
        ScanHandler,
        "start_scan",
        side_effect=Exception("Timeout"),
    )
    runner = CliRunner(
        env={
            SEMGREP_SETTING_ENVVAR_NAME: str(tmp_path),
            auth.SEMGREP_LOGIN_TOKEN_ENVVAR_NAME: "fake_key",
        }
    )
    result = runner.invoke(cli, ["ci"], env={})
    assert result.exit_code == 2


def test_bad_config(tmp_path, mocker):
    """
    Test that bad rules has exit code > 1
    """
    file_content = dedent(
        """
        rules:
        - id: eqeq-bad
          message: "useless comparison"
          languages: [python]
          severity: ERROR
        """
    ).lstrip()
    mocker.patch.object(ConfigPath, "_make_config_request", return_value=file_content)

    runner = CliRunner(
        env={
            SEMGREP_SETTING_ENVVAR_NAME: str(tmp_path),
            auth.SEMGREP_LOGIN_TOKEN_ENVVAR_NAME: "fake_key",
        }
    )
    result = runner.invoke(cli, ["ci"], env={})
    assert "Invalid rule schema" in result.stdout
    assert result.exit_code == 7


def test_fail_finish_scan(tmp_path, git_tmp_path_with_commit, mocker):
    """
    Test failure to send findings has exit code > 1
    """
    mocker.patch.object(ScanHandler, "report_findings", side_effect=Exception)
    runner = CliRunner(
        env={
            SEMGREP_SETTING_ENVVAR_NAME: str(tmp_path),
            auth.SEMGREP_LOGIN_TOKEN_ENVVAR_NAME: "fake_key",
        }
    )
    result = runner.invoke(cli, ["ci"], env={})
    assert result.exit_code == 2


def test_git_failure(tmp_path, git_tmp_path_with_commit, mocker):
    """
    Test failure from using git has exit code > 1
    """
    mocker.patch.object(GitMeta, "to_dict", side_effect=Exception)
    runner = CliRunner(
        env={
            SEMGREP_SETTING_ENVVAR_NAME: str(tmp_path),
            auth.SEMGREP_LOGIN_TOKEN_ENVVAR_NAME: "fake_key",
        }
    )
    result = runner.invoke(cli, ["ci"], env={})
    assert result.exit_code == 2
