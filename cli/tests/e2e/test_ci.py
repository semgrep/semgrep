import json
import re
import shutil
import subprocess
import sys
from collections import defaultdict
from pathlib import Path
from textwrap import dedent

import pytest
from tests.e2e.test_baseline import _git_commit
from tests.e2e.test_baseline import _git_merge

from semgrep import __VERSION__
from semgrep.app.scans import ScanHandler
from semgrep.app.session import AppSession
from semgrep.config_resolver import ConfigPath
from semgrep.meta import GithubMeta
from semgrep.meta import GitlabMeta
from semgrep.meta import GitMeta

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

    unknown_ext = repo_base / "xyz.txt"
    unknown_ext.write_text("xyz")

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

    mocker.patch.object(GithubMeta, "_shallow_fetch_branch", return_value=None)

    repo_copy_base = tmp_path / "checkout_project_name"
    repo_copy_base.mkdir()
    monkeypatch.chdir(repo_copy_base)
    subprocess.run(["git", "init"], check=True, capture_output=True)
    subprocess.run(
        ["git", "remote", "add", "origin", repo_base],
        check=True,
        capture_output=True,
    )
    subprocess.run(["git", "pull", "origin"])
    subprocess.run(["git", "checkout", f"{MAIN_BRANCH_NAME}"])
    subprocess.run(["git", "checkout", f"{BRANCH_NAME}"])

    yield (repo_copy_base, base_commit, head_commit)


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
    mocker.patch.object(
        ScanHandler,
        "skipped_syntactic_ids",
        ["f3b21c38bc22a1f1f870d49fc3a40244"],
    )
    mocker.patch.object(
        ScanHandler,
        "skipped_match_based_ids",
        [
            "e536489e68267e16e71dd76a61e27815fd86a7e2417d96f8e0c43af48540a41d41e6acad52f7ccda83b5c6168dd5559cd49169617e3aac1b7ea091d8a20ebf12_0"
        ],
    )
    mocker.patch("semgrep.app.auth.is_valid_token", return_value=True)
    mocker.patch.object(AppSession, "post")


@pytest.fixture(params=[True, False], ids=["autofix", "noautofix"])
def mock_autofix(request, mocker):
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
        {  # Circle CI
            "CI": "true",
            "CIRCLECI": "true",
            "CIRCLE_PROJECT_USERNAME": REPO_DIR_NAME,
            "CIRCLE_PROJECT_REPONAME": REPO_DIR_NAME,
            "CIRCLE_REPOSITORY_URL": f"git@github.com/{REPO_DIR_NAME}/{REPO_DIR_NAME}.git",
            "CIRCLE_BRANCH": BRANCH_NAME,
            "CIRCLE_BUILD_URL": "https://circle.ci.build.url",
            "CIRCLE_PULL_REQUEST": f"https://github.com/{REPO_DIR_NAME}/{REPO_DIR_NAME}/pull/35",
        },
        {  # Jenkins
            "JENKINS_URL": "some_url",
            "GIT_URL": "https://github.com/org/repo.git",
            "GIT_BRANCH": BRANCH_NAME,
            "BUILD_URL": "https://jenkins.build.url",
        },
        {  # Bitbucket
            "CI": "true",
            "BITBUCKET_BUILD_NUMBER": "hi",
            "BITBUCKET_REPO_FULL_NAME": f"{REPO_DIR_NAME}/{REPO_DIR_NAME}",
            "BITBUCKET_GIT_HTTP_ORIGIN": f"http://bitbucket.org/{REPO_DIR_NAME}/{REPO_DIR_NAME}",
            "BITBUCKET_BRANCH": BRANCH_NAME,
            "BITBUCKET_PIPELINE_UUID": "a-uuid",
            "BITBUCKET_PR_ID": "35",
        },
        {  # Azure Pipelines
            "BUILD_BUILDID": "some_id",
            "BUILD_REPOSITORY_URI": f"https://github.com/{REPO_DIR_NAME}/{REPO_DIR_NAME}.git",
            "SYSTEM_PULLREQUEST_SOURCEBRANCH": BRANCH_NAME,
            "SYSTEM_TEAMFOUNDATIONSERVERURI": "https://azure.pipeline.url/",
            "SYSTEM_TEAMPROJECTID": "project_id",
            "SYSTEM_JOBID": "job_id",
            "SYSTEM_TASKINSTANCEID": "task_id",
        },
        {  # Buildkite
            "BUILDKITE": "true",
            "BUILDKITE_REPO": f"git@github.com/{REPO_DIR_NAME}/{REPO_DIR_NAME}.git",
            "BUILDKITE_BRANCH": BRANCH_NAME,
            "BUILDKITE_BUILD_URL": "https://buildkite.build.url/something",
            "BUILDKITE_JOB_ID": "42",
            "BUILDKITE_PULL_REQUEST": "35",
            "BUILDKITE_BUILD_AUTHOR": AUTHOR_NAME,
            "BUILDKITE_BUILD_AUTHOR_EMAIL": AUTHOR_EMAIL,
            "BUILDKITE_MESSAGE": COMMIT_MESSAGE,
        },
        {  # Travis CI
            "CI": "true",
            "TRAVIS": "true",
            "TRAVIS_REPO_SLUG": f"{REPO_DIR_NAME}/{REPO_DIR_NAME}",
            "TRAVIS_PULL_REQUEST_BRANCH": BRANCH_NAME,
            "TRAVIS_JOB_WEB_URL": "https://travis.job.web.url/",
            "TRAVIS_PULL_REQUEST": "35",
            "TRAVIS_COMMIT_MESSAGE": COMMIT_MESSAGE,
        },
    ],
    ids=[
        "local",
        "github-push",
        "github-enterprise",
        "github-pr",
        "gitlab",
        "gitlab-push",
        "circleci",
        "jenkins",
        "bitbucket",
        "azure-pipelines",
        "buildkite",
        "travis",
    ],
)
@pytest.mark.skipif(
    sys.version_info < (3, 8),
    reason="snapshotting mock call kwargs doesn't work on py3.7",
)
def test_full_run(
    tmp_path,
    git_tmp_path_with_commit,
    snapshot,
    env,
    run_semgrep,
    mock_autofix,
):
    _, base_commit, head_commit = git_tmp_path_with_commit

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
                        "ref": BRANCH_NAME,
                        "number": "7",
                        "title": "placeholder-pr-title",
                        "repo": {"clone_url": "git://github.com/head/repo.git"},
                    },
                    "base": {
                        "sha": base_commit,
                        "ref": "main",
                        "repo": {"clone_url": "git://github.com/base/repo.git"},
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
    if env.get("CIRCLECI"):
        env["CIRCLE_SHA1"] = head_commit
    if env.get("JENKINS_URL"):
        env["GIT_COMMIT"] = head_commit
    if env.get("BITBUCKET_BUILD_NUMBER"):
        env["BITBUCKET_COMMIT"] = head_commit
    if env.get("BUILD_BUILDID"):
        env["SYSTEM_PULLREQUEST_SOURCECOMMITID"] = head_commit
    if env.get("BUILDKITE"):
        env["BUILDKITE_COMMIT"] = head_commit
    if env.get("TRAVIS"):
        env["TRAVIS_COMMIT"] = head_commit
    env["SEMGREP_APP_TOKEN"] = "fake-key-from-tests"

    result = run_semgrep(options=["ci"], strict=False, assert_exit_code=None, env=env)

    snapshot.assert_match(
        result.as_snapshot(
            mask=[
                head_commit,
                head_commit[:7],
                base_commit,
                re.compile(r'GITHUB_EVENT_PATH="(.+?)"'),
            ]
        ),
        "results.txt",
    )

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

    snapshot.assert_match(json.dumps(scan_create_json, indent=2), "meta.json")

    findings_json = post_calls[1].kwargs["json"]
    for f in findings_json["findings"]:
        assert f["commit_date"] is not None
        f["commit_date"] = "sanitized"
    snapshot.assert_match(json.dumps(findings_json, indent=2), "findings.json")

    ignores_json = post_calls[2].kwargs["json"]
    for f in ignores_json["findings"]:
        assert f["commit_date"] is not None
        f["commit_date"] = "sanitized"
    snapshot.assert_match(json.dumps(ignores_json, indent=2), "ignores.json")

    complete_json = post_calls[3].kwargs["json"]
    complete_json["stats"]["total_time"] = 0.5  # Sanitize time for comparison
    snapshot.assert_match(json.dumps(complete_json, indent=2), "complete.json")


def test_github_ci_bad_base_sha(
    run_semgrep, snapshot, git_tmp_path, tmp_path, monkeypatch
):
    """
    Github PullRequest Event Webhook file's reported base sha is not guaranteed
    to be the shahash of the latest commit on the base branch

    In particular the following situations can cause the base sha to be stale
    (and if we rely on it being latest cause semgrep to incorrectly calculate merge-base):
    - If new commits are pushed onto base branch and a githubaction is rerun
    - If the base branch latest is merged into some third branch and that third branch
      is merged into the PR branch

    Note that simply merging the base branch into the PR branch does cause the base sha to be updated

    This test verifies that we scan the right things even if base sha in a mocked github
    env is stale. Note that the test does not mock the exact situations above but simply
    some state where reported base sha is stale
    """

    # Setup Git Repo
    """
        *   17b3114 (HEAD -> bar) merging foo
        |\
        | * f7ee312 (foo) commit #2
        * | e04f88c commit #1
        |/
        * 191a3ac commit #1

    Regenerate this tree by running:
        git_log = subprocess.run(["git", "--no-pager", "log", "--oneline", "--decorate", "--graph"], check=True, capture_output=True, encoding="utf-8")
        print(git_log.stdout)
    """
    commits = defaultdict(list)
    foo = git_tmp_path / "foo.py"
    bar = git_tmp_path / "bar.py"

    subprocess.run(["git", "checkout", "-b", "foo"])
    foo.open("a").write(f"foo == 5\n\n")
    commits["foo"].append(_git_commit(1, add=True))

    subprocess.run(["git", "checkout", "-b", "bar"])
    bar.open("a").write(f"bar == 5\n\n")
    commits["bar"].append(_git_commit(1, add=True))

    subprocess.run(["git", "checkout", "foo"])
    foo.open("a").write(f"new == 5\n\n")
    commits["foo"].append(_git_commit(2, add=True))

    subprocess.run(["git", "checkout", "bar"])
    commits["bar"].append(_git_merge("foo"))

    # Mock Github Actions Env Vars
    env = {
        "CI": "true",
        "GITHUB_ACTIONS": "true",
        "GITHUB_EVENT_NAME": "pull_request",
        "GITHUB_REPOSITORY": f"{REPO_DIR_NAME}/{REPO_DIR_NAME}",
        # Sent in metadata but no functionality change
        "GITHUB_RUN_ID": "35",
        "GITHUB_ACTOR": "some_test_username",
        "GITHUB_REF": BRANCH_NAME,
    }
    event = {
        "pull_request": {
            "user": {
                "login": "user-login",
                "avatar_url": "some.user.avatar.com",
            },
            "head": {
                "sha": commits["bar"][-1],
                "ref": "bar",
                "number": "7",
                "title": "placeholder-pr-title",
                "repo": {"clone_url": str(git_tmp_path)},
            },
            "base": {
                "sha": commits["foo"][0],  # Note how this is not latest commit in foo
                "ref": "foo",
                "repo": {"clone_url": str(git_tmp_path)},
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
    env["SEMGREP_APP_TOKEN"] = "fake-key-from-tests"

    # Mimic having a remote by having a new repo dir and pointing origin to the repo
    # we setup above
    repo_copy_base = tmp_path / "copy"
    repo_copy_base.mkdir()
    monkeypatch.chdir(repo_copy_base)
    subprocess.run(["git", "init"], check=True, capture_output=True)
    subprocess.run(
        ["git", "remote", "add", "origin", git_tmp_path],
        check=True,
        capture_output=True,
    )
    subprocess.run(["git", "fetch", "origin", "--depth", "1", "bar:bar"])
    subprocess.run(["git", "checkout", "bar"], check=True, capture_output=True)

    result = run_semgrep(
        options=["ci", "--debug", "--no-force-color"],
        strict=False,
        assert_exit_code=None,
        env=env,
    )

    snapshot.assert_match(
        result.as_snapshot(
            mask=[
                re.compile(r'GITHUB_EVENT_PATH="(.+?)"'),
                # Mask variable debug output
                re.compile(r"/(.*)/semgrep-core"),
                re.compile(r"loaded 1 configs in(.*)"),
                re.compile(r".*https://semgrep.dev(.*).*"),
                re.compile(r"(.*Main\.Dune__exe__Main.*)"),
                re.compile(r"(.*Main\.Run_semgrep.*)"),
                re.compile(r"(.*Main\.Common.*)"),
                re.compile(r"(.*Main\.Parse_target.*)"),
                re.compile(r"semgrep ran in (.*) on 1 files"),
                re.compile(r"\"total_time\":(.*)"),
                re.compile(r"\"commit_date\":(.*)"),
                re.compile(r"-targets (.*) -timeout"),
                re.compile(r"-rules (.*).json"),
                str(git_tmp_path),
                str(tmp_path),
            ]
        ),
        "results.txt",
    )

    post_calls = AppSession.post.call_args_list  # type: ignore
    findings_json = post_calls[1].kwargs["json"]
    assert (
        len(findings_json["findings"]) == 1
    ), "Potentially scanning wrong files/commits"


def test_shallow_wrong_merge_base(
    run_semgrep, snapshot, git_tmp_path, tmp_path, monkeypatch
):
    """ """
    commits = defaultdict(list)
    foo = git_tmp_path / "foo.py"
    bar = git_tmp_path / "bar.py"
    baz = git_tmp_path / "baz.py"

    subprocess.run(["git", "checkout", "-b", "foo"])
    foo.open("a").write(f"foo == 5\n")
    commits["foo"].append(_git_commit(1, add=True))
    subprocess.run(
        [
            "git",
            "show",
            "-s",
            "--format=%ct",
            "b903231925961ac9d787ae53ee0bd15ec156e689",
        ]
    )

    subprocess.run(["git", "checkout", "-b", "baz"])
    baz.open("a").write(f"baz == 5\n")
    commits["baz"].append(_git_commit(2, add=True))

    subprocess.run(["git", "checkout", "foo"])
    foo.open("a").write("foo == 5\n")
    commits["foo"].append(_git_commit(3, add=True))

    subprocess.run(["git", "checkout", "-b", "bar"])
    bar.open("a").write(f"bar == 5\n\n")
    commits["bar"].append(_git_commit(4, add=True))

    for _ in range(16):
        subprocess.run(["git", "checkout", "foo"])
        foo.open("a").write(f"new == 5\n\n")
        commits["foo"].append(_git_commit(5, add=True))

    commits["foo"].append(_git_merge("baz"))
    git_log = subprocess.run(
        ["git", "--no-pager", "log", "--oneline", "--decorate", "--graph", "--all"],
        check=True,
        capture_output=True,
        encoding="utf-8",
    )
    print(git_log.stdout)
    subprocess.run(["git", "checkout", "bar"])
    git_log = subprocess.run(
        ["git", "--no-pager", "log", "--oneline", "--decorate", "--graph"],
        check=True,
        capture_output=True,
        encoding="utf-8",
    )
    print(git_log.stdout)

    # Mock Github Actions Env Vars
    env = {
        "CI": "true",
        "GITHUB_ACTIONS": "true",
        "GITHUB_EVENT_NAME": "pull_request",
        "GITHUB_REPOSITORY": f"{REPO_DIR_NAME}/{REPO_DIR_NAME}",
        # Sent in metadata but no functionality change
        "GITHUB_RUN_ID": "35",
        "GITHUB_ACTOR": "some_test_username",
        "GITHUB_REF": BRANCH_NAME,
    }
    event = {
        "pull_request": {
            "user": {
                "login": "user-login",
                "avatar_url": "some.user.avatar.com",
            },
            "head": {
                "sha": commits["bar"][-1],
                "ref": "bar",
                "number": "7",
                "title": "placeholder-pr-title",
                "repo": {"clone_url": str(git_tmp_path)},
            },
            "base": {
                "sha": commits["foo"][-1],  # Note how this is not latest commit in foo
                "ref": "foo",
                "repo": {"clone_url": str(git_tmp_path)},
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
    env["SEMGREP_APP_TOKEN"] = "fake-key-from-tests"

    # Mimic having a remote by having a new repo dir and pointing origin to the repo
    # we setup above
    repo_copy_base = tmp_path / "copy"
    repo_copy_base.mkdir()
    monkeypatch.chdir(repo_copy_base)
    subprocess.run(["git", "init"], check=True, capture_output=True)
    subprocess.run(
        ["git", "remote", "add", "origin", git_tmp_path],
        check=True,
        capture_output=True,
    )
    subprocess.run(["git", "fetch", "origin", "--depth", "1", "bar:bar"])
    subprocess.run(["git", "checkout", "bar"], check=True, capture_output=True)

    # Scan the wrong thing first and verify we get more findings than expected (2 > 1)
    result = run_semgrep(
        options=["ci", "--no-force-color"],
        strict=False,
        assert_exit_code=None,
        env=env,
    )
    snapshot.assert_match(
        result.as_snapshot(
            mask=[
                re.compile(r'GITHUB_EVENT_PATH="(.+?)"'),
            ]
        ),
        "bad_results.txt",
    )
    post_calls = AppSession.post.call_args_list  # type: ignore
    findings_json = post_calls[1].kwargs["json"]
    assert (
        len(findings_json["findings"]) == 2
    ), "Test might be invalid since we expect this to scan the wrong thing"

    # Run again with greater depth
    result = run_semgrep(
        options=["ci", "--no-force-color"],
        strict=False,
        assert_exit_code=None,
        env={**env, "SEMGREP_GHA_MIN_FETCH_DEPTH": "100"},
    )

    snapshot.assert_match(
        result.as_snapshot(
            mask=[
                re.compile(r'GITHUB_EVENT_PATH="(.+?)"'),
            ]
        ),
        "results.txt",
    )

    post_calls = AppSession.post.call_args_list  # type: ignore
    findings_json = post_calls[(len(post_calls) // 2) + 1].kwargs["json"]

    assert (
        len(findings_json["findings"]) == 1
    ), "Potentially scanning wrong files/commits"


def test_config_run(run_semgrep, git_tmp_path_with_commit, snapshot, mock_autofix):
    result = run_semgrep(
        "p/something",
        options=["ci"],
        strict=False,
        assert_exit_code=None,
        env={"SEMGREP_APP_TOKEN": ""},
    )
    snapshot.assert_match(result.as_snapshot(), "results.txt")


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "format",
    ["--json", "--gitlab-sast", "--gitlab-secrets", "--sarif", "--emacs", "--vim"],
)
def test_outputs(git_tmp_path_with_commit, snapshot, format, mock_autofix, run_semgrep):
    result = run_semgrep(
        options=["ci", format],
        target_name=None,
        strict=False,
        assert_exit_code=None,
        output_format=None,
        env={"SEMGREP_APP_TOKEN": "fake_key"},
    )
    snapshot.assert_match(result.as_snapshot(), "results.txt")


@pytest.mark.parametrize("nosem", ["--enable-nosem", "--disable-nosem"])
def test_nosem(git_tmp_path_with_commit, snapshot, mock_autofix, nosem, run_semgrep):
    result = run_semgrep(
        "p/something",
        options=["ci", nosem],
        target_name=None,
        strict=False,
        assert_exit_code=None,
        env={"SEMGREP_APP_TOKEN": ""},
    )
    snapshot.assert_match(result.as_snapshot(), "output.txt")


def test_dryrun(tmp_path, git_tmp_path_with_commit, snapshot, run_semgrep):
    _, base_commit, head_commit = git_tmp_path_with_commit
    result = run_semgrep(
        options=["ci", "--dry-run"],
        target_name=None,
        strict=False,
        assert_exit_code=None,
        env={"SEMGREP_APP_TOKEN": "fake-key-from-tests"},
    )

    AppSession.post.assert_not_called()  # type: ignore
    snapshot.assert_match(
        result.as_snapshot(
            mask=[
                head_commit,
                head_commit[:7],
                base_commit,
                re.compile(r'"commit_date": (.*),?'),
                re.compile(r'"total_time": (.*),?'),
            ]
        ),
        "results.txt",
    )


def test_fail_auth(run_semgrep, mocker, git_tmp_path_with_commit):
    """
    Test that failure to authenticate does not have exit code 0 or 1
    """
    mocker.patch("semgrep.app.auth.is_valid_token", return_value=False)
    run_semgrep(
        options=["ci"],
        target_name=None,
        strict=False,
        assert_exit_code=13,
        env={"SEMGREP_APP_TOKEN": "fake-key-from-tests"},
    )

    mocker.patch("semgrep.app.auth.is_valid_token", side_effect=Exception)
    run_semgrep(
        options=["ci"],
        target_name=None,
        strict=False,
        assert_exit_code=2,
        env={"SEMGREP_APP_TOKEN": "fake-key-from-tests"},
    )


def test_fail_start_scan(run_semgrep, mocker, git_tmp_path_with_commit):
    """
    Test that failing to start scan does not have exit code 0 or 1
    """
    mocker.patch.object(ScanHandler, "start_scan", side_effect=Exception("Timeout"))
    run_semgrep(
        options=["ci"],
        target_name=None,
        strict=False,
        assert_exit_code=2,
        env={"SEMGREP_APP_TOKEN": "fake-key-from-tests"},
    )


def test_bad_config(run_semgrep, mocker, git_tmp_path_with_commit):
    """
    Test that bad rules has exit code > 1
    """
    file_content = dedent(
        """
        rules:
        - id: eqeq-bad
          message: "missing pattern"
          languages: [python]
          severity: ERROR
        """
    ).lstrip()
    mocker.patch.object(ConfigPath, "_make_config_request", return_value=file_content)

    result = run_semgrep(
        options=["ci"],
        target_name=None,
        strict=False,
        assert_exit_code=7,
        env={"SEMGREP_APP_TOKEN": "fake-key-from-tests"},
    )
    assert "Invalid rule schema" in result.stderr


def test_fail_finish_scan(run_semgrep, mocker, git_tmp_path_with_commit):
    """
    Test failure to send findings has exit code > 1
    """
    mocker.patch.object(ScanHandler, "report_findings", side_effect=Exception)
    run_semgrep(
        options=["ci"],
        target_name=None,
        strict=False,
        assert_exit_code=2,
        env={"SEMGREP_APP_TOKEN": "fake-key-from-tests"},
    )


def test_git_failure(run_semgrep, git_tmp_path_with_commit, mocker):
    """
    Test failure from using git has exit code > 1
    """
    mocker.patch.object(GitMeta, "to_dict", side_effect=Exception)
    run_semgrep(
        options=["ci"],
        target_name=None,
        strict=False,
        assert_exit_code=2,
        env={"SEMGREP_APP_TOKEN": "fake-key-from-tests"},
    )


@pytest.mark.kinda_slow
def test_unsupported_langs(git_tmp_path, run_semgrep_in_tmp, mocker, snapshot):
    mocker.patch("semgrep.app.auth.is_valid_token", return_value=True)
    mocker.patch.object(AppSession, "post")

    env = {"SEMGREP_APP_TOKEN": "fake_api_token"}

    shutil.copy(
        Path(__file__).parent.parent
        / "e2e"
        / "targets"
        / "unsupported_langs"
        / "test.rkt",
        git_tmp_path / "test.rkt",
    )
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    subprocess.run(
        ["git", "commit", "-m", "dummy commit"],
        check=True,
        capture_output=True,
    )

    result = run_semgrep_in_tmp(
        target_name=None, options=["ci"], strict=False, env=env, assert_exit_code=None
    )

    post_calls = AppSession.post.call_args_list  # type: ignore
    findings_json = post_calls[(len(post_calls) // 2) + 1].kwargs["json"]

    snapshot.assert_match(
        findings_json,
        "results.txt",
    )
