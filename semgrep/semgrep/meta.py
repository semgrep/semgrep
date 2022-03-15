import json
import os
import urllib.parse
from dataclasses import dataclass
from dataclasses import field
from pathlib import Path
from typing import Any
from typing import Dict
from typing import Optional

import click
import git as gitpython
import sh
from boltons.cacheutils import cachedproperty
from glom import glom
from glom import T
from glom.core import TType
from sh.contrib import git

from semgrep_agent.constants import GIT_SH_TIMEOUT
from semgrep_agent.exc import ActionFailure
from semgrep_agent.utils import debug_echo
from semgrep_agent.utils import exit_with_sh_error
from semgrep_agent.utils import get_aligned_command


@dataclass
class GitMeta:
    """Gather metadata only from local filesystem."""

    cli_baseline_ref: Optional[str] = None
    environment: str = field(default="git", init=False)

    @cachedproperty
    def event_name(self) -> str:
        if self.pr_id:
            return "pull_request"
        return "unknown"

    @cachedproperty
    def repo(self) -> gitpython.Repo:  # type: ignore
        repo = gitpython.Repo(".", search_parent_directories=True)
        debug_echo(f"found repo: {repo!r}")
        return repo

    @cachedproperty
    def repo_name(self) -> str:
        if not self.repo.head.is_valid():
            raise RuntimeError("Semgrep action cannot run on repository with no HEAD")
        return os.getenv("SEMGREP_REPO_NAME") or str(
            os.path.basename(self.repo.working_tree_dir)
        )

    @cachedproperty
    def repo_url(self) -> Optional[str]:
        return os.getenv("SEMGREP_REPO_URL")

    @cachedproperty
    def commit_sha(self) -> Optional[str]:
        return os.getenv("SEMGREP_COMMIT") or self.repo.head.commit.hexsha

    @cachedproperty
    def head_ref(self) -> Optional[str]:
        return None

    @cachedproperty
    def base_commit_ref(self) -> Optional[str]:
        return self.cli_baseline_ref

    @cachedproperty
    def commit(self) -> gitpython.Commit:  # type: ignore
        commit = self.repo.commit(self.commit_sha)
        debug_echo(f"found commit: {commit!r}")
        return commit

    @cachedproperty
    def branch(self) -> Optional[str]:
        try:
            br = self.repo.active_branch.name
        except:
            br = None
        return os.getenv("SEMGREP_BRANCH") or br

    @cachedproperty
    def ci_job_url(self) -> Optional[str]:
        return os.getenv("SEMGREP_JOB_URL")

    @cachedproperty
    def pr_id(self) -> Optional[str]:
        return os.getenv("SEMGREP_PR_ID")

    @cachedproperty
    def pr_title(self) -> Optional[str]:
        return os.getenv("SEMGREP_PR_TITLE")

    def initialize_repo(self) -> None:
        return

    def to_dict(self) -> Dict[str, Any]:
        return {
            # REQUIRED for semgrep-app backend
            "repository": self.repo_name,
            #  OPTIONAL for semgrep-app backend
            "repo_url": self.repo_url,
            "branch": self.branch,
            "ci_job_url": self.ci_job_url,
            "commit": self.commit_sha,
            "commit_author_email": self.repo.head.commit.author.email,
            "commit_author_name": self.repo.head.commit.author.name,
            "commit_author_username": None,
            "commit_author_image_url": None,
            "commit_title": self.commit.summary,
            "on": self.event_name,
            "pull_request_author_username": None,
            "pull_request_author_image_url": None,
            "pull_request_id": self.pr_id,
            "pull_request_title": self.pr_title,
            "scan_environment": self.environment,
            "is_full_scan": self.base_commit_ref == None,
        }


@dataclass
class LocalScanMeta(GitMeta):
    environment: str = field(default="local", init=False)


@dataclass
class GithubMeta(GitMeta):
    """Gather metadata from GitHub Actions."""

    environment: str = field(default="github-actions", init=False)
    MAX_FETCH_ATTEMPT_COUNT: int = field(default=6, init=False)

    def glom_event(self, spec: TType) -> Any:
        return glom(self.event, spec, default=None)

    @cachedproperty
    def event(self) -> Dict[str, Any]:
        value = os.getenv("GITHUB_EVENT_PATH")
        if value:
            debug_echo(f"found github event data at {value}")
            return json.loads(Path(value).read_text())  # type: ignore
        return {}

    @cachedproperty
    def is_pull_request_event(self) -> bool:
        """Return if running on a PR, even for variant types such as `pull_request_target`."""
        return self.event_name in {"pull_request", "pull_request_target"}

    @cachedproperty
    def repo_name(self) -> str:
        return os.getenv("GITHUB_REPOSITORY", "[unknown]")

    @cachedproperty
    def repo_url(self) -> Optional[str]:
        if self.repo_name:
            return f"https://github.com/{self.repo_name}"
        return None

    @cachedproperty
    def commit_sha(self) -> Optional[str]:
        if self.is_pull_request_event:
            # https://github.community/t/github-sha-not-the-same-as-the-triggering-commit/18286/2
            return self.glom_event(T["pull_request"]["head"]["sha"])  # type: ignore
        if self.event_name == "push":
            return os.getenv("GITHUB_SHA")
        return super().commit_sha  # type: ignore

    @cachedproperty
    def head_ref(self) -> Optional[str]:
        if self.is_pull_request_event:
            return self.commit_sha  # type: ignore
        else:
            return None

    @cachedproperty
    def base_branch_tip(self) -> Optional[str]:
        return self.glom_event(T["pull_request"]["base"]["sha"])  # type: ignore

    def _find_branchoff_point(self, attempt_count: int = 0) -> str:
        fetch_depth = 4**attempt_count  # fetch 4, 16, 64, 256, 1024, ...
        if attempt_count >= self.MAX_FETCH_ATTEMPT_COUNT:  # get all commits on last try
            fetch_depth = 2**31 - 1  # git expects a signed 32-bit integer

        if attempt_count:  # skip fetching on first try
            debug_echo(
                f"fetching {fetch_depth} commits to find branch-off point of pull request"
            )
            git.fetch(
                "origin",
                "--depth",
                fetch_depth,
                self.base_branch_tip,
                _timeout=GIT_SH_TIMEOUT,
            )
            git.fetch(
                "origin", "--depth", fetch_depth, self.head_ref, _timeout=GIT_SH_TIMEOUT
            )

        try:  # check if both branches connect to the yet-unknown branch-off point now
            process = git("merge-base", self.base_branch_tip, self.head_ref)
        except sh.ErrorReturnCode as error:
            output = error.stderr.decode().strip()
            if (
                output  # output is empty when unable to find branch-off point
                and "Not a valid " not in output  # the error when a ref is missing
            ):
                exit_with_sh_error(error)

            if attempt_count >= self.MAX_FETCH_ATTEMPT_COUNT:
                raise ActionFailure(
                    "Could not find branch-off point between "
                    f"the baseline tip {self.base_branch_tip} and current head '{self.head_ref}' "
                )

            return self._find_branchoff_point(attempt_count + 1)
        else:
            return process.stdout.decode().strip()

    @cachedproperty
    def base_commit_ref(self) -> Optional[str]:
        if self.cli_baseline_ref:
            return self.cli_baseline_ref
        if self.is_pull_request_event and self.head_ref is not None:
            # The pull request "base" that GitHub sends us is not necessarily the merge base,
            # so we need to get the merge-base from Git
            return self._find_branchoff_point()
        return None

    @cachedproperty
    def commit_ref(self) -> Optional[str]:
        return os.getenv("GITHUB_REF")

    @cachedproperty
    def ci_job_url(self) -> Optional[str]:
        value = os.getenv("GITHUB_RUN_ID")
        if self.repo_url and value:
            return f"{self.repo_url}/actions/runs/{value}"
        return None

    @cachedproperty
    def event_name(self) -> str:
        return os.getenv("GITHUB_EVENT_NAME", "unknown")

    @cachedproperty
    def pr_id(self) -> Optional[str]:
        pr_id = self.glom_event(T["pull_request"]["number"])
        return str(pr_id) if pr_id else None

    @cachedproperty
    def pr_title(self) -> Optional[str]:
        pr_title = self.glom_event(T["pull_request"]["title"])
        return str(pr_title) if pr_title else None

    def initialize_repo(self) -> None:
        if self.is_pull_request_event and self.head_ref is not None:
            self._find_branchoff_point()
        return

    def to_dict(self) -> Dict[str, Any]:
        return {
            **super().to_dict(),
            "branch": self.commit_ref,
            "commit_author_username": self.glom_event(T["sender"]["login"]),
            "commit_author_image_url": self.glom_event(T["sender"]["avatar_url"]),
            "pull_request_author_username": self.glom_event(
                T["pull_request"]["user"]["login"]
            ),
            "pull_request_author_image_url": self.glom_event(
                T["pull_request"]["user"]["avatar_url"]
            ),
        }


@dataclass
class GitlabMeta(GitMeta):
    """Gather metadata from GitLab 10.0+"""

    environment: str = field(default="gitlab-ci", init=False)

    @staticmethod
    def _get_remote_url() -> str:
        parts = urllib.parse.urlsplit(os.environ["CI_MERGE_REQUEST_PROJECT_URL"])
        parts = parts._replace(
            netloc=f"gitlab-ci-token:{os.environ['CI_JOB_TOKEN']}@{parts.netloc}"
        )
        return urllib.parse.urlunsplit(parts)

    @cachedproperty
    def repo_name(self) -> str:
        return os.getenv("CI_PROJECT_PATH", "[unknown]")

    @cachedproperty
    def repo_url(self) -> Optional[str]:
        return os.getenv("CI_PROJECT_URL")

    @cachedproperty
    def commit_sha(self) -> Optional[str]:
        return os.getenv("CI_COMMIT_SHA")

    @cachedproperty
    def commit_ref(self) -> Optional[str]:
        return os.getenv("CI_COMMIT_REF_NAME")

    @cachedproperty
    def base_commit_ref(self) -> Optional[str]:
        if self.cli_baseline_ref:
            return self.cli_baseline_ref
        target_branch = os.getenv("CI_MERGE_REQUEST_TARGET_BRANCH_NAME")
        if not target_branch:
            return None
        head_sha = git("rev-parse", "HEAD").stdout.strip()
        git.fetch(self._get_remote_url(), target_branch, _timeout=GIT_SH_TIMEOUT)
        base_sha = (
            git("merge-base", "--all", head_sha, "FETCH_HEAD").stdout.decode().strip()
        )
        return base_sha

    @cachedproperty
    def ci_job_url(self) -> Optional[str]:
        return os.getenv("CI_JOB_URL")

    @cachedproperty
    def event_name(self) -> str:
        gitlab_event_name = os.getenv("CI_PIPELINE_SOURCE", "unknown")
        if gitlab_event_name in ["merge_request_event", "external_pull_request_event"]:
            return "pull_request"
        return gitlab_event_name

    @cachedproperty
    def pr_id(self) -> Optional[str]:
        return os.getenv("CI_MERGE_REQUEST_IID")

    @cachedproperty
    def start_sha(self) -> Optional[str]:
        return os.getenv("CI_MERGE_REQUEST_DIFF_BASE_SHA")

    @cachedproperty
    def pr_title(self) -> Optional[str]:
        return os.getenv("CI_MERGE_REQUEST_TITLE")

    def to_dict(self) -> Dict[str, Any]:
        return {
            **super().to_dict(),
            "branch": self.commit_ref,
            "base_sha": self.base_commit_ref,
            "start_sha": self.start_sha,
        }


def generate_meta_from_environment(
    baseline_ref: Optional[str], scan_environment: Optional[str]
) -> GitMeta:
    # https://help.github.com/en/actions/configuring-and-managing-workflows/using-environment-variables
    if os.getenv("GITHUB_ACTIONS") == "true":
        return GithubMeta(baseline_ref)

    # https://docs.gitlab.com/ee/ci/variables/predefined_variables.html
    elif os.getenv("GITLAB_CI") == "true":
        return GitlabMeta(baseline_ref)

    else:  # nosem
        if not baseline_ref:
            click.echo(
                get_aligned_command(
                    "scan type",
                    "full scan (set --baseline-ref to scan changed files only)",
                ),
                err=True,
            )
        if scan_environment == "local":
            return LocalScanMeta(baseline_ref)

        return GitMeta(baseline_ref)
