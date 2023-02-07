import json
import os
import subprocess
import urllib.parse
from dataclasses import dataclass
from dataclasses import field
from pathlib import Path
from typing import Any
from typing import Dict
from typing import Optional

import requests
from boltons.cacheutils import cachedproperty
from glom import glom
from glom import T
from glom.core import TType

from semgrep import __VERSION__
from semgrep.external.git_url_parser import Parser
from semgrep.state import get_state
from semgrep.util import git_check_output
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


def get_url_from_sstp_url(sstp_url: Optional[str]) -> Optional[str]:
    """Gets regular url from sstp url.
    We use repo urls on semgrep-app to link to files, so we need to make sure they are
    in the right format to be appended to. We do this by parsing the url with a git url
    parser and rebuilding it into an HTTP/S url
    """

    if sstp_url is None:
        return None
    p = Parser(sstp_url)
    result = p.parse()
    protocol = result.protocol
    if protocol != "http" and protocol != "https":
        # let's just pick https
        protocol = "https"

    # We need to parse the URL into a clickable format, a la these supported formats:
    # https://stackoverflow.com/questions/31801271/what-are-the-supported-git-url-formats
    # We only support a few, though, so we may get `None` if we run into a format we do
    # not support.
    # So if we know that this URL is going to be unclickable, we should return
    # the original URL
    if None in [protocol, result.resource, result.owner, result.name]:
        return sstp_url

    return f"{protocol}://{result.resource}/{result.owner}/{result.name}"


def get_repo_name_from_repo_url(repo_url: Optional[str]) -> Optional[str]:
    """Pulls repository name from the url using a git url parser"""
    if repo_url is None:
        return None
    p = Parser(repo_url)
    result = p.parse()

    return f"{result.owner}/{result.name}"


@dataclass
class GitMeta:
    """Gather metadata only from local filesystem."""

    cli_baseline_ref: Optional[str] = None
    environment: str = field(default="git", init=False)

    @property
    def event_name(self) -> str:
        if self.pr_id:
            return "pull_request"
        return "unknown"

    @property
    def repo_name(self) -> str:
        env = get_state().env
        repo_name = os.getenv("SEMGREP_REPO_NAME")
        if repo_name:
            return repo_name

        # nosem: use-git-check-output-helper
        rev_parse = subprocess.run(
            ["git", "rev-parse", "--show-toplevel"],
            capture_output=True,
            encoding="utf-8",
            timeout=env.git_command_timeout,
        )
        if rev_parse.returncode != 0:
            raise Exception(
                "Unable to infer repo_name. Set SEMGREP_REPO_NAME environment variable or run in a valid git project"
            )

        repo_root_str = rev_parse.stdout.strip()
        return str(os.path.basename(repo_root_str))

    @property
    def repo_url(self) -> Optional[str]:
        return get_url_from_sstp_url(os.getenv("SEMGREP_REPO_URL"))

    @property
    def commit_sha(self) -> Optional[str]:
        """
        Read commit hash of head from env var or run `git rev-parse HEAD`
        """
        commit = os.getenv("SEMGREP_COMMIT")
        if commit:
            return commit

        return git_check_output(["git", "rev-parse", "HEAD"])

    @cachedproperty
    def merge_base_ref(self) -> Optional[str]:
        return self.cli_baseline_ref

    @property
    def ci_job_url(self) -> Optional[str]:
        return os.getenv("SEMGREP_JOB_URL")

    @property
    def pr_id(self) -> Optional[str]:
        return os.getenv("SEMGREP_PR_ID")

    @property
    def pr_title(self) -> Optional[str]:
        return os.getenv("SEMGREP_PR_TITLE")

    @property
    def branch(self) -> Optional[str]:
        branch = os.getenv("SEMGREP_BRANCH")
        if branch:
            return branch

        try:
            return git_check_output(["git", "rev-parse", "--abbrev-ref", "HEAD"])
        except Exception as e:
            logger.debug(f"Could not get branch name using git: {e}")
            return None

    @property
    def commit_datetime(self) -> str:
        """
        Returns epoch time as str of head commit
        """
        return git_check_output(["git", "show", "-s", "--format=%ct"])

    def to_dict(self) -> Dict[str, Any]:
        commit_title = git_check_output(["git", "show", "-s", "--format=%B"])
        commit_author_email = git_check_output(["git", "show", "-s", "--format=%ae"])
        commit_author_name = git_check_output(["git", "show", "-s", "--format=%an"])

        return {
            "semgrep_version": __VERSION__,
            # REQUIRED for semgrep-app backend
            "repository": self.repo_name,
            #  OPTIONAL for semgrep-app backend
            "repo_url": self.repo_url,
            "branch": self.branch,
            "ci_job_url": self.ci_job_url,
            "commit": self.commit_sha,
            "commit_author_email": commit_author_email,
            "commit_author_name": commit_author_name,
            "commit_author_username": None,
            "commit_author_image_url": None,
            "commit_title": commit_title,
            "on": self.event_name,
            "pull_request_author_username": None,
            "pull_request_author_image_url": None,
            "pull_request_id": self.pr_id,
            "pull_request_title": self.pr_title,
            "scan_environment": self.environment,
            "is_full_scan": self.merge_base_ref is None,
        }


@dataclass
class GithubMeta(GitMeta):
    """Gather metadata from GitHub Actions."""

    environment: str = field(default="github-actions", init=False)
    # the last attempt will be 4**10 == 1048576 commits
    MAX_FETCH_ATTEMPT_COUNT: int = field(default=10, init=False)

    def glom_event(self, spec: TType) -> Any:
        return glom(self.event, spec, default=None)

    @property
    def event(self) -> Dict[str, Any]:
        value = os.getenv("GITHUB_EVENT_PATH")
        if value:
            return json.loads(Path(value).read_text())  # type: ignore
        return {}

    @property
    def gh_token(self) -> Optional[str]:
        return os.getenv("GH_TOKEN")

    @property
    def is_pull_request_event(self) -> bool:
        """Return if running on a PR, even for variant types such as `pull_request_target`."""
        return self.event_name in {"pull_request", "pull_request_target"}

    @property
    def repo_name(self) -> str:
        repo_name = os.getenv("GITHUB_REPOSITORY")
        if repo_name:
            return repo_name
        else:
            raise Exception("Could not get repo_name when running in GithubAction")

    @property
    def repo_url(self) -> Optional[str]:
        server_url = os.getenv("GITHUB_SERVER_URL", "https://github.com")

        if self.repo_name:
            return f"{server_url}/{self.repo_name}"
        return None

    @property
    def api_url(self) -> Optional[str]:
        return os.getenv("GITHUB_API_URL")

    @property
    def commit_sha(self) -> Optional[str]:
        if self.is_pull_request_event:
            # https://github.community/t/github-sha-not-the-same-as-the-triggering-commit/18286/2
            return self.glom_event(T["pull_request"]["head"]["sha"])  # type: ignore
        if self.event_name == "push":
            return os.getenv("GITHUB_SHA")
        return super().commit_sha

    def _shallow_fetch_branch(self, branch_name: str) -> None:
        """
        Split out shallow fetch so we can mock it away in tests
        """
        logger.debug(f"Trying to shallow fetch branch {branch_name} from origin")
        git_check_output(
            [
                "git",
                "fetch",
                "origin",
                "--depth=1",
                "--force",
                "--update-head-ok",
                f"{branch_name}:{branch_name}",
            ]
        )

    def _shallow_fetch_commit(self, commit_hash: str) -> None:
        """
        Split out shallow fetch so we can mock it away in tests

        Different from _shallow_fetch_branch because it does not assign a local
        name to the commit. It just does the fetch.
        """
        logger.debug(f"Trying to shallow fetch commit {commit_hash} from origin")
        git_check_output(
            [
                "git",
                "fetch",
                "origin",
                "--depth=1",
                "--force",
                "--update-head-ok",
                commit_hash,
            ]
        )

    def _get_latest_commit_hash_in_branch(self, branch_name: str) -> str:
        """
        Return sha hash of latest commit in a given branch

        Does a git fetch of given branch with depth = 1
        """
        # Fetch latest
        self._shallow_fetch_branch(branch_name)
        branch_rev_parse = git_check_output(["git", "rev-parse", branch_name])
        return branch_rev_parse

    @cachedproperty
    def head_branch_hash(self) -> str:
        """
        Commit of the head branch, reported via the GitHub pull_request event
        This will also ensure that a fetch is done prior to returning.

        Assumes we are in PR context
        """
        head_branch_name = self._head_branch_ref
        commit = self.glom_event(T["pull_request"]["head"]["sha"])
        logger.debug(
            f"head branch ({head_branch_name}) has latest commit {commit}, fetching that commit now."
        )
        git_check_output(
            [
                "git",
                "fetch",
                "origin",
                "--force",
                "--depth=1",
                commit,
            ]
        )
        return str(commit)

    @cachedproperty
    def _head_branch_ref(self) -> str:
        """
        Ref name of the branch pull request if from
        """
        return self.glom_event(T["pull_request"]["head"]["ref"])  # type:ignore

    @cachedproperty
    def _base_branch_ref(self) -> str:
        """
        Ref name of the branch pull request is merging into
        """
        return self.glom_event(T["pull_request"]["base"]["ref"])  # type:ignore

    @cachedproperty
    def base_branch_hash(self) -> str:
        """
        Latest commit hash of the base branch of PR is being merged to

        Assumes we are in PR context
        """
        base_branch_name = self._base_branch_ref
        commit = self._get_latest_commit_hash_in_branch(base_branch_name)
        logger.debug(f"base branch ({base_branch_name}) has latest commit {commit}")
        return commit

    def _find_branchoff_point(self, attempt_count: int = 0) -> str:
        """
        GithubActions is a shallow clone and the "base" that github sends
        is not the merge base. We must fetch and get the merge-base ourselves
        """
        # Should only be called if head_branch_hash is defined
        assert self.head_branch_hash is not None
        assert self.base_branch_hash is not None
        env = get_state().env

        # fetch 0, 4, 16, 64, 256, 1024, ...
        fetch_depth = 4**attempt_count if attempt_count else 0
        fetch_depth += get_state().env.min_fetch_depth
        if attempt_count > self.MAX_FETCH_ATTEMPT_COUNT:  # get all commits on last try
            fetch_depth = 2**31 - 1  # git expects a signed 32-bit integer

        logger.debug(
            f"Attempting to find merge base, attempt_count={attempt_count}, fetch_depth={fetch_depth}"
        )

        if attempt_count:
            logger.debug(
                f"Trying to fetch branch {self._base_branch_ref} from origin as base branch"
            )
            git_check_output(
                [
                    "git",
                    "fetch",
                    "origin",
                    "--force",
                    "--update-head-ok",
                    "--depth",
                    str(fetch_depth),
                    f"{self._base_branch_ref}:{self._base_branch_ref}",
                ]
            )

            # Note that head must be fetched by commit not branch name since if the head
            # is from a fork repo, the branch name doesn't exist in the base context
            logger.debug(
                f"Trying to fetch branch {self._head_branch_ref} as commit from origin as head branch tip commit"
            )
            git_check_output(
                [
                    "git",
                    "fetch",
                    "origin",
                    "--force",
                    "--update-head-ok",
                    "--depth",
                    str(fetch_depth),
                    f"{self.head_branch_hash}",
                ]
            )

        # By default, the GitHub Actions checkout action gives you a shallow
        # clone of the repository. In order to get the merge base, we need to
        # fetch the history of the head branch and the base branch, all the way
        # back to the point where the head branch diverged. In a large
        # repository, this can be a lot of commits, and this fetching can
        # dramatically impact performance.
        #
        # To avoid this, on the first attempt to find the merge base, we try to
        # use the GitHub REST API instead of fetching enough history to compute
        # it locally. We only do this if the `GH_TOKEN` environment variable is
        # provided. GitHub Actions provides that token to workflows, but the
        # workflow needs to explicitly make it available to Semgrep via an
        # environment variable like this:
        #
        # env:
        #   GH_TOKEN: ${{ github.token }}
        #
        # This will allow Semgrep to make this API request even for private
        # repositories.
        if (
            attempt_count == 0
            and self.gh_token is not None
            and self.api_url is not None
            and self.repo_name is not None
        ):
            logger.debug("Trying to get merge base using GitHub API")
            try:
                headers = {"Authorization": f"Bearer {self.gh_token}"}
                req = requests.get(
                    f"{self.api_url}/repos/{self.repo_name}/compare/{self.base_branch_hash}...{self.head_branch_hash}",
                    headers=headers,
                )
                if req.status_code == 200:
                    compare_json = json.loads(req.text)
                    base = glom(
                        compare_json, T["merge_base_commit"]["sha"], default=None
                    )
                    if type(base) == str:
                        logger.debug(f"Got merge base using GitHub API: {base}")
                        # Normally, we fetch commits until we can compute the
                        # merge base locally. That guarantees that the merge
                        # base itself has been fetched. However, when we just
                        # query the GH API, we don't necessarily have anything
                        # locally. Later steps will check out the merge base, so
                        # we need to make sure it is available locally or those
                        # steps will error.
                        self._shallow_fetch_commit(base)
                        return base
            except Exception as e:
                # We're relying on an external service here. If something goes
                # wrong, just log the exception and continue with the ordinary
                # method of computing the merge base.
                logger.debug(
                    f"Encountered error while getting merge base using GitHub API: {repr(e)}"
                )

        try:  # check if both branches connect to the yet-unknown branch-off point now
            # nosem: use-git-check-output-helper
            process = subprocess.run(
                ["git", "merge-base", self.base_branch_hash, self.head_branch_hash],
                encoding="utf-8",
                capture_output=True,
                check=True,
                timeout=env.git_command_timeout,
            )
        except subprocess.CalledProcessError as e:
            output = e.stderr.strip()
            if (
                output  # output is empty when unable to find branch-off point
                and "Not a valid " not in output  # the error when a ref is missing
            ):
                raise Exception(f"Unexpected git merge-base error message: ({output})")

            if attempt_count >= self.MAX_FETCH_ATTEMPT_COUNT:
                raise Exception(
                    f"Could not find branch-off point between the baseline tip {self._base_branch_ref} @ {self.base_branch_hash} and current head {self._head_branch_ref} @ {self.head_branch_hash}"
                )

            return self._find_branchoff_point(attempt_count + 1)
        else:
            merge_base = process.stdout.strip()
            logger.info(
                f"Using {merge_base} as the merge-base of {self.base_branch_hash} and {self.head_branch_hash}"
            )
            return merge_base

    @cachedproperty
    def merge_base_ref(self) -> Optional[str]:
        if self.cli_baseline_ref:
            return self.cli_baseline_ref
        if self.is_pull_request_event and self.head_branch_hash is not None:
            return self._find_branchoff_point()
        return None

    @property
    def ci_job_url(self) -> Optional[str]:
        value = os.getenv("GITHUB_RUN_ID")
        if self.repo_url and value:
            return f"{self.repo_url}/actions/runs/{value}"
        return None

    @property
    def event_name(self) -> str:
        return os.getenv("GITHUB_EVENT_NAME", "unknown")

    @property
    def pr_id(self) -> Optional[str]:
        pr_id = self.glom_event(T["pull_request"]["number"])
        return str(pr_id) if pr_id else None

    @property
    def pr_title(self) -> Optional[str]:
        pr_title = self.glom_event(T["pull_request"]["title"])
        return str(pr_title) if pr_title else None

    @property
    def branch(self) -> Optional[str]:
        """This branch name gets used for tracking issue state over time on the backend.

        The head ref is in GITHUB_HEAD_REF and the base ref is in GITHUB_REF.

        Event name            GITHUB_HEAD_REF -> GITHUB_REF
        ---------------------------------------------------
        pull_request        - johnny-patch-1  -> refs/pulls/123/merge
        pull_request_target - johnny-patch-1  -> refs/heads/main
        push/schedule/etc.  - <unset>         -> refs/heads/main

        This code originally always sent GITHUB_REF.
        This caused obvious breakage for pull_request_target,
        so we just fixed the ref we report for that event.
        But it's more subtly wrong for pull_request events:
        what we're scanning there is still the head ref;
        we force-switch to the head ref in `fix_head_if_github_action`.
        But fixing this slight data inaccuracy would be incompatible with all existing data.
        So as of May 2022 we have not corrected it.
        """
        if self.event_name == "pull_request_target":
            return os.getenv("GITHUB_HEAD_REF")
        return os.getenv("GITHUB_REF")

    def to_dict(self) -> Dict[str, Any]:
        return {
            **super().to_dict(),
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
    def _fetch_branch_get_merge_base(branch_name: str, head_sha: str) -> str:
        """
        Return merge base of current head and head commit in branch_name

        Use Gitlab env vars to fetch target branch
        By default gitlab pipelines do a shallow clone

        Moved out to method so tests can mock this

        Because this is mocked it is not well tested. Use caution when modifying
        """
        parts = urllib.parse.urlsplit(os.environ["CI_MERGE_REQUEST_PROJECT_URL"])
        parts = parts._replace(
            netloc=f"gitlab-ci-token:{os.environ['CI_JOB_TOKEN']}@{parts.netloc}"
        )
        url = urllib.parse.urlunsplit(parts)
        git_check_output(["git", "fetch", url, branch_name])

        base_sha = git_check_output(
            ["git", "merge-base", "--all", head_sha, "FETCH_HEAD"]
        )
        return base_sha

    @property
    def repo_name(self) -> str:
        return os.getenv("CI_PROJECT_PATH", "[unknown]")

    @property
    def repo_url(self) -> Optional[str]:
        return os.getenv("CI_PROJECT_URL")

    @property
    def commit_sha(self) -> Optional[str]:
        return os.getenv("CI_COMMIT_SHA")

    @property
    def commit_ref(self) -> Optional[str]:
        return os.getenv("CI_COMMIT_REF_NAME")

    @cachedproperty
    def merge_base_ref(self) -> Optional[str]:
        if self.cli_baseline_ref:
            return self.cli_baseline_ref
        target_branch = os.getenv("CI_MERGE_REQUEST_TARGET_BRANCH_NAME")
        if not target_branch:
            return None

        head_sha = git_check_output(["git", "rev-parse", "HEAD"])
        return self._fetch_branch_get_merge_base(target_branch, head_sha)

    @property
    def ci_job_url(self) -> Optional[str]:
        return os.getenv("CI_JOB_URL")

    @property
    def event_name(self) -> str:
        gitlab_event_name = os.getenv("CI_PIPELINE_SOURCE", "unknown")
        if gitlab_event_name in ["merge_request_event", "external_pull_request_event"]:
            return "pull_request"
        return gitlab_event_name

    @property
    def pr_id(self) -> Optional[str]:
        return os.getenv("CI_MERGE_REQUEST_IID")

    @property
    def start_sha(self) -> Optional[str]:
        return os.getenv("CI_MERGE_REQUEST_DIFF_BASE_SHA")

    @property
    def pr_title(self) -> Optional[str]:
        return os.getenv("CI_MERGE_REQUEST_TITLE")

    def to_dict(self) -> Dict[str, Any]:
        return {
            **super().to_dict(),
            "branch": self.commit_ref,
            "base_sha": self.merge_base_ref,
            "start_sha": self.start_sha,
        }


@dataclass
class CircleCIMeta(GitMeta):
    """Gather metadata from Circle CI."""

    environment: str = field(default="circleci", init=False)

    @property
    def repo_name(self) -> str:
        repo_name = os.getenv("SEMGREP_REPO_NAME")
        if repo_name:
            return repo_name

        project_name = os.getenv("CIRCLE_PROJECT_USERNAME", "")
        repo_name = os.getenv("CIRCLE_PROJECT_REPONAME", "")
        if repo_name == "" and project_name == "":
            # try using the repo url
            name = get_repo_name_from_repo_url(os.getenv("CIRCLE_REPOSITORY_URL"))
            return name if name else super().repo_name
        return f"{project_name}/{repo_name}"

    @property
    def repo_url(self) -> Optional[str]:
        repo_url = os.getenv("SEMGREP_REPO_URL")
        if repo_url:
            return repo_url

        # may be in SSH url format
        url = get_url_from_sstp_url(os.getenv("CIRCLE_REPOSITORY_URL"))
        return url if url else super().repo_url

    @property
    def branch(self) -> Optional[str]:
        branch = os.getenv("SEMGREP_BRANCH")
        if branch:
            return branch

        return os.getenv("CIRCLE_BRANCH")

    @property
    def ci_job_url(self) -> Optional[str]:
        job_url = os.getenv("SEMGREP_JOB_URL")
        if job_url:
            return job_url

        return os.getenv("CIRCLE_BUILD_URL")

    @property
    def commit_sha(self) -> Optional[str]:
        commit = os.getenv("SEMGREP_COMMIT")
        if commit:
            return commit

        return os.getenv("CIRCLE_SHA1")

    @property
    def pr_id(self) -> Optional[str]:
        pr_id = os.getenv("SEMGREP_PR_ID")
        if pr_id:
            return pr_id

        # have to use the pull request url to get the id
        return os.getenv("CIRCLE_PULL_REQUEST", "").split("/")[-1]


@dataclass
class JenkinsMeta(GitMeta):
    """Gather metadata from Jenkins CI."""

    environment: str = field(default="jenkins", init=False)

    @property
    def repo_name(self) -> str:
        """Constructs the repo name from the git url.
        This assumes that the url is in the github format.
        """
        repo_name = os.getenv("SEMGREP_REPO_NAME")
        if repo_name:
            return repo_name

        name = get_repo_name_from_repo_url(os.getenv("GIT_URL"))
        return name if name else super().repo_name

    @property
    def repo_url(self) -> Optional[str]:
        repo_url = os.getenv("SEMGREP_REPO_URL")
        if repo_url:
            return repo_url

        url = get_url_from_sstp_url(os.getenv("GIT_URL", os.getenv("GIT_URL_1")))
        return url if url else super().repo_url

    @property
    def branch(self) -> Optional[str]:
        branch = os.getenv("SEMGREP_BRANCH")
        if branch:
            return branch

        branch_or_tag = os.getenv("GIT_BRANCH", "")
        if "tags/" not in branch_or_tag:
            return branch_or_tag
        return None

    @property
    def ci_job_url(self) -> Optional[str]:
        job_url = os.getenv("SEMGREP_JOB_URL")
        if job_url:
            return job_url

        return os.getenv("BUILD_URL")

    @property
    def commit_sha(self) -> Optional[str]:
        commit = os.getenv("SEMGREP_COMMIT")
        if commit:
            return commit

        return os.getenv("GIT_COMMIT")


@dataclass
class BitbucketMeta(GitMeta):
    """Gather metadata from BitBucket."""

    environment: str = field(default="bitbucket", init=False)

    @property
    def repo_name(self) -> str:
        repo_name = os.getenv("SEMGREP_REPO_NAME")
        if repo_name:
            return repo_name

        name = os.getenv("BITBUCKET_REPO_FULL_NAME")
        if name is None:
            # try pulling from url
            name = get_repo_name_from_repo_url(os.getenv("BITBUCKET_GIT_HTTP_ORIGIN"))
        return name if name else super().repo_name

    @property
    def repo_url(self) -> Optional[str]:
        repo_url = os.getenv("SEMGREP_REPO_URL")
        if repo_url:
            return repo_url

        # Bitbucket Cloud URLs should be in the format: http://bitbucket.org/<workspace>/<repo>
        # Bitbucker Server URLs should be in the format: https://bitbucket<company>.com/projects/<PROJECT>/repos/<REPO_NAME>
        url = os.getenv("BITBUCKET_GIT_HTTP_ORIGIN")
        return url if url else super().repo_url

    @property
    def branch(self) -> Optional[str]:
        branch = os.getenv("SEMGREP_BRANCH")
        if branch:
            return branch

        return os.getenv("BITBUCKET_BRANCH")

    @property
    def ci_job_url(self) -> Optional[str]:
        job_url = os.getenv("SEMGREP_JOB_URL")
        if job_url:
            return job_url

        url = "{}/addon/pipelines/home#!/results/{}".format(
            os.getenv("BITBUCKET_GIT_HTTP_ORIGIN"), os.getenv("BITBUCKET_PIPELINE_UUID")
        )
        return url

    @property
    def commit_sha(self) -> Optional[str]:
        commit = os.getenv("SEMGREP_COMMIT")
        if commit:
            return commit
        return os.getenv("BITBUCKET_COMMIT")

    @property
    def pr_id(self) -> Optional[str]:
        pr_id = os.getenv("SEMGREP_PR_ID")
        if pr_id:
            return pr_id

        return os.getenv("BITBUCKET_PR_ID")


@dataclass
class AzurePipelinesMeta(GitMeta):
    """Gather metadata from Azure pipelines.
    Pulled a lot from https://github.com/DataDog/dd-trace-py/blob/f583fec63c4392a0784b4199b0e20931f9aae9b5/ddtrace/ext/ci.py
    """

    environment: str = field(default="azure-pipelines", init=False)

    @property
    def repo_name(self) -> str:
        repo_name = os.getenv("SEMGREP_REPO_NAME")
        if repo_name:
            return repo_name

        name = get_repo_name_from_repo_url(self.repo_url)
        return name if name else super().repo_name

    @property
    def repo_url(self) -> Optional[str]:
        repo_url = os.getenv("SEMGREP_REPO_URL")
        if repo_url:
            return repo_url

        url = os.getenv("SYSTEM_PULLREQUEST_SOURCEREPOSITORYURI") or os.getenv(
            "BUILD_REPOSITORY_URI"
        )
        return url if url else super().repo_url

    @property
    def branch(self) -> Optional[str]:
        branch = os.getenv("SEMGREP_BRANCH")
        if branch:
            return branch

        branch_or_tag = (
            os.getenv("SYSTEM_PULLREQUEST_SOURCEBRANCH")
            or os.getenv("BUILD_SOURCEBRANCH")
            or os.getenv("BUILD_SOURCEBRANCHNAME")
            or ""
        )
        if "tags/" not in branch_or_tag:
            return branch_or_tag
        return None

    @property
    def ci_job_url(self) -> Optional[str]:
        job_url = os.getenv("SEMGREP_JOB_URL")
        if job_url:
            return job_url

        if (
            os.getenv("SYSTEM_TEAMFOUNDATIONSERVERURI")
            and os.getenv("SYSTEM_TEAMPROJECTID")
            and os.getenv("BUILD_BUILDID")
        ):
            base_url = "{}{}/_build/results?buildId={}".format(
                os.getenv("SYSTEM_TEAMFOUNDATIONSERVERURI"),
                os.getenv("SYSTEM_TEAMPROJECTID"),
                os.getenv("BUILD_BUILDID"),
            )
            return base_url + "&view=logs&j={}&t={}".format(
                os.getenv("SYSTEM_JOBID"), os.getenv("SYSTEM_TASKINSTANCEID")
            )
        return None

    @property
    def commit_sha(self) -> Optional[str]:
        commit = os.getenv("SEMGREP_COMMIT")
        if commit:
            return commit

        return os.getenv("SYSTEM_PULLREQUEST_SOURCECOMMITID") or os.getenv(
            "BUILD_SOURCEVERSION"
        )

    @property
    def pr_id(self) -> Optional[str]:
        pr_id = os.getenv("SEMGREP_PR_ID")
        if pr_id:
            return pr_id

        return os.getenv("SYSTEM_PULLREQUEST_PULLREQUESTNUMBER")


@dataclass
class BuildkiteMeta(GitMeta):
    """Gather metadata from Buildkite."""

    environment: str = field(default="buildkite", init=False)

    @property
    def repo_name(self) -> str:
        repo_name = os.getenv("SEMGREP_REPO_NAME")
        if repo_name:
            return repo_name

        name = get_repo_name_from_repo_url(os.getenv("BUILDKITE_REPO"))
        return name if name else super().repo_name

    @property
    def repo_url(self) -> Optional[str]:
        repo_url = os.getenv("SEMGREP_REPO_URL")
        if repo_url:
            return repo_url

        url = get_url_from_sstp_url(os.getenv("BUILDKITE_REPO"))
        return url if url else super().repo_url

    @property
    def branch(self) -> Optional[str]:
        branch = os.getenv("SEMGREP_BRANCH")
        if branch:
            return branch

        return os.getenv("BUILDKITE_BRANCH")

    @property
    def ci_job_url(self) -> Optional[str]:
        job_url = os.getenv("SEMGREP_JOB_URL")
        if job_url:
            return job_url

        return "{}#{}".format(
            os.getenv("BUILDKITE_BUILD_URL"), os.getenv("BUILDKITE_JOB_ID")
        )

    @property
    def commit_sha(self) -> Optional[str]:
        commit = os.getenv("SEMGREP_COMMIT")
        if commit:
            return commit

        return os.getenv("BUILDKITE_COMMIT")

    @property
    def pr_id(self) -> Optional[str]:
        pr_id = os.getenv("SEMGREP_PR_ID")
        if pr_id:
            return pr_id

        # might be "false" if there is no PR id
        pr_id = os.getenv("BUILDKITE_PULL_REQUEST")
        return None if pr_id == "false" else pr_id

    def to_dict(self) -> Dict[str, Any]:
        return {
            **super().to_dict(),
            "commit_author_email": os.getenv("BUILDKITE_BUILD_AUTHOR_EMAIL"),
            "commit_author_name": os.getenv("BUILDKITE_BUILD_AUTHOR"),
            "commit_title": os.getenv("BUILDKITE_MESSAGE"),
        }


@dataclass
class TravisMeta(GitMeta):
    """Gather metadata from Travis CI."""

    environment: str = field(default="travis-ci", init=False)

    @property
    def repo_name(self) -> str:
        repo_name = os.getenv("SEMGREP_REPO_NAME")
        if repo_name:
            return repo_name

        repo_name = os.getenv("TRAVIS_REPO_SLUG")
        return repo_name if repo_name else super().repo_name

    @property
    def repo_url(self) -> Optional[str]:
        repo_url = os.getenv("SEMGREP_REPO_URL")
        if repo_url:
            return repo_url

        return f"https://github.com/{self.repo_name}"

    @property
    def branch(self) -> Optional[str]:
        branch = os.getenv("SEMGREP_BRANCH")
        if branch:
            return branch

        return os.getenv("TRAVIS_PULL_REQUEST_BRANCH") or os.getenv("TRAVIS_BRANCH")

    @property
    def ci_job_url(self) -> Optional[str]:
        job_url = os.getenv("SEMGREP_JOB_URL")
        if job_url:
            return job_url

        return os.getenv("TRAVIS_JOB_WEB_URL")

    @property
    def commit_sha(self) -> Optional[str]:
        commit = os.getenv("SEMGREP_COMMIT")
        if commit:
            return commit

        return os.getenv("TRAVIS_COMMIT")

    @property
    def pr_id(self) -> Optional[str]:
        pr_id = os.getenv("SEMGREP_PR_ID")
        if pr_id:
            return pr_id

        return os.getenv("TRAVIS_PULL_REQUEST")

    def to_dict(self) -> Dict[str, Any]:
        return {**super().to_dict(), "commit_title": os.getenv("TRAVIS_COMMIT_MESSAGE")}


def generate_meta_from_environment(baseline_ref: Optional[str]) -> GitMeta:
    # https://help.github.com/en/actions/configuring-and-managing-workflows/using-environment-variables
    if os.getenv("GITHUB_ACTIONS") == "true":
        return GithubMeta(baseline_ref)

    # https://docs.gitlab.com/ee/ci/variables/predefined_variables.html
    elif os.getenv("GITLAB_CI") == "true":
        return GitlabMeta(baseline_ref)

    # https://circleci.com/docs/2.0/env-vars/#built-in-environment-variables
    elif os.getenv("CIRCLECI") == "true":
        return CircleCIMeta(baseline_ref)

    # https://e.printstacktrace.blog/jenkins-pipeline-environment-variables-the-definitive-guide/
    elif os.getenv("JENKINS_URL") is not None:
        return JenkinsMeta(baseline_ref)

    # https://support.atlassian.com/bitbucket-cloud/docs/variables-and-secrets/
    elif os.getenv("BITBUCKET_BUILD_NUMBER") is not None:
        return BitbucketMeta(baseline_ref)

    # https://github.com/DataDog/dd-trace-py/blob/f583fec63c4392a0784b4199b0e20931f9aae9b5/ddtrace/ext/ci.py#L90
    # picked an env var that is only defined by Azure Pipelines
    elif os.getenv("BUILD_BUILDID") is not None:
        return AzurePipelinesMeta(baseline_ref)

    # https://buildkite.com/docs/pipelines/environment-variables#bk-env-vars-buildkite-build-author-email
    elif os.getenv("BUILDKITE") == "true":
        return BuildkiteMeta(baseline_ref)

    # https://docs.travis-ci.com/user/environment-variables/
    elif os.getenv("TRAVIS") == "true":
        return TravisMeta(baseline_ref)

    else:
        return GitMeta(baseline_ref)
