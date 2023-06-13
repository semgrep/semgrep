import os
import subprocess
import tempfile
from contextlib import contextmanager
from pathlib import Path
from textwrap import indent
from typing import Dict
from typing import Iterator
from typing import List
from typing import NamedTuple

from semgrep.state import get_state
from semgrep.util import git_check_output
from semgrep.verbose_logging import getLogger


logger = getLogger(__name__)


def zsplit(s: str) -> List[str]:
    """Split a string on null characters."""
    s = s.strip("\0")
    if s:
        return s.split("\0")
    else:
        return []


def get_git_root_path() -> Path:
    git_output = git_check_output(["git", "rev-parse", "--show-toplevel"])
    root_path = Path(git_output)
    logger.debug(f"Git root path: {root_path}")
    return root_path


class GitStatus(NamedTuple):
    added: List[Path]
    modified: List[Path]
    removed: List[Path]
    unmerged: List[Path]
    renamed: Dict[str, Path]  # keys are new names, values are old paths


class StatusCode:
    Added = "A"
    Deleted = "D"
    Renamed = "R"
    Modified = "M"
    Unmerged = "U"
    TypeChanged = "T"  # changed between file / symlink / submodule
    Ignored = "!"
    Untracked = "?"
    Unstaged = " "  # but changed
    Unknown = "X"
    Broken = "B"


class BaselineHandler:
    """
    base_commit: Git ref to compare against

    is_mergebase: Is it safe to assume that the given commit is the mergebase?
    If not, we have to compute the mergebase ourselves, which can be impossible
    on shallow checkouts.
    """

    def __init__(self, base_commit: str, is_mergebase: bool = False) -> None:
        """
        Raises Exception if
        - cwd is not in git repo
        - base_commit is not valid git hash
        """
        self._base_commit = base_commit
        self._is_mergebase = is_mergebase

        try:
            # Check commit hash exists
            git_check_output(["git", "cat-file", "-e", base_commit])

            self.status = self._get_git_status()
        except subprocess.CalledProcessError as e:
            raise Exception(
                f"Error initializing baseline. While running command {e.cmd} recieved non-zero exit status of {e.returncode}.\n(stdout)->{e.stdout}\n(strerr)->{e.stderr}"
            )

    def _get_git_status(self) -> GitStatus:
        """
        Read and parse git diff output to keep track of all status types
        in GitStatus object

        Paths in GitStatus object are absolute paths

        Ignores files that are symlinks to directories

        Raises CalledProcessError if there are any problems running `git diff` command
        """
        env = get_state().env
        logger.debug("Initializing git status")

        # Output of git command will be relative to git project root not cwd
        logger.debug("Running git diff")
        status_cmd = [
            "git",
            "diff",
            "--cached",
            "--name-status",
            "--no-ext-diff",
            "-z",
            "--diff-filter=ACDMRTUXB",
            "--ignore-submodules",
            "--relative",
            self._base_commit,
        ]
        try:
            if self._is_mergebase:
                cmd = status_cmd
            else:
                cmd = [*status_cmd, "--merge-base"]
            # nosemgrep: python.lang.security.audit.dangerous-subprocess-use.dangerous-subprocess-use
            raw_output = subprocess.run(
                cmd,
                timeout=env.git_command_timeout,
                capture_output=True,
                encoding="utf-8",
                check=True,
            ).stdout

        except subprocess.CalledProcessError as exc:
            if exc.stderr.strip() == "fatal: multiple merge bases found":
                logger.warn(
                    "git could not find a single branch-off point, so we will compare the baseline commit directly"
                )
                # nosemgrep: python.lang.security.audit.dangerous-subprocess-use.dangerous-subprocess-use
                raw_output = subprocess.run(
                    status_cmd,
                    timeout=env.git_command_timeout,
                    capture_output=True,
                    encoding="utf-8",
                    check=True,
                ).stdout
            else:
                raise exc
        status_output = zsplit(raw_output)
        logger.debug("Finished git diff. Parsing git status output")
        logger.debug(status_output)
        added = []
        modified = []
        removed = []
        unmerged = []
        renamed = {}  # maps new names for renamed files to their old file paths
        while status_output:
            code = status_output.pop(0)
            fname = status_output.pop(0)
            new_fname = None

            # code is RXXX, where XXX is percent similarity
            if code[0] == StatusCode.Renamed:
                new_fname = status_output.pop(0)

            if not code.strip():
                continue
            if code == StatusCode.Untracked or code == StatusCode.Ignored:
                continue

            path = Path(fname)

            if path.is_symlink() and path.is_dir():
                logger.verbose(
                    f"| Skipping {path} since it is a symlink to a directory: {path.resolve()}",
                )
                continue
            # The following detection for unmerged codes comes from `man git-status`
            if code == StatusCode.Unmerged:
                unmerged.append(path)
            # code is RXXX, where XXX is percent similarity
            if code[0] == StatusCode.Renamed and new_fname:
                removed.append(path)
                added.append(Path(new_fname))
                renamed[new_fname] = path
            if code == StatusCode.Added:
                added.append(path)
            if code == StatusCode.Modified:
                modified.append(path)
            if code == StatusCode.TypeChanged and not path.is_symlink():
                modified.append(path)
            if code == StatusCode.Deleted:
                removed.append(path)

        logger.debug(
            f"Git status:\nadded: {added}\nmodified: {modified}\nremoved: {removed}\nrenamed: {renamed}\nunmerged: {unmerged}"
        )

        return GitStatus(added, modified, removed, unmerged, renamed)

    def _get_git_merge_base(self) -> str:
        # If we already know that the base commit is the merge base, just return
        # the base commit. This allows us to operate on shallow checkouts where
        # we might not have the information locally to compute the merge base.
        # In this case, calling `git merge-base` may fail.
        if self._is_mergebase:
            return self._base_commit
        else:
            return git_check_output(["git", "merge-base", self._base_commit, "HEAD"])

    @contextmanager
    def baseline_context(self) -> Iterator[None]:
        """
        Yields context where pwd is modified to be self.commit_hash
        upon exiting the context returns pwd to what it was initially

        Usage:

        bh = BaselineHandler(commit_hash)
        with baseline_context():
            # Do stuff here
            # pwd will be on commit_hash
        # pwd will be back to what it was


        Raises CalledProcessError if any calls to git return non-zero exit code
        """
        # Reabort in case for some reason aborting in __init__ did not cause
        # semgrep to exit

        cwd = Path.cwd()
        git_root = get_git_root_path()
        relative_path = cwd.relative_to(git_root)
        # `git worktree` is doing 90% of the heavy lifting here. Docs:
        # https://git-scm.com/docs/git-worktree
        #
        # In short, git allows you to have multiple working trees checked out at
        # the same time. This means you can essentially have X different
        # branches/commits checked out from the same repo, in different locations
        #
        # Different worktrees share the same .git directory, so this is a lot
        # faster/cheaper than cloning the repo multiple times
        #
        # This also allows us to not worry about git state, since
        # unstaged/staged files are not shared between worktrees. This means we
        # don't need to git stash anything, or expect a clean working tree.
        with tempfile.TemporaryDirectory() as tmpdir:
            try:
                merge_base_sha = self._get_git_merge_base()
                logger.debug("Running git checkout for baseline context")
                # Add a new working tree at the temporary directory
                git_check_output(["git", "worktree", "add", tmpdir, merge_base_sha])
                logger.debug("Finished git checkout for baseline context")

                # Change the working directory to the new working tree
                os.chdir(Path(tmpdir) / relative_path)

                # We are now in the temporary working tree, and scans should be
                # identical to as if we had checked out the baseline commit
                yield
            finally:
                os.chdir(cwd)
                logger.debug("Cleaning up git worktree")
                # Remove the working tree
                git_check_output(["git", "worktree", "remove", tmpdir])
                logger.debug("Finished cleaning up git worktree")

    def print_git_log(self) -> None:
        base_commit_sha = git_check_output(["git", "rev-parse", self._base_commit])
        head_commit_sha = git_check_output(["git", "rev-parse", "HEAD"])
        merge_base_sha = self._get_git_merge_base()
        if head_commit_sha != merge_base_sha:
            logger.info(
                "  Will report findings introduced by these commits (may be incomplete for shallow checkouts):"
            )
            log = git_check_output(
                ["git", "log", "--oneline", "--graph", f"{merge_base_sha}..HEAD"]
            )
            logger.info(indent(log, "    "))

        if merge_base_sha != base_commit_sha:
            logger.warning(
                "  The current branch is missing these commits from the baseline branch:"
            )
            log = git_check_output(
                [
                    "git",
                    "log",
                    "--oneline",
                    "--graph",
                    f"{merge_base_sha}..{base_commit_sha}",
                ]
            )
            logger.info(indent(log, "    ").rstrip())

            logger.info(
                "  Any finding these commits fixed will look like a new finding in the current branch."
            )
            logger.info(
                "  To avoid reporting such findings, compare to the branch-off point with:\n"
                f"    --baseline-commit=$(git merge-base {self._base_commit} HEAD)"
            )
