import subprocess
from contextlib import contextmanager
from pathlib import Path
from textwrap import indent
from typing import Dict
from typing import Iterator
from typing import List
from typing import NamedTuple
from typing import Optional

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
        - there are tracked files with pending changes
        - there are untracked files that will be overwritten by a file in the base commit
        """
        self._base_commit = base_commit
        self._is_mergebase = is_mergebase
        self._dirty_paths_by_status: Optional[Dict[str, List[Path]]] = None

        try:
            # Check commit hash exists
            git_check_output(["git", "cat-file", "-e", base_commit])

            self.status = self._get_git_status()
            self._abort_on_pending_changes()
            self._abort_on_conflicting_untracked_paths(self.status)
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

    def _get_dirty_paths_by_status(self) -> Dict[str, List[Path]]:
        """
        Returns all paths that have a git status, grouped by change type.

        Raises CalledProcessError if `git status` command fails though
        not clear how that would happen since at this point cwd must be valid repo

        These can be staged, unstaged, or untracked.
        """
        if self._dirty_paths_by_status is not None:
            return self._dirty_paths_by_status

        logger.debug("Initializing dirty paths")
        git_status_output = git_check_output(
            ["git", "status", "--porcelain", "-z", "--ignore-submodules"]
        )
        logger.debug(f"Git status output: {git_status_output}")
        output = zsplit(git_status_output)
        logger.debug("finished getting dirty paths")

        dirty_paths: Dict[str, List[Path]] = {}
        for line in output:
            status_code = line[0]
            path = Path(line[3:])

            if status_code in dirty_paths:
                dirty_paths[status_code].append(path)
            else:
                dirty_paths[status_code] = [path]

        logger.debug(str(dirty_paths))

        # Cache dirty paths
        self._dirty_paths_by_status = dirty_paths
        return dirty_paths

    def _abort_on_pending_changes(self) -> None:
        """
        Raises Exception if any tracked files are changed.

        We are aborting for now to prevent inadvertently deleting files while
        doing a baseline scan until we confidently stash and unstash said changes
        """
        if set(self._get_dirty_paths_by_status()) - {StatusCode.Untracked}:
            raise Exception(
                "Found pending changes in tracked files. Baseline scans runs require a clean git state."
            )

    def _abort_on_conflicting_untracked_paths(self, status: GitStatus) -> None:
        """
        Raises Exception if untracked paths in head were touched in baseline commit
        This would mean checking out the baseline would overwrite said file

        :raises Exception: If the git repo is not in a clean state
        """
        changed_paths = set(
            status.added + status.modified + status.removed + status.unmerged
        )
        untracked_paths = {
            str(path)
            for path in (
                self._get_dirty_paths_by_status().get(StatusCode.Untracked, [])
            )
        }
        overlapping_paths = untracked_paths & changed_paths

        if overlapping_paths:
            raise Exception(
                f"Found files that are untracked by git but exist in {self._base_commit}",
                "Running a baseline scan will cause changes to be overwritten, so aborting.",
                f"Please commit or stash your untracked changes in these paths: {overlapping_paths}.",
            )

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
        env = get_state().env
        # Reabort in case for some reason aborting in __init__ did not cause
        # semgrep to exit
        self._abort_on_pending_changes()
        self._abort_on_conflicting_untracked_paths(self.status)

        logger.debug("Running git write-tree")

        current_head = git_check_output(["git", "rev-parse", "HEAD"])
        try:
            merge_base_sha = git_check_output(
                ["git", "merge-base", self._base_commit, "HEAD"]
            )

            logger.debug("Running git checkout for baseline context")
            git_check_output(["git", "reset", "--hard", merge_base_sha])
            logger.debug("Finished git checkout for baseline context")
            yield
        finally:
            # Return to non-baseline state

            # git checkout will fail if the checked-out index deletes all files in the repo
            # In this case, we still want to continue without error.
            # Note that we have no good way of detecting this issue without inspecting the checkout output
            # message, which means we are fragile with respect to git version here.
            logger.debug("Running git reset to return original context")
            # nosem: use-git-check-output-helper
            x = subprocess.run(
                ["git", "reset", "--hard", current_head],
                capture_output=True,
                timeout=env.git_command_timeout,
            )
            logger.debug("Finished git reset to return original context")

            if x.returncode != 0:
                output = x.stderr.decode()
                if (
                    output
                    and len(output) >= 2
                    and "pathspec '.' did not match any file(s) known to git"
                    in output.strip()
                ):
                    logger.debug(
                        "Restoring git index failed due to total repository deletion; skipping checkout"
                    )
                else:
                    raise Exception(
                        f"Fatal error restoring Git state; please restore your repository state manually:\n{output}"
                    )

    def print_git_log(self) -> None:
        base_commit_sha = git_check_output(["git", "rev-parse", self._base_commit])
        merge_base_sha = git_check_output(
            ["git", "merge-base", self._base_commit, "HEAD"]
        )
        logger.info("  Will report findings introduced by these commits:")
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
