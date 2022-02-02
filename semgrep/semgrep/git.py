import subprocess
from contextlib import contextmanager
from pathlib import Path
from typing import Dict
from typing import Iterator
from typing import List
from typing import NamedTuple
from typing import Optional

from semgrep.verbose_logging import getLogger


logger = getLogger(__name__)


GIT_SH_TIMEOUT = 100


def zsplit(s: str) -> List[str]:
    """Split a string on null characters."""
    s = s.strip("\0")
    if s:
        return s.split("\0")
    else:
        return []


class GitStatus(NamedTuple):
    added: List[Path]
    modified: List[Path]
    removed: List[Path]
    unmerged: List[Path]


class StatusCode:
    Added = "A"
    Deleted = "D"
    Renamed = "R"
    Modified = "M"
    Unmerged = "U"
    Ignored = "!"
    Untracked = "?"
    Unstaged = " "  # but changed


class BaselineHandler:
    """
    base_commit: Git ref to compare against
    """

    def __init__(self, base_commit: str) -> None:
        """
        Raises Exception if
        - cwd is not in git repo
        - base_commit is not valid git hash
        - there are tracked files with pending changes
        - there are untracked files that will be overwritten by a file in the base commit
        """
        self._base_commit = base_commit
        self._dirty_paths_by_status: Optional[Dict[str, List[Path]]] = None

        try:
            self._repo_root_dir = Path(self._get_repo_root())

            # Check commit hash exists
            try:
                subprocess.run(
                    ["git", "cat-file", "-e", base_commit],
                    check=True,
                    stderr=subprocess.PIPE,
                    stdout=subprocess.PIPE,
                )
            except subprocess.CalledProcessError:
                raise Exception(f"{base_commit} does not resolve to a valid commit")

            self._status = self._get_git_status()
            self._abort_on_pending_changes()
            self._abort_on_conflicting_untracked_paths(self._status)
        except subprocess.CalledProcessError as e:
            raise Exception(
                f"Error initializing baseline. While running command {e.cmd} recieved non-zero exit status of {e.returncode}.\n(stdout)->{e.stdout}\n(strerr)->{e.stderr}"
            )

    def _relative_to_repo_root_to_absolute(self, fname: str) -> Path:
        return (Path(self._repo_root_dir) / fname).resolve()

    def _get_repo_root(self) -> Path:
        """
        Returns Path object to the root of the git project cwd is in

        Raises Exception if cwd is not in a git project
        """
        rev_parse = subprocess.run(
            ["git", "rev-parse", "--show-toplevel"],
            stderr=subprocess.PIPE,
            stdout=subprocess.PIPE,
            encoding="utf-8",
            timeout=GIT_SH_TIMEOUT,
        )
        if rev_parse.returncode != 0:
            raise Exception(
                "Current directory is not in a git project. Aborting baseline scan."
            )

        repo_root_str = rev_parse.stdout.strip()
        return Path(rev_parse.stdout.strip())

    def _get_git_status(self) -> GitStatus:
        """
        Read and parse git diff output to keep track of all status types
        in GitStatus object

        Paths in GitStatus object are absolute paths

        Ignores files that are symlinks to directories

        Raises CalledProcessError if there are any problems running `git diff` command
        """
        logger.debug("Initializing git status")

        # Output of git command will be relative to git project root not cwd
        logger.debug("Running git diff")
        status_output = zsplit(
            subprocess.run(
                [
                    "git",
                    "diff",
                    "--cached",
                    "--name-status",
                    "--no-ext-diff",
                    "-z",
                    "--diff-filter=ACDMRTUXB",
                    "--ignore-submodules",
                    "--merge-base",
                    f"{self._base_commit}",
                ],
                timeout=GIT_SH_TIMEOUT,
                stderr=subprocess.PIPE,
                stdout=subprocess.PIPE,
                encoding="utf-8",
                check=True,
            ).stdout
        )
        logger.debug("Finished git diff. Parsing git status output")
        logger.debug(status_output)
        added = []
        modified = []
        removed = []
        unmerged = []
        while status_output:
            code = status_output[0]
            fname = status_output[1]
            trim_size = 2

            if not code.strip():
                continue
            if code == StatusCode.Untracked or code == StatusCode.Ignored:
                continue

            resolved_name = self._relative_to_repo_root_to_absolute(fname)

            # If file is symlink to directory, skip
            absolute_name = Path(self._repo_root_dir) / fname
            if absolute_name.is_symlink() and resolved_name.is_dir():
                logger.verbose(
                    f"| Skipping {absolute_name} since it is a symlink to a directory: {resolved_name}",
                )
            else:
                # The following detection for unmerged codes comes from `man git-status`
                if code == StatusCode.Unmerged:
                    unmerged.append(resolved_name)
                if (
                    code[0] == StatusCode.Renamed
                ):  # code is RXXX, where XXX is percent similarity
                    removed.append(resolved_name)
                    fname = status_output[2]
                    trim_size += 1
                    added.append(self._relative_to_repo_root_to_absolute(fname))
                if code == StatusCode.Added:
                    added.append(resolved_name)
                if code == StatusCode.Modified:
                    modified.append(resolved_name)
                if code == StatusCode.Deleted:
                    removed.append(resolved_name)

            status_output = status_output[trim_size:]
        logger.debug(
            f"Git status:\nadded: {added}\nmodified: {modified}\nremoved: {removed}\nunmerged: {unmerged}"
        )

        return GitStatus(added, modified, removed, unmerged)

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
        sub_out = subprocess.run(
            ["git", "status", "--porcelain", "-z"],
            timeout=GIT_SH_TIMEOUT,
            stderr=subprocess.PIPE,
            stdout=subprocess.PIPE,
            encoding="utf-8",
            check=True,
        )
        git_status_output = sub_out.stdout
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
            self._relative_to_repo_root_to_absolute(str(path))
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
        status = self._status

        # Reabort in case for some reason aborting in __init__ did not cause
        # semgrep to exit
        self._abort_on_pending_changes()
        self._abort_on_conflicting_untracked_paths(self._status)

        logger.debug("Running git write-tree")
        current_tree = subprocess.run(
            ["git", "write-tree"],
            timeout=GIT_SH_TIMEOUT,
            stderr=subprocess.PIPE,
            stdout=subprocess.PIPE,
            encoding="utf-8",
            check=True,
        ).stdout.strip()
        try:
            for a in status.added:
                try:
                    a.unlink()
                except FileNotFoundError:
                    logger.verbose(
                        f"| {a} was not found when trying to delete", err=True
                    )

            logger.debug("Running git checkout for baseline context")
            subprocess.run(
                ["git", "checkout", f"{self._base_commit}", "--", "."],
                timeout=GIT_SH_TIMEOUT,
                check=True,
            )
            logger.debug("Finished git checkout for baseline context")
            yield
        finally:
            # Return to non-baseline state

            # git checkout will fail if the checked-out index deletes all files in the repo
            # In this case, we still want to continue without error.
            # Note that we have no good way of detecting this issue without inspecting the checkout output
            # message, which means we are fragile with respect to git version here.
            logger.debug("Running git checkout to return original context")
            x = subprocess.run(
                ["git", "checkout", f"{current_tree.strip()}", "--", "."],
                timeout=GIT_SH_TIMEOUT,
            )
            logger.debug("Finished git checkout to return original context")

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

            if status.removed:
                # Need to check if file exists since it is possible file was deleted
                # in both the base and head. Only call if there are files to delete
                to_remove = [r for r in status.removed if r.exists()]
                if to_remove:
                    logger.debug("Running git rm")
                    subprocess.run(
                        ["git", "rm", "-f", *(str(r) for r in to_remove)],
                        timeout=GIT_SH_TIMEOUT,
                    )
                    logger.debug("finished git rm")
