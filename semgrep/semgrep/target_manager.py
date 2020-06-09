import subprocess
from pathlib import Path
from typing import Dict
from typing import List
from typing import Set

from semgrep.error import NotGitProjectError
from semgrep.util import partition_set


class TargetManager:
    def __init__(
        self,
        includes: List[str],
        excludes: List[str],
        targets: List[str],
        visible_to_git_only: bool = True,
    ) -> None:
        """
            Handles all file include/exclude logic for semgrep

            If visible_to_git_only is true then will only consider files that are
            tracked or (untracked but not ignored) by git
        """
        self._targets = targets
        self._includes = includes
        self._excludes = excludes
        self._visible_to_git_only = visible_to_git_only

        self._filtered_targets: Dict[str, Set[Path]] = {}

    @staticmethod
    def resolve_targets(targets: List[str]) -> Set[Path]:
        """
            Return list of Path objects appropriately resolving relative paths
            (relative to cwd) if necessary
        """
        base_path = Path(".")
        return set(
            Path(target) if Path(target).is_absolute() else base_path.joinpath(target)
            for target in targets
        )

    def _expand_dir(self, curr_dir: Path, lang: str) -> Set[Path]:
        """
            Recursively go through a directory and return list of all files with
            default file extention of language
        """
        ext = "py"
        if self._visible_to_git_only:
            try:
                # Tracked files
                tracked_output = subprocess.check_output(
                    ["git", "ls-files", f"*.{ext}"],
                    cwd=curr_dir.resolve(),
                    encoding="utf-8",
                    stderr=subprocess.DEVNULL,
                )

                # Untracked but not ignored files
                untracked_output = subprocess.check_output(
                    ["git", "ls-files", "--other", "--exclude-standard", f"*.{ext}"],
                    cwd=curr_dir.resolve(),
                    encoding="utf-8",
                    stderr=subprocess.DEVNULL,
                )
            except subprocess.CalledProcessError:
                raise NotGitProjectError(
                    f"{curr_dir.resolve()} is not a git repository."
                )

            tracked = set(Path(elem) for elem in tracked_output.strip().split("\n"))
            untracked_unignored = set(
                Path(elem) for elem in untracked_output.strip().split("\n")
            )
            return tracked.union(untracked_unignored)

        else:
            output = subprocess.run(
                ["find", curr_dir, "-type", "f", "-name", f"*.{ext}"],
                encoding="utf-8",
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            )
            return set(Path(elem) for elem in output.stdout.strip().split("\n"))

    def expand_targets(self, targets: Set[Path], lang: str) -> Set[Path]:
        """
            Explore all directories. Remove duplicates
        """
        expanded = set()
        for target in targets:
            if not target.exists():
                continue

            if target.is_dir():
                expanded.update(self._expand_dir(target, lang))
            else:
                expanded.add(target)

        return expanded

    @staticmethod
    def match_glob(path: Path, globs: List[str]) -> bool:
        """
            Return true if path or any parent of path matches any glob in globs
        """
        subpaths = [path, *path.parents]
        return any(p.match(glob) for p in subpaths for glob in globs)

    @staticmethod
    def filter_includes(arr: Set[Path], includes: List[str]) -> Set[Path]:
        """
            Returns all elements in arr that match any includes pattern

            If includes is empty, returns arr unchanged
        """
        if not includes:
            return arr

        return set(elem for elem in arr if TargetManager.match_glob(elem, includes))

    @staticmethod
    def filter_excludes(arr: Set[Path], excludes: List[str]) -> Set[Path]:
        """
            Returns all elements in arr that do not match any excludes excludes
        """
        return set(elem for elem in arr if not TargetManager.match_glob(elem, excludes))

    def filtered_files(self, lang: str) -> Set[Path]:
        """
            Return all files that are decendants of any directory in TARGET that have
            an extension matching LANG that match any pattern in INCLUDES and do not
            match any pattern in EXCLUDES. Any file in TARGET bypasses excludes and includes.
        """
        if lang in self._filtered_targets:
            return self._filtered_targets[lang]

        targets = self.resolve_targets(self._targets)
        explicit_files, directories = partition_set(lambda p: not p.is_dir(), targets)
        targets = self.expand_targets(directories, lang)
        targets = self.filter_includes(targets, self._includes)
        targets = self.filter_excludes(targets, self._excludes)

        self._filtered_targets[lang] = targets.union(explicit_files)
        return self._filtered_targets[lang]

    def get_files(
        self, lang: str, includes: List[str], excludes: List[str]
    ) -> List[Path]:
        """
            Returns list of files that should be analyzed for a LANG

            Given this object's TARGET, self.INCLUDE, and self.EXCLUDE will return list
            of all descendant files of directories in TARGET that end in extension
            typical for LANG. If self.INCLUDES is non empty then all files will have an ancestor
            that matches a pattern in self.INCLUDES. Will not include any file that has
            an ancestor that matches a pattern in self.EXCLUDES. Any explcilty named files
            in TARGET will bypass this global INCLUDE/EXCLUDE filter. The local INCLUDE/EXCLUDE
            filter is then applied.
        """
        targets = self.filtered_files(lang)
        targets = self.filter_includes(targets, includes)
        targets = self.filter_excludes(targets, excludes)
        return list(targets)


if __name__ == "__main__":
    try:
        target_manager = TargetManager(includes=[], excludes=["tests"], targets=["."])
        target_manager.get_files("python", [], [])
    except Exception as e:
        print(str(e))
