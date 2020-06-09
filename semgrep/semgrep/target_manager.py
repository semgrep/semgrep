import subprocess
from pathlib import Path
from typing import Dict
from typing import List
from typing import Optional
from typing import Set


class TargetManager:
    def __init__(
        self,
        includes: List[str],
        excludes: List[str],
        targets: List[str],
        git_ignore_file: Optional[Path] = None,
    ) -> None:
        """
            Handles all file include/exclude logic for semgrep
        """
        self._targets = targets
        self._includes = includes
        self._excludes = excludes
        self._git_ignore_file = git_ignore_file
        self._filtered_targets: Dict[str, List[Path]] = {}

    @staticmethod
    def resolve_targets(targets: List[str]) -> List[Path]:
        """
            Return list of Path objects appropriately resolving relative paths
            (relative to cwd) if necessary
        """
        base_path = Path(".")
        return [
            Path(target) if Path(target).is_absolute() else base_path.joinpath(target)
            for target in targets
        ]

    @staticmethod
    def _expand_dir(curr_dir: Path, lang: str) -> Set[Path]:
        """
            Recursively go through a directory and return list of all files with
            default file extention of language
        """
        ext = "py"
        output = subprocess.run(
            ["find", curr_dir, "-type", "f", "-name", f"*.{ext}"],
            encoding="utf-8",
            stdout=subprocess.PIPE,
        )
        return set(Path(elem) for elem in output.stdout.strip().split("\n"))

    @staticmethod
    def expand_targets(targets: List[Path], lang: str) -> List[Path]:
        """
            Explore all directories. Remove duplicates
        """
        expanded = set()
        for target in targets:
            if not target.exists():
                continue

            if target.is_dir():
                expanded.update(TargetManager._expand_dir(target, lang))
            else:
                expanded.add(target)

        return list(expanded)

    @staticmethod
    def match_glob(path: Path, globs: List[str]) -> bool:
        """
            Return true if path or any parent of path matches any glob in globs
        """
        subpaths = [path, *path.parents]
        return any(p.match(glob) for p in subpaths for glob in globs)

    @staticmethod
    def filter_includes(arr: List[Path], includes: List[str]) -> List[Path]:
        """
            Returns all elements in arr that match any includes pattern

            If includes is empty, returns arr unchanged
        """
        if not includes:
            return arr

        return [elem for elem in arr if TargetManager.match_glob(elem, includes)]

    @staticmethod
    def filter_excludes(arr: List[Path], excludes: List[str]) -> List[Path]:
        """
            Returns all elements in arr that do not match any excludes excludes
        """
        return [elem for elem in arr if not TargetManager.match_glob(elem, excludes)]

    def filtered_files(self, lang: str) -> List[Path]:
        if lang in self._filtered_targets:
            return self._filtered_targets[lang]

        targets = self.resolve_targets(self._targets)
        targets = self.expand_targets(targets, lang)
        targets = self.filter_includes(targets, self._includes)
        targets = self.filter_excludes(targets, self._excludes)

        # TODO filter out gitignore

        self._filtered_targets[lang] = targets
        return targets

    def get_files(
        self, lang: str, includes: List[str], excludes: List[str]
    ) -> List[Path]:
        """
            per rule include and excludes
        """
        targets = self.filtered_files(lang)
        targets = self.filter_includes(targets, includes)
        targets = self.filter_excludes(targets, excludes)
        return targets


if __name__ == "__main__":
    target_manager = TargetManager(includes=[], excludes=[], targets=["."])
    target_manager.get_files("python", [], [])
