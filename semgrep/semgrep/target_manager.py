import contextlib
import os
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Collection
from typing import Dict
from typing import FrozenSet
from typing import Iterator
from typing import List
from typing import Set

import attr
from wcmatch import glob as wcglob

from semgrep.config_resolver import resolve_targets
from semgrep.error import FilesNotFoundError
from semgrep.output import OutputHandler
from semgrep.semgrep_types import Language
from semgrep.target_manager_extensions import ALL_EXTENSIONS
from semgrep.target_manager_extensions import FileExtension
from semgrep.target_manager_extensions import lang_to_exts
from semgrep.util import partition_set
from semgrep.util import sub_check_output
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


@contextlib.contextmanager
def optional_stdin_target(target: List[str]) -> Iterator[List[str]]:
    """
    Read target input from stdin if "-" is specified
    """
    if target == ["-"]:
        try:
            with tempfile.NamedTemporaryFile(delete=False) as fd:
                fd.write(sys.stdin.buffer.read())
                fname = fd.name
            yield [fname]
        finally:
            os.remove(fname)
    else:
        yield target


@attr.s(auto_attribs=True)
class TargetManager:
    """
    Handles all file include/exclude logic for semgrep

    If respect_git_ignore is true then will only consider files that are
    tracked or (untracked but not ignored) by git

    If skip_unknown_extensions is False then targets with extensions that are
    not understood by semgrep will always be returned by get_files. Else will discard
    targets with unknown extensions
    """

    includes: List[str]
    excludes: List[str]
    max_target_bytes: int
    targets: List[str]
    respect_git_ignore: bool
    output_handler: OutputHandler
    skip_unknown_extensions: bool

    _filtered_targets: Dict[Language, FrozenSet[Path]] = attr.ib(factory=dict)

    @staticmethod
    def resolve_targets(targets: List[str]) -> FrozenSet[Path]:
        """
        Return list of Path objects appropriately resolving relative paths
        (relative to cwd) if necessary
        """
        return frozenset(resolve_targets(targets))

    @staticmethod
    def _is_valid_file_or_dir(path: Path) -> bool:
        """Check this is a valid file or directory for semgrep scanning."""
        return os.access(path, os.R_OK) and not path.is_symlink()

    @staticmethod
    def _is_valid_file(path: Path) -> bool:
        """Check if file is a readable regular file.

        This eliminates files that should never be semgrep targets. Among
        others, this takes care of excluding symbolic links (because we don't
        want to scan the target twice), directories (which may be returned by
        globbing or by 'git ls-files' e.g. submodules), and files missing
        the read permission.
        """
        return TargetManager._is_valid_file_or_dir(path) and path.is_file()

    @staticmethod
    def _filter_valid_files(paths: FrozenSet[Path]) -> FrozenSet[Path]:
        """Keep only readable regular files"""
        return frozenset(path for path in paths if TargetManager._is_valid_file(path))

    @staticmethod
    def _expand_dir(
        curr_dir: Path, language: Language, respect_git_ignore: bool
    ) -> FrozenSet[Path]:
        """
        Recursively go through a directory and return list of all files with
        default file extension of language
        """

        def _parse_output(output: str, curr_dir: Path) -> FrozenSet[Path]:
            """
            Convert a newline delimited list of files to a set of path objects
            prepends curr_dir to all paths in said list

            If list is empty then returns an empty set
            """
            files: FrozenSet[Path] = frozenset()
            if output:
                files = frozenset(
                    p
                    for p in (
                        Path(curr_dir) / elem for elem in output.strip().split("\n")
                    )
                    if TargetManager._is_valid_file(p)
                )
            return files

        def _find_files_with_extension(
            curr_dir: Path, extension: FileExtension
        ) -> FrozenSet[Path]:
            """
            Return set of all files in curr_dir with given extension
            """
            return frozenset(
                p
                for p in curr_dir.rglob(f"*{extension}")
                if TargetManager._is_valid_file(p)
            )

        extensions = lang_to_exts(language)
        expanded: FrozenSet[Path] = frozenset()

        for ext in extensions:
            if respect_git_ignore:
                try:
                    # Tracked files
                    tracked_output = sub_check_output(
                        ["git", "ls-files", f"*{ext}"],
                        cwd=curr_dir.resolve(),
                        encoding="utf-8",
                        stderr=subprocess.DEVNULL,
                    )

                    # Untracked but not ignored files
                    untracked_output = sub_check_output(
                        [
                            "git",
                            "ls-files",
                            "--other",
                            "--exclude-standard",
                            f"*{ext}",
                        ],
                        cwd=curr_dir.resolve(),
                        encoding="utf-8",
                        stderr=subprocess.DEVNULL,
                    )

                    deleted_output = sub_check_output(
                        ["git", "ls-files", "--deleted", f"*{ext}"],
                        cwd=curr_dir.resolve(),
                        encoding="utf-8",
                        stderr=subprocess.DEVNULL,
                    )
                except (subprocess.CalledProcessError, FileNotFoundError):
                    logger.verbose(
                        f"Unable to ignore files ignored by git ({curr_dir} is not a git directory or git is not installed). Running on all files instead..."
                    )
                    # Not a git directory or git not installed. Fallback to using rglob
                    ext_files = _find_files_with_extension(curr_dir, ext)
                    expanded = expanded.union(ext_files)
                else:
                    tracked = _parse_output(tracked_output, curr_dir)
                    untracked_unignored = _parse_output(untracked_output, curr_dir)
                    deleted = _parse_output(deleted_output, curr_dir)
                    expanded = expanded.union(tracked)
                    expanded = expanded.union(untracked_unignored)
                    expanded = expanded.difference(deleted)
            else:
                ext_files = _find_files_with_extension(curr_dir, ext)
                expanded = expanded.union(ext_files)

        return TargetManager._filter_valid_files(expanded)

    @staticmethod
    def expand_targets(
        targets: Collection[Path], lang: Language, respect_git_ignore: bool
    ) -> FrozenSet[Path]:
        """
        Explore all directories. Remove duplicates
        """
        expanded: Set[Path] = set()
        for target in targets:
            if not TargetManager._is_valid_file_or_dir(target):
                continue

            if target.is_dir():
                expanded.update(
                    TargetManager._expand_dir(target, lang, respect_git_ignore)
                )
            else:
                expanded.add(target)

        return frozenset(expanded)

    @staticmethod
    def preprocess_path_patterns(patterns: List[str]) -> List[str]:
        """Convert semgrep's path include/exclude patterns to wcmatch's glob patterns.

        In semgrep, pattern "foo/bar" should match paths "x/foo/bar", "foo/bar/x", and
        "x/foo/bar/x". It implicitly matches zero or more directories at the beginning and the end
        of the pattern. In contrast, we have to explicitly specify the globstar (**) patterns in
        wcmatch. This function will converts a pattern "foo/bar" into "**/foo/bar" and
        "**/foo/bar/**". We need the pattern without the trailing "/**" because "foo/bar.py/**"
        won't match "foo/bar.py".
        """
        result = []
        for pattern in patterns:
            result.append("**/" + pattern)
            result.append("**/" + pattern + "/**")
        return result

    @staticmethod
    def filter_includes(arr: FrozenSet[Path], includes: List[str]) -> FrozenSet[Path]:
        """
        Returns all elements in arr that match any includes pattern

        If includes is empty, returns arr unchanged
        """
        if not includes:
            return arr

        includes = TargetManager.preprocess_path_patterns(includes)
        return frozenset(
            wcglob.globfilter(arr, includes, flags=wcglob.GLOBSTAR | wcglob.DOTGLOB)
        )

    @staticmethod
    def filter_excludes(arr: FrozenSet[Path], excludes: List[str]) -> FrozenSet[Path]:
        """
        Returns all elements in arr that do not match any excludes pattern

        If excludes is empty, returns arr unchanged
        """
        if not excludes:
            return arr

        excludes = TargetManager.preprocess_path_patterns(excludes)
        return arr - frozenset(
            wcglob.globfilter(arr, excludes, flags=wcglob.GLOBSTAR | wcglob.DOTGLOB)
        )

    @staticmethod
    def filter_by_size(arr: FrozenSet[Path], max_target_bytes: int) -> FrozenSet[Path]:
        """
        Return all the files whose size doesn't exceed the limit.

        If max_target_bytes is zero or negative, all paths are returned.
        If some paths are invalid, they may or may not be included in the
        result.
        """
        if max_target_bytes <= 0:
            return arr
        else:
            return frozenset(
                path
                for path in arr
                if TargetManager._is_valid_file(path)
                and os.path.getsize(path) <= max_target_bytes
            )

    def filtered_files(self, lang: Language) -> FrozenSet[Path]:
        """
        Return all files that are decendants of any directory in TARGET that have
        an extension matching LANG that match any pattern in INCLUDES and do not
        match any pattern in EXCLUDES. Any file in TARGET bypasses excludes and includes.
        If a file in TARGET has a known extension that is not for langugage LANG then
        it is also filtered out

        Note also filters out any directory and decendants of `.git`
        """
        if lang in self._filtered_targets:
            return self._filtered_targets[lang]

        targets = self.resolve_targets(self.targets)

        files, directories = partition_set(lambda p: not p.is_dir(), targets)

        # Error on non-existent files
        explicit_files, nonexistent_files = partition_set(lambda p: p.is_file(), files)
        if nonexistent_files:
            self.output_handler.handle_semgrep_error(
                FilesNotFoundError(tuple(nonexistent_files))
            )

        targets = self.expand_targets(directories, lang, self.respect_git_ignore)
        targets = self.filter_includes(targets, self.includes)
        targets = self.filter_excludes(targets, self.excludes + [".git"])
        targets = self.filter_by_size(targets, self.max_target_bytes)

        # Remove explicit_files with known extensions.
        explicit_files_with_lang_extension = frozenset(
            f
            for f in explicit_files
            if (any(f.match(f"*{ext}") for ext in lang_to_exts(lang)))
        )
        targets = targets.union(explicit_files_with_lang_extension)

        if not self.skip_unknown_extensions:
            explicit_files_with_unknown_extensions = frozenset(
                f
                for f in explicit_files
                if not any(f.match(f"*{ext}") for ext in ALL_EXTENSIONS)
            )
            targets = targets.union(explicit_files_with_unknown_extensions)

        self._filtered_targets[lang] = targets
        return self._filtered_targets[lang]

    def get_files(
        self, lang: Language, includes: List[str], excludes: List[str]
    ) -> FrozenSet[Path]:
        """
        Returns list of files that should be analyzed for a LANG

        Given this object's TARGET, self.INCLUDE, and self.EXCLUDE will return list
        of all descendant files of directories in TARGET that end in extension
        typical for LANG. If self.INCLUDES is non empty then all files will have an ancestor
        that matches a pattern in self.INCLUDES. Will not include any file that has
        an ancestor that matches a pattern in self.EXCLUDES. Any explicitly named files
        in TARGET will bypass this global INCLUDE/EXCLUDE filter. The local INCLUDE/EXCLUDE
        filter is then applied.
        """
        targets = self.filtered_files(lang)
        targets = self.filter_includes(targets, includes)
        targets = self.filter_excludes(targets, excludes)
        targets = self.filter_by_size(targets, self.max_target_bytes)
        return targets
