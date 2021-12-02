import contextlib
import os
import subprocess
import sys
import tempfile
import time
from pathlib import Path
from typing import Collection
from typing import Dict
from typing import FrozenSet
from typing import Iterable
from typing import Iterator
from typing import List
from typing import Optional
from typing import Sequence
from typing import Set

import attr
from wcmatch import glob as wcglob

from semgrep.config_resolver import resolve_targets
from semgrep.error import FilesNotFoundError
from semgrep.ignores import FileIgnore
from semgrep.semgrep_types import FileExtension
from semgrep.semgrep_types import LANGUAGE
from semgrep.semgrep_types import Language
from semgrep.semgrep_types import LanguageDefinition
from semgrep.semgrep_types import Shebang
from semgrep.util import partition_set
from semgrep.util import sub_check_output
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

MAX_CHARS_TO_READ_FOR_SHEBANG = 255


ALL_EXTENSIONS: Collection[FileExtension] = {
    ext
    for definition in LANGUAGE.definition_by_id.values()
    for ext in definition.exts
    if ext != FileExtension("")
}


@contextlib.contextmanager
def converted_pipe_targets(targets: Sequence[str]) -> Iterator[Sequence[str]]:
    """
    Provides a context in which FIFOs have been copied into temp files

    This is necessary as we can not easily rewire these pipes into the called semgrep-core
    process.

    :param targets: Input target specifiers
    :return: A sequence of non-pipe specifiers (Path(t).is_file() returns true)
    """

    out_targets = []
    with tempfile.TemporaryDirectory() as temp_dir:
        for t in targets:
            if t == "-":
                with (Path(temp_dir) / "stdin").open("wb") as fd:
                    fd.write(sys.stdin.buffer.read())
                out_targets.append(fd.name)
            elif Path(t).is_fifo():
                with (Path(temp_dir) / t[1:].replace("/", "_")).open("wb") as fd:
                    with Path(t).open("rb") as td:
                        fd.write(td.read())
                out_targets.append(fd.name)
            else:
                out_targets.append(t)
        yield out_targets


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

    includes: Sequence[str]
    excludes: Sequence[str]
    max_target_bytes: int
    targets: Sequence[str] = attr.ib()
    respect_git_ignore: bool
    skip_unknown_extensions: bool
    file_ignore: Optional[FileIgnore]

    _filtered_targets: Dict[Language, FrozenSet[Path]] = attr.ib(factory=dict)

    @targets.validator
    def _check_exists(self, attribute: str, value: Sequence[str]) -> None:
        """
        Raise FilesNotFoundError if any element of targets is not a dir/regular file or
        symlink to a dir/regular file:

        i.e. does not exist, is a mount/socket etc.
        """
        targets = self.resolve_targets(self.targets)
        files, _directories = partition_set(lambda p: not p.is_dir(), targets)
        _explicit_files, nonexistent_files = partition_set(lambda p: p.is_file(), files)
        if nonexistent_files:
            raise FilesNotFoundError(tuple(nonexistent_files))

    @staticmethod
    def resolve_targets(targets: Sequence[str]) -> FrozenSet[Path]:
        """
        Return list of Path objects appropriately resolving relative paths
        (relative to cwd) if necessary
        """
        return frozenset(resolve_targets(targets))

    @staticmethod
    def _is_valid_file_or_dir(path: Path) -> bool:
        """Check this is a valid file or directory for semgrep scanning."""
        return os.access(str(path), os.R_OK) and not path.is_symlink()

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

        def _executes_with_shebang(f: Path, shebangs: Collection[Shebang]) -> bool:
            """
            Returns if a path is executable and executes with one of a set of programs
            """
            if not os.access(str(f), os.X_OK | os.R_OK):
                return False
            try:
                with f.open("r") as fd:
                    hline = fd.readline(MAX_CHARS_TO_READ_FOR_SHEBANG).rstrip()
                return any(hline.endswith(s) for s in shebangs)
            except UnicodeDecodeError:
                logger.debug(
                    f"Encountered likely binary file {f} while reading shebang; skipping this file"
                )
                return False

        def _find_files_with_extension_or_shebang(
            paths: Iterable[Path],
            definition: LanguageDefinition,
        ) -> FrozenSet[Path]:
            """
            Finds all files in a collection of paths that either:
            - end with one of a set of extensions
            - is a script that executes with one of a set of programs

            Takes ~ 100 ms to execute on a Mac PowerBook on a repo with 3000 files.
            """
            res: Set[Path] = set()
            before = time.time()
            for path in paths:
                if path.is_dir():
                    res.update(
                        p for ext in definition.exts for p in path.rglob(f"*{ext}")
                    )
                    res.update(
                        Path(root) / f
                        for root, _, files in os.walk(str(curr_dir))
                        for f in files
                        if _executes_with_shebang(Path(root) / f, definition.shebangs)
                    )
                else:
                    if any(str(path).endswith(ext) for ext in definition.exts):
                        res.add(path)
                    if _executes_with_shebang(path, definition.shebangs):
                        res.add(path)
            logger.debug(
                f"Scanned file system for matching files in {time.time() - before} s"
            )
            return frozenset(res)

        definition = LANGUAGE.definition_by_id[language]

        if respect_git_ignore:
            try:
                # git ls-files is significantly faster than os.walk when performed on a git project,
                # so identify the git files first, then filter those

                # Tracked files
                tracked_output = sub_check_output(
                    ["git", "ls-files"],
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
                    ],
                    cwd=curr_dir.resolve(),
                    encoding="utf-8",
                    stderr=subprocess.DEVNULL,
                )

                deleted_output = sub_check_output(
                    ["git", "ls-files", "--deleted"],
                    cwd=curr_dir.resolve(),
                    encoding="utf-8",
                    stderr=subprocess.DEVNULL,
                )
                tracked = _parse_output(tracked_output, curr_dir)
                untracked_unignored = _parse_output(untracked_output, curr_dir)
                deleted = _parse_output(deleted_output, curr_dir)
                paths = tracked.union(untracked_unignored).difference(deleted)
                results = _find_files_with_extension_or_shebang(paths, definition)

            except (subprocess.CalledProcessError, FileNotFoundError):
                logger.verbose(
                    f"Unable to ignore files ignored by git ({curr_dir} is not a git directory or git is not installed). Running on all files instead..."
                )
                # Not a git directory or git not installed. Fallback to using rglob
                results = _find_files_with_extension_or_shebang([curr_dir], definition)
        else:
            results = _find_files_with_extension_or_shebang([curr_dir], definition)

        return TargetManager._filter_valid_files(results)

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
    def preprocess_path_patterns(patterns: Sequence[str]) -> List[str]:
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
    def filter_includes(
        arr: FrozenSet[Path], includes: Sequence[str]
    ) -> FrozenSet[Path]:
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
    def filter_excludes(
        arr: FrozenSet[Path], excludes: Sequence[str]
    ) -> FrozenSet[Path]:
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

        # Non dir/non file should not exist cause of init time validation
        # See _check_exists()
        targets = self.resolve_targets(self.targets)
        files, directories = partition_set(lambda p: not p.is_dir(), targets)
        explicit_files, _ = partition_set(lambda p: p.is_file(), files)

        targets = self.expand_targets(directories, lang, self.respect_git_ignore)
        targets = self.filter_includes(targets, self.includes)
        targets = self.filter_excludes(targets, [*self.excludes, ".git"])
        targets = self.filter_by_size(targets, self.max_target_bytes)

        # Remove explicit_files with known extensions.
        explicit_files_with_lang_extension = frozenset(
            f
            for f in explicit_files
            if (any(f.match(f"*{ext}") for ext in LANGUAGE.definition_by_id[lang].exts))
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
        self, lang: Language, includes: Sequence[str], excludes: Sequence[str]
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
        targets = (
            self.file_ignore.filter_paths(targets) if self.file_ignore else targets
        )
        targets = self.filter_includes(targets, includes)
        targets = self.filter_excludes(targets, excludes)
        targets = self.filter_by_size(targets, self.max_target_bytes)
        return targets
