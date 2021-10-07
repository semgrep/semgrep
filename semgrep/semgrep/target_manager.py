import contextlib
import os
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Collection
from typing import Dict
from typing import Iterator
from typing import List
from typing import NamedTuple
from typing import Sequence
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


# Target files obtained from the command line (explicit) and discovered
# by scanning folders specified on the command line (filterable).
# The latter are subject to further filtering by semgrep-core, which is
# why we keep them separate and we let semgrep-core know which are which.
class TargetFiles(NamedTuple):
    explicit: Set[Path]
    filterable: Set[Path]


def print_target_files(msg: str, targets: TargetFiles) -> None:
    """ Print target files for debugging purposes."""
    print(f"{msg}:")
    print(f"  filterable targets = {targets.filterable}")
    print(f"  explicit targets = {targets.explicit}")


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
    targets: Sequence[str]  # explicit target files or directories
    respect_git_ignore: bool
    output_handler: OutputHandler
    skip_unknown_extensions: bool

    # For each language, a pair (explicit target files, filterable target files)
    _filtered_targets: Dict[Language, TargetFiles] = attr.ib(factory=dict)

    @staticmethod
    def resolve_targets(targets: Sequence[str]) -> Set[Path]:
        """Turn all paths to absolute paths.

        Return list of Path objects appropriately resolving relative paths
        (relative to cwd) if necessary.
        """
        return set(resolve_targets(targets))

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
    def _is_executable_file(path: Path) -> bool:
        return TargetManager._is_valid_file(path) and os.access(path, os.X_OK)

    @staticmethod
    def _filter_valid_files(paths: Set[Path]) -> Set[Path]:
        """Keep only readable regular files"""
        return set(path for path in paths if TargetManager._is_valid_file(path))

    @staticmethod
    def _expand_dir(
        curr_dir: Path, language: Language, respect_git_ignore: bool
    ) -> Set[Path]:
        """
        Recursively go through a directory and return list of all files with
        default file extension of language
        """

        def _parse_output(output: str, curr_dir: Path) -> Set[Path]:
            """
            Convert a newline delimited list of files to a set of path objects
            prepends curr_dir to all paths in said list

            If list is empty then returns an empty set
            """
            files: Set[Path] = set()
            if output:
                files = set(
                    p
                    for p in (
                        Path(curr_dir) / elem for elem in output.strip().split("\n")
                    )
                    if TargetManager._is_valid_file(p)
                )
            return files

        def _find_files_with_extension(
            curr_dir: Path, extension: FileExtension
        ) -> Set[Path]:
            """Return set of all files in curr_dir with given extension."""
            return set(
                p
                for p in curr_dir.rglob(f"*{extension}")
                if TargetManager._is_valid_file(p)
            )

        def _find_executable_files(curr_dir: Path) -> Set[Path]:
            """Return set of all executable files without an extension in curr_dir."""
            return set(
                p
                for p in curr_dir.rglob(f"*")
                if TargetManager._is_executable_file(p) and not p.match("*.*")
            )

        extensions = lang_to_exts(language)
        expanded: Set[Path] = set()

        # Include all executables without an extension
        # exec_files = _find_executable_files(curr_dir)
        # expanded = expanded.union(exec_files)

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
        root_paths: Collection[Path], lang: Language, respect_git_ignore: bool
    ) -> Set[Path]:
        """Explore all directories."""
        expanded: Set[Path] = set()
        for target in root_paths:
            if not TargetManager._is_valid_file_or_dir(target):
                continue

            if target.is_dir():
                expanded.update(
                    TargetManager._expand_dir(target, lang, respect_git_ignore)
                )
            else:
                expanded.add(target)
        return expanded

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
    def filter_includes(paths: Set[Path], includes: Sequence[str]) -> Set[Path]:
        """Restrict the set to the files matching the 'includes' pattern, if specified."""
        if not includes:
            return paths
        includes = TargetManager.preprocess_path_patterns(includes)
        paths = set(
            wcglob.globfilter(paths, includes, flags=wcglob.GLOBSTAR | wcglob.DOTGLOB)
        )
        return paths

    @staticmethod
    def filter_excludes(paths: Set[Path], excludes: Sequence[str]) -> Set[Path]:
        """Restrict the set to the files not matching the 'excludes' pattern."""
        if not excludes:  # just an optimization
            return paths
        excludes = TargetManager.preprocess_path_patterns(excludes)
        paths = paths - set(
            wcglob.globfilter(paths, excludes, flags=wcglob.GLOBSTAR | wcglob.DOTGLOB)
        )
        return paths

    def filtered_files(self, lang: Language) -> TargetFiles:
        """Return files that should be analyzed for a language.

        This is a lazy computation. Scanning the file system is done only on
        the first call of this method.

        Target directories specified on the command line (or during object
        creation) are used as scanning roots to discover target files.
        Such discovered files are filtered out based on file extensions
        required by the language or other generic criteria.
        User-specified glob patterns are used to include or exclude certain
        paths or file names in addition to this.

        Files that are not directories are considered explicit targets
        and by default are not filtered out by any mechanism.
        """
        if lang in self._filtered_targets:
            return self._filtered_targets[lang]

        # Turn all targets into absolute paths. Not doing this because
        # of complications during testing. Absolute target paths such
        # as '/home/yourname/whatever/...` differ from one user to another.
        # If we're ok permanently with relative paths, let's remove the
        # resolve_targets() code rather than making it not do what it's
        # supposed to do (it was previously prepending './' and then
        # removing it, which was misleading).
        # root_targets = self.resolve_targets(self.targets)
        root_targets = set(Path(path_str) for path_str in self.targets)

        files, directories = partition_set(lambda p: not p.is_dir(), root_targets)

        # Error on non-existent files
        explicit_files, nonexistent_files = partition_set(lambda p: p.is_file(), files)
        if nonexistent_files:
            self.output_handler.handle_semgrep_error(
                FilesNotFoundError(tuple(nonexistent_files))
            )

        # Scan file system and filter for language lang.
        filterable_targets = self.expand_targets(
            directories, lang, self.respect_git_ignore
        )
        explicit_targets = explicit_files

        # Avoid duplicates (e.g. foo/bar can be both an explicit file and
        # discovered in folder foo/)
        filterable_targets = filterable_targets - explicit_targets

        # Filter based on custom glob patterns.
        filterable_targets = self.filter_includes(filterable_targets, self.includes)
        filterable_targets = self.filter_excludes(
            filterable_targets, [*self.excludes, ".git"]
        )

        # Remove explicit targets with *known* extensions.
        # This violates "process all target files explicitly passed on the
        # command line".
        #
        # For now, is the best solution we have for dealing with a rule
        # that works for multiple languages. We exclude the explicit target
        # if it has a well-known extension that's not for the requested
        # language.
        # See https://github.com/returntocorp/semgrep/issues/966
        #
        # A better solution would be to not filter a target against a single
        # language. Instead, the list of allowed languages would stay as a list,
        # and we would pass (target, [lang1, lang2]) to semgrep-core.
        # semgrep-core would then try one language and then the other
        # if needed, which would avoid duplicate matches and would avoid
        # reporting a parsing error if parsing was successful with one language.
        #
        explicit_targets_without_standard_extension = set(
            f
            for f in explicit_targets
            if f.match(f"*.*") and not any(f.match(f"*{ext}") for ext in ALL_EXTENSIONS)
        )

        explicit_targets_with_expected_extension = set(
            f
            for f in explicit_targets
            if not f.match(f"*.*")
            or any(f.match(f"*{ext}") for ext in lang_to_exts(lang))
        )

        # Optionally ignore explicit files with incorrect extensions for the
        # language (CLI option --skip-unknown-extensions).
        if self.skip_unknown_extensions:
            explicit_targets = explicit_targets_with_expected_extension
        else:  # default
            explicit_targets = explicit_targets_with_expected_extension.union(
                explicit_targets_without_standard_extension
            )
        targets = TargetFiles(explicit=explicit_targets, filterable=filterable_targets)
        self._filtered_targets[lang] = targets
        return targets

    def get_files(
        self, lang: Language, extra_includes: List[str], extra_excludes: List[str]
    ) -> TargetFiles:
        """Return target files with extra glob patterns to include or exclude.

        This is meant for adding or removing target files from the default
        set on a rule-specific basis.

        Explicitly targeted files are still subject to filtering by the rule includes/excludes
        but will not be further filtered by semgrep-core
        """
        targets = self.filtered_files(lang)
        filterable_targets = self.filter_includes(targets.filterable, extra_includes)
        filterable_targets = self.filter_excludes(filterable_targets, extra_excludes)

        explicit_targets = self.filter_includes(targets.explicit, extra_includes)
        explicit_targets = self.filter_excludes(explicit_targets, extra_excludes)

        targets = TargetFiles(explicit=explicit_targets, filterable=filterable_targets)
        return targets
