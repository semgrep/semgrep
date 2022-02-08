import contextlib
import os
import subprocess
import sys
import tempfile
import time
from collections import defaultdict
from pathlib import Path
from typing import Callable
from typing import cast
from typing import Collection
from typing import Dict
from typing import FrozenSet
from typing import Iterable
from typing import Iterator
from typing import List
from typing import Mapping
from typing import Optional
from typing import Sequence
from typing import Set
from typing import Tuple

# usually this would be a try...except ImportError
# but mypy understands only this
# see https://github.com/python/mypy/issues/1393
if sys.version_info[:2] >= (3, 8):
    # Literal is available in stdlib since Python 3.8
    from typing import Literal
else:
    from typing_extensions import Literal

import attr
import click
from attr import Factory
from wcmatch import glob as wcglob

from semgrep.config_resolver import resolve_targets
from semgrep.constants import Colors
from semgrep.error import FilesNotFoundError
from semgrep.formatter.text import width
from semgrep.ignores import FileIgnore
from semgrep.semgrep_types import FileExtension
from semgrep.semgrep_types import LANGUAGE
from semgrep.semgrep_types import Language
from semgrep.semgrep_types import LanguageDefinition
from semgrep.semgrep_types import Shebang
from semgrep.types import FilteredTargets
from semgrep.util import partition_set
from semgrep.util import sub_check_output
from semgrep.util import with_color
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

MAX_CHARS_TO_READ_FOR_SHEBANG = 255
PATHS_ALWAYS_SKIPPED = (".git",)

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
class IgnoreLog:
    """Keeps track of which paths were ignored for what reason.

    Each attribute is a distinct reason why files could be ignored.

    Some reason can apply once per rule; these are mappings keyed on the rule id.
    """

    target_manager: "TargetManager"

    semgrepignored: Set[Path] = Factory(set)
    always_skipped: Set[Path] = Factory(set)
    cli_includes: Set[Path] = Factory(set)
    cli_excludes: Set[Path] = Factory(set)
    size_limit: Set[Path] = Factory(set)

    rule_includes: Dict[str, Set[Path]] = Factory(lambda: defaultdict(set))
    rule_excludes: Dict[str, Set[Path]] = Factory(lambda: defaultdict(set))

    @property
    def rule_ids_with_skipped_paths(self) -> FrozenSet[str]:
        """All rule IDs that have skipped paths.

        Note that if a rule ID defines excludes/includes,
        but they didn't skip any paths,
        that rule ID will not show up here.
        """
        return frozenset(
            {
                *(rule_id for rule_id, skips in self.rule_includes.items() if skips),
                *(rule_id for rule_id, skips in self.rule_excludes.items() if skips),
            }
        )

    def __str__(self) -> str:
        skip_fragments = []

        if self.target_manager.respect_git_ignore:
            skip_fragments.append("all .gitignored files")

        if self.cli_includes:
            skip_fragments.append(
                f"{len(self.cli_includes)} files not matching --include patterns"
            )
        if self.cli_excludes:
            skip_fragments.append(
                f"{len(self.cli_excludes)} files matching --exclude patterns"
            )
        if self.size_limit:
            skip_fragments.append(
                f"{len(self.size_limit)} files larger than {self.target_manager.max_target_bytes / 1000 / 1000} MB"
            )
        if self.semgrepignored:
            skip_fragments.append(
                f"{len(self.semgrepignored)} files matching .semgrepignore patterns"
            )

        if not skip_fragments:
            return "no files were skipped"

        message = "skipped: " + ", ".join(skip_fragments)

        message += "\nfor a detailed list of skipped files, run semgrep with the --verbose flag\n"
        return message

    def yield_verbose_lines(self) -> Iterator[Tuple[Literal[0, 1, 2], str]]:
        """Yields lines of verbose output for the skipped files.

        The returned tuple is (level, message).
        The level is a number; one of 0, 1, or 2, which sets the indentation when outputting the line.
        """
        yield 0, "Files skipped:"

        yield 1, "Always skipped by Semgrep:"
        if self.always_skipped:
            for path in self.always_skipped:
                yield 2, with_color(Colors.cyan, str(path))
        else:
            yield 2, "<none>"

        yield 1, "Skipped by .gitignore:"
        if self.target_manager.respect_git_ignore:
            yield 1, "(Disable by passing --no-git-ignore)"
            yield 2, "<all files not listed by `git ls-files` were skipped>"
        else:
            yield 1, "(Disabled with --no-git-ignore)"
            yield 2, "<none>"

        yield 1, "Skipped by .semgrepignore:"
        yield 1, "(See: https://semgrep.dev/docs/ignoring-files-folders-code/#understanding-semgrep-defaults)"
        if self.semgrepignored:
            for path in self.semgrepignored:
                yield 2, with_color(Colors.cyan, str(path))
        else:
            yield 2, "<none>"

        yield 1, "Skipped by --include patterns:"
        if self.cli_includes:
            for path in self.cli_includes:
                yield 2, with_color(Colors.cyan, str(path))
        else:
            yield 2, "<none>"

        yield 1, "Skipped by --exclude patterns:"
        if self.cli_excludes:
            for path in self.cli_excludes:
                yield 2, with_color(Colors.cyan, str(path))
        else:
            yield 2, "<none>"

        yield 1, f"Skipped by limiting to files smaller than {self.target_manager.max_target_bytes} bytes:"
        yield 1, "(Adjust with the --max-target-bytes flag)"
        if self.size_limit:
            for path in self.size_limit:
                yield 2, with_color(Colors.cyan, str(path))
        else:
            yield 2, "<none>"

        for rule_id in self.rule_ids_with_skipped_paths:
            if rule_id.startswith("fingerprints."):
                # Skip fingerprint rules, since they all have include patterns
                continue
            yield 1, f"Skipped only for {with_color(Colors.bright_blue, rule_id)} by the rule's include patterns:"
            if self.rule_includes[rule_id]:
                for path in self.rule_includes[rule_id]:
                    yield 2, with_color(Colors.cyan, str(path))
            else:
                yield 2, "<none>"

            yield 1, f"Skipped only for {with_color(Colors.bright_blue, rule_id)} by the rule's exclude patterns:"
            if self.rule_excludes[rule_id]:
                for path in self.rule_excludes[rule_id]:
                    yield 2, with_color(Colors.cyan, str(path))
            else:
                yield 2, "<none>"

    def verbose_output(self) -> str:
        formatters_by_level: Mapping[int, Callable[[str], str]] = {
            0: lambda line: "\n".join([40 * "=", line, 40 * "="]),
            1: lambda line: click.wrap_text(
                with_color(Colors.foreground, line, bold=True),
                width,
                2 * " ",
                2 * " ",
                False,
            ),
            2: lambda line: click.wrap_text(
                line,
                width,
                "   • ",
                "     ",
                False,
            ),
        }
        output = ""

        prev_level = None
        for level, line in self.yield_verbose_lines():
            if prev_level != level:
                output += "\n"
            formatter = formatters_by_level[level]
            output += formatter(line) + "\n"
            prev_level = level

        return output


@attr.s(auto_attribs=True)
class TargetManager:
    """
    Handles all file include/exclude logic for semgrep

    If respect_git_ignore is true then will only consider files that are
    tracked or (untracked but not ignored) by git

    If skip_unknown_extensions is False then targets with extensions that are
    not understood by semgrep will always be returned by get_files. Else will discard
    targets with unknown extensions

    TargetManager not to be confused with https://jobs.target.com/search-jobs/store%20manager
    """

    includes: Sequence[str]
    excludes: Sequence[str]
    max_target_bytes: int
    targets: Sequence[str] = attr.ib()
    respect_git_ignore: bool
    skip_unknown_extensions: bool
    file_ignore: Optional[FileIgnore]
    ignore_log: IgnoreLog = Factory(IgnoreLog, takes_self=True)

    _filtered_targets: Dict[Language, FilteredTargets] = attr.ib(factory=dict)

    @targets.validator
    def _check_exists(self, attribute: str, value: Sequence[str]) -> None:
        """
        Raise FilesNotFoundError if any element of targets is not a dir/regular file or
        symlink to a dir/regular file:

        i.e. does not exist, is a mount/socket etc.
        """
        targets = self.resolve_targets(self.targets)
        files, _ = partition_set(lambda p: not p.is_dir(), targets)
        _, nonexistent_files = partition_set(lambda p: p.is_file(), files)
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
        includes: Sequence[str], *, candidates: FrozenSet[Path]
    ) -> FilteredTargets:
        """
        Returns all elements in candidates that match any includes pattern

        If includes is empty, returns candidates unchanged
        """
        if not includes:
            return FilteredTargets(candidates)

        includes = TargetManager.preprocess_path_patterns(includes)
        # Need cast b/c types-wcmatch doesn't use generics properly :(
        kept = frozenset(
            cast(
                Iterable[Path],
                wcglob.globfilter(
                    candidates, includes, flags=wcglob.GLOBSTAR | wcglob.DOTGLOB
                ),
            )
        )
        return FilteredTargets(kept, frozenset(candidates - kept))

    @staticmethod
    def filter_excludes(
        excludes: Sequence[str], *, candidates: FrozenSet[Path]
    ) -> FilteredTargets:
        """
        Returns all elements in candidates that do not match any excludes pattern

        If excludes is empty, returns candidates unchanged
        """
        if not excludes:
            return FilteredTargets(candidates)

        excludes = TargetManager.preprocess_path_patterns(excludes)

        # Need cast b/c types-wcmatch doesn't use generics properly :(
        removed = frozenset(
            cast(
                Iterable[Path],
                wcglob.globfilter(
                    candidates, excludes, flags=wcglob.GLOBSTAR | wcglob.DOTGLOB
                ),
            )
        )
        return FilteredTargets(frozenset(candidates - removed), removed)

    @staticmethod
    def filter_by_size(
        max_target_bytes: int, *, candidates: FrozenSet[Path]
    ) -> FilteredTargets:
        """
        Return all the files whose size doesn't exceed the limit.

        If max_target_bytes is zero or negative, all paths are returned.
        If some paths are invalid, they may or may not be included in the
        result.
        """
        if max_target_bytes <= 0:
            return FilteredTargets(candidates)

        kept, removed = partition_set(
            lambda path: TargetManager._is_valid_file(path)
            and os.path.getsize(path) <= max_target_bytes,
            candidates,
        )

        return FilteredTargets(kept, removed)

    def filtered_files(self, lang: Language) -> FilteredTargets:
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
        discovered_paths = self.resolve_targets(self.targets)
        files, directories = partition_set(lambda p: not p.is_dir(), discovered_paths)
        explicit_files, _ = partition_set(lambda p: p.is_file(), files)

        discovered_paths = self.expand_targets(
            directories, lang, self.respect_git_ignore
        )

        filtered_targets = self.filter_includes(
            self.includes,
            candidates=discovered_paths,
        )
        self.ignore_log.cli_includes.update(filtered_targets.removed)

        filtered_targets = self.filter_excludes(
            self.excludes,
            candidates=filtered_targets.kept,
        )
        self.ignore_log.cli_excludes.update(filtered_targets.removed)

        filtered_targets = self.filter_excludes(
            PATHS_ALWAYS_SKIPPED,
            candidates=filtered_targets.kept,
        )
        self.ignore_log.always_skipped.update(filtered_targets.removed)

        filtered_targets = self.filter_by_size(
            self.max_target_bytes,
            candidates=filtered_targets.kept,
        )
        self.ignore_log.size_limit.update(filtered_targets.removed)

        # Remove explicit_files with known extensions.
        explicit_files_with_lang_extension = frozenset(
            f
            for f in explicit_files
            if (any(f.match(f"*{ext}") for ext in LANGUAGE.definition_by_id[lang].exts))
        )
        targets = filtered_targets.kept.union(explicit_files_with_lang_extension)

        if not self.skip_unknown_extensions:
            explicit_files_with_unknown_extensions = frozenset(
                f
                for f in explicit_files
                if not any(f.match(f"*{ext}") for ext in ALL_EXTENSIONS)
            )
            targets = targets.union(explicit_files_with_unknown_extensions)

        filtered_targets = (
            self.file_ignore.filter_paths(candidates=targets)
            if self.file_ignore
            else FilteredTargets(targets)
        )
        self.ignore_log.semgrepignored.update(filtered_targets.removed)

        self._filtered_targets[lang] = FilteredTargets(
            filtered_targets.kept,
            discovered_paths - filtered_targets.kept,
        )
        return self._filtered_targets[lang]

    def get_files(
        self,
        lang: Language,
        includes: Sequence[str],
        excludes: Sequence[str],
        rule_id: str,
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
        filtered_targets = self.filtered_files(lang)

        filtered_targets = self.filter_includes(
            includes, candidates=filtered_targets.kept
        )
        self.ignore_log.rule_includes[rule_id].update(filtered_targets.removed)

        filtered_targets = self.filter_excludes(
            excludes, candidates=filtered_targets.kept
        )
        self.ignore_log.rule_excludes[rule_id].update(filtered_targets.removed)

        filtered_targets = self.filter_by_size(
            self.max_target_bytes, candidates=filtered_targets.kept
        )
        self.ignore_log.size_limit.update(filtered_targets.removed)

        return filtered_targets.kept
