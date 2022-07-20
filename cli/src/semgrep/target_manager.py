import os
import stat
import subprocess
import sys
from collections import defaultdict
from functools import lru_cache
from functools import partial
from pathlib import Path
from typing import Any
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

from semgrep.git import BaselineHandler

# usually this would be a try...except ImportError
# but mypy understands only this
# see https://github.com/python/mypy/issues/1393
if sys.version_info[:2] >= (3, 8):
    # Literal is available in stdlib since Python 3.8
    from typing import Literal
else:
    from typing_extensions import Literal

from attrs import define
from attrs import field
import click
from attrs import Factory, frozen
from wcmatch import glob as wcglob
from boltons.iterutils import partition

from semgrep.constants import Colors
from semgrep.error import FilesNotFoundError
from semgrep.formatter.text import width
from semgrep.ignores import FileIgnore
from semgrep.semgrep_types import FileExtension
from semgrep.semgrep_types import LANGUAGE
from semgrep.semgrep_types import Language
from semgrep.semgrep_types import Shebang
from semgrep.types import FilteredFiles
from semgrep.util import path_has_permissions, sub_check_output
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


def write_pipes_to_disk(targets: Sequence[str], temp_dir: Path) -> Sequence[str]:
    """
    Writes FIFOs into temp files

    This is necessary as we can not easily rewire these pipes into the called semgrep-core
    process.

    :param targets: Input target specifiers
    """

    out_targets = []
    for t in targets:
        if t == "-":
            with (temp_dir / "stdin").open("wb") as fd:
                fd.write(sys.stdin.buffer.read())
            out_targets.append(fd.name)
        elif Path(t).is_fifo():
            with (temp_dir / t[1:].replace("/", "_")).open("wb") as fd:
                with Path(t).open("rb") as td:
                    fd.write(td.read())
            out_targets.append(fd.name)
        else:
            out_targets.append(t)
    return out_targets


@define
class FileTargetingLog:
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

    # "None" indicates that all lines were skipped
    core_failure_lines_by_file: Mapping[Path, Optional[int]] = Factory(dict)

    by_language: Dict[Language, Set[Path]] = Factory(lambda: defaultdict(set))
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
        limited_fragments = []
        skip_fragments = []
        partial_fragments = []

        if self.target_manager.baseline_handler:
            limited_fragments.append(
                "Scan was limited to files changed since baseline commit."
            )
        elif self.target_manager.respect_git_ignore:
            # Each target could be a git repo, and we respect the git ignore
            # of each target, so to be accurate with this print statement we
            # need to check if any target is a git repo and not just the cwd
            targets_not_in_git = 0
            dir_targets = 0
            for t in self.target_manager.targets:
                if t.path.is_dir():
                    dir_targets += 1
                    try:
                        t.files_from_git_ls()
                    except (subprocess.SubprocessError, FileNotFoundError):
                        targets_not_in_git += 1
                        continue
            if targets_not_in_git != dir_targets:
                limited_fragments.append(f"Scan was limited to files tracked by git.")

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
        if self.core_failure_lines_by_file:
            partial_fragments.append(
                f"{len(self.core_failure_lines_by_file)} files only partially analyzed due to a parsing or internal Semgrep error"
            )

        if not limited_fragments and not skip_fragments and not partial_fragments:
            return ""

        message = "Some files were skipped or only partially analyzed."
        if limited_fragments:
            for fragment in limited_fragments:
                message += f"\n  {fragment}"
        if partial_fragments:
            message += "\n  Partially scanned: " + ", ".join(partial_fragments)
        if skip_fragments:
            message += "\n  Scan skipped: " + ", ".join(skip_fragments)
            message += "\n  For a full list of skipped files, run semgrep with the --verbose flag."
        message += "\n"
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

        yield 1, "Skipped by analysis failure due to parsing or internal Semgrep error"
        if self.core_failure_lines_by_file:
            for path, lines in self.core_failure_lines_by_file.items():
                if lines is None:
                    skipped = "all"
                else:
                    # TODO: use pluralization library
                    skipped = str(lines)

                yield 2, with_color(Colors.cyan, f"{path} ({skipped} lines skipped)")
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

    def yield_json_objects(self) -> Iterable[Dict[str, Any]]:
        for path in self.always_skipped:
            yield {"path": str(path), "reason": "always_skipped"}
        for path in self.semgrepignored:
            yield {"path": str(path), "reason": "semgrepignore_patterns_match"}
        for path in self.cli_includes:
            yield {"path": str(path), "reason": "cli_include_flags_do_not_match"}
        for path in self.cli_excludes:
            yield {"path": str(path), "reason": "cli_exclude_flags_match"}
        for path in self.size_limit:
            yield {
                "path": str(path),
                "reason": "exceeded_size_limit",
                "size_limit_bytes": self.target_manager.max_target_bytes,
            }
        for path in self.core_failure_lines_by_file:
            yield {
                "path": str(path),
                "reason": "analysis_failed_parser_or_internal_error",
            }


@frozen(eq=False)  #
class Target:
    """
    Represents one path that was given as a target.
    Then target.paths returns all paths that target expands to.
    This does not do any include/exclude filtering.

    Three strategies are available for gathering targets:
    1. recursively collect from file system (slowest, but always works)
    2. read the output of `git ls-files` (respects .gitignore)
    3. [TODO] read the output of `git diff` (respects --baseline-commit)
    """

    path: Path = field(converter=Path)
    git_tracked_only: bool = False
    baseline_handler: Optional[BaselineHandler] = None

    @path.validator
    def validate_path(self, _: Any, value: Path) -> None:
        """
        Check whether the targeted path exists.

        If not, the path might be a socket.
        """
        if not self._is_valid_file_or_dir(value):
            raise FilesNotFoundError(paths=tuple([value]))
        return None

    def _is_valid_file_or_dir(self, path: Path) -> bool:
        """Check this is a valid file or directory for semgrep scanning."""
        return path_has_permissions(path, stat.S_IRUSR) and not path.is_symlink()

    def _is_valid_file(self, path: Path) -> bool:
        """Check if file is a readable regular file.

        This eliminates files that should never be semgrep targets. Among
        others, this takes care of excluding symbolic links (because we don't
        want to scan the target twice), directories (which may be returned by
        globbing or by 'git ls-files' e.g. submodules), and files missing
        the read permission.
        """
        return self._is_valid_file_or_dir(path) and path.is_file()

    def _parse_git_output(self, output: str) -> FrozenSet[Path]:
        """
        Convert a newline delimited list of files to a set of path objects
        prepends curr_dir to all paths in said list

        If list is empty then returns an empty set
        """
        files: FrozenSet[Path] = frozenset()
        if output:
            files = frozenset(
                p
                for p in (self.path / elem for elem in output.strip().split("\n"))
                if self._is_valid_file(p)
            )
        return files

    def files_from_git_diff(self) -> FrozenSet[Path]:
        """
        Get only changed files since baseline commit.
        """
        if self.baseline_handler is None:
            raise RuntimeError("Can't get git diff file list without a baseline commit")
        git_status = self.baseline_handler.status
        return frozenset(git_status.added + git_status.modified)

    def files_from_git_ls(self) -> FrozenSet[Path]:
        """
        git ls-files is significantly faster than os.walk when performed on a git project,
        so identify the git files first, then filter those
        """
        run_git_command = partial(
            sub_check_output,
            cwd=self.path.resolve(),
            encoding="utf-8",
            stderr=subprocess.DEVNULL,
        )

        # Tracked files
        tracked_output = run_git_command(["git", "ls-files"])

        # Untracked but not ignored files
        untracked_output = run_git_command(
            [
                "git",
                "ls-files",
                "--other",
                "--exclude-standard",
            ]
        )

        deleted_output = run_git_command(["git", "ls-files", "--deleted"])
        tracked = self._parse_git_output(tracked_output)
        untracked_unignored = self._parse_git_output(untracked_output)
        deleted = self._parse_git_output(deleted_output)
        return frozenset(tracked | untracked_unignored - deleted)

    def files_from_filesystem(self) -> FrozenSet[Path]:
        return frozenset(
            match
            for match in self.path.glob("**/*")
            if match.is_file() and not match.is_symlink()
        )

    @lru_cache(maxsize=None)
    def files(self) -> FrozenSet[Path]:
        """
        Recursively go through a directory and return list of all files with
        default file extension of language
        """
        if not self.path.is_dir() and self.path.is_file():
            return frozenset([self.path])

        if self.baseline_handler is not None:
            try:
                return self.files_from_git_diff()
            except (subprocess.CalledProcessError, FileNotFoundError):
                logger.verbose(
                    f"Unable to target only the changed files since baseline commit. Running on all git tracked files instead..."
                )

        if self.git_tracked_only:
            try:
                return self.files_from_git_ls()
            except (subprocess.CalledProcessError, FileNotFoundError):
                logger.verbose(
                    f"Unable to ignore files ignored by git ({self.path} is not a git directory or git is not installed). Running on all files instead..."
                )

        return self.files_from_filesystem()


@define(eq=False)
class TargetManager:
    """
    Handles all file include/exclude logic for semgrep

    Assumes file system does not change during it's existence to cache
    files for a given language etc. If file system changes (i.e. git checkout),
    create a new TargetManager object

    If respect_git_ignore is true then will only consider files that are
    tracked or (untracked but not ignored) by git

    If git_baseline_commit is true then will only consider files that have
    changed since that commit

    If allow_unknown_extensions is set then targets with extensions that are
    not understood by semgrep will always be returned by get_files. Else will discard
    targets with unknown extensions

    TargetManager not to be confused with https://jobs.target.com/search-jobs/store%20manager
    """

    target_strings: Sequence[str]
    includes: Sequence[str] = Factory(list)
    excludes: Sequence[str] = Factory(list)
    max_target_bytes: int = -1
    respect_git_ignore: bool = False
    baseline_handler: Optional[BaselineHandler] = None
    allow_unknown_extensions: bool = False
    file_ignore: Optional[FileIgnore] = None
    ignore_log: FileTargetingLog = Factory(FileTargetingLog, takes_self=True)
    targets: Sequence[Target] = field(init=False)

    _filtered_targets: Dict[Language, FilteredFiles] = field(factory=dict)

    def __attrs_post_init__(self) -> None:
        self.targets = [
            Target(
                target,
                git_tracked_only=self.respect_git_ignore,
                baseline_handler=self.baseline_handler,
            )
            for target in self.target_strings
        ]
        return None

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

    def executes_with_shebang(self, path: Path, shebangs: Collection[Shebang]) -> bool:
        """
        Returns if a path is executable and executes with one of a set of programs
        """
        if not path.is_file():
            return False
        try:
            hline = self.get_shebang_line(path)
            if hline is None:
                return False
            return any(hline.endswith(s) for s in shebangs)
        except UnicodeDecodeError:
            logger.debug(
                f"Encountered likely binary file {path} while reading shebang; skipping this file"
            )
            return False

    @lru_cache(maxsize=100_000)  # size aims to be 100x of fully caching this repo
    def get_shebang_line(self, path: Path) -> Optional[str]:
        if not path_has_permissions(path, stat.S_IRUSR | stat.S_IXUSR):
            return None

        with path.open() as f:
            return f.readline(MAX_CHARS_TO_READ_FOR_SHEBANG).rstrip()

    @lru_cache(maxsize=10_000)  # size aims to be 100x of fully caching this repo
    def globfilter(self, candidates: Iterable[Path], pattern: str) -> List[Path]:
        result = wcglob.globfilter(
            candidates, pattern, flags=wcglob.GLOBSTAR | wcglob.DOTGLOB
        )
        return cast(List[Path], result)

    def filter_by_language(
        self, language: Language, *, candidates: FrozenSet[Path]
    ) -> FilteredFiles:
        """
        Returns only paths that have the correct extension or shebang.

        Finds all files in a collection of paths that either:
        - end with one of a set of extension
        - is a script that executes with one of a set of programs
        """
        kept = frozenset(
            path
            for path in candidates
            if any(str(path).endswith(ext) for ext in language.definition.exts)
            or self.executes_with_shebang(path, language.definition.shebangs)
        )
        return FilteredFiles(kept, frozenset(candidates - kept))

    def filter_known_extensions(self, *, candidates: FrozenSet[Path]) -> FilteredFiles:
        """
        Returns only paths that have an extension we don't recognize.
        """
        kept = frozenset(
            path
            for path in candidates
            if not any(path.match(f"*{ext}") for ext in ALL_EXTENSIONS)
        )
        return FilteredFiles(kept, frozenset(candidates - kept))

    def filter_includes(
        self, includes: Sequence[str], *, candidates: FrozenSet[Path]
    ) -> FilteredFiles:
        """
        Returns all elements in candidates that match any includes pattern

        If includes is empty, returns candidates unchanged
        """
        if not includes:
            return FilteredFiles(candidates)

        kept = set()
        for pattern in TargetManager.preprocess_path_patterns(includes):
            kept.update(self.globfilter(candidates, pattern))
        return FilteredFiles(frozenset(kept), frozenset(candidates - kept))

    def filter_excludes(
        self, excludes: Sequence[str], *, candidates: FrozenSet[Path]
    ) -> FilteredFiles:
        """
        Returns all elements in candidates that do not match any excludes pattern

        If excludes is empty, returns candidates unchanged
        """
        if not excludes:
            return FilteredFiles(candidates)

        removed = set()
        for pattern in TargetManager.preprocess_path_patterns(excludes):
            removed.update(self.globfilter(candidates, pattern))

        return FilteredFiles(frozenset(candidates - removed), frozenset(removed))

    @staticmethod
    def filter_by_size(
        max_target_bytes: int, *, candidates: FrozenSet[Path]
    ) -> FilteredFiles:
        """
        Return all the files whose size doesn't exceed the limit.

        If max_target_bytes is zero or negative, all paths are returned.
        If some paths are invalid, they may or may not be included in the
        result.
        """
        if max_target_bytes <= 0:
            return FilteredFiles(candidates)

        kept, removed = partition(
            candidates, lambda path: os.path.getsize(path) <= max_target_bytes
        )

        return FilteredFiles(frozenset(kept), frozenset(removed))

    @lru_cache(maxsize=None)
    def get_files_for_language(self, lang: Language) -> FilteredFiles:
        """
        Return all files that are decendants of any directory in TARGET that have
        an extension matching LANG that match any pattern in INCLUDES and do not
        match any pattern in EXCLUDES. Any file in TARGET bypasses excludes and includes.
        If a file in TARGET has a known extension that is not for langugage LANG then
        it is also filtered out

        Note also filters out any directory and descendants of `.git`
        """
        all_files = frozenset(f for target in self.targets for f in target.files())

        files = self.filter_by_language(lang, candidates=all_files)
        self.ignore_log.by_language[lang].update(files.removed)

        files = self.filter_includes(self.includes, candidates=files.kept)
        self.ignore_log.cli_includes.update(files.removed)

        files = self.filter_excludes(self.excludes, candidates=files.kept)
        self.ignore_log.cli_excludes.update(files.removed)

        files = self.filter_excludes(PATHS_ALWAYS_SKIPPED, candidates=files.kept)
        self.ignore_log.always_skipped.update(files.removed)

        files = self.filter_by_size(self.max_target_bytes, candidates=files.kept)
        self.ignore_log.size_limit.update(files.removed)

        if self.file_ignore:
            files = self.file_ignore.filter_paths(candidates=files.kept)
            self.ignore_log.semgrepignored.update(files.removed)

        kept_files = files.kept

        explicit_files = frozenset(
            t.path for t in self.targets if not t.path.is_dir() and t.path.is_file()
        )
        explicit_files_for_lang = self.filter_by_language(
            lang, candidates=explicit_files
        )
        kept_files |= explicit_files_for_lang.kept
        if self.allow_unknown_extensions:
            explicit_files_of_unknown_lang = self.filter_known_extensions(
                candidates=explicit_files
            )
            kept_files |= explicit_files_of_unknown_lang.kept

        return FilteredFiles(kept_files, all_files - kept_files)

    def get_files_for_rule(
        self,
        lang: Language,
        rule_includes: Sequence[str],
        rule_excludes: Sequence[str],
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
        paths = self.get_files_for_language(lang)

        paths = self.filter_includes(rule_includes, candidates=paths.kept)
        self.ignore_log.rule_includes[rule_id].update(paths.removed)

        paths = self.filter_excludes(rule_excludes, candidates=paths.kept)
        self.ignore_log.rule_excludes[rule_id].update(paths.removed)

        return paths.kept
