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
from typing import NewType
from typing import Sequence
from typing import Set

import attr

from semgrep.error import _UnknownLanguageError
from semgrep.error import FilesNotFoundError
from semgrep.output import OutputHandler
from semgrep.semgrep_types import Language
from semgrep.util import partition_set
from semgrep.util import sub_check_output

FileExtension = NewType("FileExtension", str)


PYTHON_EXTENSIONS = [FileExtension("py"), FileExtension("pyi")]
JAVASCRIPT_EXTENSIONS = [FileExtension("js"), FileExtension("jsx")]
TYPESCRIPT_EXTENSIONS = [FileExtension("ts"), FileExtension("tsx")]
JAVA_EXTENSIONS = [FileExtension("java")]
C_EXTENSIONS = [FileExtension("c")]
GO_EXTENSIONS = [FileExtension("go")]
RUBY_EXTENSIONS = [FileExtension("rb")]
PHP_EXTENSIONS = [FileExtension("php")]
ML_EXTENSIONS = [
    FileExtension("mli"),
    FileExtension("ml"),
]
JSON_EXTENSIONS = [FileExtension("json")]
ALL_EXTENSIONS = (
    PYTHON_EXTENSIONS
    + JAVASCRIPT_EXTENSIONS
    + TYPESCRIPT_EXTENSIONS
    + JAVA_EXTENSIONS
    + C_EXTENSIONS
    + GO_EXTENSIONS
    + RUBY_EXTENSIONS
    + ML_EXTENSIONS
    + JSON_EXTENSIONS
)


def lang_to_exts(language: Language) -> List[FileExtension]:
    """
    Convert language to expected file extensions

    If language is not a supported semgrep language then
    raises _UnknownLanguageError
    """
    lang = language.value
    if lang in {"python", "python2", "python3", "py"}:
        return PYTHON_EXTENSIONS
    elif lang in {"js", "jsx", "javascript"}:
        return JAVASCRIPT_EXTENSIONS
    elif lang in {"ts", "tsx", "typescript"}:
        return TYPESCRIPT_EXTENSIONS
    elif lang in {"java"}:
        return JAVA_EXTENSIONS
    elif lang in {"c"}:
        return C_EXTENSIONS
    elif lang in {"go", "golang"}:
        return GO_EXTENSIONS
    elif lang in {"ml", "ocaml"}:
        return ML_EXTENSIONS
    elif lang in {"rb", "ruby"}:
        return RUBY_EXTENSIONS
    elif lang in {"php"}:
        return PHP_EXTENSIONS
    elif lang in {"json", "JSON", "Json"}:
        return JSON_EXTENSIONS
    elif lang in {"none", "generic"}:
        return [FileExtension("*")]
    else:
        raise _UnknownLanguageError(f"Unsupported Language: {lang}")


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
        """
        Return list of Path objects appropriately resolving relative paths
        (relative to cwd) if necessary
        """
        base_path = Path(".")
        return set(
            Path(target) if Path(target).is_absolute() else base_path.joinpath(target)
            for target in targets
        )

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
                    Path(curr_dir) / elem for elem in output.strip().split("\n")
                )
            return files

        def _find_files_with_extension(
            curr_dir: Path, extension: FileExtension
        ) -> Set[Path]:
            """
            Return set of all files in curr_dir with given extension
            """
            return set(p for p in curr_dir.rglob(f"*.{extension}") if p.is_file())

        extensions = lang_to_exts(language)
        expanded: Set[Path] = set()

        for ext in extensions:
            if respect_git_ignore:
                try:
                    # Tracked files
                    tracked_output = sub_check_output(
                        ["git", "ls-files", f"*.{ext}"],
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
                            f"*.{ext}",
                        ],
                        cwd=curr_dir.resolve(),
                        encoding="utf-8",
                        stderr=subprocess.DEVNULL,
                    )

                    deleted_output = sub_check_output(
                        ["git", "ls-files", "--deleted", f"*.{ext}"],
                        cwd=curr_dir.resolve(),
                        encoding="utf-8",
                        stderr=subprocess.DEVNULL,
                    )
                except (subprocess.CalledProcessError, FileNotFoundError):
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

        return expanded

    @staticmethod
    def expand_targets(
        targets: Collection[Path], lang: Language, respect_git_ignore: bool
    ) -> Set[Path]:
        """Explore all directories."""
        expanded: Set[Path] = set()
        for target in targets:
            if not target.exists():
                continue

            if target.is_dir():
                expanded = expanded.union(
                    TargetManager._expand_dir(target, lang, respect_git_ignore)
                )
            else:
                expanded.add(target)

        return expanded

    @staticmethod
    def match_glob(path: Path, globs: Sequence[str]) -> bool:
        """
        Return true if path or any parent of path matches any glob in globs
        """
        subpaths = [path, *path.parents]
        return any(p.match(glob) for p in subpaths for glob in globs)

    @staticmethod
    def filter_includes(arr: Set[Path], includes: Sequence[str]) -> Set[Path]:
        """
        Returns all elements in arr that match any includes pattern

        If includes is empty, returns arr unchanged
        """
        if not includes:
            return arr

        return set(elem for elem in arr if TargetManager.match_glob(elem, includes))

    @staticmethod
    def filter_excludes(arr: Set[Path], excludes: Sequence[str]) -> Set[Path]:
        """
        Returns all elements in arr that do not match any excludes excludes
        """
        return set(elem for elem in arr if not TargetManager.match_glob(elem, excludes))

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

        targets = self.resolve_targets(self.targets)

        files, directories = partition_set(lambda p: not p.is_dir(), targets)

        # Error on non-existent files
        explicit_files, nonexistent_files = partition_set(lambda p: p.is_file(), files)
        if nonexistent_files:
            self.output_handler.handle_semgrep_error(
                FilesNotFoundError(tuple(nonexistent_files))
            )

        # Scan file system and filter for language lang.
        targets = self.expand_targets(directories, lang, self.respect_git_ignore)

        # Filter based on custom glob patterns.
        targets = self.filter_includes(targets, self.includes)
        targets = self.filter_excludes(targets, self.excludes)

        # Avoid duplicates (e.g. foo/bar can be both an explicit file and
        # discovered in folder foo/)
        targets = targets.difference(explicit_files)

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
        explicit_files_without_standard_extension = set(
            f
            for f in explicit_files
            if not any(f.match(f"*{ext}") for ext in ALL_EXTENSIONS)
        )

        explicit_files_with_expected_extension = set(
            f
            for f in explicit_files
            if any(f.match(f"*{ext}") for ext in lang_to_exts(lang))
        )

        # Optionally ignore explicit files with incorrect extensions for the
        # language (CLI option --skip-unknown-extensions).
        if self.skip_unknown_extensions:
            explicit_files = explicit_files_with_expected_extension
        else:  # default
            explicit_files = explicit_files_with_expected_extension.union(
                explicit_files_without_standard_extension
            )

        self._filtered_targets[lang] = TargetFiles(
            explicit=explicit_files, filterable=targets
        )
        return self._filtered_targets[lang]

    def get_files(
        self, lang: Language, extra_includes: List[str], extra_excludes: List[str]
    ) -> TargetFiles:
        """Return target files with extra glob patterns to include or exclude.

        This is meant for adding or removing target files from the default
        set on a rule-specific basis.
        """
        explicit_targets, filterable_targets = self.filtered_files(lang)
        filterable_targets = self.filter_includes(filterable_targets, extra_includes)
        filterable_targets = self.filter_excludes(filterable_targets, extra_excludes)
        return TargetFiles(explicit=explicit_targets, filterable=filterable_targets)
