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
from typing import NewType
from typing import Set

import attr

from semgrep.error import _UnknownLanguageError
from semgrep.error import FilesNotFoundError
from semgrep.output import OutputHandler
from semgrep.semgrep_types import GENERIC_LANGUAGE
from semgrep.semgrep_types import Language
from semgrep.semgrep_types import NONE_LANGUAGE
from semgrep.util import partition_set
from semgrep.util import sub_check_output

FileExtension = NewType("FileExtension", str)

PYTHON_EXTENSIONS = [FileExtension(".py"), FileExtension(".pyi")]
JAVASCRIPT_EXTENSIONS = [FileExtension(".js"), FileExtension(".jsx")]
TYPESCRIPT_EXTENSIONS = [FileExtension(".ts"), FileExtension(".tsx")]
JAVA_EXTENSIONS = [FileExtension(".java")]
C_EXTENSIONS = [FileExtension(".c")]
GO_EXTENSIONS = [FileExtension(".go")]
RUBY_EXTENSIONS = [FileExtension(".rb")]
PHP_EXTENSIONS = [FileExtension(".php")]
ML_EXTENSIONS = [
    FileExtension(".mli"),
    FileExtension(".ml"),
]
JSON_EXTENSIONS = [FileExtension(".json")]

# This is used to determine the set of files with known extensions,
# i.e. those for which we have a proper parser.
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

# This is used to select the files suitable for spacegrep, which is
# all of them. It is spacegrep itself that will detect and ignore binary
# files.
GENERIC_EXTENSIONS = [FileExtension("")]

def lang_to_exts(language: Language) -> List[FileExtension]:
    """
        Convert language to expected file extensions

        If language is not a supported semgrep language then
        raises _UnknownLanguageError
    """
    if language in {"python", "python2", "python3", "py"}:
        return PYTHON_EXTENSIONS
    elif language in {"js", "jsx", "javascript"}:
        return JAVASCRIPT_EXTENSIONS
    elif language in {"ts", "tsx", "typescript"}:
        return TYPESCRIPT_EXTENSIONS
    elif language in {"java"}:
        return JAVA_EXTENSIONS
    elif language in {"c"}:
        return C_EXTENSIONS
    elif language in {"go", "golang"}:
        return GO_EXTENSIONS
    elif language in {"ml", "ocaml"}:
        return ML_EXTENSIONS
    elif language in {"rb", "ruby"}:
        return RUBY_EXTENSIONS
    elif language in {"php"}:
        return PHP_EXTENSIONS
    elif language in {"json", "JSON", "Json"}:
        return JSON_EXTENSIONS
    elif language in {NONE_LANGUAGE, GENERIC_LANGUAGE}:
        return GENERIC_EXTENSIONS
    else:
        raise _UnknownLanguageError(f"Unsupported Language: {language}")


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
    targets: List[str]
    respect_git_ignore: bool
    output_handler: OutputHandler
    skip_unknown_extensions: bool

    _filtered_targets: Dict[str, Set[Path]] = attr.ib(factory=dict)

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

    @staticmethod
    def _expand_dir(
        curr_dir: Path, language: Language, respect_git_ignore: bool
    ) -> Set[Path]:
        """
            Recursively go through a directory and return list of all files with
            default file extention of language
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

        def _find_files_with_extention(
            curr_dir: Path, extension: FileExtension
        ) -> Set[Path]:
            """
                Return set of all files in curr_dir with given extension
            """
            return set(p for p in curr_dir.rglob(f"*{extension}") if p.is_file())

        extensions = lang_to_exts(language)
        expanded: Set[Path] = set()

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
                    # Not a git directory or git not installed. Fallback to using rglob
                    ext_files = _find_files_with_extention(curr_dir, ext)
                    expanded = expanded.union(ext_files)
                else:
                    tracked = _parse_output(tracked_output, curr_dir)
                    untracked_unignored = _parse_output(untracked_output, curr_dir)
                    deleted = _parse_output(deleted_output, curr_dir)
                    expanded = expanded.union(tracked)
                    expanded = expanded.union(untracked_unignored)
                    expanded = expanded.difference(deleted)

            else:
                ext_files = _find_files_with_extention(curr_dir, ext)
                expanded = expanded.union(ext_files)

        return expanded

    @staticmethod
    def expand_targets(
        targets: Collection[Path], lang: Language, respect_git_ignore: bool
    ) -> Set[Path]:
        """
            Explore all directories. Remove duplicates
        """
        expanded = set()
        for target in targets:
            if not target.exists():
                continue

            if target.is_dir():
                expanded.update(
                    TargetManager._expand_dir(target, lang, respect_git_ignore)
                )
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

    def filtered_files(self, lang: Language) -> Set[Path]:
        """
            Return all files that are decendants of any directory in TARGET that have
            an extension matching LANG that match any pattern in INCLUDES and do not
            match any pattern in EXCLUDES. Any file in TARGET bypasses excludes and includes.
            If a file in TARGET has a known extension that is not for langugage LANG then
            it is also filtered out
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
        targets = self.filter_excludes(targets, self.excludes)

        # Remove explicit_files with known extensions.
        explicit_files_with_lang_extension = set(
            f
            for f in explicit_files
            if (any(f.match(f"*{ext}") for ext in lang_to_exts(lang)))
        )
        targets = targets.union(explicit_files_with_lang_extension)

        if not self.skip_unknown_extensions:
            explicit_files_with_unknown_extensions = set(
                f
                for f in explicit_files
                if not any(f.match(f"*{ext}") for ext in ALL_EXTENSIONS)
            )
            targets = targets.union(explicit_files_with_unknown_extensions)

        self._filtered_targets[lang] = targets
        return self._filtered_targets[lang]

    def get_files(
        self, lang: Language, includes: List[str], excludes: List[str]
    ) -> List[Path]:
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
        return list(targets)
