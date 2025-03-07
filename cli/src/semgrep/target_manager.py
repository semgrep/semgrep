import copy
import os
import stat
import subprocess
import sys
from collections import defaultdict
from dataclasses import dataclass
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
from typing import Union

import semgrep.rpc_call
import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.subproject_matchers import filter_dependency_source_files
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

from semgrep.constants import TOO_MUCH_DATA
from semgrep.constants import Colors, UNSUPPORTED_EXT_IGNORE_LANGS
from semgrep.error import InvalidScanningRootError, SemgrepCoreError
from semgrep.formatter.text import BASE_WIDTH as width
from semgrep.ignores import Semgrepignore
from semgrep.semgrep_types import FileExtension
from semgrep.semgrep_types import LANGUAGE
from semgrep.semgrep_types import Language
from semgrep.semgrep_types import Shebang
from semgrep.types import FilteredFiles
from semgrep.util import (
    line_count_of_path,
    path_has_permissions,
    pretty_print_percentage,
    sub_check_output,
    unit_str,
)
from semgrep.util import with_color
from semgrep.verbose_logging import getLogger


logger = getLogger(__name__)

MAX_CHARS_TO_READ_FOR_SHEBANG = 255
PATHS_ALWAYS_SKIPPED = (".git",)

SCA_PRODUCT = out.Product(out.SCA())
SAST_PRODUCT = out.Product(out.SAST())
SECRETS_PRODUCT = out.Product(out.Secrets())
ALL_PRODUCTS = (SAST_PRODUCT, SCA_PRODUCT, SECRETS_PRODUCT)

ALL_EXTENSIONS: Collection[FileExtension] = {
    ext
    for definition in LANGUAGE.definition_by_id.values()
    for ext in definition.exts
    if ext != FileExtension("")
}


def write_pipes_to_disk(scanning_roots: Sequence[str], temp_dir: Path) -> Sequence[str]:
    """
    Writes FIFOs into temp files

    This is necessary as we can not easily rewire these pipes into the called semgrep-core
    process.

    :param scanning_roots: Input scanning root specifiers (files, folders, '-', ...)
    """

    out_scanning_roots = []
    for t in scanning_roots:
        path = Path(t)
        if t == "-":
            with (temp_dir / "stdin").open("wb") as fd:
                fd.write(sys.stdin.buffer.read())
            out_scanning_roots.append(fd.name)
        else:
            if os.access(path, os.R_OK) and path.is_fifo():
                with (temp_dir / t[1:].replace("/", "_")).open("wb") as fd:
                    with Path(t).open("rb") as td:
                        fd.write(td.read())
                out_scanning_roots.append(fd.name)
            else:
                # We keep the scanning root even if we already
                # know it doesn't exist. This will be reported cleanly
                # later.
                out_scanning_roots.append(t)
    return out_scanning_roots


@define
class FileErrorLog:
    rule_errors: List[SemgrepCoreError] = list()
    line_errors: Optional[List[SemgrepCoreError]] = None

    def __init__(
        self,
        rule_errors: Optional[List[SemgrepCoreError]] = None,
        line_errors: Optional[List[SemgrepCoreError]] = None,
    ):
        self.rule_errors = rule_errors if rule_errors else []
        self.line_errors = line_errors

    def add_error(self, err: SemgrepCoreError) -> None:
        if self.line_errors and err.spans:
            self.line_errors.append(err)
        elif err.spans:
            self.line_errors = [err]
        if err.core.rule_id is not None:
            self.rule_errors.append(err)
        # else ??? is this case possible (path specific error but no line or
        # rule id associated? @austin refactored this code but the original
        # did not handle this case

    def num_lines_skipped(self) -> Optional[int]:
        error_spans = [e.spans for e in self.line_errors] if self.line_errors else None
        return (
            sum(
                [
                    s.end.line - s.start.line + 1
                    for spans in error_spans
                    if spans
                    for s in spans
                ]
            )
            if error_spans
            else None
        )

    def num_rules_skipped(self) -> int:
        rule_ids = list(set([e.rule_id.value for e in self.rule_errors if e.rule_id]))
        return len(rule_ids)


@define
class FileTargetingLog:
    """Keeps track of which paths were ignored for what reason.

    Each attribute is a distinct reason why files could be ignored.

    Some reason can apply once per rule; these are mappings keyed on the rule id.
    """

    target_manager: "TargetManager"

    always_skipped: Set[Path] = Factory(set)
    cli_includes: Set[Path] = Factory(set)
    cli_excludes: Set[Path] = Factory(set)
    insufficient_permissions: Set[Path] = Factory(set)
    size_limit: Set[Path] = Factory(set)
    semgrepignored: Set[Path] = Factory(set)

    # "None" indicates that all lines were skipped
    core_failure_lines_by_file: Mapping[Path, FileErrorLog] = Factory(dict)

    # Indicates which files were NOT scanned by each language
    # e.g. for python, should be a list of all non-python-compatible files
    by_language: Dict[
        Union[Language, Literal["dependency_source_files"]], Set[Path]
    ] = Factory(lambda: defaultdict(set))
    rule_includes: Dict[str, Set[Path]] = Factory(lambda: defaultdict(set))
    rule_excludes: Dict[str, Set[Path]] = Factory(lambda: defaultdict(set))

    def unsupported_lang_paths(self, *, product: out.Product) -> FrozenSet[Path]:
        """
        RETURNS: paths of all files that were ignored by ALL non-generic langs

        Note: if only generic languages were scanned, returns all file paths
        """
        unsupported_lang_paths = (
            [
                unsupported_paths
                for lang, unsupported_paths in self.by_language.items()
                if lang not in UNSUPPORTED_EXT_IGNORE_LANGS
            ]
            if self.by_language
            else []
        )
        return (
            frozenset(set.intersection(*unsupported_lang_paths))
            if unsupported_lang_paths
            else self.target_manager.get_all_files(product=product)
        )

    def list_skipped_paths_with_reason(self) -> List[Tuple[Path, str]]:
        res: List[Tuple[Path, str]] = []
        # The strings used to describe the reason are those defined
        # in semgrep_output_v1.atd for type 'skip_reason':
        for x in self.always_skipped:
            res.append((x, "always_skipped"))
        for x in self.cli_includes:
            res.append((x, "cli_include_flags_do_not_match"))
        for x in self.cli_excludes:
            res.append((x, "cli_exclude_flags_match"))
        for x in self.insufficient_permissions:
            res.append((x, "insufficient_permissions"))
        for x in self.size_limit:
            res.append((x, "exceeded_size_limit"))
        for x in self.semgrepignored:
            res.append((x, "semgrepignored"))
        return sorted(res)

    def __str__(self) -> str:
        limited_fragments: List[str] = []
        skip_fragments: List[str] = []

        if self.target_manager.baseline_handler:
            limited_fragments.append(
                "Scan was limited to files changed since baseline commit."
            )
        elif self.target_manager.respect_git_ignore:
            # Each scanning root could be a git repo, and we respect the
            # gitignore exclusions in each repo, so to be accurate with
            # this print statement we need to check if any target is in a
            # git repo.
            roots_not_in_git = 0
            dir_targets = 0
            for t in self.target_manager.scanning_roots:
                if t.path.is_dir():
                    dir_targets += 1
                    try:
                        t.files_from_git_ls()
                    except (subprocess.SubprocessError, FileNotFoundError):
                        roots_not_in_git += 1
                        continue
            if roots_not_in_git != dir_targets:
                limited_fragments.append(f"Scan was limited to files tracked by git")

        if self.cli_includes:
            skip_fragments.append(
                f"Not matching --include patterns: {len(self.cli_includes)}"
            )
        if self.cli_excludes:
            skip_fragments.append(
                f"Matching --exclude patterns: {len(self.cli_excludes)}"
            )
        if self.insufficient_permissions:
            # Show a list of broken symlinks or files we can't open for reading.
            # This is a best effort. What we can show depends on the method
            # used to list the files.
            skip_fragments.append(
                f"Files without read access: {len(self.insufficient_permissions)}"
            )
        if self.size_limit:
            skip_fragments.append(
                f"Files larger than  files {self.target_manager.max_target_bytes / 1000 / 1000} MB: {len(self.size_limit)}"
            )

        if self.semgrepignored:
            skip_fragments.append(
                f"Files matching .semgrepignore patterns: {len(self.semgrepignored)}"
            )

        message = ""
        if not limited_fragments and not skip_fragments:
            return message

        if skip_fragments:
            skip_fragments.insert(0, "\n • Scan skipped: ")
            message += "\n   ◦ ".join(skip_fragments)
        if limited_fragments:
            for fragment in limited_fragments:
                message += f"\n • {fragment}"
        return message

    def yield_verbose_lines(
        self, too_many_entries: int
    ) -> Iterator[Tuple[Literal[0, 1, 2, 3, 4], str]]:
        """Yields lines of verbose output for the skipped files.

        The returned tuple is (level, message).
        The level is a number; one of 0, 1, 2, 3 or 4 which sets the indentation when outputting the line.
        """
        yield 0, "Files skipped:"

        yield 1, "Always skipped by Semgrep:"
        if self.always_skipped:
            for path in sorted(self.always_skipped):
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
        yield (
            1,
            "- https://semgrep.dev/docs/ignoring-files-folders-code/#understand-semgrep-defaults",
        )
        if self.semgrepignored:
            if too_many_entries > 0 and len(self.semgrepignored) > too_many_entries:
                yield 2, TOO_MUCH_DATA
            else:
                for path in sorted(self.semgrepignored):
                    yield 2, with_color(Colors.cyan, str(path))
        else:
            yield 2, "<none>"

        yield 1, "Skipped by --include patterns:"
        if self.cli_includes:
            for path in sorted(self.cli_includes):
                yield 2, with_color(Colors.cyan, str(path))
        else:
            yield 2, "<none>"

        yield 1, "Skipped by --exclude patterns:"
        if self.cli_excludes:
            if too_many_entries > 0 and len(self.cli_excludes) > too_many_entries:
                yield 2, TOO_MUCH_DATA
            else:
                for path in sorted(self.cli_excludes):
                    yield 2, with_color(Colors.cyan, str(path))
        else:
            yield 2, "<none>"

        yield (
            1,
            f"Files that couldn't be accessed:",
        )
        if self.insufficient_permissions:
            for path in sorted(self.insufficient_permissions):
                yield 2, with_color(Colors.cyan, str(path))
        else:
            yield 2, "<none>"

        yield (
            1,
            f"Skipped by limiting to files smaller than {self.target_manager.max_target_bytes} bytes:",
        )
        yield 1, "(Adjust with the --max-target-bytes flag)"
        if self.size_limit:
            for path in sorted(self.size_limit):
                yield 2, with_color(Colors.cyan, str(path))
        else:
            yield 2, "<none>"

        yield 1, "Partially analyzed due to parsing or internal Semgrep errors"
        if self.core_failure_lines_by_file:
            for path, file_error_log in sorted(self.core_failure_lines_by_file.items()):
                num_rule_ids = file_error_log.num_rules_skipped()
                num_lines_skipped = file_error_log.num_lines_skipped()
                total_lines = line_count_of_path(path)
                percent_lines_skipped = (
                    pretty_print_percentage(num_lines_skipped, total_lines)
                    if num_lines_skipped
                    else None
                )
                lines_skipped = (
                    f"{percent_lines_skipped} of lines always skipped"
                    if percent_lines_skipped
                    else ""
                )
                rules_skipped = (
                    f"{unit_str(num_rule_ids, 'rule')} failed to run"
                    if num_rule_ids
                    else ""
                )
                join = ", " if num_rule_ids and percent_lines_skipped else ""
                details = (
                    f" ({rules_skipped}{join}{lines_skipped})"
                    if rules_skipped or lines_skipped
                    else ""
                )
                yield 2, with_color(Colors.cyan, f"{path}{details}")

                if file_error_log.rule_errors:
                    yield 3, with_color(
                        Colors.white,
                        f"The following {unit_str(num_rule_ids, 'rule')} failed to run on this file:",
                    )
                    for err in file_error_log.rule_errors:
                        yield 4, with_color(
                            Colors.cyan,
                            f"Rule {err.core.rule_id.value if err.core.rule_id else '<unknown rule>'}",
                        ) + with_color(
                            Colors.yellow,
                            f' due to exception "{err.type_().kind}" raised during analysis',
                        )

                if file_error_log.line_errors:
                    yield 3, with_color(
                        Colors.white,
                        f"The following lines were skipped for all analysis:",
                    )
                    for err in file_error_log.line_errors:
                        # not sure if this is actually possible
                        if not err.spans:
                            yield 4, with_color(
                                Colors.cyan, "<unknown lines>"
                            ) + with_color(Colors.yellow, f"({err.type_().kind})")
                            continue

                        for span in err.spans:
                            yield 4, with_color(
                                Colors.cyan,
                                f"lines {span.start.line}-{span.end.line}",
                            ) + with_color(
                                Colors.yellow,
                                f' due to exception "{err.type_().kind}" raised during analysis',
                            )
        else:
            yield 2, "<none>"

    def verbose_output(self, too_many_entries: int) -> str:
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
            3: lambda line: click.wrap_text(
                line,
                width,
                "     ",
                "     ",
                False,
            ),
            4: lambda line: click.wrap_text(
                line,
                width,
                "     • ",
                "       ",
                False,
            ),
        }
        output = ""

        prev_level = None
        for level, line in self.yield_verbose_lines(too_many_entries):
            if prev_level != level:
                output += "\n"
            formatter = formatters_by_level[level]
            output += formatter(line) + "\n"
            prev_level = level

        return output

    # TODO: return directly a out.SkippedTarget
    def yield_json_objects(self) -> Iterable[Dict[str, Any]]:
        # coupling: if you add a reason here,
        # add it also to semgrep_output_v1.atd.
        for path in self.always_skipped:
            yield {"path": str(path), "reason": "always_skipped"}
        for path in self.semgrepignored:
            yield {"path": str(path), "reason": "semgrepignore_patterns_match"}
        for path in self.cli_includes:
            yield {"path": str(path), "reason": "cli_include_flags_do_not_match"}
        for path in self.cli_excludes:
            yield {"path": str(path), "reason": "cli_exclude_flags_match"}
        for path in self.insufficient_permissions:
            yield {"path": str(path), "reason": "insufficient_permissions"}
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


# This used to patch the targeting_conf just before using it.
# We could the same mechanism with the list of excludes that depend
# on the "product". It might clarify the code a bit.
#
# Current status: this is unused because baseline_commit isn't supported
# by the OCaml implementation.
#
def copy_and_update_targeting_conf(
    *, conf: out.TargetingConf, force_novcs_project: bool = False
) -> out.TargetingConf:
    # Not sure if a shallow copy (copy.copy) would work or would be preferable
    conf = copy.deepcopy(conf)
    conf.force_novcs_project = force_novcs_project
    return conf


@dataclass
class TargetScanResult:
    selected_files: FrozenSet[Path]
    # legacy semgrepignore v1 only:
    files_with_insufficient_permissions: FrozenSet[Path]
    # semgrepignore v2 only:
    skipped_targets: List[out.SkippedTarget]


@frozen(eq=False)  #
class ScanningRoot:
    """
    Represents one path that was given as a scanning root.
    Then scanning_root.paths returns all target paths it expands to.
    This does not do any include/exclude filtering.

    Three strategies are available for gathering targets:
    1. recursively collect from file system (slowest, but always works)
    2. read the output of `git ls-files` (respects .gitignore)
    3. [TODO] read the output of `git diff` (respects --baseline-commit)
    """

    path: Path = field(converter=Path)
    git_tracked_only: bool = False
    baseline_handler: Optional[BaselineHandler] = None
    # semgrepignore v2 only:
    targeting_conf_v2: Optional[Mapping[out.Product, out.TargetingConf]] = None

    @path.validator
    def validate_path(self, _: Any, value: Path) -> None:
        """
        Check whether the targeted path exists.

        If not, the path might be a socket.
        """
        if not self._is_valid_file_or_dir(value):
            raise InvalidScanningRootError(paths=tuple([value]))
        return None

    def _is_valid_file_or_dir(self, path: Path) -> bool:
        """Check this is a valid file or directory for semgrep scanning."""
        return (
            path_has_permissions(path, stat.S_IRUSR, follow_symlinks=False)
            and not path.is_symlink()
        )

    def _is_valid_file(self, path: Path) -> bool:
        """Check if file is a readable regular file.

        This eliminates files that should never be semgrep targets. Among
        others, this takes care of excluding symbolic links (because we don't
        want to scan the target twice), directories (which may be returned by
        globbing or by 'git ls-files' e.g. submodules), and files missing
        the read permission.
        """
        return self._is_valid_file_or_dir(path) and path.is_file()

    def _parse_git_output_nulsep(self, output: str) -> FrozenSet[Path]:
        """
        Convert a null-delimited list of files to a set of path objects
        prepends curr_dir to all paths in said list

        If list is empty then returns an empty set
        """
        files: FrozenSet[Path] = frozenset()

        if output:
            files = frozenset(
                p
                for p in (self.path / elem for elem in output.split("\x00") if elem)
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
        # Instead of crashing when we encounter a file with non utf-8 characters, we replace invalid
        # characters e.g if 'c' was not a utf-8 char then abc.ml -> ab?.ml. Since ab?.ml doesn't
        # exist, the file abc.ml will not be processed, and any attempts to access ab?.ml by semgrep
        # later is handled properly.
        run_git_command = partial(
            sub_check_output,
            cwd=self.path.resolve(),
            encoding="utf-8",
            errors="replace",
            stderr=subprocess.DEVNULL,
        )

        # Tracked files
        tracked_output = run_git_command(["git", "ls-files", "-z"])

        # Untracked but not ignored files
        untracked_output = run_git_command(
            [
                "git",
                "ls-files",
                "-z",
                "--others",
                "--exclude-standard",
            ]
        )

        deleted_output = run_git_command(["git", "ls-files", "-z", "--deleted"])
        tracked = self._parse_git_output_nulsep(tracked_output)
        untracked_unignored = self._parse_git_output_nulsep(untracked_output)
        deleted = self._parse_git_output_nulsep(deleted_output)
        return frozenset(tracked | untracked_unignored - deleted)

    def files_from_filesystem(
        self,
    ) -> TargetScanResult:
        all = frozenset(match for match in self.path.glob("**/*"))
        # We need to check for access permission before checking file kind
        insufficient_permissions = frozenset(
            match for match in all if not os.access(match, os.R_OK)
        )
        access_ok = all - insufficient_permissions
        regular_files = frozenset(
            match for match in access_ok if match.is_file() and not match.is_symlink()
        )
        return TargetScanResult(
            selected_files=regular_files,
            files_with_insufficient_permissions=insufficient_permissions,
            skipped_targets=[],
        )

    @lru_cache(maxsize=None)
    def target_files_full(
        self, *, product: out.Product, ignore_baseline_handler: bool = False
    ) -> TargetScanResult:
        """
        Recursively go through a directory and return list of all files with
        default file extension of language.
        Return the selected files, the files with insufficient permissions
        (legacy), and the full list of skipped files (semgrepignore v2 only).

        ignore_baseline_handler: if True, will ignore the baseline handler and scan all files. Used in the context of scanning unchanged lockfiles for their dependencies and doing reachability analysis.
        """
        if self.targeting_conf_v2 is not None:
            # New: Use semgrep-core to discover target files
            targeting_conf = self.targeting_conf_v2[product]
            if ignore_baseline_handler:
                targeting_conf = copy_and_update_targeting_conf(
                    conf=targeting_conf, force_novcs_project=True
                )
            arg = out.ScanningRoots(
                root_paths=[out.Fpath(str(self.path))], targeting_conf=targeting_conf
            )
            res: out.TargetDiscoveryResult = semgrep.rpc_call.get_targets(arg)
            target_paths = frozenset([Path(fpath.value) for fpath in res.target_paths])
            # TODO: check for errors?
            return TargetScanResult(
                selected_files=target_paths,
                files_with_insufficient_permissions=frozenset(),
                skipped_targets=res.skipped,
            )
        else:
            # Legacy: use pysemgrep to discover target files
            if not self.path.is_dir() and self.path.is_file():
                return TargetScanResult(
                    selected_files=frozenset([self.path]),
                    files_with_insufficient_permissions=frozenset(),
                    skipped_targets=[],
                )

            if self.baseline_handler is not None:
                # Adding this conditional to scan all lockfiles for their dependencies, even in diff-aware scans
                if ignore_baseline_handler:
                    return self.files_from_filesystem()

                try:
                    return TargetScanResult(
                        selected_files=self.files_from_git_diff(),
                        files_with_insufficient_permissions=frozenset(),
                        skipped_targets=[],
                    )
                except (subprocess.CalledProcessError, FileNotFoundError):
                    logger.verbose(
                        f"Unable to target only the changed files since baseline commit. Running on all git tracked files instead..."
                    )

            if self.git_tracked_only:
                try:
                    return TargetScanResult(
                        selected_files=self.files_from_git_ls(),
                        files_with_insufficient_permissions=frozenset(),
                        skipped_targets=[],
                    )
                except (subprocess.CalledProcessError, FileNotFoundError):
                    logger.verbose(
                        f"Unable to ignore files ignored by git ({self.path} is not a git directory or git is not installed). Running on all files instead..."
                    )

            return self.files_from_filesystem()

    # cached (see _target_files())
    def target_files(
        self, *, product: out.Product, ignore_baseline_handler: bool = False
    ) -> FrozenSet[Path]:
        """Discover target files from the scanning root and cache the result"""
        return self.target_files_full(
            product=product, ignore_baseline_handler=ignore_baseline_handler
        ).selected_files


@define(eq=False, kw_only=True)
class TargetManager:
    """
    Handles all file include/exclude logic for semgrep

    Assumes file system does not change during its existence to cache
    files for a given language etc. If file system changes (i.e. git checkout),
    create a new TargetManager object

    If respect_git_ignore is true then will only consider files that are
    tracked or (untracked but not ignored) by git

    If git_baseline_commit is true then will only consider files that have
    changed since that commit

    If allow_unknown_extensions is set then targets with extensions that are
    not understood by semgrep will always be returned by get_files when searching for
    code targets. Else will discard targets with unknown extensions. Unknown
    extensions are never returned when looking for manifest/lockfile targets.

    TargetManager not to be confused with https://jobs.target.com/search-jobs/store%20manager
    """

    # TODO: rename scanning_root_strings -> scanning_root_paths
    scanning_root_strings: FrozenSet[Path]
    # use_semgrepignore_v2 is a temporary option while we migrate to
    # the new implementation of target selection.
    use_semgrepignore_v2: bool
    includes: Sequence[str] = Factory(list)
    excludes: Mapping[out.Product, Sequence[str]] = Factory(dict)
    force_novcs_project: bool = False
    force_project_root: Optional[str] = None
    max_target_bytes: int = -1
    respect_git_ignore: bool = False
    respect_rule_paths: bool = True
    baseline_handler: Optional[BaselineHandler] = None
    allow_unknown_extensions: bool = False
    semgrepignore_profiles: Mapping[out.Product, Semgrepignore] = Factory(dict)
    ignore_log: FileTargetingLog = Factory(FileTargetingLog, takes_self=True)
    scanning_roots: Sequence[ScanningRoot] = field(init=False)
    respect_semgrepignore: bool = True
    targeting_conf: Mapping[out.Product, out.TargetingConf] = field(init=False)

    _filtered_targets: Dict[Language, FilteredFiles] = field(factory=dict)

    # This initializes the class attributes marked with '= field(init=False)':
    def __attrs_post_init__(self) -> None:
        # for use_semgrepignore_v2 i.e. delegating target discovery to semgrep-core
        self.targeting_conf = {
            product: out.TargetingConf(
                exclude=list(self.excludes.get(product, [])),
                max_target_bytes=self.max_target_bytes,
                respect_gitignore=self.respect_git_ignore,
                respect_semgrepignore_files=self.respect_semgrepignore,
                # explicit targets = target files that are passed explicitly
                # on the command line and are normally not ignored by semgrepignore
                # or other filters.
                always_select_explicit_targets=True,
                # shouldn't be needed since we provide the scanning roots:
                explicit_targets=[],
                force_novcs_project=self.force_novcs_project,
                exclude_minified_files=False,
                include_=(list(self.includes) or None),
                force_project_root=(
                    out.ProjectRoot(out.Filesystem(self.force_project_root))
                    if not self.force_project_root is None
                    else None
                ),
                baseline_commit=(
                    self.baseline_handler.base_commit()
                    if self.baseline_handler
                    else None
                ),
            )
            for product in ALL_PRODUCTS
        }
        self.scanning_roots = [
            ScanningRoot(
                root,
                git_tracked_only=self.respect_git_ignore,
                baseline_handler=self.baseline_handler,
                targeting_conf_v2=(
                    self.targeting_conf if self.use_semgrepignore_v2 else None
                ),
            )
            for root in self.scanning_root_strings
        ]
        return None

    @staticmethod
    def preprocess_path_patterns(patterns: Sequence[str]) -> List[str]:
        """Convert semgrep's path include/exclude patterns to wcmatch's glob patterns.

        In semgrep, pattern "foo/bar" should match paths "x/foo/bar", "foo/bar/x", and
        "x/foo/bar/x". It implicitly matches zero or more directories at the beginning and the end
        of the pattern. In contrast, we have to explicitly specify the globstar (**) patterns in
        wcmatch. This function will convert a pattern "foo/bar" into "**/foo/bar" and
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
        self,
        language: Union[None, Language],
        *,
        candidates: FrozenSet[Path],
    ) -> FilteredFiles:
        """
        Returns only paths that have the correct extension or shebang

        Finds all files in a collection of paths that either:
        - end with one of a set of extension
        - is a script that executes with one of a set of programs
        """
        if isinstance(language, Language):
            kept = frozenset(
                path
                for path in candidates
                if any(str(path).endswith(ext) for ext in language.definition.exts)
                or self.executes_with_shebang(path, language.definition.shebangs)
            )
        else:
            kept = frozenset(candidates)
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
    def filter_by_permission(candidates: FrozenSet[Path]) -> FilteredFiles:
        """
        Exclude files we can't read
        """
        # TODO: os.access() returns True if the user is root, even if the
        # effective user ID is unpriviledged and results in the file
        # being not readable!
        # This is a problem when running pysemgrep as root but only if the
        # euid is different from the uid.
        kept, removed = partition(
            candidates,
            lambda path: os.access(path, os.R_OK),
        )

        return FilteredFiles(frozenset(kept), frozenset(removed))

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
            candidates,
            lambda path: os.path.isfile(path)
            and os.path.getsize(path) <= max_target_bytes,
        )

        return FilteredFiles(frozenset(kept), frozenset(removed))

    @lru_cache(maxsize=None)
    def get_all_files(
        self, *, product: out.Product, ignore_baseline_handler: bool = False
    ) -> FrozenSet[Path]:
        return frozenset(
            selected_file
            for root in self.scanning_roots
            for selected_file in root.target_files(
                ignore_baseline_handler=ignore_baseline_handler, product=product
            )
        )

    @lru_cache(maxsize=None)
    def get_paths_with_insufficient_permissions(
        self, *, product: out.Product, ignore_baseline_handler: bool = False
    ) -> FrozenSet[Path]:
        """
        Return paths with insufficient permissions we already know about.
        This is not always all of them depending on how these paths were
        obtained.
        Legacy use_semgrepignore_v2=False
        """
        return frozenset(
            f
            for root in self.scanning_roots
            for f in (
                root.target_files_full(
                    product=product, ignore_baseline_handler=ignore_baseline_handler
                )
            ).files_with_insufficient_permissions
        )

    @lru_cache(maxsize=None)
    def get_skipped_files(
        self, *, product: out.Product, ignore_baseline_handler: bool = False
    ) -> List[out.SkippedTarget]:
        """
        Return all the skipped files reported by the RPC to semgrep-core.
        Requires use_semgrepignore_v2=True.
        """
        return [
            f
            for root in self.scanning_roots
            for f in (
                root.target_files_full(
                    product=product, ignore_baseline_handler=ignore_baseline_handler
                )
            ).skipped_targets
        ]

    @lru_cache(maxsize=None)
    def get_files_for_language(
        self,
        *,
        lang: Union[None, Language, Literal["dependency_source_files"]],
        product: out.Product,
        ignore_baseline_handler: bool = False,
    ) -> FilteredFiles:
        """
        Return all files that are descendants of any directory in TARGET that have
        an extension matching LANG or are a lockfile for LANG ecosystem that match any pattern in INCLUDES and do not
        match any pattern in EXCLUDES. Any file in TARGET bypasses excludes and includes.
        If a file in TARGET has a known extension that is not for language LANG then
        it is also filtered out.

        Lang can be:
        - A true language, which causes this function to return source code files in that language
        - `None`, which skips language filtering altogether
        - `"dependency_source_files"`, which finds files that might contain dependency source information across all project types

        Note also filters out any directory and descendants of `.git`

        ignore_baseline_handler: if True, will ignore the baseline handler and scan all files. Used in the context of scanning unchanged lockfiles for their dependencies and doing reachability analysis.
        """
        all_files = self.get_all_files(
            ignore_baseline_handler=ignore_baseline_handler, product=product
        )

        if isinstance(lang, Language):
            files = self.filter_by_language(lang, candidates=all_files)
            self.ignore_log.by_language[lang].update(files.removed)
        elif lang == "dependency_source_files":
            kept = filter_dependency_source_files(candidates=all_files)
            files = FilteredFiles(kept, all_files - kept)
            self.ignore_log.by_language[lang].update(files.removed)
        else:
            files = FilteredFiles(frozenset(all_files), frozenset())

        ####################################################################
        # language-independent, rule-independent target filtering
        # TODO: generic filtering should not be done here!
        # This prevents us from getting the count of targets before filtering
        # them further based on applicable rules. This number is reported
        # correctly by osemgrep but incorrectly by pysemgrep.
        ####################################################################
        if self.use_semgrepignore_v2:
            # Populate the ignore_log with the skipped files so they can
            # be printed out.
            skipped_files = self.get_skipped_files(
                product=product, ignore_baseline_handler=ignore_baseline_handler
            )
            includes = []
            excludes = []
            insufficient_permissions = []
            size_limit = []
            semgrepignored = []
            for skipped_file in skipped_files:
                path = Path(skipped_file.path.value)
                reason = skipped_file.reason.value  # Union[...]
                if isinstance(reason, out.CliIncludeFlagsDoNotMatch):
                    includes.append(path)
                elif isinstance(reason, out.CliExcludeFlagsMatch):
                    excludes.append(path)
                elif isinstance(reason, out.InsufficientPermissions):
                    insufficient_permissions.append(path)
                elif isinstance(reason, out.ExceededSizeLimit):
                    size_limit.append(path)
                elif isinstance(reason, out.SemgrepignorePatternsMatch):
                    if str(path) not in PATHS_ALWAYS_SKIPPED:
                        # Compatibility with legacy implementation:
                        # In the OCaml v2 implementation, '.git' is ignored by the
                        # default '.semgrepignore' file rather than being always ignored.
                        # It causes '.git' to be reported as semgrepignored rather
                        # than always skipped.
                        # This feature is desirable because it allows scanning
                        # a '.git' folder in non-git projects.
                        # TODO: we might want to revisit how we report this once the semgrep
                        #  text output is migrated to OCaml.
                        semgrepignored.append(path)
                else:
                    # TODO: report the many other cases as "other" or whatever
                    pass
            self.ignore_log.cli_includes.update(includes)
            self.ignore_log.cli_excludes.update(excludes)
            self.ignore_log.insufficient_permissions.update(insufficient_permissions)
            self.ignore_log.size_limit.update(size_limit)
            self.ignore_log.semgrepignored.update(semgrepignored)
        else:  # legacy, not use_semgrepignore_v2
            files = self.filter_includes(self.includes, candidates=files.kept)
            self.ignore_log.cli_includes.update(files.removed)

            files = self.filter_excludes(
                self.excludes.get(product, []), candidates=files.kept
            )
            self.ignore_log.cli_excludes.update(files.removed)

            files = self.filter_excludes(PATHS_ALWAYS_SKIPPED, candidates=files.kept)
            self.ignore_log.always_skipped.update(files.removed)

            paths_with_insufficient_permissions = (
                self.get_paths_with_insufficient_permissions(
                    product=product, ignore_baseline_handler=ignore_baseline_handler
                )
            )
            # Depending on how the files were obtained, we need to check
            # for file permissions here
            files = self.filter_by_permission(files.kept)
            self.ignore_log.insufficient_permissions.update(
                set(files.removed | paths_with_insufficient_permissions)
            )

            # Lockfiles are easy to parse, and regularly surpass 1MB for big repos
            if lang != "dependency_source_files":
                files = self.filter_by_size(
                    self.max_target_bytes, candidates=files.kept
                )
                self.ignore_log.size_limit.update(files.removed)

            # TODO: explain what's going on with these semgrepignore profiles
            if product in self.semgrepignore_profiles and self.respect_semgrepignore:
                semgrepignore = self.semgrepignore_profiles[product]
                files = semgrepignore.filter_paths(candidates=files.kept)
                # TODO: Fix ignore_log to log which profile filtered which files.
                self.ignore_log.semgrepignored.update(files.removed)

        kept_files = files.kept

        explicit_files = frozenset(
            t.path
            for t in self.scanning_roots
            if not t.path.is_dir() and t.path.is_file()
        )

        ####################################################################
        # language-specific target filtering
        ####################################################################
        explicit_files_for_lang = self.filter_by_language(
            lang if isinstance(lang, Language) else None, candidates=explicit_files
        )
        kept_files |= explicit_files_for_lang.kept
        if self.allow_unknown_extensions and lang != "dependency_source_files":
            # add unknown extensions back in for languages. Don't do so when searching
            # for dependency source information
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
        rule_product: out.Product,
    ) -> FrozenSet[Path]:
        """
        Returns list of target files that should be analyzed for a LANG

        Given this object's SCANNING_ROOT, self.INCLUDE, and self.EXCLUDE will return list
        of all descendant files of directories in SCANNING_ROOT that end in extension
        typical for LANG. If self.INCLUDES is nonempty then all files will have an ancestor
        that matches a pattern in self.INCLUDES. Will not include any file that has
        an ancestor that matches a pattern in self.EXCLUDES. Any explicitly named files
        in SCANNING_ROOT will bypass this global INCLUDE/EXCLUDE filter. The local INCLUDE/EXCLUDE
        filter is then applied.
        """
        paths = self.get_files_for_language(lang=lang, product=rule_product)

        if self.respect_rule_paths:
            paths = self.filter_includes(rule_includes, candidates=paths.kept)
            self.ignore_log.rule_includes[rule_id].update(paths.removed)

            paths = self.filter_excludes(rule_excludes, candidates=paths.kept)
            self.ignore_log.rule_excludes[rule_id].update(paths.removed)

        return paths.kept

    def get_all_dependency_source_files(
        self,
        ignore_baseline_handler: bool = False,
    ) -> FrozenSet[Path]:
        """
        Return all files that might be used as a source of dependency information
        """
        all_files = self.get_files_for_language(
            lang="dependency_source_files",
            product=out.Product(out.SCA()),
            ignore_baseline_handler=ignore_baseline_handler,
        )
        return all_files.kept
