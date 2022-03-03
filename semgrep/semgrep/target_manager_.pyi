from pathlib import Path
from typing import Any, Dict, List, Sequence, Set, Tuple, Optional, Collection
from typing import Iterator, FrozenSet
import contextlib
import semgrep.ignores as ignores
import semgrep.semgrep_types as semgrep_types

@contextlib.contextmanager
def converted_pipe_targets(targets: Sequence[str]) -> Iterator[Sequence[str]]:
    """
    Provides a context in which FIFOs have been copied into temp files

    This is necessary as we can not easily rewire these pipes into the called semgrep-core
    process.

    :param targets: Input target specifiers
    :return: A sequence of non-pipe specifiers (Path(t).is_file() returns true)
    """
    ...

class IgnoreLog:
    """Keeps track of which paths were ignored for what reason.

    Each attribute is a distinct reason why files could be ignored.

    Some reason can apply once per rule; these are mappings keyed on the rule id.
    """

    target_manager: "TargetManager"

    semgrepignored: Set[Path]
    always_skipped: Set[Path]
    cli_includes: Set[Path]
    cli_excludes: Set[Path]
    size_limit: Set[Path]

    rule_includes: Dict[str, Set[Path]]
    rule_excludes: Dict[str, Set[Path]]

    @property
    def size_limited_paths(self) -> Set[Path]: ...
    @property
    def rule_ids_with_skipped_paths(self) -> Set[Path]: ...

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
    targets: Sequence[str]
    respect_git_ignore: bool
    skip_unknown_extensions: bool
    file_ignore: Optional[ignores.FileIgnore]

    def get_files(
        self,
        lang: semgrep_types.Language,
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
        ...
