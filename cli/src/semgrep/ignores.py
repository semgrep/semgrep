import fnmatch
import os
import re
from functools import lru_cache
from pathlib import Path
from typing import FrozenSet
from typing import Iterable
from typing import Iterator
from typing import Optional
from typing import Set
from typing import TextIO

from attr import frozen
from attrs import define
from boltons.iterutils import partition

from semgrep.constants import TOO_MUCH_DATA
from semgrep.error import SemgrepError
from semgrep.state import get_state
from semgrep.types import FilteredFiles
from semgrep.verbose_logging import getLogger

CONTROL_REGEX = re.compile(r"(?!<\\):")  # Matches unescaped colons
MULTI_CHAR_REGEX = re.compile(
    r"(?!<\\)\[.*(?!<\\)\]"
)  # Matches anything in unescaped brackets
COMMENT_START_REGEX = re.compile(r"(?P<ignore_pattern>.*?)(?:\s+|^)#.*")
SEMGREPIGNORE_FILE_NAME = ".semgrepignore"

logger = getLogger(__name__)


# path.is_relative_to is only available starting with Python 3.9
# So we just copy its implementation
def path_is_relative_to(p1: Path, p2: Path) -> bool:
    try:
        p1.relative_to(p2)
        return True
    except ValueError:
        return False


@frozen
# @define(kw_only=True)  # incompatible with @frozen (?)
class Semgrepignore:
    """Hold the data extracted from the .semgrepignore file.
    base_path is the work directory which is normally the folder containing
    the user-specified .semgrepignore file if it exists.
    """

    # base path = absolute path to current folder = location of '.semgrepignore'
    base_path: Path
    fnmatch_patterns: FrozenSet[str]
    max_log_list_entries: int = -1

    @lru_cache(maxsize=100_000)  # size aims to be 100x of fully caching this repo
    def _survives(self, path: Path) -> bool:
        """
        Determine if a single Path survives the ignore filter.
        """
        # The path is relative to the base path where the '.semgrepignore' file exists,
        # which is also the current folder.
        # A pattern can match the full path or only a prefix.
        # For example, the fnmatch pattern 'tests' will match the paths
        # 'tests' and 'tests/foo'.

        path.is_dir()
        path_is_relative_to_base = path_is_relative_to(path, self.base_path)
        matchable_path = (
            str(path.relative_to(self.base_path))
            if path_is_relative_to_base
            else str(path)
        )
        for pat in self.fnmatch_patterns:
            if path_is_relative_to_base or pat.startswith("**/"):
                if fnmatch.fnmatch(matchable_path, pat):
                    return False
        return True

    @lru_cache(maxsize=100_000)  # size aims to be 100x of fully caching this repo
    def _filter(self, path: Path) -> bool:
        absolute_path = path.absolute()
        return path.exists() and (
            self._survives(absolute_path) or absolute_path.samefile(self.base_path)
        )

    def filter_paths(self, *, candidates: Iterable[Path]) -> FilteredFiles:
        kept, removed = partition(sorted(candidates), self._filter)
        too_many_entries = self.max_log_list_entries
        if too_many_entries > 0 and len(removed) > too_many_entries:
            logger.verbose(f"Ignoring due to .semgrepignore:")
            logger.verbose(TOO_MUCH_DATA)
        else:
            for path in removed:
                logger.verbose(f"Ignoring {path} due to .semgrepignore")

        return FilteredFiles(frozenset(kept), frozenset(removed))

    @classmethod
    def from_unprocessed_patterns(
        cls, base_path: Path, patterns: Iterable[str], max_log_list_entries: int
    ) -> "Semgrepignore":
        patterns = list(patterns)
        return cls(
            base_path=base_path,
            fnmatch_patterns=frozenset(
                SemgrepignoreProcessor(base_path).process(patterns)
            ),
            max_log_list_entries=max_log_list_entries,
        )


# This class is an exact duplicate of the Parser class in semgrep-action
# ^ wtf? [2024: https://github.com/semgrep/semgrep-action was archived; not touching it]
@define
class SemgrepignoreParser:
    r"""
    A parser for semgrepignore syntax.

    semgrepignore syntax mirrors gitignore syntax, with the following modifications:
    - "Include" patterns (lines starting with "!") are not supported.
    - "Character range" patterns (lines including a collection of characters inside brackets) are not supported.
    - An ":include ..." directive is added, which allows another file to be included in the ignore pattern list;
      typically this included file would be the project .gitignore. No attempt at cycle detection is made.
    - Any line beginning with a colon, but not ":include ", will raise a SemgrepError.
    - "\:" is added to escape leading colons.

    Unsupported patterns are silently removed from the pattern list (this is done so that gitignore files may be
    included without raising errors), although the removal will be logged.

    Unfortunately there's no available parser for gitignore syntax in python, so we have
    to make our own. The syntax is simple enough that we can just roll our own parser, so
    I deliberately skip using a parser generator or combinator library, which would either need to
    parse on a character-by-character basis or make use of a large number of regex scans.

    The parser steps are, for each line in the input stream:
    1. Remove comments
    2. Remove unsupported gitignore syntax
    3. Expand directives

    The end result of this parsing is a set of human-readable patterns corresponding to gitignore syntax.
    To use these patterns with fnmatch, however, a final postprocessing step is needed, achieved by calling
    SemgrepignoreProcessor.process().

    :param file_path: the path to the .semgrepignore file. In Semgrepignore v1
     (this implementation), this is in the current folder. In Semgrepignore v2
     (OCaml implementation), it's found at the project root.
    :param base_path: The path relative to which :include directives should be
     evaluated. Isn't it always the folder containing .semgrepignore?
    """

    # Parser steps are each represented as Generators. This allows us to chain
    # steps, whether the step is a transformation, a filter, an expansion, or any combination thereof.

    file_path: Path
    base_path: Path

    @staticmethod
    def remove_comments(line: str) -> Iterator[str]:
        """If a line has a comment, remove the comment and just return the ignore pattern"""
        m = COMMENT_START_REGEX.match(line)
        if m:
            yield m.groupdict().get(
                "ignore_pattern", ""
            )  # return empty string if entire line is a comment
        else:
            yield line.rstrip()

    @staticmethod
    def filter_supported(line: str) -> Iterator[str]:
        """Remove unsupported gitignore patterns"""
        if not line:
            pass
        elif line.startswith("!") or MULTI_CHAR_REGEX.search(line):
            logger.debug(f"Skipping unsupported gitignore pattern '{line}'")
        else:
            yield line

    def expand_directives(self, line: str) -> Iterable[str]:
        """Load :include files"""
        metrics = get_state().metrics
        if line.startswith(":include "):
            include_path = self.base_path / line[9:]
            if include_path.is_file():
                with include_path.open() as include_lines:
                    sub_base = include_path.parent.resolve()
                    sub_parser = SemgrepignoreParser(
                        file_path=include_path, base_path=sub_base
                    )
                    metrics.add_feature("semgrepignore", "include")
                    return sub_parser.parse(include_lines)
            else:
                logger.debug(
                    f"Skipping `:include {include_path}` directive, file not found"
                )
                return []
        elif line == ":":
            return []
        elif CONTROL_REGEX.match(line):
            raise SemgrepError(
                f"While parsing .semgrepignore: unknown ignore directive in {self.file_path}: '{line}'"
            )
        else:
            return (line for _ in range(1))

    def parse(self, stream: TextIO) -> Set[str]:
        """Performs parsing of an input stream"""
        return {
            pattern
            for line in stream
            for no_comments in self.remove_comments(line)
            for supported in self.filter_supported(no_comments)
            for pattern in self.expand_directives(supported)
        }


# This class is an exact duplicate of the SemgrepignoreProcessor class in semgrep-action
# ^ wtf? [2024: https://github.com/semgrep/semgrep-action was archived; not touching it]
@define
class SemgrepignoreProcessor:
    """
    A post-processor for parsed semgrepignore files.

    The postprocessor is responsible for converting the parser's intermediate representation to a set of
    patterns compatible with fnmatch. The steps are:
    1. Unescape escape characters
    2. Convert gitignore patterns into fnmatch patterns
    """

    # Per SemgrepignoreParser, each SemgrepignoreProcessor step is represented as a Generator.

    base_path: Path

    @staticmethod
    def unescape(line: str) -> Iterator[str]:
        """Expands escape characters"""
        out = ""
        is_escape = False
        for c in line:
            if is_escape:
                out += c
                is_escape = False
            elif c == "\\":
                is_escape = True
            else:
                out += c
        yield out

    # TODO: fnmatch doesn't support '**'!!! Somebody do something!
    # It's only good for matching individual path segments not containing
    # slashes.
    #
    # Possible fixes:
    # 1. Do nothing and migrate to osemgrep
    # 2. Upgrade to Python 3.13 and use PurePath.full_match
    # 3. Install the wcmatch library and use
    #      wcmatch.pathlib.PurePath(str).globmatch(pat)
    #
    def to_fnmatch(self, git_pat: str) -> Iterator[str]:
        """Convert a single pattern from gitignore to fnmatch syntax"""
        # The input path against which we'll match the pattern is assumed to be
        # relative to current folder = relative to the project root.
        #
        # If the input path is a folder, it must end with a slash.
        #
        # When matching outside paths such as /tmp/foo, we'll ignore patterns that
        # are relative to the project root (= not starting with **/).
        #
        # gitignore pattern   fnmatch patterns         notes
        # a                   a, a/**, **/a, **/a/**   unanchored, match files and folders
        # a/                  a/, a/**, **/a/**        unanchored, match folders
        # /a                  a, a/**                  anchored, match files and folders
        # /a/                 a/**                     anchored, match folders
        # /a/b                a/b, a/b/**              anchored, match files and folders
        # a/b                 a/b, a/b/**              the middle slash anchors the pattern!

        # For gitignore, a leading slash or a slash in the middle anchors it i.e.
        # indicates a path relative to the root.
        is_anchored: bool = git_pat.rstrip("/").find("/") >= 0

        # A trailing slash forces the path to be a folder
        is_folder: bool = git_pat.endswith("/")

        # Preprocessing for fnmatch
        pat = git_pat
        if is_anchored:
            # Remove any leading slash
            if git_pat.startswith("./") or git_pat.startswith("/"):
                pat = git_pat.lstrip(".").lstrip("/")

        # X -> X/**
        # X/ -> X/**
        anchored_prefix_pat = os.path.join(pat, "**")
        yield anchored_prefix_pat
        if not is_folder:
            # X
            yield pat

        if not is_anchored:
            # Prepend **/ then repeat the steps above:
            unanchored_pat = os.path.join("**", pat)
            # **/X -> **/X/**
            # **/X/ -> **/X/**
            unanchored_prefix_pat = os.path.join(unanchored_pat, "**")
            yield unanchored_prefix_pat
            if not is_folder:
                # **/X
                yield unanchored_pat

    def process(self, pre: Iterable[str]) -> Set[str]:
        """Post-processes an intermediate representation"""
        patterns = {
            pattern
            for pat in pre
            for unescaped in self.unescape(pat)
            for pattern in self.to_fnmatch(unescaped)
        }
        return patterns


def parse_semgrepignore_file(
    *,
    semgrepignore_path: Path,
    workdir: Optional[Path] = None,
    max_log_list_entries: int = -1,
) -> Semgrepignore:
    """Parse a semgrepignore file. The optional workdir must be specified
    if the semgrepignore file is read from somewhere else than in workdir
    which happens when reading the fallback semgrepignore file.
    """
    if workdir is None:
        base_path = semgrepignore_path.parent
    else:
        base_path = workdir
    with semgrepignore_path.open() as f:
        semgrepignore = Semgrepignore.from_unprocessed_patterns(
            base_path=base_path,
            patterns=SemgrepignoreParser(
                file_path=semgrepignore_path, base_path=base_path
            ).parse(f),
            max_log_list_entries=max_log_list_entries,
        )
        return semgrepignore
