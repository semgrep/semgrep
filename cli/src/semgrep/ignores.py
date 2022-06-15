import fnmatch
import os
import re
from functools import lru_cache
from pathlib import Path
from typing import FrozenSet
from typing import Iterable
from typing import Iterator
from typing import Set
from typing import TextIO

from attr import frozen
from attrs import define
from boltons.iterutils import partition

from semgrep.error import SemgrepError
from semgrep.state import get_state
from semgrep.types import FilteredFiles
from semgrep.verbose_logging import getLogger

CONTROL_REGEX = re.compile(r"(?!<\\):")  # Matches unescaped colons
MULTI_CHAR_REGEX = re.compile(
    r"(?!<\\)\[.*(?!<\\)\]"
)  # Matches anything in unescaped brackets
COMMENT_START_REGEX = re.compile(r"(?P<ignore_pattern>.*?)(?:\s+|^)#.*")
IGNORE_FILE_NAME = ".semgrepignore"

logger = getLogger(__name__)

# For some reason path.is_relative_to produces a complaint that 'PosixPath' object has no attribute 'is_relative_to'
# So we just copy its implementation
def path_is_relative_to(p1: Path, p2: Path) -> bool:
    try:
        p1.relative_to(p2)
        return True
    except ValueError:
        return False


## TODO: This files duplicates the .semgrepignore functionality from semgrep-action.
## We should ultimately remove this from semgrep-action, and keep it as part of the CLI

# This class is a duplicate of the FileIgnore class in semgrep-action, but with all file walking functionality removed
@frozen
class FileIgnore:
    base_path: Path
    patterns: FrozenSet[str]

    @lru_cache(maxsize=100_000)  # size aims to be 100x of fully caching this repo
    def _survives(self, path: Path) -> bool:
        """
        Determines if a single Path survives the ignore filter.
        """
        path_is_dir = path.is_dir()
        path_is_relative_to_base = path_is_relative_to(path, self.base_path)
        if path_is_relative_to_base:
            path_relative_to_base = str(path.relative_to(self.base_path))
        else:
            path_relative_to_base = ""
        for p in self.patterns:
            if path_is_dir and p.endswith("/") and fnmatch.fnmatch(str(path), p[:-1]):
                logger.verbose(f"Ignoring {path} due to .semgrepignore")
                return False
            if fnmatch.fnmatch(str(path), p):
                logger.verbose(f"Ignoring {path} due to .semgrepignore")
                return False

            # Check any subpath of path satisfies a pattern
            # i.e. a/b/c/d is ignored with rule a/b
            # This is a hack. TargetFileManager should be pruning while searching
            # instead of post filtering to avoid this
            # Note: Use relative to base to avoid ignore rules firing on parent directories
            # i.e. /private/var/.../instabot should not be ignored with var/ rule
            # in instabot dir as base_path
            # Note: Append "/" to path before running fnmatch so **/pattern matches with pattern/stuff
            if (
                path_is_relative_to_base
                and p.endswith("/")
                and fnmatch.fnmatch("/" + path_relative_to_base, p + "*")
            ):
                logger.verbose(f"Ignoring {path} due to .semgrepignore")
                return False
            if (
                p.endswith("/")
                and p.startswith(str(self.base_path))
                and fnmatch.fnmatch(str(path), p + "*")
            ):
                logger.verbose(f"Ignoring {path} due to .semgrepignore")
                return False

        return True

    @lru_cache(maxsize=100_000)  # size aims to be 100x of fully caching this repo
    def _filter(self, path: Path) -> bool:
        absolute_path = path.absolute()
        return path.exists() and (
            self._survives(absolute_path) or absolute_path.samefile(self.base_path)
        )

    def filter_paths(self, *, candidates: Iterable[Path]) -> FilteredFiles:
        kept, removed = partition(candidates, self._filter)
        return FilteredFiles(frozenset(kept), frozenset(removed))

    @classmethod
    def from_unprocessed_patterns(
        cls, base_path: Path, patterns: Iterable[str]
    ) -> "FileIgnore":
        return cls(base_path, frozenset(Processor(base_path).process(patterns)))


# This class is an exact duplicate of the Parser class in semgrep-action
@define
class Parser:
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
    Processor.process().

    :param base_path:   The path relative to which :include directives should be evaluated
    """

    # Parser steps are each represented as Generators. This allows us to chain
    # steps, whether the step is a transformation, a filter, an expansion, or any combination thereof.

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
                    sub_parser = Parser(sub_base)
                    metrics.add_feature("semgrepignore", "include")
                    return sub_parser.parse(include_lines)
            else:
                logger.debug(
                    f"Skipping `:include {include_path}` directive, file not found"
                )
                return []
        elif CONTROL_REGEX.match(line):
            raise SemgrepError(
                f"Unknown ignore directive in Semgrep ignore file at {self.base_path}: '{line}'"
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


# This class is an exact duplicate of the Processor class in semgrep-action
@define
class Processor:
    """
    A post-processor for parsed semgrepignore files.

    The postprocessor is responsible for converting the parser's intermediate representation to a set of
    patterns compatible with fnmatch. The steps are:
    1. Unescape escape characters
    2. Convert gitignore patterns into fnmatch patterns
    """

    # Per Parser, each Processor step is represented as a Generator.

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

    def to_fnmatch(self, pat: str) -> Iterator[str]:
        """Convert a single pattern from gitignore to fnmatch syntax"""
        if pat.rstrip("/").find("/") < 0:
            # Handles:
            #   file
            #   path/
            pat = os.path.join("**", pat)
        if pat.startswith("./") or pat.startswith("/"):
            # Handles:
            #   /relative/to/root
            #   ./relative/to/root
            pat = pat.lstrip(".").lstrip("/")
        if not pat.startswith("**"):
            # Handles:
            #   path/to/absolute
            #   */to/absolute
            #   path/**/absolute
            pat = os.path.join(self.base_path, pat)
        yield pat

    def process(self, pre: Iterable[str]) -> Set[str]:
        """Post-processes an intermediate representation"""
        return {
            pattern
            for pat in pre
            for unescaped in self.unescape(pat)
            for pattern in self.to_fnmatch(unescaped)
        }
