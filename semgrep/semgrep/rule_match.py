import binascii
import hashlib
import itertools
import textwrap
from functools import total_ordering
from pathlib import Path
from typing import Any
from typing import Dict
from typing import Iterable
from typing import List
from typing import Optional
from typing import Set
from typing import Tuple
from typing import TYPE_CHECKING

import pymmh3
from attrs import evolve
from attrs import field
from attrs import frozen

from semgrep.constants import NOSEM_INLINE_COMMENT_RE
from semgrep.constants import RuleSeverity
from semgrep.types import JsonObject

if TYPE_CHECKING:
    from semgrep.rule import Rule


@frozen(order=True)
class CoreLocation:
    """
    parses:
     {
        "line": 5
        "col": 6
        "offset": 30
     }
    into an object
    """

    line: int
    col: int
    offset: int

    def to_dict(self) -> JsonObject:
        return {
            "line": self.line,
            "col": self.col,
            "offset": self.offset,
        }

    @classmethod
    def parse(cls, raw_json: JsonObject) -> "CoreLocation":
        line = raw_json.get("line")
        col = raw_json.get("col")
        offset = raw_json.get("offset")

        # Please mypy
        assert isinstance(line, int)
        assert isinstance(col, int)
        assert isinstance(offset, int)

        return cls(line, col, offset)


def rstrip(value: Optional[str]) -> Optional[str]:
    return value.rstrip() if value else None


@total_ordering
@frozen(eq=False)
class RuleMatch:
    """
    A section of code that matches a single rule (which is potentially many patterns)
    """

    rule_id: str
    message: str = field(repr=False)
    severity: RuleSeverity
    metadata: Dict[str, Any] = field(repr=False)
    extra: Dict[str, Any] = field(repr=False)

    path: Path = field(repr=str)
    start: CoreLocation
    end: CoreLocation

    # We call rstrip() for consistency with semgrep-core, which ignores whitespace
    # including newline chars at the end of multiline patterns
    fix: Optional[str] = field(converter=rstrip)
    fix_regex: Optional[Dict[str, Any]]

    index: int = 0

    # None means we didn't check; ignore status is unknown
    is_ignored: Optional[bool] = field(default=None)

    # derived attributes
    lines: List[str] = field(init=False, repr=False)
    lines_hash: str = field(init=False, repr=False)
    previous_line: str = field(init=False, repr=False)
    syntactic_context: str = field(init=False, repr=False)
    cli_unique_key: Tuple = field(init=False, repr=False)
    ci_unique_key: Tuple = field(init=False, repr=False)
    ordering_key: Tuple = field(init=False, repr=False)
    syntactic_id: str = field(init=False, repr=False)

    @lines.default
    def get_lines(self) -> List[str]:
        """
        Return lines in file that this RuleMatch is referring to.

        Assumes file exists.

        Need to do on initialization instead of on read since file might not be the same
        at read time
        """
        # Start and end line are one-indexed, but the subsequent slice call is
        # inclusive for start and exclusive for end, so only subtract from start
        start_line = self.start.line - 1
        end_line = self.end.line

        if start_line == -1 and end_line == 0:
            # Completely empty file
            return []

        # buffering=1 turns on line-level reads
        with self.path.open(buffering=1, errors="replace") as fd:
            result = list(itertools.islice(fd, start_line, end_line))

        return result

    @lines_hash.default
    def get_lines_hash(self) -> str:
        return hashlib.sha256("\n".join(self.lines).encode()).hexdigest()

    @previous_line.default
    def get_previous_line(self) -> str:
        """Return the line preceding the match, if any.

        This is meant for checking for the presence of a nosemgrep comment.
        This implementation was derived from the 'lines' method below.
        Refer to it for relevant comments.
        IT feels like a lot of duplication. Feel free to improve.
        """
        # see comments in '_get_lines' method
        start_line = self.start.line - 2
        end_line = start_line + 1
        is_empty_file = self.end.line <= 0

        if start_line < 0 or is_empty_file:
            # no previous line
            return ""

        with self.path.open(buffering=1, errors="replace") as fd:
            res = list(itertools.islice(fd, start_line, end_line))

        if res:
            return res[0]
        else:
            return ""

    @syntactic_context.default
    def get_syntactic_context(self) -> str:
        code = "\n".join(self.lines)
        code = textwrap.dedent(code)
        code = NOSEM_INLINE_COMMENT_RE.sub("", code)
        code = code.strip()
        return code

    @cli_unique_key.default
    def get_cli_unique_key(self) -> Tuple:
        return (
            self.rule_id,
            self.path,
            self.start.offset,
            self.end.offset,
            self.message,
        )

    @ci_unique_key.default
    def get_ci_unique_key(self) -> Tuple:
        return (self.rule_id, self.path, self.syntactic_context, self.index)

    @ordering_key.default
    def get_ordering_key(self) -> Tuple:
        return (self.path, self.start, self.end, self.rule_id, self.message)

    @syntactic_id.default
    def get_syntactic_id(self) -> str:
        # Upon reviewing an old decision,
        # there's no good reason for us to use MurmurHash3 here,
        # but we need to keep consistent hashes so we cannot change this easily
        hash_int = pymmh3.hash128(str(self.ci_unique_key))
        hash_bytes = int.to_bytes(hash_int, byteorder="big", length=16, signed=False)
        return str(binascii.hexlify(hash_bytes), "ascii")

    def __hash__(self) -> int:
        return hash(self.cli_unique_key)

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, type(self)):
            return False
        return self.cli_unique_key == other.cli_unique_key

    def __lt__(self, other: "RuleMatch") -> bool:
        if not isinstance(other, type(self)):
            return NotImplemented
        return self.ordering_key < other.ordering_key


class RuleMatchSet(Set[RuleMatch]):
    """
    A set type which is aware
    that two rule matches are not to be considered the same
    even if they have the same line of code.
    It amends rule matches that have identical code during insertion
    to set a unique zero-indexed "index" value on them.
    """

    def __init__(self, __iterable: Optional[Iterable[RuleMatch]] = None) -> None:

        self._seen_ci_keys: Set[str] = set()
        if __iterable is None:
            super().__init__()
        else:
            super().__init__(__iterable)

    def add(self, rule_match: RuleMatch) -> None:
        """
        Add finding, even if the same (rule, path, code) existed.
        This is used over regular `.add` to increment the finding's index
        if it already exists in the set, thereby retaining multiple copies
        of the same (rule_id, path, line_of_code) tuple.
        """
        while rule_match.ci_unique_key in self._seen_ci_keys:
            rule_match = evolve(rule_match, index=rule_match.index + 1)
        super().add(rule_match)

    def update(self, *rule_match_iterables: Iterable[RuleMatch]) -> None:
        """
        Add findings, even if the same (rule, path, code) exist.
        This is used over regular `.update` to increment the findings' indexes
        if they already exists in the set, thereby retaining multiple copies
        of the same (path, rule_id, line_of_code) tuples.
        """
        for rule_matches in rule_match_iterables:
            for rule_match in rule_matches:
                self.add(rule_match)


OrderedRuleMatchList = List[RuleMatch]
RuleMatchMap = Dict["Rule", OrderedRuleMatchList]
