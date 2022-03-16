import hashlib
import itertools
from pathlib import Path
from typing import Any
from typing import Dict
from typing import List
from typing import Optional

from attrs import field
from attrs import frozen

from semgrep.constants import RuleSeverity
from semgrep.types import JsonObject


@frozen
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


@frozen
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

    # None means we didn't check; ignore status is unknown
    is_ignored: Optional[bool] = field(default=None)

    # derived attributes
    lines: List[str] = field(init=False, repr=False)
    lines_hash: str = field(init=False, repr=False)
    previous_line: str = field(init=False, repr=False)

    @lines.default
    def get_lines(self) -> List[str]:
        """
        Return lines in file that this RuleMatch is referring to.

        Assumes file exists.

        Need to do on initializtion instead of on read since file might not be the same
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

    def is_baseline_equivalent(self, other: "RuleMatch") -> bool:
        # Note should not override __eq__ of this object since technically not equal
        return (
            self.rule_id == other.rule_id
            and self.path == other.path
            and self.lines_hash == other.lines_hash
            and self.lines == other.lines
        )
