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


@frozen
class RuleMatch:
    """
    A section of code that matches a single rule (which is potentially many patterns)
    """

    _id: str = field()
    _message: str = field(repr=False)
    _metadata: Dict[str, Any] = field(repr=False)
    _severity: RuleSeverity = field(repr=False)
    _fix: Optional[str] = field(repr=False)
    _fix_regex: Optional[Dict[str, Any]] = field(repr=False)
    _path: Path = field(repr=str)
    _start: CoreLocation = field()
    _end: CoreLocation = field()
    _extra: Dict[str, Any] = field(repr=False)

    # optional attributes
    _is_ignored: Optional[bool] = field(default=None)

    # derived attributes
    _lines: List[str] = field()
    _lines_hash: str = field()
    _previous_line: str = field()

    @_lines.default
    def _get_lines(self) -> List[str]:
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

    @_lines_hash.default
    def _initialize_lines_hash(self) -> str:
        return hashlib.sha256("\n".join(self.lines).encode()).hexdigest()

    @property
    def id(self) -> str:
        return self._id

    @property
    def path(self) -> Path:
        return self._path

    @property
    def extra(self) -> Dict[str, Any]:
        return self._extra

    @property
    def fix(self) -> Optional[str]:
        # We do this for consistency with semgrep-core, which ignores whitespace
        # including newline chars at the end of multiline patterns
        return self._fix.rstrip() if self._fix else self._fix

    @property
    def fix_regex(self) -> Optional[Dict[str, Any]]:
        return self._fix_regex

    @property
    def message(self) -> str:
        return self._message

    @property
    def metadata(self) -> Dict[str, Any]:
        return self._metadata

    @property
    def severity(self) -> RuleSeverity:
        return self._severity

    @property
    def start(self) -> CoreLocation:
        return self._start

    @property
    def end(self) -> CoreLocation:
        return self._end

    @property
    def is_ignored(self) -> Optional[bool]:
        return self._is_ignored

    @property
    def lines(self) -> List[str]:
        return self._lines

    @property
    def lines_hash(self) -> str:
        """
        sha256 digest of lines of this rule_match
        """
        return self._lines_hash

    @property
    def previous_line(self) -> str:
        return self._previous_line

    @_previous_line.default
    def _get_previous_line(self) -> str:
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
            self.id == other.id
            and self.path == other.path
            and self.lines_hash == other.lines_hash
            and self.lines == other.lines
        )
