import itertools
from pathlib import Path
from typing import Any
from typing import Dict
from typing import List
from typing import Optional
from typing import Tuple

import attr

from semgrep.constants import RuleSeverity
from semgrep.types import JsonObject


@attr.s(auto_attribs=True, frozen=True)
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


@attr.s(frozen=True)
class RuleMatch:
    """
    A section of code that matches a single rule (which is potentially many patterns)
    """

    _id: str = attr.ib()
    _message: str = attr.ib(repr=False)
    _metadata: Dict[str, Any] = attr.ib(repr=False)
    _severity: RuleSeverity = attr.ib(repr=False)
    _fix: Optional[str] = attr.ib(repr=False)
    _fix_regex: Optional[Dict[str, Any]] = attr.ib(repr=False)
    _path: Path = attr.ib(repr=str)
    _start: CoreLocation = attr.ib()
    _end: CoreLocation = attr.ib()

    _extra: Dict[str, Any] = attr.ib(repr=False)
    _lines_cache: Dict[Tuple[int, int], List[str]] = attr.ib(repr=False)

    # optional attributes
    _is_ignored: Optional[bool] = attr.ib(default=None)

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
        """
        Return lines in file that this RuleMatch is referring to.

        Assumes file exists.
        """
        # Start and end line are one-indexed, but the subsequent slice call is
        # inclusive for start and exclusive for end, so only subtract from start
        start_line = self.start.line - 1
        end_line = self.end.line

        if start_line == -1 and end_line == 0:
            # Completely empty file
            return []

        try:
            return self._lines_cache[(start_line, end_line)]
        except KeyError:
            pass

        # buffering=1 turns on line-level reads
        with self.path.open(buffering=1, errors="replace") as fd:
            result = list(itertools.islice(fd, start_line, end_line))

        self._lines_cache[(start_line, end_line)] = result
        return result
