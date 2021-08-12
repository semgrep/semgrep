import itertools
from pathlib import Path
from typing import Any
from typing import Dict
from typing import List
from typing import Optional
from typing import Tuple

import attr

from semgrep.pattern_match import PatternMatch


@attr.s(frozen=True)
class RuleMatch:
    """
    A section of code that matches a single rule (which is potentially many patterns)
    """

    _id: str = attr.ib()
    _pattern_match: PatternMatch = attr.ib(repr=False)
    _message: str = attr.ib(repr=False)
    _metadata: Dict[str, Any] = attr.ib(repr=False)
    _severity: str = attr.ib(repr=False)
    _fix: Optional[str] = attr.ib(repr=False)
    _fix_regex: Optional[Dict[str, Any]] = attr.ib(repr=False)

    # derived attributes
    _path: Path = attr.ib(repr=str)
    _start: Dict[str, Any] = attr.ib(repr=str)
    _end: Dict[str, Any] = attr.ib(repr=str)
    _extra: Dict[str, Any] = attr.ib(repr=False)
    _lines_cache: Dict[Tuple[str, str], List[str]] = attr.ib(repr=False)

    # optional attributes
    _is_ignored: Optional[bool] = attr.ib(default=None)

    @classmethod
    def from_pattern_match(
        cls,
        rule_id: str,
        pattern_match: PatternMatch,
        message: str,
        metadata: Dict[str, Any],
        severity: str,
        fix: Optional[str],
        fix_regex: Optional[Dict[str, Any]],
    ) -> "RuleMatch":
        path = pattern_match.path
        start = pattern_match.start
        end = pattern_match.end

        # note that message in extra is still the old value defined before metavar interpolation
        extra = pattern_match.extra
        return cls(
            rule_id,
            pattern_match,
            message,
            metadata,
            severity,
            fix,
            fix_regex,
            path,
            start,
            end,
            extra,
            {},
        )

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
    def severity(self) -> str:
        return self._severity

    @property
    def start(self) -> Dict[str, Any]:
        return self._start

    @property
    def end(self) -> Dict[str, Any]:
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
        start_line = self.start["line"] - 1
        end_line = self.end["line"]

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
