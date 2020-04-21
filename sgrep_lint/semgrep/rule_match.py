from pathlib import Path
from typing import Any
from typing import Dict

from semgrep.pattern_match import PatternMatch


class RuleMatch:
    """
        A section of code that matches a single rule (which is potentially many patterns)
    """

    def __init__(self, id: str, message: str, pattern_match: PatternMatch) -> None:
        self._id = id
        self._message = message

        self._path = pattern_match.path
        self._start = pattern_match.start
        self._end = pattern_match.end

        self._extra = pattern_match.extra

        self._pattern_match = pattern_match

    @property
    def path(self) -> Path:
        return self._path

    @property
    def extra(self) -> Dict[str, Any]:
        return self._extra

    @property
    def metavars(self) -> Dict[str, Any]:
        return self._extra["metavars"]

    @property
    def start(self) -> Dict[str, Any]:
        return self._start

    @property
    def end(self) -> Dict[str, Any]:
        return self._end

    def to_json(self) -> Dict[str, Any]:
        json_obj = self._pattern_match._raw_json
        json_obj["check_id"] = self._id
        json_obj["extra"]["message"] = self._message
        json_obj["start"] = self._start
        json_obj["end"] = self._end
        return json_obj
