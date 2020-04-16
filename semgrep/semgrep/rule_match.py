from pathlib import Path
from typing import Any
from typing import Dict
from typing import List
from typing import Optional

from semgrep.pattern_match import PatternMatch
from semgrep.util import fetch_lines_in_file


class RuleMatch:
    """
        A section of code that matches a single rule (which is potentially many patterns)
    """

    def __init__(
        self,
        id: str,
        message: str,
        pattern_match: PatternMatch,
        *,
        metadata: Dict[str, Any],
        file_lines: List[str],
    ) -> None:
        self._id = id
        self._message = message
        self._metadata = metadata
        self._file_lines = file_lines

        self._path = pattern_match.path
        self._start = pattern_match.start
        self._end = pattern_match.end

        self._extra = pattern_match.extra

        self._pattern_match = pattern_match

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
    def metavars(self) -> Dict[str, Any]:
        return self._extra["metavars"]

    @property
    def start(self) -> Dict[str, Any]:
        return self._start

    @property
    def end(self) -> Dict[str, Any]:
        return self._end

    @property
    def raw_lines(self) -> Dict[str, Any]:
        return self._end

    def to_json(self) -> Dict[str, Any]:
        json_obj = self._pattern_match._raw_json.copy()

        del json_obj["extra"]["metavars"]

        json_obj["check_id"] = self._id
        json_obj["extra"]["message"] = self._message
        json_obj["extra"]["metadata"] = self._metadata
        json_obj["extra"]["file_lines"] = self._file_lines
        json_obj["start"] = self._start
        json_obj["end"] = self._end
        return json_obj
