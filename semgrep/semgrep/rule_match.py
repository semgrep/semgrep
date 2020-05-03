from pathlib import Path
from typing import Any
from typing import Dict
from typing import Optional

from semgrep.pattern_match import PatternMatch


class RuleMatch:
    """
        A section of code that matches a single rule (which is potentially many patterns)
    """

    def __init__(
        self,
        id: str,
        pattern_match: PatternMatch,
        *,
        message: str,
        metadata: Dict[str, Any],
        severity: str,
        fix: Optional[str],
    ) -> None:
        self._id = id
        self._message = message
        self._metadata = metadata
        self._severity = severity
        self._fix = fix

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
        return self._extra.get("metavars", {})

    @property
    def fix(self) -> Optional[str]:
        return self._fix

    @property
    def fix(self) -> Optional[str]:
        return self._fix

    @property
    def start(self) -> Dict[str, Any]:
        return self._start

    @property
    def end(self) -> Dict[str, Any]:
        return self._end

    @property
    def should_fail_run(self) -> bool:
        return self._severity in {"WARNING", "ERROR"}

    def to_json(self) -> Dict[str, Any]:
        json_obj = self._pattern_match._raw_json
        json_obj["check_id"] = self._id
        json_obj["extra"]["message"] = self._message
        json_obj["extra"]["metadata"] = self._metadata
        json_obj["extra"]["severity"] = self._severity
        if self._fix:
            json_obj["extra"]["fix"] = self._fix
        json_obj["start"] = self._start
        json_obj["end"] = self._end
        return json_obj
