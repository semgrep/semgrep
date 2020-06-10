from pathlib import Path
from typing import Any
from typing import Dict

from semgrep.semgrep_types import PatternId
from semgrep.semgrep_types import Range


class PatternMatch:
    """
        Encapsulates a section of code that matches a single pattern
    """

    def __init__(self, raw_json: Dict[str, Any]) -> None:
        self._raw_json = raw_json

    @property
    def rule_index(self) -> int:
        return int(self._raw_json["check_id"].split(".")[0])

    @property
    def id(self) -> PatternId:
        return PatternId(".".join(self._raw_json["check_id"].split(".")[1:]))

    @property
    def path(self) -> Path:
        return Path(self._raw_json["path"])

    @property
    def metavars(self) -> Dict[str, Any]:
        return self.extra.get("metavars", {})

    @property
    def extra(self) -> Dict[str, Any]:
        return self._raw_json["extra"]

    @property
    def vars(self) -> Dict[str, Any]:
        metavars = {v: data.get("unique_id", {}) for v, data in self.metavars.items()}
        return {v: uid.get("sid", uid.get("md5sum")) for v, uid in metavars.items()}

    @property
    def range(self) -> Range:
        return Range(
            self._raw_json["start"]["offset"],
            self._raw_json["end"]["offset"],
            self.vars,
        )

    @property
    def start(self) -> Dict[str, Any]:
        # https://docs.r2c.dev/en/latest/api/output.html does not support offset at the moment
        start = dict(self._raw_json["start"])
        if "offset" in start:
            del start["offset"]
        return start

    @property
    def end(self) -> Dict[str, Any]:
        # https://docs.r2c.dev/en/latest/api/output.html does not support offset at the moment
        end = dict(self._raw_json["end"])
        if "offset" in end:
            del end["offset"]
        return end

    def __repr__(self) -> str:
        return f"<{self.__class__.__name__} id={self.id} range={self.range}>"
