from pathlib import Path
from typing import Any
from typing import Dict
from typing import Optional

from semgrep.semgrep_types import PatternId
from semgrep.semgrep_types import Range


class PatternMatch:
    """
    Encapsulates a section of code that matches a single pattern
    """

    def __init__(self, raw_json: Dict[str, Any]) -> None:
        self._raw_json = raw_json
        self._metavariable_values: Optional[Dict[str, str]] = None

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
    def metavariables(self) -> Dict[str, Any]:
        return self.extra.get("metavars", {})

    @property
    def extra(self) -> Dict[str, Any]:
        return self._raw_json["extra"]

    @property
    def metavariable_uids(self) -> Dict[str, Any]:
        def _get_uid(metavariable_data: Any) -> Any:
            try:
                return metavariable_data["unique_id"]["sid"]
            except KeyError:
                try:
                    return metavariable_data["unique_id"]["md5sum"]
                except KeyError:
                    return None

        return {
            metavariable: _get_uid(data)
            for metavariable, data in self.metavariables.items()
        }

    @property
    def range(self) -> Range:
        return Range(
            self._raw_json["start"]["offset"],
            self._raw_json["end"]["offset"],
            self.metavariable_uids,
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

    def _read_metavariable_values(self) -> Dict[str, str]:
        """
        Read self.path and lookup all values of metavariables in self.metavariables
        """
        result = {}

        # open path and ignore non-utf8 bytes. https://stackoverflow.com/a/56441652
        with open(self.path, errors="replace") as fd:
            for metavariable, metavariable_data in self.metavariables.items():
                # Offsets are start inclusive and end exclusive
                start_offset = metavariable_data["start"]["offset"]
                end_offset = metavariable_data["end"]["offset"]
                length = end_offset - start_offset

                fd.seek(start_offset)
                result[metavariable] = fd.read(length)

        return result

    def get_metavariable_value(self, metavariable: str) -> str:
        """
        Use metavariable's start and end to read into the file to find what the
        metavariable in this pattern match maps to in the file

        Assumes METAVARIABLE is a key in self.metavariables
        """
        if self._metavariable_values is None:
            self._metavariable_values = self._read_metavariable_values()

        return self._metavariable_values[metavariable]

    def __repr__(self) -> str:
        return f"<{self.__class__.__name__} id={self.id} range={self.range}>"
