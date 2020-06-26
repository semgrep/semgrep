from pathlib import Path
from typing import Any
from typing import Dict

from semgrep.rule_lang import Position


class CoreException:
    def __init__(
        self,
        check_id: str,
        path: Path,
        start: Position,
        end: Position,
        extra: Dict[str, Any],
        language: str,
    ):
        """
            Object that encapsulates exception information returned by
            semgrep-core
        """
        self._check_id = check_id
        self._path = path
        self._start = start
        self._end = end
        self._language = language

        if "message" not in extra or "line" not in extra:
            # in pfff/h_program-lang/R2c.ml these fields always exist
            raise ValueError("SemgrepCore error extra field missing information")
        self._extra = extra

    @classmethod
    def from_json(  # type: ignore
        cls, json_obj: Dict[str, Any], language: str
    ) -> "CoreException":
        if {"check_id", "path", "start", "end", "extra"}.difference(
            json_obj.keys()
        ) != set():
            raise ValueError(f"cannot parse {json_obj} as {cls.__name__}")

        start = json_obj["start"]
        end = json_obj["end"]
        if (
            "line" not in start
            or "col" not in start
            or "line" not in end
            or "col" not in end
        ):
            raise ValueError(f"cannot parse {json_obj} as {cls.__name__}")

        start_pos = Position(start["line"], start["col"])
        end_pos = Position(end["line"], end["col"])

        return cls(
            json_obj["check_id"],
            Path(json_obj["path"]),
            start_pos,
            end_pos,
            json_obj["extra"],
            language,
        )

    def __str__(self) -> str:
        header = (
            f"--> Failed to parse {self._path}:{self._start.line} as {self._language}"
        )
        line_1 = self._extra["line"]
        start_col = self._start.col
        line_2 = " " * (start_col - 1) + "^"
        return "\n".join([header, line_1, line_2])
