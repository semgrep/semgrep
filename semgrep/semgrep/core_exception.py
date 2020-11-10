from pathlib import Path
from typing import Any
from typing import Dict

from semgrep.error import LexicalError
from semgrep.error import MatchTimeoutError
from semgrep.error import OutOfMemoryError
from semgrep.error import SemgrepError
from semgrep.error import SourceParseError
from semgrep.rule_lang import Position
from semgrep.rule_lang import SourceTracker
from semgrep.rule_lang import Span


class CoreException:
    def __init__(
        self,
        check_id: str,
        path: Path,
        start: Position,
        end: Position,
        extra: Dict[str, Any],
        language: str,
        rule_id: str,
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
        self._rule_id = rule_id

        if "message" not in extra or "line" not in extra:
            # in pfff/h_program-lang/R2c.ml these fields always exist
            raise ValueError("SemgrepCore error extra field missing information")
        self._extra = extra

    @classmethod
    def from_json(  # type: ignore
        cls, json_obj: Dict[str, Any], language: str, rule_id: str
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

        # Semgrep-Core caches TimeoutErrors as FatalErrors
        # Hack to treat these as TimeoutErrors
        check_id = json_obj["check_id"]
        if check_id == "FatalError" and "Timeout" in json_obj["extra"]["message"]:
            check_id = "Timeout"

        return cls(
            check_id,
            Path(json_obj["path"]),
            start_pos,
            end_pos,
            json_obj["extra"],
            language,
            rule_id,
        )

    def into_semgrep_error(self) -> SemgrepError:
        if self._check_id == "Timeout":
            return MatchTimeoutError(self._path, self._rule_id)
        elif self._check_id == "OutOfMemory":
            return OutOfMemoryError(self._path, self._rule_id)
        elif self._check_id == "LexicalError":
            return LexicalError(self._path, self._rule_id)
        else:
            with open(self._path, errors="replace") as f:
                file_hash = SourceTracker.add_source(f.read())
            error_span = Span(
                start=self._start,
                end=self._end,
                source_hash=file_hash,
                file=str(self._path),
            )
            return SourceParseError(
                short_msg="parse error",
                long_msg=f"Could not parse {self._path.name} as {self._language}",
                spans=[error_span],
                help="If the code appears to be valid, this may be a semgrep bug.",
            )
