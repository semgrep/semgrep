import itertools
from pathlib import Path
from typing import Any
from typing import Dict
from typing import List
from typing import Optional

from junit_xml import TestCase

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
        fix_regex: Optional[Dict[str, Any]],
    ) -> None:
        self._id = id
        self._message = message
        self._metadata = metadata
        self._severity = severity
        self._fix = fix
        self._fix_regex = fix_regex

        self._path = pattern_match.path
        self._start = pattern_match.start
        self._end = pattern_match.end

        self._extra = (
            pattern_match.extra
        )  # note that message is still the old value defined before metavar interpolation

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
    def fix_regex(self) -> Optional[Dict[str, Any]]:
        return self._fix_regex

    @property
    def message(self) -> str:
        return self._message

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
    def lines(self) -> List[str]:
        """
            Return lines in file that this RuleMatch is referring to.

            Assumes file exists.  Note that start/end line is one-indexed
        """
        if "lines" in self.extra:
            return self.extra["lines"]

        with self.path.open(
            buffering=1, errors="replace"
        ) as fin:  # buffering=1 turns on line-level reads
            return list(itertools.islice(fin, self.start["line"] - 1, self.end["line"]))

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
        if self._fix_regex:
            json_obj["extra"]["fix_regex"] = self._fix_regex
        json_obj["start"] = self._start
        json_obj["end"] = self._end
        # self.lines already contains \n at the end of each line
        json_obj["extra"]["lines"] = "".join(self.lines).rstrip()

        return json_obj

    def to_junit_xml(self) -> Dict[str, Any]:
        test_case = TestCase(self.id, file=str(self.path), line=self.start["line"])
        test_case.add_failure_info(
            message=self.message, output=self.lines, failure_type=self.severity
        )
        return test_case

    def to_sarif(self) -> Dict[str, Any]:
        return {
            "ruleId": self.id,
            "message": {"text": self.message},
            "locations": [
                {
                    "physicalLocation": {
                        "artifactLocation": {
                            "uri": str(self.path),
                            "uriBaseId": "%SRCROOT%",
                        },
                        "region": {
                            "startLine": self.start["line"],
                            "startColumn": self.start["col"],
                            "endLine": self.end["line"],
                            "endColumn": self.end["col"],
                        },
                    }
                }
            ],
        }

    def __repr__(self) -> str:
        return f"<{self.__class__.__name__} id={self.id} start={str(self.start)} end={str(self.end)}>"
