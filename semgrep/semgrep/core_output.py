from collections import defaultdict
from pathlib import Path
from typing import Dict
from typing import List
from typing import NewType
from typing import Optional

import attr

from semgrep.types import JsonObject


RuleId = NewType("RuleId", str)
CoreErrorMessage = NewType("CoreErrorMessage", str)
SkipReason = NewType("SkipReason", str)
SkipDetails = NewType("SkipDetails", str)


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


@attr.s(auto_attribs=True, frozen=True)
class MetavarValue:
    start: CoreLocation
    end: CoreLocation

    @classmethod
    def parse(cls, raw_json: JsonObject) -> "MetavarValue":
        start = CoreLocation.parse(raw_json["start"])
        end = CoreLocation.parse(raw_json["end"])
        return cls(start, end)


@attr.s(auto_attribs=True, frozen=True)
class CoreMetavars:
    metavars: Dict[str, MetavarValue]

    @classmethod
    def parse(cls, raw_json: JsonObject) -> "CoreMetavars":
        metavars: Dict[str, MetavarValue] = {}
        for key, value in raw_json.items():
            metavars[key] = MetavarValue.parse(value)
        return cls(metavars)

    def get(self, key: str) -> MetavarValue:
        return self.metavars[key]

    def keys(self) -> List[str]:
        return list(self.metavars.keys())


@attr.s(auto_attribs=True, frozen=True)
class CoreMatch:
    """
    Encapsulates finding returned by semgrep-core
    """

    rule_id: RuleId
    path: Path
    start: CoreLocation
    end: CoreLocation
    metavars: CoreMetavars

    @classmethod
    def parse(cls, raw_json: JsonObject) -> "CoreMatch":
        rule_id = RuleId(raw_json["check_id"])
        path_str = raw_json["path"]
        assert isinstance(path_str, str)
        path = Path(path_str)
        start = CoreLocation.parse(raw_json["start"])
        end = CoreLocation.parse(raw_json["end"])
        metavars = CoreMetavars.parse(raw_json.get("extra", {}).get("metavars"))
        return cls(rule_id, path, start, end, metavars)


@attr.s(auto_attribs=True, frozen=True)
class CoreError:
    """"""

    rule_id: Optional[RuleId]
    path: Optional[Path]
    start: CoreLocation
    end: CoreLocation
    message: CoreErrorMessage

    @classmethod
    def parse(cls, raw_json: JsonObject) -> "CoreError":
        raw_rule_id = raw_json.get("check_id")
        rule_id = RuleId(raw_rule_id) if raw_rule_id else None
        raw_path = raw_json.get("path")
        path = Path(raw_path) if raw_path else None
        start = CoreLocation.parse(raw_json["start"])
        end = CoreLocation.parse(raw_json["end"])
        _extra = raw_json.get("extra", {})
        message = CoreErrorMessage(_extra.get("message", "<no error message>"))
        return cls(rule_id, path, start, end, message)


@attr.s(auto_attribs=True, frozen=True)
class CoreSkipped:
    rule_id: RuleId
    path: Path
    reason: SkipReason
    details: SkipDetails

    @classmethod
    def parse(cls, raw_json: JsonObject) -> "CoreSkipped":
        rule_id = RuleId(raw_json["check_id"])
        path = Path(raw_json["path"])
        reason = SkipReason(raw_json["reason"])
        details = SkipDetails(raw_json["details"])
        return cls(rule_id, path, reason, details)


@attr.s(auto_attribs=True, frozen=True)
class CoreOutput:
    """
    Parses output of semgrep-core
    """

    matches_by_rule: Dict[RuleId, List[CoreMatch]]
    errors: List[CoreError]
    skipped: List[CoreSkipped]

    @classmethod
    def parse(cls, raw_json: JsonObject) -> "CoreOutput":
        parsed_errors = []
        errors = raw_json["errors"]
        for error in errors:
            parsed_errors.append(CoreError.parse(error))

        parsed_matches_by_rule = defaultdict(list)
        matches = raw_json["matches"]
        for match in matches:
            parsed_match = CoreMatch.parse(match)
            rule_id = parsed_match.rule_id
            parsed_matches_by_rule[rule_id].append(parsed_match)

        parsed_skipped = []
        skipped = raw_json["skipped"]
        for skip in skipped:
            parsed_skipped.append(CoreSkipped.parse(skip))

        return cls(dict(parsed_matches_by_rule), parsed_errors, parsed_skipped)


y = {
    "matches": [
        {
            "check_id": "println",
            "path": "./basic_equality.java",
            "start": {"line": 6, "col": 13, "offset": 155},
            "end": {"line": 6, "col": 46, "offset": 188},
            "extra": {"message": "Using println", "metavars": {}, "lines": []},
        },
        {
            "check_id": "equality",
            "path": "./basic_equality.java",
            "start": {"line": 3, "col": 13, "offset": 76},
            "end": {"line": 3, "col": 19, "offset": 82},
            "extra": {
                "message": "Comparing two variables",
                "metavars": {
                    "$Y": {
                        "start": {"line": 3, "col": 18, "offset": 81},
                        "end": {"line": 3, "col": 19, "offset": 82},
                        "abstract_content": "y",
                        "unique_id": {"type": "id", "sid": 2},
                    },
                    "$X": {
                        "start": {"line": 3, "col": 13, "offset": 76},
                        "end": {"line": 3, "col": 14, "offset": 77},
                        "abstract_content": "x",
                        "unique_id": {"type": "id", "sid": 1},
                    },
                },
                "lines": [],
            },
        },
        {
            "check_id": "println",
            "path": "./hello_world.java",
            "start": {"line": 3, "col": 9, "offset": 73},
            "end": {"line": 3, "col": 44, "offset": 108},
            "extra": {"message": "Using println", "metavars": {}, "lines": []},
        },
        {
            "check_id": "equality",
            "path": "./parse_error.java",
            "start": {"line": 2, "col": 14, "offset": 57},
            "end": {"line": 2, "col": 20, "offset": 63},
            "extra": {
                "message": "Comparing two variables",
                "metavars": {
                    "$Y": {
                        "start": {"line": 2, "col": 19, "offset": 62},
                        "end": {"line": 2, "col": 20, "offset": 63},
                        "abstract_content": "y",
                        "unique_id": {
                            "type": "AST",
                            "md5sum": "ce1472eb18f617c55928ddaf2c8f6b2c",
                        },
                    },
                    "$X": {
                        "start": {"line": 2, "col": 14, "offset": 57},
                        "end": {"line": 2, "col": 15, "offset": 58},
                        "abstract_content": "x",
                        "unique_id": {
                            "type": "AST",
                            "md5sum": "96811ca43f052adf9eeb430fe0b4ce44",
                        },
                    },
                },
                "lines": [],
            },
        },
    ],
    "errors": [
        {
            "check_id": "ParseError",
            "path": "./parse_error.java",
            "start": {"line": 1, "col": 28, "offset": 0},
            "end": {"line": 1, "col": 42, "offset": 14},
            "extra": {
                "message": "Syntax error",
                "line": "  public static bool main(int x, int y) {",
            },
        },
        {
            "check_id": "ParseError",
            "path": "./parse_error.java",
            "start": {"line": 1, "col": 28, "offset": 0},
            "end": {"line": 1, "col": 42, "offset": 14},
            "extra": {
                "message": "Syntax error",
                "line": "  public static bool main(int x, int y) {",
            },
        },
    ],
    "skipped": [
        {
            "check_id": "ParseError",
            "path": "./console_log.js",
            "reason": "wrong_language",
            "details": "target file doesn’t look like language Java",
        },
        {
            "check_id": "ParseError",
            "path": "./jquery-ui.min.js",
            "reason": "wrong_language",
            "details": "target file doesn’t look like language Java",
        },
        {
            "check_id": "ParseError",
            "path": "./rules.yaml",
            "reason": "wrong_language",
            "details": "target file doesn’t look like language Java",
        },
    ],
    "stats": {"okfiles": 2, "errorfiles": 1},
}
x = CoreOutput.parse(y)
print(x)
