import semgrep.types as types

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
    def parse(cls, raw_json: types.JsonObject) -> "CoreLocation": ...

class RuleMatch:
    """
    A section of code that matches a single rule (which is potentially many patterns)
    """

    ...
