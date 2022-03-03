from typing import Any, Dict, List, Optional, Sequence, Set, Tuple

from pathlib import Path
import semgrep.types as types
import semgrep.constants as constants

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

    # TODO: caller should use metadata(), or we should use @dataclass/@define?
    _metadata: Dict[str, Any] = ...
    _is_ignored: Optional[bool] = ...

    @property
    def id(self) -> str: ...
    @property
    def path(self) -> Path: ...
    @property
    def extra(self) -> Dict[str, Any]: ...
    @property
    def fix(self) -> Optional[str]: ...
    @property
    def fix_regex(self) -> Optional[Dict[str, Any]]: ...
    @property
    def message(self) -> str: ...
    @property
    def metadata(self) -> Dict[str, Any]: ...
    @property
    def severity(self) -> constants.RuleSeverity: ...
    @property
    def start(self) -> CoreLocation: ...
    @property
    def end(self) -> CoreLocation: ...
    @property
    def is_ignored(self) -> Optional[bool]: ...
    @property
    def lines(self) -> List[str]: ...
    @property
    def previous_line(self) -> str: ...
