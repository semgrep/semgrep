from enum import auto
from enum import Enum
from typing import Any
from typing import Mapping
from typing import NewType
from typing import Sequence
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from semgrep.rule import Rule
    from semgrep.rule_match import RuleMatch

JsonObject = Mapping[str, Any]

RuleId = NewType("RuleId", str)

RuleMatchMap = Mapping["Rule", Sequence["RuleMatch"]]


class MetricsState(Enum):
    """
    Configures metrics upload.

    ON - Metrics always sent
    OFF - Metrics never sent
    AUTO - Metrics only sent if config is pulled from the server
    """

    ON = auto()
    OFF = auto()
    AUTO = auto()
