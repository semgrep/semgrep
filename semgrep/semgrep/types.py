from enum import auto
from enum import Enum
from typing import Any
from typing import Mapping
from typing import Sequence

from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch

JsonObject = Mapping[str, Any]

RuleMatchMap = Mapping[Rule, Sequence[RuleMatch]]


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
