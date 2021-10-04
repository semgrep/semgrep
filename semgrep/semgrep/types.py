from enum import auto
from enum import Enum
from typing import Any
from typing import Mapping
from typing import NewType

JsonObject = Mapping[str, Any]

RuleId = NewType("RuleId", str)


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
