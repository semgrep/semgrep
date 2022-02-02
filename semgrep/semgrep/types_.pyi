from typing import Any, Dict, List, Sequence, Set, Tuple, Optional, Collection
from typing import Mapping, NewType
from enum import Enum

JsonObject = Mapping[str, Any]

RuleId = NewType("RuleId", str)

class MetricsState(Enum):
    """
    Configures metrics upload.

    ON - Metrics always sent
    OFF - Metrics never sent
    AUTO - Metrics only sent if config is pulled from the server
    """

    ON = ...
    OFF = ...
    AUTO = ...
