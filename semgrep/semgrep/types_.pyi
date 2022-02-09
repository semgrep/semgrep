from typing import Any, Dict, List, Sequence, Set, Tuple, Optional, Collection
from typing import Mapping, NewType, FrozenSet
from pathlib import Path
from enum import Enum
import attr

JsonObject = Mapping[str, Any]

RuleId = NewType("RuleId", str)

Targets = FrozenSet[Path]

@attr.s(auto_attribs=True, frozen=True)
class FilteredTargets:
    """
    The return value of functions that filters target paths.
    """

    kept: Targets
    removed: Targets = attr.ib(factory=frozenset)

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
