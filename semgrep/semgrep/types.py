from enum import auto
from enum import Enum
from pathlib import Path
from typing import Any
from typing import FrozenSet
from typing import Mapping
from typing import NewType

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

    ON = auto()
    OFF = auto()
    AUTO = auto()
