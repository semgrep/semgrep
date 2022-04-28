from enum import auto
from enum import Enum
from pathlib import Path
from typing import Any
from typing import FrozenSet
from typing import Mapping

from attrs import field
from attrs import frozen

JsonObject = Mapping[str, Any]

Targets = FrozenSet[Path]


@frozen
class FilteredFiles:
    """
    The return value of functions that filters target files.
    """

    kept: Targets
    removed: Targets = field(factory=frozenset)


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
