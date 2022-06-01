from collections import defaultdict
from pathlib import Path
from typing import Any
from typing import FrozenSet
from typing import Mapping
from typing import TYPE_CHECKING

from attrs import field
from attrs import frozen

if TYPE_CHECKING:
    from semgrep.rule_match import RuleMatchMap

JsonObject = Mapping[str, Any]

Targets = FrozenSet[Path]


@frozen
class FilteredFiles:
    """
    The return value of functions that filters target files.
    """

    kept: Targets
    removed: Targets = field(factory=frozenset)


@frozen
class FilteredMatches:
    """
    The return value of functions that filter matches files.
    """

    kept: "RuleMatchMap"
    removed: "RuleMatchMap" = field(factory=lambda: defaultdict(list))
