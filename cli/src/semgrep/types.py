import dataclasses
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import Any
from typing import FrozenSet
from typing import Mapping
from typing import Set
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


@dataclass
class SelectedTargets:
    """
    Immutable container for selected targets that were obtained with
    Semgrepignore v1 and v2.
    """

    use_semgrepignore_v2: bool
    v1_targets: frozenset[Path] = dataclasses.field(default_factory=frozenset)
    v2_targets: frozenset[Path] = dataclasses.field(default_factory=frozenset)

    def targets(self) -> frozenset[Path]:
        """Return either Semgrepignore v1 targets or v2 targets
        depending on the configuration"""
        if self.use_semgrepignore_v2:
            return self.v2_targets
        else:
            return self.v1_targets


@dataclass
class TargetAccumulator:
    """
    Accumulate targets that are scanned by Semgrep rules.
    If no rule applies, to some target, it won't show up here.
    """

    use_semgrepignore_v2: bool
    v1_targets: Set[Path] = dataclasses.field(default_factory=set)
    v2_targets: Set[Path] = dataclasses.field(default_factory=set)

    def targets(self) -> Set[Path]:
        """Return either Semgrepignore v1 targets or v2 targets
        depending on the configuration
        """
        if self.use_semgrepignore_v2:
            return self.v2_targets
        else:
            return self.v1_targets
