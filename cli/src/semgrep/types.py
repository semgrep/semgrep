#
# Copyright (c) 2021-2025 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
import dataclasses
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
from pathlib import PurePosixPath
from typing import Any
from typing import FrozenSet
from typing import Iterable
from typing import Mapping
from typing import Optional
from typing import Set
from typing import TYPE_CHECKING

from attrs import field
from attrs import frozen

import semgrep.semgrep_interfaces.semgrep_output_v1 as out

if TYPE_CHECKING:
    from semgrep.rule_match import RuleMatchMap

JsonObject = Mapping[str, Any]


@dataclass(frozen=True)
class TargetInfo:
    """A weaker version of Target that supports targets handled only on
    the Python side that won't be passed to semgrep-core. These targets
    may leave 'original' unset."""

    fpath: Path
    original: Optional[out.Fppath]

    def __hash__(self) -> int:
        return hash(self.fpath)

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, TargetInfo):
            return False
        return self.fpath == other.fpath


@dataclass(frozen=True)
class Target(TargetInfo):
    """
    A wrapper around out.Fppath such that paths are available as
    the Path type rather than strings.

    For the purpose of this definition, a Target is a file that is subject
    to being directly analyzed by a rule i.e. subject to being filtered
    by paths.include and paths.exclude. This is why we need to carry around
    the ppath (path from the project root).
    """

    ppath: PurePosixPath
    original: out.Fppath

    def __hash__(self) -> int:
        return hash(self.fpath)

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, TargetInfo):
            return False
        return self.fpath == other.fpath

    # This is needed in tests for 'assert a == b' to produce useful diffs
    # when 'a' and 'b' are frozensets.
    def __lt__(self, other: object) -> bool:
        if not isinstance(other, TargetInfo):
            return False
        return self.fpath < other.fpath


Targets = FrozenSet[Target]


def fake_target_of_path(path: Path) -> Target:
    """For tests only!"""
    fake_ppath_str = path.as_posix()
    if not fake_ppath_str.startswith("/"):
        fake_ppath_str = "/" + fake_ppath_str
    fake_ppath = PurePosixPath(fake_ppath_str)
    return Target(
        fpath=path,
        ppath=fake_ppath,
        original=out.Fppath(
            fpath=out.Fpath(str(path)), ppath=out.Ppath(fake_ppath_str)
        ),
    )


def fake_targets_of_paths(paths: Iterable[Path]) -> FrozenSet[Target]:
    """For tests only!"""
    return frozenset(fake_target_of_path(path) for path in paths)


def fpaths_of_targets(targets: Iterable[Target]) -> FrozenSet[Path]:
    return frozenset(target.fpath for target in targets)


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
    Immutable container for selected targets that were obtained.

    It used to be needed to store targets obtained with Semgrepignore v1
    and v2. Eliminate this class if it gets in the way.
    """

    targets: frozenset[Target] = dataclasses.field(default_factory=frozenset)

    def fpaths(self) -> FrozenSet[Path]:
        return frozenset(target.fpath for target in self.targets)

    def ppaths(self) -> FrozenSet[PurePosixPath]:
        return frozenset(target.ppath for target in self.targets)


@dataclass
class TargetAccumulator:
    """
    Accumulate targets that are scanned by Semgrep rules.
    If no rule applies, to some target, it won't show up here.
    """

    targets: Set[Target] = dataclasses.field(default_factory=set)


@dataclass
class TargetInfoAccumulator:
    """
    Accumulate targets that were scanned, for reporting purposes.
    """

    targets: Set[TargetInfo] = dataclasses.field(default_factory=set)


# conversion from class to superclass; feel free to improve
def target_info_acc_of_target_acc(acc: TargetAccumulator) -> TargetInfoAccumulator:
    targets = set(
        TargetInfo(fpath=target.fpath, original=target.original)
        for target in acc.targets
    )
    return TargetInfoAccumulator(targets=targets)
