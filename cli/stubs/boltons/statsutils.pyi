from __future__ import annotations

from typing import Any
from typing import Callable
from typing import List
from typing import Optional
from typing import Tuple
from typing import Type
from typing import Union

def format_histogram_counts(
    bin_counts: list[tuple[float, int]],
    width: int | None = ...,
    format_bin: Callable | None = ...,
) -> str: ...

class Stats:
    def __init__(
        self,
        data: list[int] | list[float] | range,
        default: float = ...,
        use_copy: bool = ...,
        is_sorted: bool = ...,
    ) -> None: ...
    def _calc_min(self) -> float: ...
    def _get_bin_bounds(
        self, count: int | None = ..., with_max: bool = ...
    ) -> list[float]: ...
    def _get_pow_diffs(self, power: int) -> list[float]: ...
    @staticmethod
    def _get_quantile(
        sorted_data: list[int] | list[float], q: float
    ) -> float: ...
    def _get_sorted_data(self) -> list[int] | list[float]: ...
    def format_histogram(
        self, bins: list[float] | int | None = ..., **kw: Any
    ) -> str: ...
    def get_histogram_counts(
        self, bins: list[float] | int | None = ..., **kw: Any
    ) -> list[tuple[float, int]]: ...
    def get_quantile(self, q: float) -> float: ...

class _StatsProperty:
    def __get__(
        self, obj: Stats | None, objtype: type[Stats] | None = ...
    ) -> _StatsProperty | float: ...
