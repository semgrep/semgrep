from __future__ import annotations

from datetime import date
from datetime import timedelta
from typing import Iterator
from typing import Tuple

def daterange(
    start: date,
    stop: date,
    step: tuple[int, int, int] = ...,
    inclusive: bool = ...,
) -> Iterator[date]: ...
def total_seconds(td: timedelta) -> float: ...
