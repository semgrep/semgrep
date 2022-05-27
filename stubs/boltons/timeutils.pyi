from datetime import date, timedelta
from typing import Iterator, Tuple

def daterange(
    start: date,
    stop: date,
    step: Tuple[int, int, int] = ...,
    inclusive: bool = ...,
) -> Iterator[date]: ...
def total_seconds(td: timedelta) -> float: ...
