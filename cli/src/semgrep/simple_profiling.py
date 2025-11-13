#
# Copyright (c) 2025 Semgrep Inc.
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
"""
Simple profiling similar to Profiling.ml

This allows collecting how long the program spent on a given piece of
code and how many times.

Profiling data is imported from OCaml and aggregated via the import function.
"""
import atexit
from collections import defaultdict
from contextlib import contextmanager
from dataclasses import dataclass
from functools import wraps
from time import time
from typing import Callable
from typing import Dict
from typing import Iterator
from typing import ParamSpec
from typing import TypeVar

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

# Whether automatic reporting is enabled. Data collection takes place
# no matter what.
enabled_simple_profiling = False


@dataclass
class ProfilingEntry:
    """Mutable accumulator for execution time"""

    total_time: float = 0.0
    count: int = 0


# Global accumulator
entries: Dict[str, ProfilingEntry] = defaultdict(ProfilingEntry)


def _add(name: str, elapsed: float, count: int = 1) -> None:
    e = entries[name]
    e.total_time = e.total_time + elapsed
    e.count = e.count + count


T = TypeVar("T")


@contextmanager
def profiling(name: str) -> Iterator[None]:
    """Measure how long it takes to run a block of code

    Usage:
      with profiling("foo"):
        ...
    """
    t1 = time()
    try:
        yield
    finally:
        t2 = time()
        _add(name, t2 - t1)


# Type variable to represent the type of function arguments
P = ParamSpec("P")

# Type variable to represent the return type of a function
R = TypeVar("R")


def simple_profiling(func: Callable[P, R]) -> Callable[P, R]:
    """A decorator for measuring how much time is spent in the function or method

    Usage:
      @simple_profiling
      def foo(...):
        ...
    """

    @wraps(func)
    def wrapper(*args: P.args, **kwargs: P.kwargs) -> R:
        t1 = time()
        try:
            result = func(*args, **kwargs)
            return result
        finally:
            t2 = time()
            _add(f"{func.__module__}.{func.__qualname__}", t2 - t1)

    return wrapper


def export_simple_profiling() -> list[out.ProfilingEntry]:
    """Produce a list sorted by decreasing total_time"""
    return sorted(
        [
            out.ProfilingEntry(name=name, total_time=e.total_time, count=e.count)
            for name, e in entries.items()
        ],
        key=lambda e: e.total_time,
        reverse=True,
    )


def import_simple_profiling(entries: list[out.ProfilingEntry]) -> None:
    """Import profiling data from semgrep-core"""
    for e in entries:
        _add(name=e.name, elapsed=e.total_time, count=e.count)


def _make_report() -> str:
    """Make a report of the profiling data gathered so far"""
    buf: list[str] = []
    buf.append("-" * 78 + "\n")
    buf.append("Simple profiling results\n")
    buf.append(
        "                                                       total time /      count\n"
    )
    for e in export_simple_profiling():
        buf.append(f"{e.name:<50} : {e.total_time:10.3f} s / {e.count:10d}\n")
    buf.append("-" * 78 + "\n")
    return "".join(buf)


def _report() -> None:
    """Print a report to stderr in a human-readable format"""
    if enabled_simple_profiling:
        logger.info(_make_report())


atexit.register(_report)
