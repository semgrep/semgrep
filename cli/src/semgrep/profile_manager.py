from __future__ import annotations

import time
from typing import Any
from typing import Dict


class ProfileManager:
    """
    An incredibly rudimentary tool for tracking profiled calls with an associated named key
    """

    calls: dict[str, float] = {}

    # This method is an even more rudimentary tool for profiling
    # function calls
    def save(self, key: str, start_time: float) -> Any:
        self.calls[key] = time.time() - start_time

    def dump_stats(self) -> dict[str, float]:
        return self.calls
