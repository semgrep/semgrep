#
# Copyright (c) 2020-2024 Semgrep Inc.
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
import time
from typing import Any
from typing import Dict


class ProfileManager:
    """
    An incredibly rudimentary tool for tracking profiled calls with an associated named key
    """

    calls: Dict[str, float] = {}

    # This method is an even more rudimentary tool for profiling
    # function calls
    def save(self, key: str, start_time: float) -> Any:
        self.calls[key] = time.time() - start_time

    def dump_stats(self) -> Dict[str, float]:
        return self.calls
