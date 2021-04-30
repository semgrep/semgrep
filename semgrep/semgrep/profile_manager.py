import collections
import time
from typing import Any
from typing import Dict
from typing import List


class ProfileManager:
    """
    An incredibly rudimentary tool for tracking profiled calls with an associated named key
    """

    calls: Dict[str, List[float]] = collections.defaultdict(list)

    def track(self, key: str, callable: Any, *args: Any, **kwargs: Any) -> Any:
        start_time = time.time()
        result = callable(*args, **kwargs)
        self.calls[key].append(time.time() - start_time)
        return result

    # This method is an even more rudimentary tool for profiling
    # function calls
    def save(self, key: str, start_time: float) -> Any:
        self.calls[key].append(time.time() - start_time)

    def dump_stats(self) -> Dict[str, List[float]]:
        return dict(sorted(self.calls.items(), key=lambda x: x[1]))
