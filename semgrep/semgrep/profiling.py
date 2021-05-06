from typing import Dict
from typing import NamedTuple

Semgrep_run = NamedTuple("Semgrep_run", [("rule", str), ("target", str)])

Times = NamedTuple(
    "Times", [("parse_time", float), ("match_time", float), ("run_time", float)]
)


class ProfilingData:
    def __init__(self) -> None:
        self._match_time_matrix: Dict[Semgrep_run, Times] = {}

    def get_times(self, rule: str, target: str) -> Times:
        return self._match_time_matrix.get(
            Semgrep_run(rule=rule, target=target),
            Times(parse_time=0.0, match_time=0.0, run_time=0.0),
        )

    def set_times(self, rule: str, target: str, times: Times) -> None:
        self._match_time_matrix[Semgrep_run(rule=rule, target=target)] = times
