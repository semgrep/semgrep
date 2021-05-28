from pathlib import Path
from typing import Dict
from typing import NamedTuple
from typing import Optional

from semgrep.rule import Rule

Semgrep_run = NamedTuple("Semgrep_run", [("rule", Rule), ("target", Path)])

Times = NamedTuple(
    "Times", [("parse_time", float), ("match_time", float), ("run_time", float)]
)


class ProfilingData:
    def __init__(self) -> None:
        self._match_time_matrix: Dict[Semgrep_run, Times] = {}
        self._rule_parse_times: Dict[Rule, float] = {}
        self._file_parse_times: Dict[Path, float] = {}

        self._rule_match_times: Dict[Rule, float] = {}
        self._rule_run_times: Dict[Rule, float] = {}
        self._rule_bytes_scanned: Dict[Rule, int] = {}
        self._file_match_times: Dict[Path, float] = {}
        self._file_run_times: Dict[Path, float] = {}
        self._file_num_times_scanned: Dict[Path, int] = {}

    def get_run_times(self, rule: Rule, target: Path) -> Times:
        return self._match_time_matrix.get(
            Semgrep_run(rule=rule, target=target),
            Times(parse_time=0.0, match_time=0.0, run_time=0.0),
        )

    def get_file_parse_time(self, target: Path) -> Optional[float]:
        """
        Return time taken to parse a file. This is the max of all
        times reported to parse the file from semgrep-core since
        it caches the parsed file.

        Return None if target has no reported parse time
        """
        return self._file_parse_times.get(target)

    def get_rule_match_time(self, rule: Rule) -> Optional[float]:
        """
        Return total match time for a given rule over all the files scanned
        with said rule

        Return None if RULE has no timing information saved
        """
        return self._rule_match_times.get(rule)

    def get_rule_run_time(self, rule: Rule) -> Optional[float]:
        """
        Return total run time for a given rule over all the files scanned with
        said rule

        Return None if RULE has no timing information saved
        """
        return self._rule_run_times.get(rule)

    def get_rule_bytes_scanned(self, rule: Rule) -> int:
        """
        Return total number of bytes scanned by a given rule
        """
        return self._rule_bytes_scanned.get(rule, 0)

    def get_file_match_time(self, target: Path) -> Optional[float]:
        """
        Return total match time for a given file over all the rules that
        scanned the file

        Return None if TARGET has no timing information saved
        """
        return self._file_match_times.get(target)

    def get_file_run_time(self, target: Path) -> Optional[float]:
        """
        Return total run time for a given file over all the rules that
        scanned the file

        Return None if TARGET has no timing information saved
        """
        return self._file_run_times.get(target)

    def get_file_num_times_scanned(self, target: Path) -> int:
        """
        Returns number of times a file was scanned with rules.
        Assumes that each entry to set_run_times means a target
        was scanned once
        """
        return self._file_num_times_scanned.get(target, 0)

    def set_run_times(self, rule: Rule, target: Path, times: Times) -> None:
        num_bytes = target.stat().st_size

        self._rule_run_times[rule] = (
            self._rule_run_times.get(rule, 0.0) + times.run_time
        )
        self._rule_match_times[rule] = (
            self._rule_match_times.get(rule, 0.0) + times.match_time
        )
        self._rule_bytes_scanned[rule] = (
            self._rule_bytes_scanned.get(rule, 0) + num_bytes
        )

        self._file_num_times_scanned[target] = (
            self._file_num_times_scanned.get(target, 0) + 1
        )
        self._file_run_times[target] = (
            self._file_run_times.get(target, 0.0) + times.run_time
        )
        self._file_match_times[target] = (
            self._file_match_times.get(target, 0.0) + times.match_time
        )

        # File parse time is max of all parse times since others will be
        # cache hits
        self._file_parse_times[target] = max(
            times.parse_time, self._file_parse_times.get(target, 0.0)
        )

        self._match_time_matrix[Semgrep_run(rule=rule, target=target)] = times

    def get_rule_parse_time(self, rule: Rule) -> float:
        return self._rule_parse_times.get(rule, 0.0)

    def set_rule_parse_time(self, rule: Rule, parse_time: float) -> None:
        self._rule_parse_times[rule] = parse_time
