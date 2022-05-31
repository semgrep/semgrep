from collections import defaultdict
from pathlib import Path
from typing import Dict
from typing import NamedTuple
from typing import Optional

import semgrep.output_from_core as core
from semgrep.rule import Rule


class Semgrep_run(NamedTuple):
    rule: core.RuleId
    target: Path


class Times(NamedTuple):
    parse_time: float = 0.0
    match_time: float = 0.0


class ProfilingData:
    def __init__(self) -> None:
        self._rules_parse_time: float = 0.0
        self._file_parse_time: Dict[Path, float] = defaultdict(float)
        self._file_run_time: Dict[Path, float] = defaultdict(float)
        self._match_time_matrix: Dict[Semgrep_run, Times] = defaultdict(Times)

        self._rule_match_times: Dict[core.RuleId, float] = defaultdict(float)
        self._rule_bytes_scanned: Dict[core.RuleId, int] = defaultdict(int)
        self._file_match_times: Dict[Path, float] = defaultdict(float)
        self._file_num_times_scanned: Dict[Path, int] = defaultdict(int)

    def get_run_times(self, rule: Rule, target: Path) -> Times:
        return self._match_time_matrix[Semgrep_run(rule=rule.id2, target=target)]

    def get_file_parse_time(self, target: Path) -> Optional[float]:
        """
        Return time taken to parse a file. This is the max of all
        times reported to parse the file from semgrep-core since
        it caches the parsed file.

        Return None if target has no reported parse time
        """
        return self._file_parse_time[target]

    def get_rule_match_time(self, rule: Rule) -> Optional[float]:
        """
        Return total match time for a given rule over all the files scanned
        with said rule

        Return None if RULE has no timing information saved
        """
        return self._rule_match_times[rule.id2]

    def get_rule_bytes_scanned(self, rule: Rule) -> int:
        """
        Return total number of bytes scanned by a given rule
        """
        return self._rule_bytes_scanned[rule.id2]

    def get_file_match_time(self, target: Path) -> Optional[float]:
        """
        Return total match time for a given file over all the rules that
        scanned the file

        Return None if TARGET has no timing information saved
        """
        return self._file_match_times[target]

    def get_file_run_time(self, target: Path) -> Optional[float]:
        """
        Return total run time for a given file over all the rules that
        scanned the file

        Return None if TARGET has no timing information saved
        """
        return self._file_run_time[target]

    def get_file_num_times_scanned(self, target: Path) -> int:
        """
        Returns number of times a file was scanned with rules.
        Assumes that each entry to set_file_times means a target
        was scanned once
        """
        return self._file_num_times_scanned[target]

    def set_file_times(
        self, target: Path, times: Dict[core.RuleId, Times], run_time: float
    ) -> None:
        num_bytes = target.stat().st_size

        self._file_run_time[target] = run_time

        parse_match_times = [times[rule] for rule in times]
        if len(parse_match_times) > 0:
            self._file_parse_time[target] = max(time[0] for time in parse_match_times)
            self._file_match_times[target] = sum(time[1] for time in parse_match_times)
        self._file_num_times_scanned[target] = len(parse_match_times)

        for rule in times:
            rule_times = times[rule]

            self._match_time_matrix[Semgrep_run(rule=rule, target=target)] = rule_times
            self._rule_match_times[rule] += rule_times.match_time
            self._rule_bytes_scanned[rule] += num_bytes

    def get_rules_parse_time(self) -> float:
        return self._rules_parse_time

    def set_rules_parse_time(self, parse_time: float) -> None:
        self._rules_parse_time = parse_time
