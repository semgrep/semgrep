from typing import Any, Dict, List, Optional, Sequence, Set, Tuple
from typing import NamedTuple

from pathlib import Path

from semgrep.rule import Rule

Times = NamedTuple(
    "Times", [("parse_time", float), ("match_time", float), ("run_time", float)]
)

class ProfilingData:
    def __init__(self) -> None: ...
    def init_empty(self, rules: List[Rule], targets: List[Path]) -> None: ...
    def get_run_times(self, rule: Rule, target: Path) -> Times: ...
    def get_file_parse_time(self, target: Path) -> Optional[float]:
        """
        Return time taken to parse a file. This is the max of all
        times reported to parse the file from semgrep-core since
        it caches the parsed file.

        Return None if target has no reported parse time
        """
        ...
    def get_rule_match_time(self, rule: Rule) -> Optional[float]:
        """
        Return total match time for a given rule over all the files scanned
        with said rule

        Return None if RULE has no timing information saved
        """
        ...
    def get_rule_bytes_scanned(self, rule: Rule) -> int:
        """
        Return total number of bytes scanned by a given rule
        """
        ...
    def get_file_match_time(self, target: Path) -> Optional[float]:
        """
        Return total match time for a given file over all the rules that
        scanned the file

        Return None if TARGET has no timing information saved
        """
        ...
    def get_file_run_time(self, target: Path) -> Optional[float]:
        """
        Return total run time for a given file over all the rules that
        scanned the file

        Return None if TARGET has no timing information saved
        """
        ...
    def get_file_num_times_scanned(self, target: Path) -> int:
        """
        Returns number of times a file was scanned with rules.
        Assumes that each entry to set_file_times means a target
        was scanned once
        """
        ...
    def set_file_times(
        self, target: Path, times: Dict[Rule, Times], run_time: float
    ) -> None: ...
    def get_rules_parse_time(self) -> float: ...
    def set_rules_parse_time(self, parse_time: float) -> None: ...
