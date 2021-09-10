import json
import logging
import sys
from collections import defaultdict
from operator import add
from typing import Any
from typing import Dict
from typing import List
from typing import Optional
from typing import Union

logger = logging.getLogger(__file__)
logger.setLevel(logging.INFO)
handler = logging.StreamHandler(stream=sys.stderr)
handler.setFormatter(
    logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s")
)
logger.addHandler(handler)


class RuleStats:
    def __init__(
        self,
        total_rule_time: float = 0.0,
        maximum_rule_time: float = 0.0,
        minimum_rule_time: float = 0.0,
        num_targets: int = 0,
    ):
        self.total_rule_time = total_rule_time
        self.maximum_rule_time = maximum_rule_time
        self.minimum_rule_time = minimum_rule_time
        self.num_targets = num_targets

    def __add__(self, other: "RuleStats") -> "RuleStats":
        added_total_rule_time = self.total_rule_time + other.total_rule_time
        added_num_targets = self.num_targets + other.num_targets
        new_max = max(self.maximum_rule_time, other.maximum_rule_time)
        new_min = min(
            self.minimum_rule_time or other.minimum_rule_time, other.minimum_rule_time
        )
        return RuleStats(added_total_rule_time, new_max, new_min, added_num_targets)

    def return_average(self) -> float:
        if self.total_rule_time == 0 or self.num_targets == 0:
            return 0.0
        return self.total_rule_time / self.num_targets

    def to_dict(self) -> Dict[str, Union[float, int]]:
        return {
            "min": self.minimum_rule_time,
            "max": self.maximum_rule_time,
            "avg": self.return_average(),
            "sum": self.total_rule_time,
            "n_targets": self.num_targets,
        }


class RepositoryRuleTimings:
    def __init__(
        self,
        output_file: str,
        repo_to_times_per_rule: Optional[Dict[str, Dict[str, float]]] = None,
        rules_to_total_time_num_files: Optional[Dict[str, RuleStats]] = None,
    ) -> None:
        self.repo_to_times_per_rule = repo_to_times_per_rule or defaultdict(dict)
        self.output_file = output_file
        self.rules_to_total_time_num_files = (
            rules_to_total_time_num_files or defaultdict(RuleStats)
        )

    def times_per_file_to_times_per_rule(
        self, repo_name: str, times_per_file: dict
    ) -> None:
        if not "time" in times_per_file:
            logger.error(
                "Semgrep-core ran without the --time flag, please try again with --time set to true."
            )
            sys.exit(1)

        rule_ids = []
        for rule_id in times_per_file["time"]["rules"]:
            rule_ids.append(rule_id["id"])

        # this is for repo -> rule_id -> time
        total_time_per_rule: List[float] = [0.0] * len(rule_ids)

        # this is for rule_id -> average file time, want to remove all 0 run-times
        repo_time_per_rule_no_zeroes: Dict[str, RuleStats] = defaultdict(RuleStats)

        for target in times_per_file["time"]["targets"]:
            total_time_per_rule = list(
                map(add, target["run_times"], total_time_per_rule)
            )
            for idx, run_time in enumerate(target["run_times"]):
                if run_time > 0:
                    repo_time_per_rule_no_zeroes[
                        rule_ids[idx]
                    ] = repo_time_per_rule_no_zeroes[rule_ids[idx]] + RuleStats(
                        run_time, run_time, run_time, 1
                    )

        current_repo_times = dict(zip(rule_ids, total_time_per_rule))
        self.repo_to_times_per_rule[repo_name] = dict(
            sorted(current_repo_times.items(), key=lambda item: -item[1])
        )

        # add to the overall per-rule statistics
        for (
            rule_id,
            total_time_and_target_per_rule,
        ) in repo_time_per_rule_no_zeroes.items():
            changed_results = (
                self.rules_to_total_time_num_files[rule_id]
                + total_time_and_target_per_rule
            )
            self.rules_to_total_time_num_files[rule_id] = changed_results

    def to_dict(self) -> Dict[str, Any]:
        return {
            "repository_stats": self.repo_to_times_per_rule,
            "rule_stats": {
                rule_id: stats.to_dict()
                for rule_id, stats in self.rules_to_total_time_num_files.items()
            },
        }

    def __str__(self) -> str:
        return json.dumps(self.to_dict())
