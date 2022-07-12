from typing import List
from typing import MutableMapping

# Get current get branch and commit hash

# changes that we do not want to record
# Git branch change - Reset metrics instead
# Git commit change + document change


class LSPMetrics:
    rule_seen_max_map: MutableMapping[str, int] = {}
    rules_seen_last_map: MutableMapping[str, int] = {}
    rule_seen_closed_map: MutableMapping[str, int] = {}

    @property
    def rules(self) -> List[str]:
        return list(self.rule_seen_max_map.keys())

    def update(self, rule_id: str, count: int, closed: int) -> None:
        self.rule_seen_max_map[rule_id] = max(
            self.rule_seen_max_map.get(rule_id, 0), count
        )
        self.rules_seen_last_map[rule_id] = count
        self.rule_seen_closed_map[rule_id] = closed + self.rule_seen_closed_map.get(
            rule_id, 0
        )

    def send(self) -> None:
        pass
