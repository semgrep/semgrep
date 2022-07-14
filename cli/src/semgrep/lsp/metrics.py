from typing import List
from typing import MutableMapping
from typing import Optional

from semgrep.metrics import Metrics
from semgrep.metrics import MetricsState

# Get current get branch and commit hash

# changes that we do not want to record
# Git branch change - Reset metrics instead
# Git commit change + document change


# Here we're measuring two things:
# 1. A lower bound on how many fixes a user has made
#    - We're assuming if a user sees at some point X findings from a scan, and the last scan was Y findings,
#      then they've made at least X-Y fixes.
# 2. An upper bound on how many fixes a user has made
#    - For every scan, if there's X findings, and on the next
#      there's Y w/ Y < X findings, we will increase the counter by X-Y

# 1 may not cover if users fix one thing and another pops up during the
# session, but unlike 2 we don't count a bunch of redos + autosave + undos as
# possible fixes. Really 2 should be more accurate unless a user undos a lot of
# stuff all the time
class LSPMetrics:
    # The most findings we have shown for a given rule at one time to a user
    rule_seen_max_map: MutableMapping[str, int] = {}
    # The # of findings we have shown for a given rule on the last scan
    rules_seen_last_map: MutableMapping[str, int] = {}
    # How many rules have we closed that were once open
    rule_seen_closed_map: MutableMapping[str, int] = {}

    @property
    def rules(self) -> List[str]:
        """Return a list of all rules that have been seen."""
        return list(self.rule_seen_max_map.keys())

    def update(self, rule_id: str, opened: int, closed: int) -> None:
        """Update the metrics with the given rule and count."""
        self.rule_seen_max_map[rule_id] = max(
            self.rule_seen_max_map.get(rule_id, 0), opened
        )
        self.rules_seen_last_map[rule_id] = opened
        self.rule_seen_closed_map[rule_id] = closed + self.rule_seen_closed_map.get(
            rule_id, 0
        )

    def send(
        self, state: MetricsState, project_url: Optional[str], token: Optional[str]
    ) -> None:
        """Send the metrics to the backend."""
        metrics = Metrics()
        metrics.configure(state, None)
        upper_limits = {}
        # Calculate lower limit of fix rate
        for r in self.rule_seen_max_map:
            upper_limits[r] = self.rule_seen_max_map[r] - self.rules_seen_last_map[r]
        metrics.add_fix_rate(upper_limits, self.rule_seen_closed_map)
        metrics.add_feature("lsp", "fix_rate")
        metrics.add_project_url(project_url)
        metrics.add_token(token)
        metrics.send()
