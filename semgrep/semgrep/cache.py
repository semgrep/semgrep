import pickle  # TODO: yes yes not too safe, will have to find something better
import sqlite3
from collections import defaultdict
from typing import Dict
from typing import List
from typing import Set
from typing import Tuple
from typing import TYPE_CHECKING

from semgrep.constants import USER_DATA_FOLDER
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch


if TYPE_CHECKING:
    from semgrep.core_runner import Plan
    from semgrep.rule_match import RuleMatch
    from semgrep.rule import Rule


class FindingsCache:
    def __init__(self) -> None:
        self.db = sqlite3.connect(USER_DATA_FOLDER / "findings-cache.sqlite3")
        cur = self.db.cursor()
        # TODO how do we update schema?
        cur.execute(
            """CREATE TABLE IF NOT EXISTS tasks (hash text PRIMARY KEY, findings text)"""
        )

    def save(self, plan: "Plan", results: Dict["Rule", List["RuleMatch"]]) -> None:
        rows: Dict[str, Set["RuleMatch"]] = {task.hash: set() for task in plan}
        for rule, matches in results.items():
            for match in matches:
                task = next(
                    task
                    for task in plan
                    if rule.id in task.rule_ids
                    and task.language in rule.languages
                    and task.path == str(match.path)
                )
                rows[task.hash].add(match)
        cur = self.db.cursor()
        cur.executemany(
            """INSERT INTO tasks VALUES (?, ?) ON CONFLICT(hash) DO UPDATE SET findings=excluded.findings""",
            [
                (task_hash, pickle.dumps(list(findings)))
                for task_hash, findings in rows.items()
            ],
        )
        self.db.commit()

    def load(
        self, plan: "Plan", rules: List["Rule"]
    ) -> Tuple[Dict["Rule", List["RuleMatch"]], "Plan"]:
        cur = self.db.cursor()
        rows = dict(
            cur.execute(
                f"""SELECT hash, findings FROM tasks WHERE hash IN ({','.join(['?']*len(plan))})""",
                [task.hash for task in plan],
            ).fetchall()
        )
        results: Dict["Rule", List["RuleMatch"]] = defaultdict(list)
        for findings in rows.values():
            for finding in pickle.loads(findings):
                rule = next(rule for rule in rules if rule.id == finding.rule_id)
                results[rule].append(finding)
        return results, plan.without_hashes(set(rows))
