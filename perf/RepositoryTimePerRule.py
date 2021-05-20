from collections import defaultdict
from typing import Dict, List
from operator import add

import copy
import json
import sys

class RepositoryTimePerRule:
    def __init__(
        self,
        output_file: str,
        repo_to_times_per_rule: Dict[str, Dict[str, int]] = defaultdict(dict)
    ) -> None:
        self.repo_to_times_per_rule = repo_to_times_per_rule
        self.output_file = output_file

    def times_per_file_to_times_per_rule(self, repo_name: str, times_per_file_bytes: bytes):
        try:
            times_per_file = json.loads(times_per_file_bytes.decode('utf-8'))
        except (UnicodeDecodeError, ValueError):
            print ("Unable to decode the Semgrep result as JSON.")
            sys.exit(1)

        if not 'time' in times_per_file:
            print ("Semgrep-core ran without the --time flag, please try again with --time set to true.")
            sys.exit(1)

        rule_ids = []
        for rule_id in times_per_file['time']['rules']:
            rule_ids.append(rule_id['id'])

        total_time_per_rule: List[str] = [0] * len(rule_ids)
        for target in times_per_file['time']['targets']:
            total_time_per_rule = list(map(add, target['parse_times'], total_time_per_rule))

        current_repo_times = dict(zip(rule_ids, total_time_per_rule))
        self.repo_to_times_per_rule[repo_name] = current_repo_times

    def print_repo_to_times_per_rule(self):
        with open(self.output_file, 'w') as f:
            json.dump(self.repo_to_times_per_rule, f)
