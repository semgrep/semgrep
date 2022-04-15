import argparse
import json
from pprint import pprint

parser = argparse.ArgumentParser(description="Read JSON output.")
parser.add_argument("newer", type=str, help="newer semgrep output object")
parser.add_argument("older", type=str, help="older semgrep output object")

args = parser.parse_args()

# Read json files (expect semgrep CLI format)

newer = open(args.newer)
newer_results = json.load(newer)
newer_result_times = newer_results["time"]

older = open(args.older)
older_results = json.load(older)
older_result_times = older_results["time"]

# Compute intersecting targets

newer_targets = set()
for target in newer_result_times["targets"]:
    newer_targets.add(target["path"])

older_targets = set()
for target in older_result_times["targets"]:
    older_targets.add(target["path"])

shared_targets = newer_targets.intersection(older_targets)

# Compute intersecting rules

newer_rules = [rule["id"] for rule in newer_result_times["rules"]]
older_rules = [rule["id"] for rule in older_result_times["rules"]]
shared_rules = set(newer_rules).intersection(set(older_rules))

# Save per file and per rule time information
newer_files = {}
all_run_time = 0
newer_rule_times = {rule: 0 for rule in shared_rules}

for target in newer_result_times["targets"]:
    target_run_time = 0
    for i in range(0, len(newer_rules)):
        rule = newer_rules[i]
        if rule in shared_rules:
            target_run_time += target["run_times"][i]
            newer_rule_times[rule] += target["run_times"][i]
    newer_files[target["path"]] = target_run_time
    all_run_time += target_run_time

print("Total run time from the newer config of shared rules", all_run_time)

older_files = {}
older_rule_times = {rule: 0 for rule in shared_rules}
for target in older_result_times["targets"]:
    target_run_time = 0
    for i in range(0, len(older_result_times["rules"])):
        rule = older_rules[i]
        if rule in shared_rules:
            target_run_time += target["run_times"][i]
            older_rule_times[rule] += target["run_times"][i]
    older_files[target["path"]] = target_run_time


# Get diffs
newer_keys = newer_files.keys()
older_keys = older_files.keys()
diff_keys = newer_keys - older_keys
diffs = list((key, newer_files[key]) for key in diff_keys)

print("Files only appearing in the new timing:")
pprint(sorted(diffs, key=lambda x: x[1], reverse=True)[:10])

print("Time spent on files only in the new timing: ", end="")
pprint(sum(x[1] for x in diffs))
print("Number of files only in the new timing: ", end="")
print(len(diffs))
print("***********************")

engine_diff = 0
lang_diffs = {}
lang_times = {}

for file in newer_files:
    if file in older_files:
        diff = newer_files[file] - older_files[file]
        engine_diff += diff

        lang = file.split(".")[-1]
        if len(lang) < 5:
            if lang not in lang_diffs:
                lang_diffs[lang] = 0
                lang_times[lang] = 0
            lang_diffs[lang] += diff
            lang_times[lang] += newer_files[file]

print("Newer time - older time from shared files only", engine_diff)
print("Time differences by file extension")
pprint(sorted(lang_diffs.items(), key=lambda kv: kv[1], reverse=True))
print("Total time spent by the newer run, by file extension")
pprint(sorted(lang_times.items(), key=lambda kv: kv[1], reverse=True))

print("***********************")
print("Rules where one version took significantly longer than the other")
for rule in sorted(shared_rules):
    if abs(newer_rule_times[rule] - older_rule_times[rule]) > 0.01:
        print(newer_rule_times[rule], older_rule_times[rule], rule)
