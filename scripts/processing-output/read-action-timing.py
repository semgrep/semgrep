import argparse
import json

parser = argparse.ArgumentParser(description="Read JSON output.")
parser.add_argument("file", type=str, help="semgrep output object")

args = parser.parse_args()

f = open(args.file)
results = json.load(f)

rules = results["rules"]
rule_counts = {rule["id"]: 0 for rule in rules}

for target in results["longest_targets"]:
    print(target["path"], target["num_bytes"])

summed_time = 0
for rule in rules:
    summed_time += rule["run_time"]

    if rule["run_time"] > 30:
        print(rule["run_time"], rule["id"])

    rule_counts[rule["id"]] += 1

for rule in sorted(rules, key=lambda rule: rule["run_time"]):
    print(rule_counts[rule["id"]], rule)

print(len(rules))
print(len(rule_counts))

total_time = results["total_time"]
total_files = results["total_time"]

print(f"Summed run time { summed_time } ")
print(f"Total run time { total_time }")
