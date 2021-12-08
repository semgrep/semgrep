import argparse
import json

# Measure the effect of accidental duplication
# Should not be useful soon!

parser = argparse.ArgumentParser(description="Read JSON output.")
parser.add_argument("file", type=str, help="semgrep output object")

args = parser.parse_args()

f = open(args.file)
results = json.load(f)
result_times = results

rules = results["rules"]
num_rules = len(rules)

# Parse file

summed_time = 0
rule_parse_time = 0
file_parse_time = 0
run_time = 0

rule_parse_times = result_times["rule_parse_info"]
target_times = result_times["targets"]

run_times = rule_parse_times

for target in target_times:
    for i in range(num_rules):
        r_time = target["run_times"][i]
        run_times[i] += r_time

rule_run_times = {}
for i in range(num_rules):
    rule = rules[i]["id"]
    if not rule in rule_run_times:
        rule_run_times[rule] = []
    rule_run_times[rule].append(run_times[i])

total_time = 0
unique_time = 0
for rule_run_time in rule_run_times:
    time = rule_run_times[rule_run_time]
    total_time += sum(time)
    unique_time += min(time)

print(total_time, unique_time)
