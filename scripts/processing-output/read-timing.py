import argparse
import json

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
for parse_time in rule_parse_times:
    summed_time += parse_time
    rule_parse_time += parse_time

target_times = result_times["targets"]

for target in target_times:
    for i in range(num_rules):
        r_time = target["run_times"][i]
        p_time = target["parse_times"][i]

        summed_time += r_time + p_time
        run_time += r_time
        file_parse_time += p_time
        if r_time > 10:
            print(target["path"], target["num_bytes"], rules[i], r_time, p_time)
num_files = len(target_times)
total_time = result_times["total_time"]

print(f"Files included { num_files } ")
print(f"Summed rule parse time { rule_parse_time } ")
print(f"Summed file parse time { file_parse_time } ")
print(f"Summed run time { run_time } ")
print(f"Total time { summed_time } ")
print(f"Total time reported { total_time }")
