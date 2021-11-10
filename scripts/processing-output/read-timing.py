import argparse
import json

parser = argparse.ArgumentParser(description="Read JSON output.")
parser.add_argument("file", type=str, help="semgrep output object")

args = parser.parse_args()

f = open(args.file)
results = json.load(f)

summed_time = 0
rule_parse_time = 0
file_parse_time = 0
run_time = 0

rule_parse_times = results["time"]["rule_parse_info"]
for parse_time in rule_parse_times:
    summed_time += parse_time
    rule_parse_time += parse_time

target_times = results["time"]["targets"]

for target in target_times:
    for time in target["run_times"]:
        summed_time += time
        run_time += time
    for time in target["parse_times"]:
        summed_time += time
        file_parse_time += time
num_files = len(target_times)
total_time = results["time"]["total_time"]

print(f"Files included { num_files } ")
print(f"Summed rule parse time { rule_parse_time } ")
print(f"Summed file parse time { file_parse_time } ")
print(f"Summed run time { run_time } ")
print(f"Total time { summed_time } ")
print(f"Total time reported { total_time }")
