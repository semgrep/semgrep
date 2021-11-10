import argparse
import json

parser = argparse.ArgumentParser(description="Read JSON output.")
parser.add_argument("file", type=str, help="semgrep output object")

args = parser.parse_args()

f = open(args.file)
results = json.load(f)

target_times = results["time"]

summed_time = 0
for target in target_times:
    for time in target["run_times"]:
        summed_time += time
num_files = len(target_times)
total_time = results["total_time"]
total_files = results["total_time"]

print(f"Files included { num_files } ")
print(f"Summed run time { summed_time } ")
print(f"Total run time { total_time }")
