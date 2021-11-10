import argparse
import json

parser = argparse.ArgumentParser(description="Read JSON output.")
parser.add_argument("file", type=str, help="semgrep output object")

args = parser.parse_args()

f = open(args.file)
results = json.load(f)

summed_time = 0
for rule in results["rules"]:
    summed_time += rule["run_time"]

total_time = results["total_time"]
total_files = results["total_time"]

print(f"Summed run time { summed_time } ")
print(f"Total run time { total_time }")
