import argparse
import json
from pprint import pprint

parser = argparse.ArgumentParser(description="Read JSON output.")
parser.add_argument("file", type=str, help="semgrep output object")

args = parser.parse_args()

f = open(args.file)
results = json.load(f)
result_times = results["time"]

rules = result_times["rules"]
num_rules = len(rules)

# Parse file

summed_time = 0
rule_parse_time = 0
file_parse_time = 0
run_time = 0
match_time = 0

rule_parse_times = result_times["rule_parse_info"]
for parse_time in rule_parse_times:
    summed_time += parse_time
    rule_parse_time += parse_time

target_times = result_times["targets"]

run_errors = 0
parse_errors = 0
match_errors = 0

for target in target_times:
    for i in range(num_rules):
        path = target["path"]

        r_time = target["run_times"][i]
        p_time = target["parse_times"][i]
        m_time = target["match_times"][i]

        # Add timing info
        if r_time >= 0:
            summed_time += r_time
            run_time += r_time
        else:
            run_errors += 1

        if m_time >= 0:
            match_time += m_time
        else:
            match_errors += 1

        if p_time >= 0:
            file_parse_time += p_time
        else:
            parse_errors += 1

if run_errors > 0 or parse_errors > 0 or match_errors > 0:
    print(
        f"There were { run_errors } run errors, { parse_errors } parse errors, and { match_errors } match errors"
    )

num_files = len(target_times)

print(f"Files included { num_files } ")
print(f"Summed rule parse time { rule_parse_time } ")
print(f"Summed file parse time { file_parse_time } ")
print(f"Summed run time { run_time } ")
print(f"Summed match time { match_time } ")
print(f"Total time { summed_time } ")
print("====== Profiling times: =======")
pprint(result_times["profiling_times"])
