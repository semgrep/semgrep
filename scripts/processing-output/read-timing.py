import argparse
import json
from pprint import pprint

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!! TODO refactor this to use `semgrep_output_v1.py` !!!!
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

rules_parse_time = result_times["rules_parse_time"]

target_times = result_times["targets"]

run_errors = 0
parse_errors = 0
match_errors = 0

for target in target_times:
    r_time = target["run_time"]
    for i in range(num_rules):
        path = target["path"]

        p_time = target["parse_times"][i]
        m_time = target["match_times"][i]

        if m_time >= 0:
            match_time += m_time
        else:
            match_errors += 1

        if p_time >= 0:
            file_parse_time += p_time
        else:
            parse_errors += 1

    # Add timing info
    if r_time >= 0:
        summed_time += r_time
        run_time += r_time
    else:
        run_errors += 1

if run_errors > 0 or parse_errors > 0 or match_errors > 0:
    print(
        f"There were { run_errors } run errors, { parse_errors } parse errors, and { match_errors } match errors"
    )

num_files = len(target_times)

print(f"Files included { num_files } ")
print(f"Rule parse time { rules_parse_time } ")
print(f"Summed file parse time { file_parse_time } ")
print(f"Summed run time { run_time } ")
print(f"Summed match time { match_time } ")
print(f"Total time { summed_time } ")
print("====== Profiling times: =======")
pprint(result_times["profiling_times"])
