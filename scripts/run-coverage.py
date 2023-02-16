#!/usr/bin/python3
import os
import re

path = "_build/default/tests"

print("Coverage statistics for files under matching/")


def report_summary_stat() -> str:
    stat = os.popen("bisect-ppx-report summary").read()
    patt = re.compile(r"Coverage:\s+\d+/\d+\s+\((\d+\.\d*)%\)")
    # mobj = patt.match("Coverage: 4/4 (4.4%)")
    mobj = patt.match(stat)
    if mobj is not None:
        return mobj.group(1)
    raise Exception("")


def report_summary_for_file_stat(file: str) -> str:
    stat = os.popen("bisect-ppx-report summary --per-file").readlines()
    patt = re.compile(rf"\s*(\d+.\d*)\s+%\s+\d+/\d+\s+{file}")
    for line in stat:
        mobj = patt.match(line)
        if mobj is not None:
            return mobj.group(1)
    raise Exception("")


metrics_URL = "https://dashboard.semgrep.dev/api/metric"


def add_metric(category: str, value: str) -> None:
    path = f"semgrep.core.{category}.coverage.matching.pct"
    cmd = f"curl -X POST {metrics_URL}/{path} -d '{value}' 2> /dev/null"
    out = os.popen(cmd).read()  # nosem
    patt = re.compile(".*successfully recorded")
    if patt.match(out) is None:
        raise Exception(f"Could not push the metric: {out}")


# to compile with coverage on
os.system("dune runtest --instrument-with bisect_ppx --force 2> /dev/null")
global_stat = report_summary_stat()
print(f"Aggregated coverage for all languages: {global_stat}%")
add_metric("all", global_stat)

# Substrings to pass to text.exe to run a test subset; see ../tests/Test.ml
languages = [
    "Python",
    "Javascript",
    "Typescript",
    "JSON",
    "Java",
    "C",
    "Go",
    "OCaml",
    "Ruby",
    "PHP",
]
# cleanup and run the analysis for each separate languages
for lang in languages:
    os.system(f"rm -f {path}/*.coverage")  # nosem
    os.system(f"cd {path};./test.exe {lang} > /dev/null")  # nosem
    lang_stat = report_summary_stat()
    print(f"Coverage stat for {lang}: {lang_stat}%")
    add_metric(lang.lower(), lang_stat)

subsystems = [
    ("eval", "matching/Eval_generic.ml"),
]


for test, file in subsystems:
    os.system(f"rm -f {path}/*.coverage")  # nosem
    os.system(f"cd {path};./test.exe {test} > /dev/null")  # nosem
    stat = report_summary_for_file_stat(file)
    print(f"Coverage stat for {test} in file {file}: {stat}%")
