#! /usr/bin/env python3
#
# Main function is in run-benchmarks. Can be called via
#               ./run-benchmarks --semgrep_core
#
# Run semgrep-core on a series of pairs (rules, repo) with different options
# and report the time it takes.
#
# semgrep-core needs the user to pass in the language to analyze in.
# We implement that by adding the main language of the rules being run to
# each corpus. However, this limits the set of corpuses we can use.
#
# Right now, the corpus is labelled with a language because we chose to only
# use corpuses that are primarily one language, which makes their runtime
# comparable to the runs using semgrep. We could also have chosen to
# label the rules, or change the pair to (rules, repo, lang), but ultimately
# we would like semgrep-core to be able to infer the language
#
# We are additionally limited by what semgrep-core is able to parse, as
# noted by some commented out tests
#
# The end goal is for semgrep-core to replace semgrep in analyzing rules
# and to unify the two files. The --semgrep_core option is currently meant
# for local convenience, not use in CI
#
# Requires semgrep-core (make install in the semgrep-core folder)
#
import os
import subprocess
import time
import urllib.request
from contextlib import contextmanager
from typing import Iterator
from typing import List

from constants import DASHBOARD_URL
from corpus import Corpus
from corpus import DUMMY_CORPUSES
from corpus import SEMGREP_CORE_CORPUSES


class SemgrepVariant:
    def __init__(self, name: str, semgrep_core_extra: str):
        # name for the input corpus (rules and targets)
        self.name = name

        # space-separated extra arguments to pass to the default semgrep
        # command
        self.semgrep_core_extra = semgrep_core_extra


# Semgrep-core variants are separate for now because semgrep-core with
# -config is not official. Still uses the class SemgrepVariant
SEMGREP_CORE_VARIANTS = [
    SemgrepVariant("std", ""),
    SemgrepVariant("no-bloom", "-no_bloom_filter"),
    SemgrepVariant("use-sets", "-set_filter"),
    SemgrepVariant("filter-irrelevant-rules", "-filter_irrelevant_rules"),
    SemgrepVariant(
        "filter-rules_no-bloom", "-filter_irrelevant_rules -no_bloom_filter"
    ),
]

# Add support for: with chdir(DIR): ...
@contextmanager
def chdir(dir: str) -> Iterator[None]:
    old_dir = os.getcwd()
    os.chdir(dir)
    try:
        yield
    finally:
        os.chdir(old_dir)


def upload_result(metric_name: str, value: float) -> None:
    url = f"{DASHBOARD_URL}/api/metric/{metric_name}"
    print(f"Uploading to {url}")
    r = urllib.request.urlopen(  # nosem
        url=url,
        data=str(value).encode("ascii"),
    )
    print(r.read().decode())


def run_semgrep_core(corpus: Corpus, variant: SemgrepVariant) -> float:
    common_args = ["-timeout", "0"]
    # Using absolute paths because run_semgrep did, and because it is convenient
    # to be able to run the command in different folders
    args: List[str] = [
        "semgrep-core",
        "-j",
        "8",
        "-lang",
        corpus.language or "",
        "-config",
        os.path.abspath(corpus.rule_dir) or "",
        os.path.abspath(corpus.target_dir) or "",
    ]
    args.extend(common_args)
    if variant.semgrep_core_extra != "":
        args.extend(variant.semgrep_core_extra.split(" "))

    print(f"current directory: {os.getcwd()}")
    print("semgrep-core command: {}".format(" ".join(args)))

    t1 = time.time()
    res = subprocess.run(args)  # nosem
    t2 = time.time()

    status = res.returncode
    print(f"semgrep-core exit status: {status}")
    if status == 0:
        print("success")
    elif status == 3:
        print("warning: some files couldn't be parsed")
    else:
        res.check_returncode()

    return t2 - t1


def run_benchmarks(dummy: bool, upload: bool) -> None:
    results = []
    corpuses = SEMGREP_CORE_CORPUSES
    if dummy:
        corpuses = DUMMY_CORPUSES
    for corpus in corpuses:
        with chdir(corpus.name):
            corpus.prep()
            for variant in SEMGREP_CORE_VARIANTS:
                name = ".".join(["semgrep-core", "bench", corpus.name, variant.name])
                metric_name = ".".join([name, "duration"])
                print(f"------ {name} ------")
                duration = run_semgrep_core(corpus, variant)
                msg = f"{metric_name} = {duration:.3f} s"
                print(msg)
                results.append(msg)
                if upload:
                    upload_result(metric_name, duration)
    for msg in results:
        print(msg)
