#!/usr/bin/env python3
import argparse
import collections
import json
import os
import subprocess
import time
from typing import Any
from typing import Dict
from typing import List

# cache for "we already ran query x on this file, ignore it"
query_to_path_cache: Dict[str, Dict[str, List[Any]]] = collections.defaultdict(
    lambda: collections.defaultdict(list)
)
CACHE_WARNING = "queries will be cached, so the searched directory should not change during this interactive session"


def show_cache(expr: str):
    for path, lines in query_to_path_cache[expr].items():
        for (line, col) in lines:
            print(f"{path}:{line}:{col}")


def main(language: str, directory: str):
    expr = input("> ")
    exprs = expr.split(" | ")
    directories = [directory]
    start_time = time.time()
    for i, expr in enumerate(exprs):
        directories = cached_query(
            expr,
            [os.path.join(directory, d) for d in directories],
            i == len(exprs) - 1,
            start_time,
        )


def cached_query(
    expr: str, paths: List[str], show: bool, start_time: float
) -> List[str]:
    """Returns a list of paths the query was found in"""
    aa = ["./sgrep", "-r2c", f"-e", expr, *paths]
    if expr in query_to_path_cache:
        if show:
            show_cache(expr)
    else:
        json_output = subprocess.check_output(aa, shell=False).decode("utf-8")
        try:
            results = json.loads(json_output)["results"]
            for result in results:
                path = result["path"]
                line = result["start"]["line"]
                col = result["start"]["col"]
                query_to_path_cache[expr][path].append((line, col))
            if show:
                show_cache(expr)
                print(
                    f"{len(results)} results in {round(time.time() - start_time, 2)}s"
                )
        except json.decoder.JSONDecodeError:
            print(f"bad query: {json_output}")
    return list(query_to_path_cache[expr].keys())


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("search_dir", help=CACHE_WARNING)
    parser.add_argument("-lang", "--language", help="langauage to analyze")
    args = parser.parse_args()

    print("welcome to sgrep interactive")
    print(CACHE_WARNING)

    while True:
        main(args.language, args.search_dir)
