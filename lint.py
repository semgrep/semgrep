#!/usr/bin/env python3
import collections
import os
import pathlib
import subprocess
import sys
import tempfile
import traceback
from dataclasses import dataclass
from pathlib import PurePath
from typing import Any, List

import click
import yaml

# validate input yaml files

# validate patterns inside yaml files

# glob yaml files into a single rule files, adjusting check ids
MUST_HAVE_KEYS = set(['id', 'pattern', 'message', 'languages', 'severity'])
YML_EXTENSIONS = ['.yml', '.yaml']

SGREP_PATH = "sgrep"


def sgrep_pattern():
    """

    You can assumle that there will be a -sgrep_lint2 command line parameter to the sgrep engine, 
    which will still take a yaml file with a flat list of rules, and it will return a JSON with
     an array of objects with the rule id, matched range, and an array with the value for the metavars

    your job will be to create this yaml file for me that decompose a pattern
     using AND OR NOT in a simpler pattern, and I will returned the
      matched ranges for those simple patterns (I will not do anymore OK hack)


    Assume that sgrep-lint gives us output that looks like this:

    - id: subprocess-1
    - pattern: subprocess.Popen($X)

    output: { "check_id": "subprocess-1", range: [505, 510], metavars={"$X": "foobar"}}

    - id: subprocess-1
    - not-pattern: subprocess.Popen($X, safeflag=True)
    - and-pattern: subprocess.Popen($X)
    - or-patterns:
        - 
        - 
    - message: this is dangerous

    ==== suggestion, just do and, and-not, maybe "and-or"

    - id: subprocess-1
    - AND-EITHER:
        - subprocess.Popen($X, safeflag=True)
        - subprocess.Popen($X, othersafeflag=True)
    - AND:
        - subprocess.Popen($X)
    - message: this is dangerous

    - id: subprocess-1
    - AND-NOT: subprocess.Popen($X, safeflag=True)       <- P1
    - AND: subprocess.Popen($X)                          <- P2
    - booleans: !P1 && P2
    - message: this is dangerous

insight: our algebra needs to express "AND-INSIDE" as opposed to "AND-NOT-INSIDE" 
(effectively CFG-dominance relations) in order to handle "in this function"
let's makee the and implicit, then we'd have:
    either-pattern:
    pattern:
    not-pattern:
    inside-pattern:
    not-inside-pattern:

also, the last one in the list (end of expression) needs to be the one that we want to print

    """


@dataclass(frozen=True)
class Range:
    start: int
    end: int

    def is_enclosing_or_eq(self, other_range):
        return self.start <= other_range.start and other_range.end <= self.end

    def __repr__(self):
        return f'{self.start}-{self.end}'


class OPERATORS:
    AND_NOT = "and_not"
    AND = "and"
    AND_EITHER = "and_either"
    AND_INSIDE = "and_inside"
    AND_NOT_INSIDE = "and_not_inside"


def testA():
    """

    TODO: what about nested booleans?

    let pattern1 = subprocess.Popen($X, safeflag=True)
    let pattern2 =  subprocess.Popen($X, ...)

        import subprocess
        subprocess.Popen(subprocess.Popen(safeFlag=True))
        ------------------------------------------------- P2        R1
                         ------------------------------- P1, P2     R2

    and-not P1 --> remove all ranges equal to P1 exactly (removes R2)
    and P2 --> remove all ranges that don't equal P2 (R1 stays)
    OUTPUT: R1

    """
    results = {
        "pattern1": [Range(30, 100)],
        "pattern2": [Range(0, 100), Range(30, 100)],
    }
    expression = [(OPERATORS.AND_NOT, ["pattern1"]),
                  (OPERATORS.AND, ["pattern2"])]
    result = evaluate_expression(expression, results)
    assert result == set([Range(0, 100)]), f"{result}"


def testB():
    """

        let pattern1 = subprocess.Popen($X, safeflag=True)
        let pattern2 = subprocess.Popen($X, ...)
        let pattern3 = importlib.Popen($X, ...)


        import subprocess
        subprocess.Popen(subprocess.Popen(bad), safeFlag=True)
        ----------------------------------------------------- P1, P2        R1
                         ---------------------                P2            R2

        and-not P1 --> remove all ranges == P1 exactly (R1 is removed)
        and-or (P2, P3) -->  remove any ranges not exactly == P2 or P3. (R2 stays)
        OUTPUT: R2
    """
    results = {
        "pattern1": [Range(0, 100)],
        "pattern2": [Range(30, 70), Range(0, 100)],
        "pattern3": []
    }
    expression = [(OPERATORS.AND_NOT, ["pattern1"]),
                  (OPERATORS.AND_EITHER, ["pattern2", "pattern3"])]
    result = evaluate_expression(expression, results)
    assert result == set([Range(30, 70)]), f"{result}"


def testC():
    """
        let pattern1 = subprocess.Popen($X, safeflag=True)
        let pattern2 = subprocess.Popen($X, ...)
        let pattern4 = def __eq__(): \n...

        def __eq__():
            import subprocess
            subprocess.Popen(subprocess.Popen(bad), safeFlag=True)
            -----------------------------------------------------   P1, P2      R1
                            ---------------------                   P2          R2
        ----------------------------------------------------------  P4          R3

        and-inside P4 --> remove all ranges that are not enclosed by P4. Now only ranges inside or equal to P4 are left (all ranges remain)
        and-not P1 --> remove all ranges == P1. Now only ranges that don't have P1 remain (R2, R3 remain)
        and P2 --> remove all ranges not == P2. R2 remains.
        OUTPUT: R2 
    """
    results = {
        "pattern1": [Range(100, 1000)],
        "pattern2": [Range(100, 1000), Range(200, 300)],
        "pattern4": [Range(0, 1000)]
    }
    expression = [(OPERATORS.AND_INSIDE, ["pattern4"]),
                  (OPERATORS.AND_NOT, ["pattern1"]),
                  (OPERATORS.AND, ["pattern2"])]
    result = evaluate_expression(expression, results)
    assert result == set([Range(200, 300)]), f"{result}"


def testD():
    """
        let pattern1 = subprocess.Popen($X, safeflag=True)
        let pattern2 = subprocess.Popen($X, ...)
        let pattern4 = def __eq__(): \n...

        def __eq__():
            import subprocess
            subprocess.Popen(subprocess.Popen(bad), safeFlag=True)
            -----------------------------------------------------   P1, P2      R1
                            ---------------------                   P2          R2
        ----------------------------------------------------------  P4          R3

        and-not-inside P4 --> remove all ranges that are not enclosed by P4. Now only ranges inside or equal to P4 are left (no ranges remain)
        and-not P1 --> no effect
        and P2 --> no effect
        OUTPUT: [] 
    """
    results = {
        "pattern1": [Range(100, 1000)],
        "pattern2": [Range(100, 1000), Range(200, 300)],
        "pattern4": [Range(0, 1000)]
    }
    expression = [(OPERATORS.AND_NOT_INSIDE, ["pattern4"]),
                  (OPERATORS.AND_NOT, ["pattern1"]),
                  (OPERATORS.AND, ["pattern2"])]
    result = evaluate_expression(expression, results)
    assert result == set([Range(200, 300)]), f"{result}"


def testE():
    """
    let pattern1 = bad()
    let pattern2 = def __eq__(): \n...
    let pattern3 = def normal(): \n...


        0-100 def __eq__(): 
        100-200    bad()

        200-300 def normal():
        300-400     bad(bad())
        400-500     def __eq__():
        500-600         bad()

        and-inside P3
        and-not-inside P2
        and P1 
        OUTPUT: [300-400], [350-400]
    """
    results = {
        "pattern1": [Range(100, 200), Range(300, 400), Range(350, 400)],
        "pattern2": [Range(0, 200), Range(400, 600)],
        "pattern3": [Range(200, 600)]
    }
    expression = [(OPERATORS.AND_INSIDE, ["pattern3"]),
                  (OPERATORS.AND_NOT_INSIDE, ["pattern2"]),
                  (OPERATORS.AND, ["pattern1"])]
    result = evaluate_expression(expression, results)
    assert result == set([Range(300, 400), Range(350, 400)]), f"{result}"


def flatten(L: List[List[Any]]) -> List[Any]:
    for list in L:
        for item in list:
            yield item


def evaluate_single_expression(operator, pattern_id, results, ranges_left):
    if operator == OPERATORS.AND:
        # remove all ranges that don't equal the ranges for this pattern
        return ranges_left.intersection(results[pattern_id])
    elif operator == OPERATORS.AND_NOT:
        # remove all ranges that DO equal the ranges for this pattern
        # difference_update = Remove all elements of another set from this set.
        return ranges_left.difference(results[pattern_id])
    elif operator == OPERATORS.AND_INSIDE or operator == OPERATORS.AND_NOT_INSIDE:
        # remove all ranges (not enclosed by) or (not equal to) the inside ranges
        output_ranges = set()
        for arange in ranges_left:
            keep_this_range = False
            for keep_inside_this_range in results[pattern_id]:
                is_enclosed = keep_inside_this_range.is_enclosing_or_eq(arange)
                print(
                    f'candidate range is {arange}, needs to be `{operator}` {keep_inside_this_range}: {is_enclosed}')
                if (OPERATORS.AND_INSIDE) and is_enclosed or (OPERATORS.AND_NOT_INSIDE and not is_enclosed):
                    output_ranges.add(arange)
                    break  # found a match, no need to keep going

        return output_ranges
        print(f"after filter `{operator}`: {ranges_left}")
    else:
        assert False, f'unknown operator {operator} in {expression}'


def evaluate_expression(expression, results) -> List[Range]:
    ranges_left = set(flatten(results.values()))
    for (operator, pattern_ids) in expression:
        if operator == OPERATORS.AND_EITHER:
            # create a set from the union of the expressions in the `or` block
            either_ranges = set(flatten((results[pid]) for pid in pattern_ids))
            # remove anything that does not equal one of these ranges
            return ranges_left.intersection(either_ranges)
        else:
            assert len(
                pattern_ids) == 1, f'only {OPERATORS.AND_EITHER} expressions can have multiple pattern names'
            return evaluate_single_expression(operator, pattern_ids[0], results, ranges_left)


def print_error(e):
    sys.stderr.write(e + os.linesep)
    sys.stderr.flush()


def parse_sgrep_yml(file_path: str):
    #print_error(f'loading rules from {file_path}...')
    try:
        y = yaml.safe_load(open(file_path))
    except FileNotFoundError:
        return None
    except yaml.scanner.ScannerError as se:
        print_error(se)
        return None

    if not 'rules' in y:
        print_error(f'{file_path} should have top-level key named `rules`')
        return None

    rules = []
    for i, rule in enumerate(y['rules']):
        if not rule:
            continue
        rule_id_err_msg = f'(rule id: {rule["id"]})' if ('id' in rule) else ''
        if MUST_HAVE_KEYS != set(rule.keys()):
            print_error(
                f'{file_path} is missing keys at rule {i}{rule_id_err_msg}, must have: {MUST_HAVE_KEYS}')
        else:
            rules.append(rule)
    return rules


@click.command()
@click.argument("yaml_file_or_dirs", nargs=1, type=click.Path(),
                #help=f"The YAML file or directory of YAML files ending in {YML_EXTENSIONS} with rules",
                )
@click.argument("target_files_or_dirs", nargs=-1, type=click.Path())
def main(yaml_file_or_dirs, target_files_or_dirs):
    all_rules = []
    errors, not_errors = 0, 0
    for root, dirs, files in os.walk(yaml_file_or_dirs):
        dirs.sort()
        for filename in sorted(files):
            if pathlib.Path(filename).suffix in YML_EXTENSIONS:
                full_path = os.path.join(root, filename)
                rules_in_file = parse_sgrep_yml(full_path)
                if rules_in_file is None:
                    errors += 1
                else:
                    not_errors += 1
                    for rule in rules_in_file:
                        prefix = '.'.join([x for x in PurePath(
                            pathlib.Path(full_path)).parts[:-1] if len(x)])
                        new_id = f"{prefix}.{rule['id']}".lstrip('.')
                        rule['id'] = new_id
                    all_rules.extend(list(rules_in_file))

    # create unified yml file
    unified = {'rules': list(all_rules)}
    print_error(
        f'running {len(all_rules)} rules from {not_errors} yaml files ({errors} yaml files were invalid)')
    with tempfile.NamedTemporaryFile('w') as fout:
        fout.write(yaml.safe_dump(unified, sort_keys=False))
        fout.flush()
        cmd = f'{SGREP_PATH} -rule_file={fout.name} {" ".join(list(target_files_or_dirs))}'
        output = subprocess.check_output(cmd, shell=True)
        print(output.decode('utf-8'))


def testAll():
    testA()
    testB()
    testC()
    testD()
    print('8'*80)
    testE()


if __name__ == '__main__':
    testAll()
    # main()
