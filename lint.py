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
from typing import Any, Generator, List, Set, Tuple

import click
import yaml


class OPERATORS:
    AND_NOT = "and_not"
    AND = "and"
    AND_EITHER = "and_either"
    AND_INSIDE = "and_inside"
    AND_NOT_INSIDE = "and_not_inside"


MUST_HAVE_KEYS = set(['id', 'message', 'languages', 'severity'])

PATTERN_NAMES_MAP = {
    "pattern-inside": OPERATORS.AND_INSIDE,
    "pattern-not-inside": OPERATORS.AND_NOT_INSIDE,
    "pattern-either": OPERATORS.AND_EITHER,
    "pattern-not": OPERATORS.AND_NOT,
    "pattern": OPERATORS.AND,
}

YML_EXTENSIONS = ['.yml', '.yaml']

SGREP_PATH = "sgrep"


def build_boolean_expression(rule):
    """
    Build a (flat, not nested #TODO boolean expression from the yml lines in the rule)
    """
    def _parse_boolean_expression(rule_patterns, counter=0):
        for pattern in rule_patterns:
            for boolean_operator, pattern_text in pattern.items():
                if boolean_operator == 'pattern-either':
                    yield (OPERATORS.AND_EITHER, list(range(counter, counter + len(pattern_text))))
                    counter += len(pattern_text)
                else:
                    yield (operator_for_pattern_name(boolean_operator), [counter])
                    counter += 1

    if 'pattern' in rule:  # single pattern
        yield (OPERATORS.AND, [0])
    elif 'patterns' in rule:  # multiple patterns
        yield from _parse_boolean_expression(rule['patterns'])
    else:
        assert False


def operator_for_pattern_name(pattern_name):
    return PATTERN_NAMES_MAP[pattern_name]


def parse_rule_patterns(rule):
    if 'pattern' in rule:  # single pattern
        yield (0, rule['pattern'])
    elif 'patterns' in rule:  # multiple patterns
        yield from parse_pattern_expression(rule['patterns'])
    else:
        assert False

# -> Generator[Tuple[int, str]]:


def parse_pattern_expression(rule_patterns, counter=0):
    #    print((counter, rule_patterns))
    for pattern in rule_patterns:
        for boolean_operator, pattern_text in pattern.items():
            if boolean_operator == 'pattern-either':
                yield from parse_pattern_expression(pattern_text, counter)
                counter += len(pattern_text)
            else:
                yield (counter, pattern_text)
                counter += 1


def parse_sgrep_yml(file_path: str):
    """
rules:
  - id: assert-eqeq-is-ok
    pattern: |
      def __eq__():
          ...
          $X == $X
    message: "possibly useless comparison but in eq function"
    languages: [python]
    severity: OK
  - id: eqeq-is-bad
    patterns:
      - pattern-not-inside: |
          def __eq__():
              ...
      - pattern-not-inside: assert(...)
      - pattern-not-inside: assertTrue(...)
      - pattern-not-inside: assertFalse(...)
      - pattern-not: 1 == 1
      - pattern-either:
          - pattern: $X == $X
          - pattern: $X != $X
    message: "useless comparison operation `$X == $X` or `$X != $X`; possible bug?"
    langauges: [python]
    severity: ERROR
  - id: python37-compatability-os-module
    patterns:
      - pattern-not-inside: |
          if hasattr(os, 'pwrite'):
              ...
      - pattern: os.pwrite(...)
    message: "this function is only available on Python 3.7+"
    languages: [python]
    severity: ERROR

    """
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

    counter = 0

    rules = []
    for i, rule in enumerate(y['rules']):
        if not rule:
            continue
        rule_id_err_msg = f'(rule id: {rule["id"]})' if ('id' in rule) else ''
        if set(rule.keys()).issubset(MUST_HAVE_KEYS):
            print_error(
                f'{file_path} is missing keys at rule {i+1}{rule_id_err_msg}, must have: {MUST_HAVE_KEYS}')
        elif not 'pattern' in rule and not 'patterns' in rule:
            print_error(
                f'{file_path} is missing key `pattern` or `patterns` at rule {i+1}{rule_id_err_msg}')
        else:
            rules.append(rule)
    return rules


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


def flatten(L: List[List[Any]]) -> List[Any]:
    for list in L:
        for item in list:
            yield item


def _evaluate_single_expression(operator, pattern_id, results, ranges_left: Set[Range]) -> List[Range]:
    if operator == OPERATORS.AND:
        # remove all ranges that don't equal the ranges for this pattern
        return ranges_left.intersection(results[pattern_id])
    elif operator == OPERATORS.AND_NOT:
        # remove all ranges that DO equal the ranges for this pattern
        # difference_update = Remove all elements of another set from this set.
        return ranges_left.difference(results[pattern_id])
    elif operator == OPERATORS.AND_INSIDE:
        # remove all ranges (not enclosed by) or (not equal to) the inside ranges
        output_ranges = set()
        for arange in ranges_left:
            for keep_inside_this_range in results[pattern_id]:
                is_enclosed = keep_inside_this_range.is_enclosing_or_eq(arange)
                # print(
                #    f'candidate range is {arange}, needs to be `{operator}` {keep_inside_this_range}; keep?: {keep}')
                if is_enclosed:
                    output_ranges.add(arange)
                    break  # found a match, no need to keep going
        # print(f"after filter `{operator}`: {output_ranges}")
        return output_ranges
    elif operator == OPERATORS.AND_NOT_INSIDE:
        # remove all ranges enclosed by or equal to
        output_ranges = ranges_left.copy()
        for arange in ranges_left:
            for keep_inside_this_range in results[pattern_id]:
                if keep_inside_this_range.is_enclosing_or_eq(arange):
                    output_ranges.remove(arange)
                    break
        # print(f"after filter `{operator}`: {output_ranges}")
        return output_ranges
    else:
        assert False, f'unknown operator {operator} in {expression}'


def evaluate_expression(expression, results) -> List[Range]:
    ranges_left = set(flatten(results.values()))
    for (operator, pattern_ids) in expression:
        if operator == OPERATORS.AND_EITHER:
            # create a set from the union of the expressions in the `or` block
            either_ranges = set(flatten((results[pid]) for pid in pattern_ids))
            # remove anything that does not equal one of these ranges
            ranges_left.intersection_update(either_ranges)
        else:
            assert len(
                pattern_ids) == 1, f'only {OPERATORS.AND_EITHER} expressions can have multiple pattern names'
            ranges_left = _evaluate_single_expression(
                operator, pattern_ids[0], results, ranges_left)
    return ranges_left


def print_error(e):
    sys.stderr.write(str(e) + os.linesep)
    sys.stderr.flush()


def output_to_ranges(sgrep_output: str) -> List[Range]:
    # TODO: take sgrep output and convert it to ranges on a per-file or other basis
    return [Range(0, 100)]


@click.command()
@click.argument("yaml_file_or_dirs", nargs=1, type=click.Path(),
                # help=f"The YAML file or directory of YAML files ending in {YML_EXTENSIONS} with rules",
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

    unified = list(all_rules)
    print_error(
        f'running {len(unified)} rules from {not_errors} yaml files ({errors} yaml files were invalid)')
    # send each rule to sgrep one by one
    for rule in unified:
        output_by_pattern_index = {}
        patterns_with_ids = list(parse_rule_patterns(rule))
        for (patttern_index, pattern) in patterns_with_ids:
            cmd = f'{SGREP_PATH} -pattern={pattern}'
            # print(cmd)
            output = b''
            # output = subprocess.check_output(cmd, shell=True)
            ranges = output_to_ranges(output.decode('utf-8'))
            output_by_pattern_index[patttern_index] = ranges

        # we have the output, but now we need to build the expression
        expression = list(build_boolean_expression(rule))
        print(f'expression: {expression}')
        print(f'output: {output_by_pattern_index}')
        print(f'pattern map: {patterns_with_ids}')
        print('-'*80)
        compiled_result = evaluate_expression(
            expression, output_by_pattern_index)


if __name__ == '__main__':
    main()
