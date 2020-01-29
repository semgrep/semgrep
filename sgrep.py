#!/usr/bin/env python3
import argparse
import collections
import itertools
import json
import os
import pathlib
import subprocess
import sys
import tempfile
import traceback
from dataclasses import dataclass
from pathlib import PurePath
from typing import Any, Dict, Generator, List, Set, Tuple

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
DEBUG = False
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
        print_error(f'YAML file at {file_path} not found')
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
        if not set(rule.keys()).issuperset(MUST_HAVE_KEYS):
            print_error(
                f'{file_path} is missing keys at rule {i+1}{rule_id_err_msg}, must have: {MUST_HAVE_KEYS}')
        elif not 'pattern' in rule and not 'patterns' in rule:
            print_error(
                f'{file_path} is missing key `pattern` or `patterns` at rule {i+1}{rule_id_err_msg}')
        else:
            rules.append(rule)
    return rules


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
    results_for_pattern = results.get(pattern_id, [])
    if operator == OPERATORS.AND:
        # remove all ranges that don't equal the ranges for this pattern
        return ranges_left.intersection(results_for_pattern)
    elif operator == OPERATORS.AND_NOT:
        # remove all ranges that DO equal the ranges for this pattern
        # difference_update = Remove all elements of another set from this set.
        return ranges_left.difference(results_for_pattern)
    elif operator == OPERATORS.AND_INSIDE:
        # remove all ranges (not enclosed by) or (not equal to) the inside ranges
        output_ranges = set()
        for arange in ranges_left:
            for keep_inside_this_range in results_for_pattern:
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
            for keep_inside_this_range in results_for_pattern:
                if keep_inside_this_range.is_enclosing_or_eq(arange):
                    output_ranges.remove(arange)
                    break
        # print(f"after filter `{operator}`: {output_ranges}")
        return output_ranges
    else:
        raise NotImplementedError(f'unknown operator {operator}')


def evaluate_expression(expression, results: Dict[str, List[Range]]) -> List[Range]:
    ranges_left = set(flatten(results.values()))
    for (operator, pattern_ids) in expression:
        if operator == OPERATORS.AND_EITHER:
            # create a set from the union of the expressions in the `or` block
            either_ranges = set(flatten((results.get(pid, []))
                                        for pid in pattern_ids))
            # remove anything that does not equal one of these ranges
            ranges_left.intersection_update(either_ranges)
            # print(f"after filter `{operator}`: {ranges_left}")
        else:
            assert len(
                pattern_ids) == 1, f'only {OPERATORS.AND_EITHER} expressions can have multiple pattern names'
            ranges_left = _evaluate_single_expression(
                operator, pattern_ids[0], results, ranges_left)
    return ranges_left


def print_error(e):
    sys.stderr.write(str(e) + os.linesep)
    sys.stderr.flush()


def parse_sgrep_output(sgrep_findings: List[Dict[str, Any]]) -> Dict[str, List[Range]]:
    output = collections.defaultdict(list)
    for finding in sgrep_findings:
        check_id = finding['check_id']
        pattern_id = int(check_id.split('.')[1])
        output[pattern_id].append(sgrep_finding_to_range(finding))
    return dict(output)


def sgrep_finding_to_range(sgrep_finding: Dict[str, Any]):
    return Range(sgrep_finding['start']['offset'], sgrep_finding['end']['offset'])


def invoke_sgrep(all_rules: List[Dict[str, Any]], target_files_or_dirs: List[str]) -> Dict[str, Any]:
    """Returns parsed json output of sgrep"""
    with tempfile.NamedTemporaryFile('w') as fout:
        # very important not to sort keys here
        yaml_as_str = yaml.safe_dump({'rules': all_rules}, sort_keys=False)
        # print(yaml_as_str)
        fout.write(yaml_as_str)
        fout.flush()
        cmd = [SGREP_PATH, f'-rules_file',
               fout.name, *list(target_files_or_dirs)]
        output = subprocess.check_output(cmd, shell=False)
        output_json = json.loads((output.decode('utf-8')))
        return output_json


def debug_print(msg):
    if DEBUG:
        print(msg)


def rewrite_message_with_metavars(yaml_rule, sgrep_result):
    msg_text = yaml_rule['message']
    if 'metavars' in sgrep_result['extra']:
        for metavar, contents in sgrep_result['extra']['metavars'].items():
            msg_text = msg_text.replace(metavar, contents['abstract_content'])
    return msg_text


def collect_rules(yaml_file_or_dirs: str) -> Tuple[List[Dict[str, Any]], Tuple[int, int]]:
    collected_rules = []
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
                    collected_rules.extend(rules_in_file)
    return collected_rules, (errors, not_errors)


def flatten_rule_patterns(all_rules):
    for rule_index, rule in enumerate(all_rules):
        patterns_with_ids = list(parse_rule_patterns(rule))
        for (pattern_index, pattern) in patterns_with_ids:
            # if we don't copy an array (like `languages`), the yaml file will refer to it by reference (with an anchor)
            # which is nice and all but the sgrep YAML parser doesn't support that
            new_check_id = f'{rule_index}.{pattern_index}'
            yield {'id': new_check_id, 'pattern': pattern,
                   'severity': rule['severity'], 'languages': rule['languages'].copy(), 'message': '<internalonly>'}


def main(yaml_file_or_dirs: str, target_files_or_dirs: List[str], validate: bool, strict: bool):

    if not os.path.exists(yaml_file_or_dirs):
        print(f'path not found: {yaml_file_or_dirs}')
        sys.exit(1)

    all_rules, (errors, not_errors) = list(collect_rules(yaml_file_or_dirs))
    # TODO: validate the rule patterns are ok by invoking sgrep core

    if validate or strict:
        if errors > 0:
            print(
                f'validate flag passed and {errors} YAML files failed to parse, exiting')
            sys.exit(1)
        elif not strict:
            sys.exit(0)

    print_error(
        f'running {len(all_rules)} rules from {not_errors} yaml files ({errors} yaml files were invalid)')

    # a rule can have multiple patterns inside it. Flatten these so we can send sgrep a single yml file list of patterns
    all_patterns = list(flatten_rule_patterns(all_rules))
    output_json = invoke_sgrep(all_patterns, target_files_or_dirs)

    # group output; we want to see all of the same rule ids on the same file path
    by_rule_index = collections.defaultdict(
        lambda: collections.defaultdict(list))
    debug_print(output_json)
    for finding in output_json['matches']:
        rule_index = int(finding['check_id'].split('.')[0])
        by_rule_index[rule_index][finding['path']].append(finding)

    outputs_after_booleans = []
    for rule_index, paths in by_rule_index.items():
        expression = list(build_boolean_expression(all_rules[rule_index]))
        debug_print(f'rule expression: {expression}')
        for filepath, results in paths.items():
            debug_print(
                f"-------- rule (index {rule_index}) {all_rules[rule_index]['id']}------ filepath: {filepath}")
            check_ids_to_ranges = parse_sgrep_output(results)
            debug_print(check_ids_to_ranges)
            valid_ranges_to_output = evaluate_expression(
                expression, check_ids_to_ranges)

            # only output matches which are inside these offsets!
            debug_print(f'compiled result {valid_ranges_to_output}')
            debug_print('-'*80)
            for result in results:
                if sgrep_finding_to_range(result) in valid_ranges_to_output:
                    # restore the original rule ID
                    result['check_id'] = all_rules[rule_index]['id']
                    # restore the original message
                    result['extra']['message'] = rewrite_message_with_metavars(
                        all_rules[rule_index], result)
                    outputs_after_booleans.append(result)

    print(json.dumps({'matches': outputs_after_booleans}))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description="Helper to invoke sgrep with many patterns or files")
    parser.add_argument(
        "yaml_file_or_dirs", help=f"the YAML file or directory of YAML files ending in {YML_EXTENSIONS} with rules")
    parser.add_argument(
        "--validate", help=f"only validate that the YAML files with rules are correctly form, then exit 0 if ok", action='store_true')
    parser.add_argument(
        "--strict", help=f"only invoke sgrep if all YAML files are valid", action='store_true')
    parser.add_argument("target_files_or_dirs", nargs='+')
    args = parser.parse_args()
    main(args.yaml_file_or_dirs, args.target_files_or_dirs,
         args.validate, args.strict)
