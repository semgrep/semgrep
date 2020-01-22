#!/usr/bin/env python3
import os
import pathlib
import subprocess
import sys
import tempfile
import traceback
from pathlib import PurePath

import click
import yaml

# validate input yaml files

# validate patterns inside yaml files

# glob yaml files into a single rule files, adjusting check ids
MUST_HAVE_KEYS = set(['id', 'pattern', 'message', 'languages', 'severity'])
YML_EXTENSIONS = ['.yml', '.yaml']

SGREP_PATH = "sgrep"


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


if __name__ == '__main__':
    main()
