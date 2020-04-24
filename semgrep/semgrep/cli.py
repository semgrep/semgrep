#!/usr/bin/env python3
import argparse
import sys

import semgrep.config_resolver
import semgrep.semgrep_main
import semgrep.test
from semgrep.constants import DEFAULT_CONFIG_FILE
from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.constants import RCE_RULE_FLAG
from semgrep.constants import SGREP_URL
from semgrep.dump_ast import dump_parsed_ast
from semgrep.util import print_error
from semgrep.util import print_error_exit

__VERSION__ = "0.5.0-dev.2"


def cli() -> None:
    parser = argparse.ArgumentParser(
        description=f"semgrep CLI. For more information about semgrep, go to {SGREP_URL}",
        prog="semgrep",
    )

    # input
    parser.add_argument(
        "target",
        nargs="*",
        default=["."],
        help="Files to search (by default, entire current working directory searched). Implied argument if piping to semgrep.",
    )

    # config options
    config = parser.add_argument_group("config")
    config_ex = config.add_mutually_exclusive_group()
    config_ex.add_argument(
        "-g",
        "--generate-config",
        help=f"Generte starter {DEFAULT_CONFIG_FILE}",
        action="store_true",
    )

    config_ex.add_argument(
        "-f",
        "--config",
        help=f"Config YAML file or directory of YAML files ending in .yml|.yaml, OR URL of a config file, OR semgrep registry entry name. See the README for semgrep for information on config file format.",
    )

    config_ex.add_argument("-e", "--pattern", help="semgrep pattern")
    config.add_argument(
        "-l",
        "--lang",
        help="Parses pattern and all files in specified language. Must be used with -e/--pattern.",
    )
    config.add_argument(
        "--validate",
        help=f"Validate config file(s). No search is performed.",
        action="store_true",
    )
    config.add_argument(
        "--strict",
        help=f"only invoke semgrep if config(s) are valid",
        action="store_true",
    )

    parser.add_argument(
        "--include",
        action="append",
        default=[],
        help="Scan only files with this filename, such as --include='*.js' Add multiple times to include multiple patterns.",
    )
    parser.add_argument(
        "--exclude",
        action="append",
        default=[],
        help="Don't scan files with this filename, such as --exclude='*.js'. Add multiple times to exclude multiple patterns.",
    )
    parser.add_argument(
        "--exclude-dir",
        action="append",
        default=[],
        help="Don't scan these directories, such as --exclude-dir='tests'. Add multiple times to exclude multiple patterns.",
    )

    config.add_argument(
        RCE_RULE_FLAG,
        help=f"DANGEROUS: allow rules to run arbitrary code: ONLY ENABLE IF YOU TRUST THE SOURCE OF ALL RULES IN YOUR CONFIG.",
        action="store_true",
    )

    config.add_argument(
        "--exclude-tests",
        help=f"try to exclude tests, documentation, and examples (based on filename/path)",
        action="store_true",
    )
    config.add_argument("--precommit", help=argparse.SUPPRESS, action="store_true")

    # output options
    output = parser.add_argument_group("output")

    output.add_argument(
        "-q",
        "--quiet",
        help="Do not print anything to stdout. Search results can still be saved to an output file specified by -o/--output. Exit code provides success status.",
        action="store_true",
    )

    output.add_argument(
        "--no-rewrite-rule-ids",
        help="Do not rewrite rule ids when they appear in nested subfolders (by default, rule 'foo' in test/rules.yaml will be renamed 'test.foo')",
        action="store_true",
    )

    output.add_argument(
        "-o",
        "--output",
        help="Save search results to a file or post to URL. Default is to print to stdout.",
    )
    output.add_argument(
        "--json", help="Convert search output to JSON format.", action="store_true"
    )
    output.add_argument("--test", help="Run a test suite", action="store_true")
    parser.add_argument(
        "--test-ignore-todo",
        help="Ignore rules marked as #todoruleid: in test files",
        action="store_true",
    )
    output.add_argument(
        "--r2c",
        help="output json in r2c platform format (https://app.r2c.dev)",
        action="store_true",
    )
    output.add_argument(
        "--dump-ast",
        help="show AST of the input file or passed expression and then exit (can use --json)",
        action="store_true",
    )
    output.add_argument(
        "--error",
        help="System Exit 1 if there are findings. Useful for CI and scripts.",
        action="store_true",
    )

    output.add_argument(
        "-a",
        "--autofix",
        help="Apply the autofix patches. WARNING: data loss can occur with this flag. Make sure your files are stored in a version control system.",
        action="store_true",
    )

    # logging options
    logging = parser.add_argument_group("logging")

    logging.add_argument(
        "-v",
        "--verbose",
        help=f"Sets the logging level to verbose. E.g. statements about which files are being processed will be printed.",
        action="store_true",
    )

    parser.add_argument(
        "--version", help="Show the version and exit.", action="store_true"
    )

    ### Parse and validate
    args = parser.parse_args()
    if args.version:
        print(__VERSION__)
        sys.exit(0)

    if args.pattern and not args.lang:
        parser.error("-e/--pattern and -l/--lang must both be specified")

    # set the flags
    semgrep.util.set_flags(args.verbose, args.quiet)

    # change cwd if using docker
    semgrep.config_resolver.adjust_for_docker(args.precommit)

    try:
        if args.dump_ast:
            if not args.lang:
                print_error_exit("language must be specified to dump ASTs")
            else:
                dump_parsed_ast(args.json, args.lang, args.pattern, args.target)
        elif args.validate:
            _, invalid_configs = semgrep.semgrep_main.get_config(args)
            if invalid_configs:
                print_error_exit(
                    f"run with --validate and there were {len(invalid_configs)} errors loading configs"
                )
            else:
                print_error("Config is valid")

        elif args.test:
            semgrep.test.test_main(args)
        else:
            semgrep.semgrep_main.main(args)
    except NotImplementedError as ex:
        print_error_exit(
            f"semgrep encountered an error: {ex}; this is not your fault. {PLEASE_FILE_ISSUE_TEXT}"
        )
