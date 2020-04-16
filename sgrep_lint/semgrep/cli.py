#!/usr/bin/env python3
import argparse
import sys

import semgrep.config_resolver
import semgrep.sgrep_main
import semgrep.test
from semgrep.constants import DEFAULT_CONFIG_FILE
from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.constants import RCE_RULE_FLAG
from semgrep.constants import SGREP_URL
from semgrep.util import print_error_exit

# import logging

# import click

__VERSION__ = "0.4.9"


# def _is_running_supported_python3() -> bool:
#     python_major_v = sys.version_info.major
#     python_minor_v = sys.version_info.minor
#     logging.info(f"Python version is ({python_major_v}.{python_minor_v})")
#     return python_major_v >= 3 and python_minor_v >= 6

# @click.group(epilog="To get help for a specific command, run `semgrep COMMAND --help`")
# @click.version_option(
#     prog_name="semgrep", version=__VERSION__, message="%(prog)s/%(version)s"
# )
# @click.pass_context
# def cli(ctx: click.Context) -> None:
#     ctx.help_option_names = ["-h", "--help"]

#     if not _is_running_supported_python3():
#         raise OutdatedPythonException()

# @click.command()
# @click.pass_obj
# def test(ctx: click.Context) -> None:
#     print("testing")


# cli.add_command(test)


def cli() -> None:
    parser = argparse.ArgumentParser(
        description=f"sgrep CLI. For more information about sgrep, go to {SGREP_URL}",
        prog="sgrep",  # we have to lie to the user since they know of this as `sgrep`
    )

    # input
    parser.add_argument(
        "target",
        nargs="*",
        default=["."],
        help="Files to search (by default, entire current working directory searched). Implied argument if piping to sgrep.",
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
        help=f"Config YAML file or directory of YAML files ending in .yml|.yaml, OR URL of a config file, OR sgrep registry entry name. See the README for sgrep for information on config file format.",
    )

    config_ex.add_argument("-e", "--pattern", help="sgrep pattern")
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
        help=f"only invoke sgrep if config(s) are valid",
        action="store_true",
    )
    parser.add_argument(
        "--exclude",
        action="append",
        default=[],
        help="Path pattern to exclude. Can be added multiple times to exclude multiple patterns.",
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
        if args.test:
            semgrep.test.test_main(args)
        else:
            semgrep.sgrep_main.main(args)
    except NotImplementedError as ex:
        print_error_exit(
            f"sgrep encountered an error: {ex}; this is not your fault. {PLEASE_FILE_ISSUE_TEXT}"
        )
