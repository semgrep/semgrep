#!/usr/bin/env python3
import argparse
import multiprocessing
import os

import semgrep.config_resolver
import semgrep.semgrep_main
import semgrep.test
from semgrep.constants import __VERSION__
from semgrep.constants import DEFAULT_CONFIG_FILE
from semgrep.constants import RCE_RULE_FLAG
from semgrep.constants import SEMGREP_URL
from semgrep.dump_ast import dump_parsed_ast
from semgrep.error import SemgrepError
from semgrep.util import print_msg


try:
    CPU_COUNT = multiprocessing.cpu_count()
except NotImplementedError:
    CPU_COUNT = 1  # CPU count is not implemented on Windows


def cli() -> None:
    parser = argparse.ArgumentParser(
        description=f"semgrep CLI. For more information about semgrep, go to {SEMGREP_URL}",
        prog="semgrep",
    )

    # input
    parser.add_argument(
        "target",
        nargs="*",
        default=[os.curdir],
        help=(
            "Search these files or directories. Defaults to entire current "
            "working directory. Implied argument if piping to semgrep."
        ),
    )

    # config options
    config = parser.add_argument_group("config")
    config_ex = config.add_mutually_exclusive_group()
    config_ex.add_argument(
        "-g",
        "--generate-config",
        action="store_true",
        help=f"Generate starter configuration file, {DEFAULT_CONFIG_FILE}",
    )

    config_ex.add_argument(
        "-f",
        "--config",
        help=(
            "YAML configuration file, directory of YAML files ending in "
            ".yml|.yaml, URL of a configuration file, or semgrep registry entry "
            "name. See README for information on configuration file format."
        ),
    )

    config_ex.add_argument(
        "-e",
        "--pattern",
        help="Code search pattern. See README for information on pattern features.",
    )
    config.add_argument(
        "-l",
        "--lang",
        help=(
            "Parse pattern and all files in specified language. Must be used "
            "with -e/--pattern."
        ),
    )
    config.add_argument(
        "--validate",
        action="store_true",
        help="Validate configuration file(s). No search is performed.",
    )
    config.add_argument(
        "--strict",
        action="store_true",
        help="Only invoke semgrep if configuration files(s) are valid.",
    )

    parser.add_argument(
        "--exclude",
        action="append",
        default=[],
        help="Skip any file with this name; --exclude='*.py' will ignore foo.py as well as src/foo.py. Can add multiple times. Overrides includes.",
    )
    parser.add_argument(
        "--include",
        action="append",
        default=[],
        help="Scan only files with this name, such as --include='*.jsx'. Can add multiple times.",
    )
    parser.add_argument(
        "--exclude-dir",
        action="append",
        default=[],
        help="Skip any directory with this name; --exclude-dir=doc will ignore doc/ as well as src/doc. Can add multiple times. Overrides includes.",
    )
    parser.add_argument(
        "--include-dir",
        action="append",
        default=[],
        help="Scan only directories with this name; --include-dir=doc will include doc/ as well as src/doc. Can add multiple times.",
    )

    config.add_argument(
        RCE_RULE_FLAG,
        action="store_true",
        help=(
            "WARNING: allow rules to run arbitrary code. ONLY ENABLE IF YOU "
            "TRUST THE SOURCE OF ALL RULES IN YOUR CONFIGURATION."
        ),
    )

    config.add_argument(
        "--precommit", action="store_true", help=argparse.SUPPRESS,
    )
    config.add_argument(
        "-j",
        "--jobs",
        action="store",
        type=int,
        default=CPU_COUNT,
        help=(
            "Number of subprocesses to use to run checks in parallel. Defaults "
            "to the number of CPUs on the system."
        ),
    )

    # output options
    output = parser.add_argument_group("output")

    output.add_argument(
        "-q",
        "--quiet",
        action="store_true",
        help=(
            "Do not print anything to stdout. Search results can still be "
            "saved to an output file specified by -o/--output. Exit code "
            "provides success status."
        ),
    )

    output.add_argument(
        "--no-rewrite-rule-ids",
        action="store_true",
        help=(
            "Do not rewrite rule ids when they appear in nested sub-directories "
            "(by default, rule 'foo' in test/rules.yaml will be renamed "
            "'test.foo')."
        ),
    )

    output.add_argument(
        "-o",
        "--output",
        help=(
            "Save search results to a file or post to URL. "
            "Default is to print to stdout."
        ),
    )
    output.add_argument(
        "--json", action="store_true", help="Output results in JSON format."
    )
    output.add_argument(
        "--debugging-json",
        action="store_true",
        help="Output JSON with extra debugging information.",
    )
    output.add_argument(
        "--sarif", action="store_true", help="Output results in SARIF format."
    )
    output.add_argument("--test", action="store_true", help="Run test suite.")
    parser.add_argument(
        "--test-ignore-todo",
        action="store_true",
        help="Ignore rules marked as '#todoruleid:' in test files.",
    )
    output.add_argument(
        "--dump-ast",
        action="store_true",
        help=(
            "Show AST of the input file or passed expression and then exit "
            "(can use --json)."
        ),
    )
    output.add_argument(
        "--error",
        action="store_true",
        help="Exit 1 if there are findings. Useful for CI and scripts.",
    )

    output.add_argument(
        "-a",
        "--autofix",
        action="store_true",
        help=(
            "Apply the autofix patches. WARNING: data loss can occur with this "
            "flag. Make sure your files are stored in a version control system."
        ),
    )

    # logging options
    logging = parser.add_argument_group("logging")

    logging.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help=(
            "Set the logging level to verbose. E.g. statements about which "
            "files are being processed will be printed."
        ),
    )

    parser.add_argument(
        "--version", action="store_true", help="Show the version and exit."
    )

    ### Parse and validate
    args = parser.parse_args()
    if args.version:
        print(__VERSION__)
        return

    if args.pattern and not args.lang:
        parser.error("-e/--pattern and -l/--lang must both be specified")

    if args.dump_ast and not args.lang:
        parser.error("--dump-ast and -l/--lang must both be specified")

    # set the flags
    semgrep.util.set_flags(args.verbose, args.quiet)

    # change cwd if using docker
    semgrep.config_resolver.adjust_for_docker(args.precommit)

    if args.dump_ast:
        dump_parsed_ast(args.json, args.lang, args.pattern, args.target)
    elif args.validate:
        _, invalid_configs = semgrep.semgrep_main.get_config(
            args.pattern, args.lang, args.config
        )
        if invalid_configs:
            raise SemgrepError(
                f"run with --validate and there were {len(invalid_configs)} errors loading configs"
            )
        else:
            print_msg("Config is valid")
    elif args.generate_config:
        semgrep.config_resolver.generate_config()
    elif args.test:
        semgrep.test.test_main(args)
    else:
        semgrep.semgrep_main.main(
            target=args.target,
            pattern=args.pattern,
            lang=args.lang,
            config=args.config,
            no_rewrite_rule_ids=args.no_rewrite_rule_ids,
            jobs=args.jobs,
            include=args.include,
            include_dir=args.include_dir,
            exclude=args.exclude,
            exclude_dir=args.exclude_dir,
            json_format=args.json,
            debugging_json=args.debugging_json,
            sarif=args.sarif,
            output_destination=args.output,
            quiet=args.quiet,
            strict=args.strict,
            exit_on_error=args.error,
            autofix=args.autofix,
            dangerously_allow_arbitrary_code_execution_from_rules=args.dangerously_allow_arbitrary_code_execution_from_rules,
        )
