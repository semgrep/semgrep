#!/usr/bin/env python3
import argparse
import logging
import multiprocessing
import os

import semgrep.config_resolver
import semgrep.semgrep_main
import semgrep.test
from semgrep import __VERSION__
from semgrep.constants import DEFAULT_CONFIG_FILE
from semgrep.constants import OutputFormat
from semgrep.constants import RCE_RULE_FLAG
from semgrep.constants import SEMGREP_URL
from semgrep.dump_ast import dump_parsed_ast
from semgrep.error import SemgrepError
from semgrep.output import managed_output
from semgrep.output import OutputSettings
from semgrep.synthesize_patterns import synthesize_patterns
from semgrep.target_manager import optional_stdin_target
from semgrep.version import is_running_latest

logger = logging.getLogger(__name__)
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
        action="append",
        default=[],
        help=(
            "YAML configuration file, directory of YAML files ending in "
            ".yml|.yaml, URL of a configuration file, or semgrep registry entry "
            "name. See https://semgrep.dev/docs/writing-rules/rule-syntax for information on configuration file format."
        ),
    )

    config_ex.add_argument(
        "-e",
        "--pattern",
        help="Code search pattern. See https://semgrep.dev/docs/writing-rules/pattern-syntax for information on pattern features.",
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
        help="Skip any file or directory that matches this pattern; --exclude='*.py' will ignore"
        " the following: foo.py, src/foo.py, foo.py/bar.sh. --exclude='tests' will ignore tests/foo.py"
        " as well as a/b/tests/c/foo.py. Can add multiple times. Overrides includes.",
    )
    parser.add_argument(
        "--include",
        action="append",
        default=[],
        help="Filter files or directories by path. The argument is a"
        " glob-style pattern such as 'foo.*' that must match the path."
        " This is an extra filter in addition to other applicable filters."
        " For example, specifying the language with '-l javascript' might"
        " preselect files 'src/foo.jsx' and 'lib/bar.js'. Specifying one of"
        " '--include=src', '--include=*.jsx', or '--include=src/foo.*'"
        " will restrict the selection to the single file 'src/foo.jsx'."
        " A choice of multiple '--include' patterns can be specified."
        " For example, '--include=foo.* --include=bar.*' will select"
        " both 'src/foo.jsx' and 'lib/bar.js'."
        " Glob-style patterns follow the syntax supported by python,"
        " which is documented at https://docs.python.org/3/library/glob.html",
    )
    parser.add_argument(
        "--no-git-ignore",
        action="store_true",
        help="Don't skip files ignored by git."
        " Scanning starts from the root folder specified on the semgrep"
        " command line."
        " Normally, if the scanning root is within a git repository, "
        " only the tracked files and the new files"
        " would be scanned. Git submodules and git-ignored files would"
        " normally be skipped."
        " This option will disable git-aware filtering."
        " Setting this flag does nothing if the scanning root is not"
        " in a git repository.",
    )
    parser.add_argument(
        "--skip-unknown-extensions",
        action="store_true",
        help="Scan only known file extensions, even if unrecognized ones are explicitly targeted.",
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

    config.add_argument(
        "--timeout",
        type=int,
        default=0,
        help=(
            "Maximum time to spend running a rule on a single file in seconds. If set to 0 will not have time limit. Defaults to 0."
        ),
    )

    config.add_argument(
        "--max-memory",
        type=int,
        default=0,
        help=(
            "Maximum memory to use running a rule on a single file in MB. If set to 0 will not have memory limit. Defaults to 0."
        ),
    )

    config.add_argument(
        "--timeout-threshold",
        type=int,
        default=0,
        help=(
            "Maximum number of rules that can timeout on a file before the file is skipped. If set to 0 will not have limit. Defaults to 0."
        ),
    )

    # output options
    output = parser.add_argument_group("output")

    output.add_argument(
        "-q",
        "--quiet",
        action="store_true",
        help=(
            "Do not print any logging messages to stderr. Finding output will still be sent to stdout. Exit code "
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
        "--save-test-output-tar",
        action="store_true",
        help=(
            "Store json output as a tarball that will be uploaded as a Github artifact."
        ),
    )
    output.add_argument(
        "--debugging-json",
        action="store_true",
        help="Output JSON with extra debugging information.",
    )
    output.add_argument(
        "--junit-xml", action="store_true", help="Output results in JUnit XML format."
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
    output.add_argument("--synthesize-patterns", help=argparse.SUPPRESS)
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
    output.add_argument(
        "--dryrun",
        action="store_true",
        help=(
            "Do autofixes, but don't write them to a file. "
            "This will print the changes to the console. "
            "This lets you see the changes before you commit to them. "
            "Only works with the --autofix flag. Otherwise does nothing."
        ),
    )
    output.add_argument(
        "--disable-nosem",
        action="store_true",
        help=(
            "Disable the effect of 'nosem'. This will report findings on lines "
            "containing a 'nosem' comment at the end."
        ),
    )

    # logging options
    logging_ = parser.add_argument_group("logging")

    logging_.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help=(
            "Show more details about what rules are running, which files failed to parse, etc."
        ),
    )

    output.add_argument(
        "--debug", action="store_true", help="Set the logging level to DEBUG",
    )

    parser.add_argument(
        "--version", action="store_true", help="Show the version and exit."
    )

    parser.add_argument(
        "--force-color",
        action="store_true",
        help="Always include ANSI color in the output, even if not writing to a TTY",
    )
    parser.add_argument(
        "--disable-version-check",
        action="store_true",
        help="Disable checking for latest version.",
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
    semgrep.util.set_flags(args.debug, args.quiet, args.force_color)

    # change cwd if using docker
    try:
        semgrep.config_resolver.adjust_for_docker()
    except SemgrepError as e:
        logger.exception(str(e))
        raise e

    output_format = OutputFormat.TEXT
    if args.json:
        output_format = OutputFormat.JSON
    elif args.debugging_json:
        output_format = OutputFormat.JSON_DEBUG
    elif args.junit_xml:
        output_format = OutputFormat.JUNIT_XML
    elif args.sarif:
        output_format = OutputFormat.SARIF

    output_settings = OutputSettings(
        output_format=output_format,
        output_destination=args.output,
        error_on_findings=args.error,
        strict=args.strict,
        verbose_errors=args.verbose,
        timeout_threshold=args.timeout_threshold,
    )

    if not args.disable_version_check:
        if not is_running_latest():
            logger.warning(
                "A new version of Semgrep is available. Please see https://github.com/returntocorp/semgrep#upgrading for more information."
            )

    if args.test:
        # the test code (which isn't a "test" per se but is actually machinery to evaluate semgrep performance)
        # uses managed_output internally
        semgrep.test.test_main(args)

    # The 'optional_stdin_target' context manager must remain before
    # 'managed_output'. Output depends on file contents so we cannot have
    # already deleted the temporary stdin file.
    with optional_stdin_target(args.target) as target, managed_output(
        output_settings
    ) as output_handler:
        if args.dump_ast:
            dump_parsed_ast(args.json, args.lang, args.pattern, target)
        elif args.synthesize_patterns:
            synthesize_patterns(args.lang, args.synthesize_patterns, target)
        elif args.validate:
            configs, config_errors = semgrep.semgrep_main.get_config(
                args.pattern, args.lang, args.config
            )
            valid_str = "invalid" if config_errors else "valid"
            rule_count = len(configs.get_rules(True))
            logger.info(
                f"Configuration is {valid_str} - found {len(configs.valid)} valid configuration(s), {len(config_errors)} configuration error(s), and {rule_count} rule(s)."
            )
            if config_errors:
                for error in config_errors:
                    output_handler.handle_semgrep_error(error)
                raise SemgrepError("Please fix the above errors and try again.")
        elif args.generate_config:
            semgrep.config_resolver.generate_config()
        else:
            semgrep.semgrep_main.main(
                output_handler=output_handler,
                target=target,
                pattern=args.pattern,
                lang=args.lang,
                configs=args.config,
                no_rewrite_rule_ids=args.no_rewrite_rule_ids,
                jobs=args.jobs,
                include=args.include,
                exclude=args.exclude,
                strict=args.strict,
                autofix=args.autofix,
                dryrun=args.dryrun,
                disable_nosem=args.disable_nosem,
                dangerously_allow_arbitrary_code_execution_from_rules=args.dangerously_allow_arbitrary_code_execution_from_rules,
                no_git_ignore=args.no_git_ignore,
                timeout=args.timeout,
                max_memory=args.max_memory,
                timeout_threshold=args.timeout_threshold,
                skip_unknown_extensions=args.skip_unknown_extensions,
            )
