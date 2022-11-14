# PARTIALLY PORTED TO OCAML. DELETE PARTS AS YOU PORT THEM.

from semgrep import bytesize
from semgrep.app.version import get_no_findings_msg
from semgrep.dump_ast import dump_parsed_ast
from semgrep.notifications import possibly_notify_user
from semgrep.project import get_project_url
from semgrep.target_manager import write_pipes_to_disk

_scan_options: List[Callable] = [

 optgroup.group("Display options"),
    optgroup.option(
        "--enable-nosem/--disable-nosem",
        is_flag=True,
        default=True,
        help="""
            --enable-nosem enables 'nosem'. Findings will not be reported on lines
            containing a 'nosem' comment at the end. Enabled by default.
        """,
    ),
    optgroup.option(
        MAX_CHARS_FLAG_NAME,
        type=int,
        default=DEFAULT_MAX_CHARS_PER_LINE,
        help="Maximum number of characters to show per line.",
    ),
    optgroup.option(
        MAX_LINES_FLAG_NAME,
        type=int,
        default=DEFAULT_MAX_LINES_PER_FINDING,
        help="""
            Maximum number of lines of code that will be shown for each match before
            trimming (set to 0 for unlimited).
        """,
    ),
    optgroup.option(
        "--dataflow-traces",
        is_flag=True,
        help="Explain how non-local values reach the location of a finding (only affects text output).",
    ),
    optgroup.option(
        "-o",
        "--output",
        help="Save search results to a file or post to URL. Default is to print to stdout.",
        shell_complete=__get_file_options,
    ),


  optgroup.group("Verbosity options", cls=MutuallyExclusiveOptionGroup),

    optgroup.group(
        "Output formats",
        cls=MutuallyExclusiveOptionGroup,
        help="Uses ASCII output if no format specified.",
    ),
    optgroup.option(
        "--gitlab-sast",
        is_flag=True,
        help="Output results in GitLab SAST format.",
    ),
    optgroup.option(
        "--gitlab-secrets",
        is_flag=True,
        help="Output results in GitLab Secrets format.",
    ),
    optgroup.option(
        "--junit-xml", is_flag=True, help="Output results in JUnit XML format."
    ),
    optgroup.option("--sarif", is_flag=True, help="Output results in SARIF format."),
]


def scan_options(func: Callable) -> Callable:
    return ...


@click.option(
    "--replacement",
    help="""
        An autofix expression that will be applied to any matches found with --pattern.
        Only valid with a command-line specified pattern.
    """,
)


@optgroup.group("Alternate modes", help="No search is performed in these modes")

@optgroup.option(
    "--validate",
    is_flag=True,
    default=False,
    help="Validate configuration file(s). This will check YAML files for errors and run 'p/semgrep-rule-lints' on the YAML files. No search is performed.",
)


@optgroup.group("Test and debug options")


@optgroup.option("--test", is_flag=True, default=False, help="Run test suite.")
@optgroup.option(
    "--test-ignore-todo/--no-test-ignore-todo",
    is_flag=True,
    default=False,
    help="If --test-ignore-todo, ignores rules marked as '#todoruleid:' in test files.",
)
@optgroup.option(
    "--dump-ast/--no-dump-ast",
    is_flag=True,
    default=False,
    help="""
        If --dump-ast, shows AST of the input file or passed expression and then exit
        (can use --json).
    """,
)
@click.option(
    "--error/--no-error",
    "error_on_findings",
    is_flag=True,
    help="Exit 1 if there are findings. Useful for CI and scripts.",
)

# These flags are deprecated or experimental - users should not
# rely on their existence, or their output being stable
@click.option(
    "--deep",
    "-x",
    is_flag=True,
    hidden=True
    # help="contact support@r2c.dev for more information on this"
)
def scan(
    *,
    deep: bool,
    dump_ast: bool,
    enable_nosem: bool,
    error_on_findings: bool,
    gitlab_sast: bool,
    gitlab_secrets: bool,
    junit_xml: bool,
    max_chars_per_line: int,
    max_lines_per_finding: int,
    dataflow_traces: bool,
    output: Optional[str],
    replacement: Optional[str],
    sarif: bool,
    test: bool,
    test_ignore_todo: bool,
    validate: bool,
) -> ScanReturn:

    state = get_state()
    state.metrics.configure(metrics, metrics_legacy)

    # Note this must be after the call to `terminal.configure` so that verbosity is respected
    possibly_notify_user()

    # change cwd if using docker
    if not targets:
        semgrep.config_resolver.adjust_for_docker()
        targets = (os.curdir,)

    if gitlab_sast:
        output_format = OutputFormat.GITLAB_SAST
    elif gitlab_secrets:
        output_format = OutputFormat.GITLAB_SECRETS
    elif junit_xml:
        output_format = OutputFormat.JUNIT_XML
    elif sarif:
        output_format = OutputFormat.SARIF

    output_settings = OutputSettings(...)

    if test:
        # the test code (which isn't a "test" per se but is actually machinery to evaluate semgrep performance)
        # uses managed_output internally
        semgrep.test.test_main(
            target=targets,
            config=config,
            test_ignore_todo=test_ignore_todo,
            strict=strict,
            json=json,
            optimizations=optimizations,
            deep=deep,
        )

    run_has_findings = False

    # The 'optional_stdin_target' context manager must remain before
    # 'managed_output'. Output depends on file contents so we cannot have
    # already deleted the temporary stdin file.
    with tempfile.TemporaryDirectory() as pipes_dir:
        targets = write_pipes_to_disk(targets, Path(pipes_dir))
        output_handler = OutputHandler(output_settings)

        if dump_ast:
            dump_parsed_ast(json, __validate_lang("--dump-ast", lang), pattern, targets)
        elif validate:
            if not (pattern or lang or config):
                logger.error(
                    f"Nothing to validate, use the --config or --pattern flag to specify a rule"
                )
            else:
                resolved_configs, config_errors = semgrep.config_resolver.get_config(
                    pattern, lang, config or [], project_url=get_project_url()
                )

                # Run metachecks specifically on the config files
                if config:
                    try:
                        metacheck_errors = CoreRunner(
                            jobs=jobs,
                            timeout=timeout,
                            max_memory=max_memory,
                            timeout_threshold=timeout_threshold,
                            optimizations=optimizations,
                        ).validate_configs(config)
                    except SemgrepError as e:
                        metacheck_errors = [e]

                config_errors = list(chain(config_errors, metacheck_errors))

                valid_str = "invalid" if config_errors else "valid"
                rule_count = len(resolved_configs.get_rules(True))
                logger.info(
                    f"Configuration is {valid_str} - found {len(config_errors)} configuration error(s), and {rule_count} rule(s)."
                )
                if config_errors:
                    output_handler.handle_semgrep_errors(config_errors)
                    output_handler.output({}, all_targets=set(), filtered_rules=[])
                    raise SemgrepError("Please fix the above errors and try again.")
        else:
            try:
                (
                    filtered_matches_by_rule,
                    semgrep_errors,
                    all_targets,
                    _,
                    ignore_log,
                    filtered_rules,
                    profiler,
                    profiling_data,
                    _,
                    explanations,
                    _,
                ) = semgrep.semgrep_main.main(...)
            except SemgrepError as e:
                output_handler.handle_semgrep_errors([e])
                output_handler.output({}, all_targets=set(), filtered_rules=[])
                raise e

            output_handler.output(
                filtered_matches_by_rule,
                all_targets=all_targets,
                ignore_log=ignore_log,
                profiler=profiler,
                filtered_rules=filtered_rules,
                profiling_data=profiling_data,
                explanations=explanations,
                print_summary=True,
            )

            run_has_findings = any(filtered_matches_by_rule.values())

    if enable_version_check:
        from semgrep.app.version import version_check

        version_check()

    if not run_has_findings and enable_version_check:
        msg = get_no_findings_msg()
        # decouple CLI from app - if functionality removed, do not fail
        if msg:
            logger.info(msg)

    return ...
