# PARTIALLY PORTED TO OCAML. DELETE PARTS AS YOU PORT THEM.

from semgrep.app.version import get_no_findings_msg
from semgrep.notifications import possibly_notify_user
from semgrep.project import get_project_url
from semgrep.target_manager import write_pipes_to_disk

_scan_options: List[Callable] = [

 optgroup.group("Display options"),

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
    gitlab_sast: bool,
    gitlab_secrets: bool,
    junit_xml: bool,
    max_chars_per_line: int,
    max_lines_per_finding: int,
    dataflow_traces: bool,
    sarif: bool,
) -> ScanReturn:

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

    # The 'optional_stdin_target' context manager must remain before
    # 'managed_output'. Output depends on file contents so we cannot have
    # already deleted the temporary stdin file.
    with tempfile.TemporaryDirectory() as pipes_dir:
        # mostly repeating the loop in write_pipes_to_disk to detect if we
        # need --scan-unknown-extensions.
        for t in targets:
            if t == "-" or Path(t).is_fifo():
                logger.debug("stdin or piped targets, adding --scan-unknown-extensions")
                scan_unknown_extensions = True
        targets = write_pipes_to_disk(targets, Path(pipes_dir))
        output_handler = OutputHandler(output_settings)

        if ...:
            ...
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
        version_check()

    if not run_has_findings and enable_version_check:
        msg = get_no_findings_msg()
        # decouple CLI from app - if functionality removed, do not fail
        if msg:
            logger.info(msg)

    return ...
