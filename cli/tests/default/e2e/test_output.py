import collections
import json
import re
import shutil
import subprocess
from pathlib import Path
from typing import Dict
from xml.etree import cElementTree

import pytest
from tests.conftest import mask_variable_text
from tests.conftest import RULES_PATH
from tests.conftest import TARGETS_PATH
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat

# coupling: also in test_ci.py
REPO_DIR_NAME = "project_name"


# https://stackoverflow.com/a/10077069
@pytest.mark.kinda_slow
def _etree_to_dict(t):
    """
    A simple and sufficient XML -> dict conversion function. This function is
    used to perform basic XML test data comparisons.
    """
    d: Dict[str, Dict] = {t.tag: {}}
    children = list(t)
    if children:
        dd = collections.defaultdict(list)
        for dc in map(_etree_to_dict, children):
            for k, v in dc.items():
                dd[k].append(v)
        d = {t.tag: {k: v[0] if len(v) == 1 else v for k, v in dd.items()}}
    if t.attrib:
        d[t.tag].update(("@" + k, v) for k, v in t.attrib.items())
    if t.text:
        text = t.text.strip()
        if children or t.attrib:
            if text:
                d[t.tag]["#text"] = text
        else:
            d[t.tag] = text
    return d


@pytest.mark.kinda_slow
def test_output_highlighting(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _errors = run_semgrep_in_tmp(
        "rules/basic.yaml",
        target_name="basic.py",
        output_format=OutputFormat.TEXT,
        strict=False,
        force_color=True,
    )
    snapshot.assert_match(
        results,
        "results.txt",
    )


@pytest.mark.kinda_slow
def test_output_highlighting__no_color(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _errors = run_semgrep_in_tmp(
        "rules/basic.yaml",
        target_name="basic.py",
        output_format=OutputFormat.TEXT,
        strict=False,
        env={"NO_COLOR": "1"},
    )
    snapshot.assert_match(
        results,
        "results.txt",
    )


@pytest.mark.kinda_slow
def test_output_highlighting__force_color_and_no_color(
    run_semgrep_in_tmp: RunSemgrep, snapshot
):
    """
    NO_COLOR would normally disable color: https://no-color.org/

    But a tool specific flag should override a global flag.
    So when both are set, we should have color.
    """
    results, _errors = run_semgrep_in_tmp(
        "rules/basic.yaml",
        target_name="basic.py",
        output_format=OutputFormat.TEXT,
        strict=False,
        force_color=True,
        env={"NO_COLOR": "1"},
    )
    snapshot.assert_match(
        results,
        "results.txt",
    )


@pytest.mark.kinda_slow
def test_yaml_capturing(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _errors = run_semgrep_in_tmp(
        "rules/yaml_capture.yaml",
        target_name="yaml/yaml_capture.yaml",
        output_format=OutputFormat.TEXT,
        strict=False,
    )
    snapshot.assert_match(
        results,
        "results.txt",
    )


# This test is just for making sure that our YAML parser interacts properly
# with metavariables. We don't want to introduce regressions which might
# mess this up.
@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_yaml_metavariables(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, _ = run_semgrep_in_tmp(
        "rules/yaml_key.yaml",
        target_name="yaml/target.yaml",
        output_format=OutputFormat.JSON,
        # we now need to be logged in to access metavariables
        is_logged_in_weak=True,
    )
    parsed_output = json.loads(stdout)
    assert "results" in parsed_output

    for result in parsed_output["results"]:
        value = result["extra"]["metavars"]["$VALUE"]
        content = value["abstract_content"]

        # The message is newline-terminated, probably because
        # of how we parse the "message" field in the rule.
        assert content + "\n" == result["extra"]["message"]

        # The metavariable content should be faithful to the actual
        # given offset information.
        assert len(content) == value["end"]["offset"] - value["start"]["offset"]

    snapshot.assert_match(stdout, "report.json")


@pytest.mark.kinda_slow
def test_quiet_mode_has_empty_stderr(run_semgrep_in_tmp: RunSemgrep, snapshot):
    """
    Test that quiet mode doesn't print anything to stderr.

    This is because some contexts e.g. Kubernetes jobs force-mix stdout and stderr,
    and --quiet is the only way to get valid JSON output in that case.
    """
    stdout, stderr = run_semgrep_in_tmp(
        "rules/yaml_key.yaml",
        target_name="yaml/target.yaml",
        output_format=OutputFormat.JSON,
        options=["--quiet"],
    )
    assert stderr == ""
    json.loads(stdout)  # stdout must be parseable JSON


# junit-xml is tested in a test_junit_xml_output due to ambiguous XML attribute ordering
@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "format",
    ["--json", "--emacs", "--vim", "--sarif", "--gitlab-sast", "--gitlab-secrets"],
)
def test_output_format(run_semgrep_in_tmp: RunSemgrep, snapshot, format):
    stdout, _ = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        target_name="basic/stupid.py",
        options=[format],
        output_format=OutputFormat.TEXT,  # Not the real output format; just disables JSON parsing
    )
    snapshot.assert_match(stdout, "results.out")


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_additional_outputs(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, _ = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        target_name="basic/stupid.py",
        options=[
            "--json-output=one.json",
            "--json-output=two.json",
            "--emacs-output=emacs.txt",
            "--vim-output=vim.txt",
            "--sarif-output=sarif.json",
            "--gitlab-sast-output=gitlab_sast.json",
            "--gitlab-secrets-output=gitlab_secrets.json",
        ],
        output_format=OutputFormat.TEXT,
    )

    snapshot.assert_match(stdout, "text.expected")
    with open("one.json") as one_json:
        snapshot.assert_match(mask_variable_text(one_json.read()), "one.json.expected")
    with open("two.json") as two_json:
        snapshot.assert_match(mask_variable_text(two_json.read()), "two.json.expected")
    with open("emacs.txt") as emacs_txt:
        snapshot.assert_match(
            mask_variable_text(emacs_txt.read()), "emacs.txt.expected"
        )
    with open("vim.txt") as vim_txt:
        snapshot.assert_match(mask_variable_text(vim_txt.read()), "vim.txt.expected")
    with open("gitlab_sast.json") as gitlab_sast_json:
        snapshot.assert_match(
            mask_variable_text(gitlab_sast_json.read()), "gitlab_sast.json.expected"
        )
    with open("gitlab_secrets.json") as gitlab_secrets_json:
        snapshot.assert_match(
            mask_variable_text(gitlab_secrets_json.read()),
            "gitlab_secrets.json.expected",
        )


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "format",
    ["--json", "--emacs", "--vim", "--sarif", "--gitlab-sast", "--gitlab-secrets"],
)
@pytest.mark.osemfail
def test_additional_outputs_with_format_flag(
    run_semgrep_in_tmp: RunSemgrep, snapshot, format
):
    stdout, _ = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        target_name="basic/stupid.py",
        options=[
            format,
            "--json-output=result.json",
        ],
        output_format=OutputFormat.TEXT,
    )

    snapshot.assert_match(stdout, "result.expected")
    with open("result.json") as result_json:
        snapshot.assert_match(
            mask_variable_text(result_json.read()), "result.json.expected"
        )


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "format",
    ["--json", "--emacs", "--vim", "--sarif", "--gitlab-sast", "--gitlab-secrets"],
)
@pytest.mark.osemfail
def test_additional_outputs_with_format_output_flag(
    run_semgrep_in_tmp: RunSemgrep, snapshot, format
):
    stdout, _ = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        target_name="basic/stupid.py",
        options=[format, "--sarif-output=sarif.json", "--output=result.out"],
        output_format=OutputFormat.TEXT,  # disables json parsing
    )

    with open("sarif.json") as sarif_json:
        snapshot.assert_match(
            mask_variable_text(sarif_json.read()), "sarif.json.expected"
        )
    with open("result.out") as result_out:
        snapshot.assert_match(
            mask_variable_text(result_out.read()), "result.out.expected"
        )


@pytest.mark.kinda_slow
def test_long_rule_id(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, _ = run_semgrep_in_tmp(
        "rules/long_rule_id.yaml",
        target_name="basic.py",
        output_format=OutputFormat.TEXT,
    )
    snapshot.assert_match(stdout, "results.out")


@pytest.mark.kinda_slow
@pytest.mark.osemfail  # TODO: fix text wrapping of findings
def test_long_rule_id_long_text(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, _ = run_semgrep_in_tmp(
        "rules/long_rule_id.yaml",
        target_name="long_text.py",
        output_format=OutputFormat.TEXT,
    )
    snapshot.assert_match(stdout, "results.out")


# it should not report findings from rules using the "INVENTORY" severity
@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_omit_inventory(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, _ = run_semgrep_in_tmp(
        "rules/severity_inventory.yaml", target_name="basic.py"
    )
    snapshot.assert_match(stdout, "results.out")


# it should not report findings from rules using the "EXPERIMENT" severity
@pytest.mark.kinda_slow
def test_omit_experiment(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, _ = run_semgrep_in_tmp(
        "rules/severity_experiment.yaml",
        target_name="basic.py",
    )
    snapshot.assert_match(stdout, "results.out")


@pytest.mark.kinda_slow
def test_junit_xml_output(run_semgrep_in_tmp: RunSemgrep, snapshot):
    output, _ = run_semgrep_in_tmp(
        "rules/eqeq.yaml", output_format=OutputFormat.JUNIT_XML
    )
    result = _etree_to_dict(cElementTree.XML(output))

    filename = snapshot.snapshot_dir / "results.xml"
    expected = _etree_to_dict(cElementTree.XML(filename.read_text()))

    assert result == expected


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_junit_xml_output_flag(
    run_semgrep_in_tmp: RunSemgrep,
    snapshot,
):
    stdout, _ = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        target_name="basic/stupid.py",
        options=["--junit-xml-output=result.xml"],
        output_format=OutputFormat.TEXT,  # disables json parsing
    )

    with open("result.xml") as xml:
        snapshot.assert_match(mask_variable_text(xml.read()), "expected.xml")


@pytest.mark.kinda_slow
def test_json_output_with_dataflow_traces(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/taint_trace.yaml",
            target_name="taint/taint_trace.cpp",
            output_format=OutputFormat.JSON,
            options=["--dataflow-traces"],
        ).stdout,
        "results.json",
    )


IGNORE_LOG_REPORT_FIRST_LINE = "Some files were skipped or only partially analyzed."
IGNORE_LOG_REPORT_LAST_LINE = (
    "  For a full list of skipped files, run semgrep with the --verbose flag."
)


# TODO: remove this test: too many things being tested at once, too hard
#       to debug.
#
# pysemgrep/osemgrep status: osemgrep reports 2 more files that are being
# excluded. They're excluded in both implementations.
@pytest.mark.kinda_slow
@pytest.mark.pysemfail
def test_semgrepignore_ignore_log_report(
    run_semgrep_on_copied_files: RunSemgrep, tmp_path, snapshot
):
    shutil.copyfile(
        Path(TARGETS_PATH / "ignores" / ".semgrepignore"), tmp_path / ".semgrepignore"
    )
    # See remarks in test_ignores.py:
    shutil.copyfile(
        Path(TARGETS_PATH / "ignores" / ".gitignore"), tmp_path / ".gitignore"
    )

    _, stderr = run_semgrep_on_copied_files(
        "rules/eqeq-basic.yaml",
        # This set of options is carefully crafted
        # to trigger one entry for most ignore reasons.
        # Note that the print order is non-deterministic,
        # so you must take care not to have two skips in a category.
        options=[
            "--include=ignore.*",
            "--include=tests",
            "--include=find.*",
            "--exclude=*.min.js",
            "--max-target-bytes=100",
            "--verbose",
        ],
        output_format=OutputFormat.TEXT,
        force_color=True,
        target_name="ignores",
    )

    report = re.search(
        f"^{IGNORE_LOG_REPORT_FIRST_LINE}$.*?^{IGNORE_LOG_REPORT_LAST_LINE}$",
        stderr,
        flags=re.MULTILINE | re.DOTALL,
    )
    assert (
        report is not None
    ), "can't find ignore log report based on expected start and end lines"
    snapshot.assert_match(report.group(), "report.txt")


# Tolerate a different snapshot with pysemgrep than osemgrep.
@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_semgrepignore_ignore_log_report_pysemgrep(
    run_semgrep_on_copied_files: RunSemgrep, tmp_path, snapshot
):
    test_semgrepignore_ignore_log_report(
        run_semgrep_on_copied_files, tmp_path, snapshot
    )


# TODO: remove this test: too many things being tested at once, too hard
#       to review when something changes.
#
# pysemgrep/osemgrep status: osemgrep reports 2 more files that are being
# excluded. They're excluded in both implementations.
@pytest.mark.kinda_slow
@pytest.mark.pysemfail
def test_semgrepignore_ignore_log_json_report(
    run_semgrep_on_copied_files: RunSemgrep, tmp_path, snapshot
):
    shutil.copyfile(
        Path(TARGETS_PATH / "ignores" / ".semgrepignore"), tmp_path / ".semgrepignore"
    )
    # See remarks in test_ignores.py:
    shutil.copyfile(
        Path(TARGETS_PATH / "ignores" / ".gitignore"), tmp_path / ".gitignore"
    )

    stdout, _ = run_semgrep_on_copied_files(
        "rules/eqeq-basic.yaml",
        # This set of options is carefully crafted
        # to trigger one entry for most ignore reasons.
        # Note that the print order is non-deterministic,
        # so you must take care not to have two skips in a category.
        options=[
            "--include=ignore.*",
            "--include=tests",
            "--include=find.*",
            "--exclude=*.min.js",
            "--max-target-bytes=100",
            "--verbose",
        ],
        output_format=OutputFormat.JSON,
        target_name="ignores",
    )
    parsed_output = json.loads(stdout)
    assert "paths" in parsed_output

    snapshot.assert_match(
        json.dumps(parsed_output["paths"], indent=2, sort_keys=True), "report.json"
    )


# Tolerate a different snapshot with pysemgrep than osemgrep.
@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_semgrepignore_ignore_log_json_report_pysemgrep(
    run_semgrep_on_copied_files: RunSemgrep, tmp_path, snapshot
):
    test_semgrepignore_ignore_log_json_report(
        run_semgrep_on_copied_files, tmp_path, snapshot
    )


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "git_repo",
    [True, False],
)
@pytest.mark.osemfail
def test_git_repo_output(
    run_semgrep: RunSemgrep, git_repo, tmp_path, monkeypatch, snapshot
):
    """
    Initialize a git repo at a temp directory
    """
    repo_base = tmp_path / REPO_DIR_NAME
    repo_base.mkdir(parents=True)

    monkeypatch.chdir(repo_base)

    if git_repo:
        # Initialize State
        subprocess.run(["git", "init"], check=True, capture_output=True)
        # Symlink the gitignore to the temp directory
        (repo_base / ".gitignore").symlink_to(
            Path(TARGETS_PATH / "ignores" / ".gitignore").resolve()
        )

    # Symlink rules
    (tmp_path / "rules").symlink_to(RULES_PATH.resolve())

    monkeypatch.chdir(tmp_path)
    snapshot.assert_match(
        run_semgrep(
            "rules/eqeq-basic.yaml",
            output_format=OutputFormat.TEXT,
            assume_targets_dir=False,
            target_name=repo_base,
        ).stderr,
        "results.txt",
    )


# This is currently not passing because the loc field in the explanation
# differs between pysemgrep and osemgrep because it's a location in the rule
# (not in the target), and pysemgrep passes a preprocessed rule file to
# semgrep-core hence the mistmatch.
@pytest.mark.slow
@pytest.mark.osemfail
def test_output_matching_explanations(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, _ = run_semgrep_in_tmp(
        "rules/eqeq-basic.yaml",
        target_name="basic/stupid.js",
        options=["--matching-explanations"],
        output_format=OutputFormat.JSON,  # Not the real output format; just disables JSON parsing
    )
    snapshot.assert_match(stdout, "report.json")


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "target_dir",
    ["multilangproj", "language-filtering", "exclude_include"],
)
@pytest.mark.osemfail
def test_file_count_multifile(run_semgrep_in_tmp: RunSemgrep, snapshot, target_dir):
    _, stderr = run_semgrep_in_tmp(
        "rules/filecount.yaml",
        output_format=OutputFormat.TEXT,
        target_name=target_dir,
        options=[],
    )
    snapshot.assert_match(stderr, "result.out")


@pytest.mark.slow
@pytest.mark.osemfail
def test_output_truncated_messages(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, _ = run_semgrep_in_tmp(
        "rules/eqeq-basic-c.yaml",
        target_name="bad/invalid_c_long.c",
        output_format=OutputFormat.JSON,
        assert_exit_code=3,
    )
    snapshot.assert_match(stdout, "report.json")
    # NOTE if we display these in text mode then we should also test that
